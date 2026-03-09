package Hyacine

import chisel3._
import chisel3.util._
import SwitchNetwork._

class HyacineWriteReq(val n: Int, val dataWidth: Int, val addrWidth: Int) extends Bundle {
    val baseAddr   = UInt(addrWidth.W)
    val logicalIdx = UInt(log2Up(n).W)
    val data       = Vec(n, UInt(dataWidth.W))
}

class HyacineReadReq(val n: Int, val addrWidth: Int) extends Bundle {
    val mode       = Bool() // 0: Original matrix (Row), 1: Transposed matrix (Col)
    val baseAddr   = UInt(addrWidth.W)
    val logicalIdx = UInt(log2Up(n).W)
}

class HyacineResp(val n: Int, val dataWidth: Int) extends Bundle {
    val data = Vec(n, UInt(dataWidth.W))
}

class Hyacine(val n: Int = 8, val dataWidth: Int = 32, val addrWidth: Int = 16, val depth: Int = 1024) extends Module {
    require(isPow2(n), "n must be a power of 2")

    val io = IO(new Bundle {
        val writeReq = Flipped(Decoupled(new HyacineWriteReq(n, dataWidth, addrWidth)))
        val readReq  = Flipped(Decoupled(new HyacineReadReq(n, addrWidth)))
        val resp     = Decoupled(new HyacineResp(n, dataWidth))
    })

    val banks = Seq.fill(n)(SyncReadMem(depth, UInt(dataWidth.W)))

    val selRom = VecInit((0 until n).map { shift =>
        val pi     = Array.tabulate(n)(b => (b - shift + n) % n)
        val config = BenesRouter.generateConfig(n, pi)
        VecInit(config.toIndexedSeq.map(stage => VecInit(stage.toIndexedSeq.map(b => b.B))))
    })

    val mask = (n - 1).U

    io.writeReq.ready := true.B
    val wValid = io.writeReq.valid
    val wBase  = io.writeReq.bits.baseAddr
    val wIdx   = io.writeReq.bits.logicalIdx
    val wData  = io.writeReq.bits.data

    val wAddr = wBase + wIdx

    val writeDataVec = Wire(Vec(n, UInt(dataWidth.W)))
    for (b <- 0 until n) {
        val inIdx = (b.U - wIdx)(log2Up(n) - 1, 0)
        writeDataVec(b) := wData(inIdx)

        when(wValid) {
            banks(b).write(wAddr, writeDataVec(b))
        }
    }

    val rValid = io.readReq.valid
    val rMode  = io.readReq.bits.mode
    val rBase  = io.readReq.bits.baseAddr
    val rIdx   = io.readReq.bits.logicalIdx

    val validReg       = RegInit(false.B)
    val logicalIdxReg  = RegInit(0.U(log2Up(n).W))
    val hitReg         = RegInit(VecInit(Seq.fill(n)(false.B)))
    val forwardDataReg = RegInit(VecInit(Seq.fill(n)(0.U(dataWidth.W))))

    val outQueue = Module(new Queue(new HyacineResp(n, dataWidth), 4))

    val occupiedSlots = outQueue.io.count + validReg.asUInt
    io.readReq.ready := occupiedSlots < 4.U

    val rFire = io.readReq.fire

    val sramReadData = Wire(Vec(n, UInt(dataWidth.W)))

    for (b <- 0 until n) {
        val rAddr = Wire(UInt(addrWidth.W))
        when(rMode === 0.B) {
            rAddr := rBase + rIdx
        }.otherwise {
            val rowIdx = (b.U - rIdx)(log2Up(n) - 1, 0)
            rAddr := rBase + rowIdx
        }

        sramReadData(b) := banks(b).read(rAddr, rFire)

        val hit = wValid && rFire && (wAddr === rAddr)

        when(rFire) {
            hitReg(b)         := hit
            forwardDataReg(b) := writeDataVec(b)
        }
    }

    validReg := rFire
    when(rFire) {
        logicalIdxReg := rIdx
    }

    val actualReadData = Wire(Vec(n, UInt(dataWidth.W)))
    for (b <- 0 until n) {
        actualReadData(b) := Mux(hitReg(b), forwardDataReg(b), sramReadData(b))
    }

    val benes = Module(new Benes(UInt(dataWidth.W), n, 0))
    benes.io.in  := actualReadData
    benes.io.sel := selRom(logicalIdxReg)

    outQueue.io.enq.valid     := validReg
    outQueue.io.enq.bits.data := benes.io.out

    io.resp <> outQueue.io.deq
}
