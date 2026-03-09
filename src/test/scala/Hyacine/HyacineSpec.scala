package Hyacine

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HyacineSpec extends AnyFlatSpec with Matchers {
    behavior of "Hyacine SRAM Subsystem"

    val N         = 4 // Using 4x4 matrix for simplicity in tests
    val dataWidth = 32
    val addrWidth = 16

    it should "correctly write rows and read original rows (Mode 0)" in {
        simulate(new Hyacine(N, dataWidth, addrWidth)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)

            dut.io.writeReq.valid.poke(false.B)
            dut.io.readReq.valid.poke(false.B)
            dut.io.resp.ready.poke(true.B)

            val matrix   = Array.tabulate(N, N)((i, j) => (i + 1) * 10 + j + 1) // 11,12.. 21,22..
            val baseAddr = 0x100

            // Phase 1: Write all rows
            for (i <- 0 until N) {
                dut.io.writeReq.valid.poke(true.B)
                dut.io.writeReq.bits.baseAddr.poke(baseAddr.U)
                dut.io.writeReq.bits.logicalIdx.poke(i.U)
                for (j <- 0 until N) {
                    dut.io.writeReq.bits.data(j).poke(matrix(i)(j).U)
                }
                dut.clock.step(1)
            }
            dut.io.writeReq.valid.poke(false.B)
            dut.clock.step(5) // wait out pipeline

            // Phase 2: Read all rows
            for (i <- 0 until N) {
                dut.io.readReq.valid.poke(true.B)
                dut.io.readReq.bits.mode.poke(0.B) // Mode 0: Row Read
                dut.io.readReq.bits.baseAddr.poke(baseAddr.U)
                dut.io.readReq.bits.logicalIdx.poke(i.U)

                // wait for response
                dut.clock.step(1)
                dut.io.readReq.valid.poke(false.B)

                while (!dut.io.resp.valid.peek().litToBoolean) {
                    dut.clock.step(1)
                }

                // Verify the data
                for (j <- 0 until N) {
                    dut.io.resp.bits.data(j).expect(matrix(i)(j).U)
                }
            }
        }
    }

    it should "correctly write rows and read transposed columns (Mode 1)" in {
        simulate(new Hyacine(N, dataWidth, addrWidth)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)

            dut.io.writeReq.valid.poke(false.B)
            dut.io.readReq.valid.poke(false.B)
            dut.io.resp.ready.poke(true.B)

            val matrix   = Array.tabulate(N, N)((i, j) => (i + 1) * 100 + j + 1)
            val baseAddr = 0x200

            // Phase 1: Write all rows
            for (i <- 0 until N) {
                dut.io.writeReq.valid.poke(true.B)
                dut.io.writeReq.bits.baseAddr.poke(baseAddr.U)
                dut.io.writeReq.bits.logicalIdx.poke(i.U)
                for (j <- 0 until N) {
                    dut.io.writeReq.bits.data(j).poke(matrix(i)(j).U)
                }
                dut.clock.step(1)
            }
            dut.io.writeReq.valid.poke(false.B)
            dut.clock.step(5)

            // Phase 2: Read all columns
            for (j <- 0 until N) {
                dut.io.readReq.valid.poke(true.B)
                dut.io.readReq.bits.mode.poke(1.B)       // Mode 1: Col Read
                dut.io.readReq.bits.baseAddr.poke(baseAddr.U)
                dut.io.readReq.bits.logicalIdx.poke(j.U) // Read column j

                dut.clock.step(1)
                dut.io.readReq.valid.poke(false.B)

                while (!dut.io.resp.valid.peek().litToBoolean) {
                    dut.clock.step(1)
                }

                // Verify the data (expecting column j, i.e., elements matrix(i)(j) for i=0..N-1)
                for (i <- 0 until N) {
                    dut.io.resp.bits.data(i).expect(matrix(i)(j).U)
                }
            }
        }
    }

    it should "correctly forward written data if simultaneously read on the same address (Bypass)" in {
        simulate(new Hyacine(N, dataWidth, addrWidth)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)

            dut.io.writeReq.valid.poke(false.B)
            dut.io.readReq.valid.poke(false.B)
            dut.io.resp.ready.poke(true.B)

            val baseAddr  = 0x300
            val rowToTest = 2
            val testData  = Array(501, 502, 503, 504)

            // Concurrently poke writeReq and readReq for the same row
            dut.io.writeReq.valid.poke(true.B)
            dut.io.writeReq.bits.baseAddr.poke(baseAddr.U)
            dut.io.writeReq.bits.logicalIdx.poke(rowToTest.U)
            for (j <- 0 until N) {
                dut.io.writeReq.bits.data(j).poke(testData(j).U)
            }

            dut.io.readReq.valid.poke(true.B)
            dut.io.readReq.bits.mode.poke(0.B) // Row Read
            dut.io.readReq.bits.baseAddr.poke(baseAddr.U)
            dut.io.readReq.bits.logicalIdx.poke(rowToTest.U)

            dut.clock.step(1)

            // De-assert
            dut.io.writeReq.valid.poke(false.B)
            dut.io.readReq.valid.poke(false.B)

            // Wait for response to become valid (decoupled pipeline)
            while (!dut.io.resp.valid.peek().litToBoolean) {
                dut.clock.step(1)
            }

            for (j <- 0 until N) {
                dut.io.resp.bits.data(j).expect(testData(j).U)
            }

            dut.clock.step(1)
        }
    }
}
