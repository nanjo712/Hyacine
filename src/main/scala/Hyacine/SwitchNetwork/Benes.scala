package Hyacine.SwitchNetwork

import chisel3._
import chisel3.util._

class Benes[T <: Data](gen: T, numPorts: Int, pipelineEvery: Int = 0) extends Module {
    require(isPow2(numPorts), "numPorts must be a power of 2")
    require(numPorts >= 2, "numPorts must be at least 2")

    val n                = log2Up(numPorts)
    val numStages        = 2 * n - 1
    val switchesPerStage = numPorts / 2

    val io = IO(new Bundle {
        val in  = Input(Vec(numPorts, gen.cloneType))
        val sel = Input(Vec(numStages, Vec(switchesPerStage, Bool())))
        val out = Output(Vec(numPorts, gen.cloneType))
    })

    def route(stage: Int, port: Int): Int = {
        val isExpanding = stage < n - 1
        val groupLog    = if (isExpanding) n - stage else stage - n + 2
        val mask        = (1 << groupLog) - 1

        val highBits   = port & ~mask
        val inGroupPos = port & mask

        val routedPos = if (isExpanding) {
            (inGroupPos >> 1) | ((inGroupPos & 1) << (groupLog - 1))
        } else {
            ((inGroupPos << 1) & mask) | (inGroupPos >> (groupLog - 1))
        }
        highBits | routedPos
    }

    case class NetworkState(data: Vec[T], latency: Int)

    val finalState = (0 until numStages).foldLeft(NetworkState(io.in, 0)) { (state, s) =>
        val isPipelined = pipelineEvery > 0 && ((s + 1) % pipelineEvery == 0)

        val syncedSel = ShiftRegister(io.sel(s), state.latency)

        val switchedData = Stage(state.data, syncedSel, isPipelined)

        val nextData = if (s < numStages - 1) {
            val inverseMap = (0 until numPorts).map(i => route(s, i) -> i).toMap
            VecInit(Seq.tabulate(numPorts)(j => switchedData(inverseMap(j))))
        } else {
            switchedData
        }

        NetworkState(nextData, if (isPipelined) state.latency + 1 else state.latency)
    }

    io.out := finalState.data
}
