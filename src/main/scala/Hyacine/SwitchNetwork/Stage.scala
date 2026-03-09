package Hyacine.SwitchNetwork

import chisel3._
import chisel3.util._
import Hyacine.SwitchNetwork.Switch2to2

object Stage {
    def apply[T <: Data](in: Vec[T], sel: Vec[Bool], pipe: Boolean = false): Vec[T] = {
        require(in.length % 2 == 0, "Input length must be even")
        require(sel.length == in.length / 2, "Selection vector length must be half of input length")
        val numRows = in.length / 2
        VecInit(
          Seq.tabulate(numRows) { i =>
              Switch2to2(VecInit(in(2 * i), in(2 * i + 1)), sel(i), pipe)
          }.flatten
        )
    }
}
