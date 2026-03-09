package Hyacine.SwitchNetwork

import chisel3._
import chisel3.util._

object Switch2to2 {
    def apply[T <: Data](in: Vec[T], sel: Bool, pipe: Boolean = false): Vec[T] = {
        require(in.length == 2, "Input vector must have length 2")
        val out0 = Mux(sel, in(1), in(0))
        val out1 = Mux(sel, in(0), in(1))
        if (pipe) {
            VecInit(RegNext(out0), RegNext(out1))
        } else {
            VecInit(out0, out1)
        }
    }
}
