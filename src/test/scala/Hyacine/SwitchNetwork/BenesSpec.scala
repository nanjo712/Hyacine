package Hyacine.SwitchNetwork

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenesSpec extends AnyFlatSpec with Matchers {

  "BenesRouter" should "generate correct config for identity permutation" in {
    val n = 8
    val pi = Array.tabulate(n)(i => i)
    val config = BenesRouter.generateConfig(n, pi)
    // We can just verify it doesn't crash, the real test is in hardware
  }

  it should "generate correct config for stride permutation" in {
    val n = 8
    // stride 2: 0->0, 1->2, 2->4, 3->6, 4->1, 5->3, 6->5, 7->7
    val pi = Array(0, 2, 4, 6, 1, 3, 5, 7)
    val config = BenesRouter.generateConfig(n, pi)
  }

  "Benes" should "perform identity permutation correctly (no pipeline)" in {
    val n = 8
    val pi = Array.tabulate(n)(i => i)
    val config = BenesRouter.generateConfig(n, pi)

    simulate(new Benes(UInt(8.W), n, 0)) { dut =>
      // Apply sel config
      for (s <- 0 until 2 * 3 - 1) {
        for (i <- 0 until n / 2) {
          dut.io.sel(s)(i).poke(config(s)(i).B)
        }
      }

      for (i <- 0 until n) {
        dut.io.in(i).poke((i + 10).U)
      }
      
      dut.clock.step(1)

      for (i <- 0 until n) {
        dut.io.out(i).expect((pi(i) + 10).U)
      }
    }
  }

  it should "perform stride permutation correctly (no pipeline)" in {
    val n = 8
    val pi = Array(0, 2, 4, 6, 1, 3, 5, 7)
    val config = BenesRouter.generateConfig(n, pi)

    simulate(new Benes(UInt(8.W), n, 0)) { dut =>
      for (s <- 0 until 2 * 3 - 1) {
        for (i <- 0 until n / 2) {
          dut.io.sel(s)(i).poke(config(s)(i).B)
        }
      }

      for (i <- 0 until n) {
        dut.io.in(i).poke((i + 10).U)
      }
      
      dut.clock.step(1)

      for (i <- 0 until n) {
        dut.io.out(pi(i)).expect((i + 10).U)
      }
    }
  }

  it should "perform stride permutation correctly with pipelining" in {
    val n = 8
    val pi = Array(0, 2, 4, 6, 1, 3, 5, 7)
    val config = BenesRouter.generateConfig(n, pi)
    val pipeEvery = 2

    simulate(new Benes(UInt(8.W), n, pipeEvery)) { dut =>
      for (s <- 0 until 2 * 3 - 1) {
        for (i <- 0 until n / 2) {
          dut.io.sel(s)(i).poke(config(s)(i).B)
        }
      }

      for (i <- 0 until n) {
        dut.io.in(i).poke((i + 10).U)
      }
      
      // Calculate delay: 5 stages, pipe every 2 stages
      // Stage 0: no pipe
      // Stage 1: pipe (delay 1)
      // Stage 2: no pipe
      // Stage 3: pipe (delay 1)
      // Stage 4: no pipe
      // Total delay = 2 cycles
      dut.clock.step(2)

      for (i <- 0 until n) {
        dut.io.out(pi(i)).expect((i + 10).U)
      }
    }
  }
}
