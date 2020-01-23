package dspblocks

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.util.HasBlackBoxInline
import freechips.rocketchip.amba.axi4.DPIModule
import org.scalatest.{FlatSpec, Matchers}

class DPITestBundle(val top: Boolean) extends Bundle {
  val clock = if (top) None else Some(Input(Clock()))
  val reset = if (top) None else Some(Input(Reset()))
  val a = Input(Bool())
  val b = Output(Bool())
}

class DPIEncapsulator extends MultiIOModule {
  val io = IO(new DPITestBundle(top = true))
  val dut = Module(new DPIModule("DPIModule", new DPITestBundle(top = false)))
  // io <> dut.io
  dut.io.a := io.a
  io.b := dut.io.b
  dut.io.clock.get := clock
  dut.io.reset.get := reset
}

class DPIModuleTester[T <: DPITestBundle](c: DPIEncapsulator) extends PeekPokeTester(c) {
  poke(c.io.a, true)
  step(1)
  expect(c.io.b, false)
  poke(c.io.a, false)
  step(1)
  expect(c.io.b, true)
}


class DPIModuleSpec extends FlatSpec with Matchers {
  behavior of "DPIModule"

  it should "work with verilator" in {
    println(chisel3.Driver.emit(() => new DPIEncapsulator))

    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-tiv"),
      () => new DPIEncapsulator) { c => new DPIModuleTester(c) } should be (true)
  }
}
