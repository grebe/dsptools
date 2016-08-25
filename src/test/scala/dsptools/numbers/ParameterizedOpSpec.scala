// See LICENSE for license details.

package dsptools.numbers

import chisel3.core._
import dsptools.{DspContext, DspTester}
import dsptools.numbers._
import org.scalatest.{FreeSpec, FlatSpec, Matchers}
import spire.algebra.Ring
import spire.implicits._

//scalastyle:off magic.number

class ParameterizedNumberOperation[T <: Data:Ring](
                                          gen:() => T,
                                          val op: String = "+"
                                        ) extends Module {
  val io = new Bundle {
    val a1: T = gen().cloneType.flip()
    val a2: T = gen().cloneType.flip()
    val c:  T = gen().cloneType
  }

  val register1 = Reg(gen().cloneType)

  register1 := {
    op match {
      case "+" => io.a1 + io.a2
      case "-" => io.a1 - io.a2
      case "*" => io.a1 * io.a2
//      case "/" => io.a1 / io.a2
      case _ => throw new Exception(s"Bad operator $op passed to ParameterizedNumberOperation")
    }
  }

  io.c := register1
}

class ParameterizedOpTester[T<:Data:Ring](c: ParameterizedNumberOperation[T]) extends DspTester(c) {
  for {
    i <- 0.0 to 1.0 by 0.25
    j <- 0.0 to 4.0 by 0.5
  } {
    val expected = c.op match {
      case "+" => i + j
      case "-" => i - j
      case "*" => i * j
      case _ => i + j
    }
    poke(c.io.a1, i)
    poke(c.io.a2, j)
    step(1)

    val result = peek(c.io.c)

    println(f"TESTCASE $i%6.2f ${c.op} $j%6.2f => $result" +
      (if(result != expected) s"Error: expected $expected" else "")
        )
  }
}

class ParameterizedOpSpecification extends FreeSpec with Matchers {
  """
  The ParameterizedNumericOperation demostrates a Module that can be instantiated to
  handle different numeric types and different numerical operations
  """ -
    {
    implicit val DefaultDspContext = DspContext()
    implicit val evidence          = new DspRealRing()(DefaultDspContext)

    def getReal(): DspReal = new DspReal

    "This instance will process Real numbers with the basic mathematical operations" - {
      Seq("+", "-", "*").foreach { operation =>
        s"operation $operation should work for all inputs" in {
          chisel3.iotesters.Driver(() => new ParameterizedNumberOperation(getReal, operation)) { c =>
            new ParameterizedOpTester(c)
          } should be(true)
        }
      }
    }
  }
}

class ParameterizedOpSpec extends FlatSpec with Matchers {
  behavior of "parameterized adder circuit on blackbox real"

  it should "allow registers to be declared that infer widths" in {
    implicit val DefaultDspContext = DspContext()
    implicit val evidence          = new DspRealRing()(DefaultDspContext)

    def getReal(): DspReal = new DspReal


    chisel3.iotesters.Driver(() => new ParameterizedNumberOperation(getReal, "-")) { c =>
      new ParameterizedOpTester(c)
    } should be (true)
  }

  behavior of "parameterized adder circuit on fixed point"

  it should "allow registers to be declared that infer widths" in {
    implicit val DefaultDspContext = DspContext()
    implicit val evidence = new FixedPointRing()(DefaultDspContext)

    def getFixed(): FixedPoint = FixedPoint(OUTPUT, width = 32, binaryPoint = 16)

    chisel3.iotesters.Driver(() => new ParameterizedNumberOperation(getFixed)) { c =>
      new ParameterizedOpTester(c)
    } should be (true)
  }

  behavior of "parameterized adder circuit on complex"

  it should "allow registers to be declared that infer widths" in {
    implicit val DefaultDspContext = DspContext()
    implicit val fixedEvidence = new FixedPointRing()(DefaultDspContext)
    implicit val evidence = new DspComplexRing[FixedPoint]()(fixedEvidence, DefaultDspContext)

    def getComplex(): DspComplex[FixedPoint] = {
      DspComplex(
        FixedPoint(OUTPUT, width = 65, binaryPoint = 16),
        FixedPoint(OUTPUT, width = 65, binaryPoint = 16))
    }

    chisel3.iotesters.Driver(() => new ParameterizedNumberOperation(getComplex)) { c =>
      new ParameterizedOpTester(c)
    } should be (true)
  }

  behavior of "parameterized adder circuit on complex with reals"

  it should "allow registers to be declared that infer widths" in {
    implicit val DefaultDspContext = DspContext()
    implicit val fixedEvidence = new DspRealRing()(DefaultDspContext)
    implicit val evidence = new DspComplexRing[DspReal]()(fixedEvidence, DefaultDspContext)

    def getComplex(): DspComplex[DspReal] = {
      DspComplex(
        DspReal(1.0),
        DspReal(1.0))
    }

    chisel3.iotesters.Driver(() => new ParameterizedNumberOperation(getComplex)) { c =>
      new ParameterizedOpTester(c)
    } should be (true)
  }
}