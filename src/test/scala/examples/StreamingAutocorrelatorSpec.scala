// See LICENSE for license details.

package examples

//scalastyle:off magic.number

import chisel3.SInt
import chisel3.iotesters.{PeekPokeTester}
import dsptools.numbers.implicits._
import dsptools.{Grow, DspContext}
import org.scalatest.{Matchers, FlatSpec}

import spire.algebra.{Ring, Field}


class StreamingAutocorrelatorTester(c: StreamingAutocorrelator[SInt])
  extends PeekPokeTester(c) {

  for(num <- -5 to 5) {
    poke(c.io.input, BigInt(num))
    step(1)
    println(peek(c.io.output).toString())
  }
}

class StreamingAutocorrelatorSpec extends FlatSpec with Matchers {
  "StreamingAutocorrelatorFIR" should "compute a running average like thing" in {
    val taps = Seq.tabulate(3) { x => SInt(x)}
    //implicit val DefaultDspContext = DspContext()
    //implicit val evidence = (context :DspContext) => new SIntRing()(context)

    chisel3.iotesters.Driver(() => new StreamingAutocorrelator(SInt(width = 10), SInt(width = 20), 2, 3)) { c =>
      new StreamingAutocorrelatorTester(c)
    } should be (true)
  }
}
