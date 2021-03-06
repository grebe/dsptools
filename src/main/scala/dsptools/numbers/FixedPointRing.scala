// See LICENSE for license details.

package dsptools.numbers

import chisel3._
import dsptools.{hasContext, DspContext, Grow}

/**
  * Defines basic math functions for FixedPoint numbers
  */
trait FixedPointRing extends Any with Ring[FixedPoint] with hasContext {
  def plus(f: FixedPoint, g: FixedPoint): FixedPoint = {
    if(context.overflowType == Grow) {
      f +& g
    }
    else {
      f +% g
    }
  }
  def times(f: FixedPoint, g: FixedPoint): FixedPoint = {
    f * g
  }
  def one: FixedPoint = FixedPoint.fromBigInt(BigInt(1), binaryPoint = 0)
  def zero: FixedPoint = FixedPoint.fromBigInt(BigInt(0), binaryPoint = 0)
  def negate(f: FixedPoint): FixedPoint = zero - f
}

trait FixedPointImpl {
  implicit object FixedPointRealImpl extends FixedPointReal
}

trait FixedPointOrder extends Any with Order[FixedPoint] with hasContext {
  override def compare(x: FixedPoint, y: FixedPoint): ComparisonBundle = {
    ComparisonHelper(x === y, x < y)
  }
}

trait FixedPointSigned extends Any with Signed[FixedPoint] with hasContext {
  def signum(a: FixedPoint): ComparisonBundle = {
    ComparisonHelper(a === FixedPoint.fromBigInt(0), a < FixedPoint.fromBigInt(0))
  }

  /** An idempotent function that ensures an object has a non-negative sign. */
  def abs(a: FixedPoint): FixedPoint = Mux(a > FixedPoint.fromBigInt(0), a, FixedPoint.fromBigInt(0)-a)
}
trait FixedPointIsReal extends Any with IsReal[FixedPoint] with FixedPointOrder with FixedPointSigned with hasContext {
  def toDouble(a: FixedPoint): DspReal = ???
  def ceil(a: FixedPoint): FixedPoint = ???
  def floor(a: FixedPoint): FixedPoint = ???
  def isWhole(a: FixedPoint): Bool = ???
  def round(a: FixedPoint): FixedPoint = ???
}

trait ConvertableToFixedPoint extends ConvertableTo[FixedPoint] with hasContext {
  def fromShort(n: Short): FixedPoint = FixedPoint.fromBigInt(n.toInt)
  //def fromAlgebraic(n: Algebraic): FixedPoint = FixedPoint.fromBigInt(n.toBigInt)
  def fromBigInt(n: BigInt): FixedPoint = FixedPoint.fromBigInt(n)
  def fromByte(n: Byte): FixedPoint = FixedPoint.fromBigInt(n.toInt)
  def fromDouble(n: Double): FixedPoint = FixedPoint.fromDouble(n)
  //def fromReal(n: Real): FixedPoint = FixedPoint.fromDouble(n.toDouble)
  //def fromRational(n: Rational): FixedPoint = FixedPoint.fromDouble(n.toDouble)
  def fromType[B](n: B)(implicit c: ConvertableFrom[B]): FixedPoint = FixedPoint.fromDouble(c.toDouble(n))
  def fromInt(n: Int): FixedPoint = FixedPoint.fromBigInt(n)
  def fromFloat(n: Float): FixedPoint = FixedPoint.fromDouble(n.toDouble)
  def fromBigDecimal(n: BigDecimal): FixedPoint = FixedPoint.fromDouble(n.toDouble)
  def fromLong(n: Long): FixedPoint = FixedPoint.fromBigInt(n)
}

trait FixedPointReal extends FixedPointRing with ConvertableToFixedPoint with FixedPointIsReal with Real[FixedPoint] with hasContext {
  override def fromInt(n: Int): FixedPoint = super[FixedPointRing].fromInt(n)
}
