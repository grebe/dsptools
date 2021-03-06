// See LICENSE for license details.

package dsptools.numbers

import chisel3.{Bool, Bundle, Data}
import chisel3.util.{Valid, ValidIO}

/**
  * Much of this is drawn from non/spire, but using Chisel Bools instead of
  * Java Bools. I suppose a more general solution would be generic in
  * return type, but the use cases there seem obscure.
  */

/**
  * A type class used to determine equality between 2 instances of the same
  * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
  * Moreover, `eqv` should form an equivalence relation.
  */
trait Eq[A <: Data] extends Any {
  /** Returns `true` if `x` and `y` are equivalent, `false` otherwise. */
  def eqv(x:A, y:A): Bool

  /** Returns `false` if `x` and `y` are equivalent, `true` otherwise. */
  def neqv(x:A, y:A): Bool = !eqv(x, y)

  /**
    * Constructs a new `Eq` instance for type `B` where 2 elements are
    * equivalent iff `eqv(f(x), f(y))`.
    */
  def on[B <: Data](f:B => A): Eq[B] = new MappedEq(this)(f)
}

private[numbers] class MappedEq[A <: Data, B <: Data](eq: Eq[B])(f: A => B) extends Eq[A] {
  def eqv(x: A, y: A): Bool = eq.eqv(f(x), f(y))
}

object Eq {
  def apply[A <: Data](implicit e:Eq[A]):Eq[A] = e

  def by[A <: Data, B <: Data](f:A => B)(implicit e:Eq[B]): Eq[A] = new MappedEq(e)(f)
}


class ComparisonBundle extends Bundle {
  val eq = Bool()
  // ignore lt if eq is true
  val lt = Bool()
}

class SignBundle extends Bundle {
  val zero = Bool()
  // ignore neg if zero is true
  val neg  = Bool()
}

object ComparisonHelper {
  def apply(valid: Bool, eq: Bool, lt: Bool): ValidIO[ComparisonBundle] = {
    val ret = Valid(new ComparisonBundle())
    ret.bits.eq := eq
    ret.bits.lt := lt
    ret.valid := valid
    ret
  }
  def apply(eq: Bool, lt: Bool): ComparisonBundle = {
    val ret = new ComparisonBundle()
    ret.eq := eq
    ret.lt := lt
    ret
  }
}



