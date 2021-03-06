// See LICENSE for license details.

package dsptools.numbers

import chisel3.{Bool, Data, Mux, UInt}

import scala.language.implicitConversions

/**
  * Much of this is drawn from non/spire, but using Chisel Bools instead of
  * Java Bools. I suppose a more general solution would be generic in
  * return type, but the use cases there seem obscure.
  */

/**
  * A simple ADT representing the `Sign` of an object.
  */
sealed class Sign(that: SignBundle) extends SignBundle {
  import Sign._

  zero := that.zero
  neg  := that.neg

  def unary_-(): Sign = new Sign(Sign(this.zero, !this.neg))

  def *(that: Sign): Sign = new Sign(Sign(
    this.zero || that.zero,
    this.neg ^ that.neg
  ))

  def **(that: Int): Sign = **(UInt(that))
  def **(that: UInt): Sign = new Sign(Sign(this.zero, this.neg ^ that(0)))
}

object Sign {
  case object Zero extends Sign(Sign(Bool(true), Bool(false)))
  case object Positive extends Sign(Sign(Bool(false), Bool(false)))
  case object Negative extends Sign(Sign(Bool(false), Bool(true)))

  def apply(zero: Bool, neg: Bool): SignBundle = {
    val bundle = new SignBundle
    bundle.zero := zero
    bundle.neg  := neg
    bundle
  }

  implicit def apply(i: Int): Sign =
    if (i == 0) Zero else if (i > 0) Positive else Negative

  implicit def apply(i: ComparisonBundle): Sign = {
    val bundle = new SignBundle
    bundle.zero := i.eq
    bundle.neg := i.lt
    new Sign(bundle)
  }

  class SignAlgebra extends CMonoid[Sign] with Signed[Sign] with Order[Sign] {
    def id: Sign = Positive
    def op(a: Sign, b: Sign): Sign = a * b

    override def sign(a: Sign): Sign = a
    def signum(a: Sign): ComparisonBundle = ComparisonHelper(a.zero, a.neg)
    def abs(a: Sign): Sign = if (a == Negative) Positive else a

    def compare(x: Sign, y: Sign): ComparisonBundle = {
      val eq = Mux(x.zero,
        // if x is zero, y must also be zero for equality
        y.zero,
        // if x is not zero, y must not be zero and must have the same sign
        !y.zero && (x.neg === y.neg)
      )
      // lt only needs to be correct when eq not true
      val lt = Mux(x.zero,
        // if x is zero, then true when y positive
        !y.zero && !y.neg,
        // if x is not zero, then true when x is negative and y not negative
        x.neg && (y.zero || !y.neg)
      )

      ComparisonHelper(eq, lt)
    }
  }

  implicit final val SignAlgebra = new SignAlgebra

  implicit final val SignMultiplicativeGroup: MultiplicativeCMonoid[Sign] =
    Multiplicative(SignAlgebra)

  implicit def SignAction[A<:Data](implicit A: AdditiveGroup[A]): MultiplicativeAction[A, Sign] =
    new MultiplicativeAction[A, Sign] {
      def gtimesl(s: Sign, a: A): A = Mux(s.zero,
        A.zero,
        Mux(s.neg, A.negate(a), a )
      )
      def gtimesr(a: A, s: Sign): A = gtimesl(s, a)
    }
}

