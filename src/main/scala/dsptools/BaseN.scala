// See LICENSE for license details.

package dsptools

import chisel3._
import chisel3.util.Cat

//scalastyle:off magic.number
object BaseN {
  // Support a finite # of bases to make prime check, etc. easier
  val supportedBases = List(List(2,4),List(3),List(5),List(7),List(11))

  // TODO: Possibly reverse toIntList (?) and subsequent to index 0 = least significant digit
  // always check x, r, etc. are positive

  /** Converts a decimal representation of the number x into a List of
    * Ints representing the base-r interpretation of x (least significant digit on the right)
    */
  private def toIntListInternal(x: Int, r: Int): List[Int] = {
    if (x == 0) Nil else toIntListInternal(x / r, r) :+ (x % r)
  }
  def toIntList(x: Int, r:Int): List[Int] = {
    val temp = toIntListInternal(x,r)
    // Should return non-empty list
    if (temp.isEmpty) List(0) else temp
  }
  /** Returns # of Base N digits needed to represent the number */
  def numDigits(x: Int, r:Int): Int = toIntListInternal(x,r).length

  /** Zero pads List[Int] base-r representation */
  def toIntList(x: Int, r:Int, max:Int): List[Int] = {
    val intList = toIntList(x,r)
    val maxDigits = toIntList(max, r).length
    val fillDigits = (maxDigits - intList.length).max(0)
    val padding = List.fill(fillDigits)(0)
    padding ++ intList
  }

  /** Converts a decimal number x to an optionally padded List of UInts
    * Note that the amount of padding is determined by the maximum number that should be represented */
  def toUIntList(x: Int, r:Int, max: Int = -1): List[UInt] = {
    val intList = if (max < 0) toIntList(x,r) else toIntList(x,r,max)
    intList.map(x => UInt(x,r-1))
  }

  /** Converts a constant into BaseN Vec representation (least significant digit indexed with 0) */
  def apply(x: Int, r:Int): BaseN = apply(x,r,-1)
  def apply(x: Int, r:Int, max:Int): BaseN = {
    val temp = toUIntList(x,r,max).reverse
    BaseN(temp,r)
  }

  def toBitWidth(x: BigInt): Int = {
    if(x == BigInt(0)) 1 else x.bitLength
  }

  // TODO: Check the concatenation preserves isLit

  /** Represent x in BaseN but stored as bits concatenated together (i.e. for memory)
    * Note that the bits are stored so that the least significant digit is the right-most to make
    * converting to BaseN Vec easier. Ex: toBits(14,3,242) --> 00 00 01 01 10
    */
  def toBits(x: Int, r:Int, max:Int = -1): Bits = {
    val digitWidth = toBitWidth(r-1)
    val digits = toUIntList(x,r,max).map(x => x.litValue())
    val lit = {
      if (digits.length > 1) {
        digits.tail.foldLeft(digits.head)((x, y) => (x << digitWidth) + y)
      }
      else {
        digits.head
      }
    }
    UInt(lit,width=digits.length*digitWidth)
  }

  /** Converts a Bit representation of a number in base-r into a BaseN Vec representation with 0 indexing
    * the least significant digit
    */
  def apply(x: Bits, r:Int): BaseN ={
    val digitWidth = toBitWidth(r - 1)
    val numDigits = x.getWidth/digitWidth

    if (x.getWidth % digitWidth != 0) throw DspException("# of bits should be a multiple of digit width")
    val temp = (0 until numDigits).map(i => x((i + 1) * digitWidth - 1, i * digitWidth))
    BaseN(temp,r)
  }

  /** Create a new BaseN (to be assigned) specifying radix and max value */
  def apply(dir: Direction, rad: Int, max: Int): BaseN = {
    val maxDigits = toIntList(max, rad).length
    val temp = (0 until maxDigits).map(i => UInt(dir,rad-1))
    BaseN(temp,rad)
  }

  /** Converts list, etc. of UInts to BaseN */
  def apply(elts: Seq[UInt], rad: Int): BaseN = {
    new BaseN(elts.length, radix=rad)
  }

  // TODO: Vec, BaseN Vec to Bits
  // TODO: intList to BaseN Vec, UIntList to BaseN Vec, Vec to BaseN Vec (check element ranges < max radix)

}

// TODO: Ranging?, digit reverse with max digit specified, do I need an explicit mod?
// TODO: Better delay handling
// TODO: Handle mixed radix
/** BaseN type extends Vec */
class BaseN(val length: Int, val radix: Int) extends Bundle { //extends Vec(gen,elts){
  val underlying = Vec(length, UInt(BaseN.toBitWidth(radix)))
  def head: UInt = underlying.head
  def tail: Seq[UInt] = underlying.tail

  /** Clone type ! */
  override def cloneType: this.type = BaseN(length, radix).asInstanceOf[this.type]

  if (!BaseN.supportedBases.flatten.contains(radix)) throw DspException("Radix not supported!")

  val digitWidth = BaseN.toBitWidth(radix-1)
  val bitWidth = digitWidth*length

  /** Check for same base + same digit length */
  def sameType(b: BaseN): Unit = {
    if (length != b.length) throw DspException("BaseN Vec lengths must match!")
    sameRad(b)
  }
  def sameRad(b:BaseN): Unit = {
    if (radix != b.radix) throw DspException("Operation can only be performed when both values have the same base!")
  }

  /** Helper function for handling radices that are powers of 2 */
  private def rad2NtoUInt(): UInt = {
    // TODO: Check Vec elements have the same delay, separate out 4^n1*2 case

    val res = tail.foldLeft(head)((b,a) => Cat(a,b))
    val out = res.asUInt()
    out
  }

  /** Equality check */
  def === (b: BaseN): Bool = {
    sameType(b)
    val eqs = (this.underlying, b.underlying).zipped.map( _ === _ )
    eqs.tail.foldLeft(eqs.head)(_ & _)
  }

  /** Check that the BaseN value is a lit (all elements are lits) */
  def isBaseNLit: Boolean = {
    val temp = underlying.map(_.isLit)
    temp.tail.foldLeft(temp.head) { (x, y) => x & y }
  }

  /** Sum (that always wraps) */
  def + (b: BaseN): BaseN = {
    sameType(b)
    // Any radix that is a power of 2 doesn't require mods (can rely on simple bit manipulation)
    if (radix % 2 == 0){
      // TODO: Handle + 1 Lit separately b/c BaseN doesn't track lits ? -- or make it track lits
      // Also if it's 4^n1*2, don't need the extra end bit
      val temp = rad2NtoUInt +% b.rad2NtoUInt
      val out = BaseN(temp,radix)
      out
    }
    else{
      // TODO: Optimize mode for fixed radix (doesn't need to be as generalized as for mixed radix)
      val r = radix.U
      // Intermediate sum digits (without carry)
      val tempSum: Seq[UInt] = underlying.zip(b.underlying).map{case (x,y) => x + y}
      // Sum with base n carry chain (carryIn0 = 0)
      // Note that Mod takes carryIn_n + tempSum_n
      val resWithCarry = tempSum.tail.scanLeft(Mod(tempSum.head,r)) { (x, y) =>
        val carryIn = x._2.asUInt()
        Mod.apply( carryIn + y, r)
      }
      // Get sum digits
      val res = resWithCarry.map(x => x._1)
      BaseN(res,radix)
    }
  }

//  // TODO: More generic than 4,2 (i.e. can be 8,4,2) -- rad % 2 == 0 && rad != 2
//  // Also, should return MixedRad (?) rather than BaseN
//  /** Converts Base 4 to Mixed Radix Base [4,...,4,2] with least significant base (2) indexed @ 0 */
//  def toRad42(): BaseN = {
//    if (radix != 4) throw DspException("Conversion to mixed radix [4,...,4,2] requires that the input BaseN rad = 4")
//    val intVal = rad2NtoUInt
//    // Special case for separating out radix 2
//    val rad4Int = intVal >> 1
//
//    // Base 4 representation should have digits of width 2
//    val w = if (rad4Int.getWidth % 2 != 0) rad4Int.getWidth + 1 else rad4Int.getWidth
//    val rad4IntPadded = UInt(rad4Int, w)
//    val rad4s = BaseN(rad4IntPadded,radix)
//    // Break out radix 2 (keep same width for Mux, etc. consistency)
//    val rad2 = UInt(intVal(0),radix-1)
//    // Note: want to keep the same digit length
//    val out = BaseN(rad2 :: rad4s.underlying.toList.init, radix)
//    if (out.length != length) throw DspException("Conversion output should have the same # of digits")
//    // TODO: Check delay for each element
//    out
//  }

  // TODO: Handle Seq[T], Vec[T] in addition to BaseN
  /** Makes sure that reassignment only occurs when radices are the same */
  def <> (src: BaseN) : Unit = {
    sameType(src)
    underlying <> src.underlying
  }
  def := (src: BaseN) : Unit = {
    sameType(src)
    underlying := src.underlying
  }

  def := (src: UInt) : Unit = {
    underlying.asUInt() := src
  }

//  /** Only keep the lowest numPrimeDigits # of (prime) digits and zero out the rest (equivalent to taking a Mod)
//    * Note that useM1 is for comparing to max-1 instead of max (i.e. counter transition with control logic pipelining
//    * requires advanced notice)
//    */
//  def maskWithMaxCheck(numPrimeDigits: UInt, useM1:Boolean = false): (BaseN, Bool) = {
//    if (radix %2 != 0 || radix == 2) {
//      val (modOut,maxOut) = underlying.zipWithIndex.map {
//        case (e,i) => {
//          val activeDigit = numPrimeDigits > UInt(i)
//          val mod = Mux(activeDigit,e,UInt(0,radix-1))
//          val max = {
//            if (i == 0 && useM1) Mux(activeDigit,UInt(radix-2),UInt(0,radix-1))
//            else Mux(activeDigit,UInt(radix-1),UInt(0,radix-1))
//          }
//          (mod,max)
//        }
//      }.unzip
//      val res = BaseN(modOut,radix)
//      val max = BaseN(maxOut,radix)
//      val eqMax = res === max
//      (res, eqMax)
//    }
//    else{
//      // Stylize radix-4 as radix-2
//      val intVal = rad2NtoUInt
//      val temp = intVal.zipWithIndex.map {
//        case (e,i) => {
//          val activeDigit = (numPrimeDigits > UInt(i)).toBool
//          val mod = Mux(activeDigit,e,Bool(false))
//          val max = {
//            if (i == 0 && useM1) Bool(false)
//            else Mux(activeDigit,Bool(true),Bool(false))
//          }
//          (mod.toUInt,max.toUInt)
//        }
//      }
//      val (modOut,maxOut) = temp.tail.foldLeft(temp.head)( (b,a) => (Cat(a._1,b._1),Cat(a._2,b._2)))
//      val eqMax = DSPBool(modOut === maxOut).passDelay(this.head,0)
//      val out = BaseN(UInt(modOut,UInt.toMax(bitWidth)),radix)
//      (out,eqMax)
//    }
//  }

  // TODO: Get rid of redundancy
  /** Pad Base N representation to numDigits # of digits */
  def padTo(numDigits: Int): BaseN = {
    val padLength = (numDigits-length).max(0)
    val pad = Vec(padLength, 0.U)
    // Least significant digit indexed by 0
    val out = BaseN(this.underlying ++ pad,rad = radix)
    out
  }

  // TODO: For increments of 1, can check if the previous value was maxed by looking at ANDing all of the carry outs...,
  // Also, the above Max Check will not work for [9,...,9,3], etc.

  /** Select if true; otherwise return 0 in Base N */
  def ? (sel: Bool): BaseN = { //scalastyle:off method.name
    // TODO: Check metadata passed through
    val out = this.cloneType

    when(sel) {
      out := this
    }.otherwise {
      out := BaseN(underlying.map { _ => 0.U }, radix)
    }
    out
  }

  // TODO: Migrate + to use matchLength; should BaseN keep track of max?, padTo somewhat redundant
  /** Match BaseN lengths (pad upper digits) */
  private def matchLength(b: BaseN): (BaseN,BaseN) = {
    // Note: LSB's always correspond to lower indexing
    val diff = b.length - length
    if (diff > 0) {
      (BaseN(this.underlying ++ List.fill(diff)(UInt(0)),radix), b)
    }
    else if (diff < 0) {
      (this,BaseN(b.underlying ++ List.fill(-diff)(UInt(0)),radix))
    }
    else {
      (this, b)
    }
  }

  /** Bitwise OR (but with metadata, for muxing). Inputs should ideally be mutually exclusive */
  def | (b: BaseN): BaseN = {
    sameRad(b)
    val (x,y) = matchLength(b)
    val out = BaseN(x.underlying.zip(y.underlying).map{case (u,v) => u | v},rad = radix)
    out
  }

}
