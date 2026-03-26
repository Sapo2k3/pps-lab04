package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class Compl(re: Double, im: Double)

    // Change assignment below: should probably define a case class and use it?
    type Complex = Compl
    def complex(re: Double, im: Double): Complex = Compl(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = Compl(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex = Compl(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = Compl(re, im) match
        case Compl(0.0, 0.0) => "0.0"
        case Compl(0.0, i) => s"${i}i"
        case Compl(r, 0.0) => s"$r"
        case Compl(r, i) if i > 0 => s"$r + ${i}i"
        case Compl(r, i) if i < 0 => s"$r - ${i.abs}i"
