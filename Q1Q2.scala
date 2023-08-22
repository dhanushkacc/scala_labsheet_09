class Rational(numerator: Int, denominator: Int) {
  require(denominator > 0, "Denominator must be greater than 0")

  def num: Int = numerator
  def den: Int = denominator

  def neg: Rational = new Rational(-num, den)

  def subtract(other: Rational): Rational =
    new Rational(num * other.den - den * other.num, den * other.den)

  def -(other: Rational): Rational = this.subtract(other)

  override def toString: String = s"$num/$den"
}

object Q1Q2 extends App {
  val x = new Rational(3, 4)
  val y = new Rational(5, 8)
  val z = new Rational(2, 7)

  // Answer for Q1
  println(x.neg)

  // Answer for Q2

  val result = x - y - z
  println(result)
}
