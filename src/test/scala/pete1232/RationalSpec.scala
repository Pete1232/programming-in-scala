package pete1232

import org.scalatest.{FlatSpec, MustMatchers}

class RationalSpec extends FlatSpec with MustMatchers {

  private class TestSetup{
    import pete1232.Rational
    val half = new Rational(1, 2)
    val third = new Rational(1, 3)
    val quarter = new Rational(1, 4)
    val threeSixths = new Rational(3, 6)
    val threeSevenths = new Rational(3, 7)
    val nineElevenths = new Rational(9, 11)
    val threeNinths = new Rational(3, 9)
    val minusHalf = new Rational(-1, 2)
    val minusThreeSixths = new Rational(-3, 6)
    val minusThreeNinths = new Rational(-3, 9)
    val halfMinus = new Rational(1, -2)
    val threeSeventhsMinus = new Rational(3, -7)
    val minusHalfMinus = new Rational(-1, -2)
    val minusThreeSeventhsMinus = new Rational(-3, -7)
    val noElevenths = new Rational(0, 11)
  }

  "toString" must "return the Rational as an inline fraction in normal form" in new TestSetup {
    half.toString mustBe "1/2"
    third.toString mustBe "1/3"
    quarter.toString mustBe "1/4"
    threeSevenths.toString mustBe "3/7"
    nineElevenths.toString mustBe "9/11"
    threeSixths.toString mustBe "1/2"
    minusHalf.toString mustBe "-1/2"
    halfMinus.toString mustBe "-1/2"
    minusHalfMinus.toString mustBe "1/2"
    noElevenths.toString mustBe "0/1"
  }

  "constructing a Rational with 0 denominator" must "throw an IllegalArgumentException" in {
    an[IllegalArgumentException] must be thrownBy new Rational(5, 0)
    an[IllegalArgumentException] must be thrownBy new Rational(8, 0)
  }

  "lessThan" must "return true if this is smaller than the given Rational" in new TestSetup {
    third.lessThan(half) mustBe true
    quarter.lessThan(third) mustBe true
    threeSevenths.lessThan(threeSixths) mustBe true
    threeSevenths.lessThan(nineElevenths) mustBe true
    minusHalf.lessThan(half) mustBe true

  }
  it must "return false otherwise" in new TestSetup {
    half.lessThan(third) mustBe false
    third.lessThan(quarter) mustBe false
    threeSixths.lessThan(threeSevenths) mustBe false
    nineElevenths.lessThan(threeSevenths) mustBe false
    half.lessThan(half) mustBe false
    half.lessThan(minusHalf) mustBe false
  }

  "equalsRational" must "return true if this is equal to the given Rational" in new TestSetup {
    half.equalsRational(half) mustBe true
    half.equalsRational(new Rational(1, 2)) mustBe true
    minusHalf.equalsRational(minusHalf) mustBe true
  }
  it must "return true if this is equal to the given Rational when normalized" in new TestSetup {
    half.equalsRational(threeSixths) mustBe true
    threeSixths.equalsRational(half) mustBe true
    minusHalf.equalsRational(minusThreeSixths) mustBe true
    minusThreeSixths.equalsRational(minusHalf) mustBe true
  }
  it must "return false otherwise" in new TestSetup {
    half.equalsRational(third) mustBe false
    third.equalsRational(quarter) mustBe false
    third.equalsRational(threeSixths) mustBe false
    half.equalsRational(minusHalf) mustBe false
    minusHalf.equals(half) mustBe false
  }

  "invoking Rational with one argument" must "return the given Int as a Rational" in {
    new Rational(5).equalsRational(new Rational(5, 1)) mustBe true
    new Rational(3).equalsRational(new Rational(3, 1)) mustBe true
    new Rational(-5).equalsRational(new Rational(-5, 1)) mustBe true
    new Rational(-5).equalsRational(new Rational(5, -1)) mustBe true
  }

  "invoking Rational with a non-normal fraction" must "set num and dom to their normal form" in new TestSetup {
    half.num mustBe 1
    half.dom mustBe 2
    threeSixths.num mustBe 1
    threeSixths.dom mustBe 2
    threeNinths.num mustBe 1
    threeNinths.dom mustBe 3
    minusThreeSixths.num mustBe -1
    minusThreeSixths.dom mustBe 2
    minusHalf.num mustBe -1
    minusHalf.dom mustBe 2
  }

  "invoking Rational on a number with negative denominator" must "change the denominator to positive" in new TestSetup {
    halfMinus.dom mustBe 2
    threeSeventhsMinus.dom mustBe 7
    minusHalfMinus.dom mustBe 2
    minusThreeSeventhsMinus.dom mustBe 7
  }
  it must "set num to be negative if n is positive" in new TestSetup {
    halfMinus.num mustBe -1
    threeSeventhsMinus.num mustBe -3
  }
  it must "set num to be positive if n is negative" in new TestSetup {
    minusHalfMinus.num mustBe 1
    minusThreeSeventhsMinus.num mustBe 3
  }

  "invoking Rational on a number with 0 numerator" must "set dom to 1" in new TestSetup {
    noElevenths.dom mustBe 1
  }

  "(+)" must "add this to another Rational" in new TestSetup {
    (half + third).equalsRational(new Rational(5, 6)) mustBe true
    (third + half).equalsRational(new Rational(5, 6)) mustBe true
    (minusHalf + nineElevenths).equalsRational(new Rational(7, 22)) mustBe true
    (third + minusHalf).equalsRational(new Rational(-1, 6)) mustBe true
    (half + minusHalf).equalsRational(new Rational(0, 1)) mustBe true
  }
  it must "add this to an Int" in new TestSetup {
    (half + 1).equalsRational(new Rational(3, 2)) mustBe true
    (minusHalf + 1).equalsRational(new Rational(1, 2)) mustBe true
    (minusHalf + -1).equalsRational(new Rational(-3, 2)) mustBe true
  }

  "(*)" must "multiply this by another Rational" in new TestSetup {
    (half * third).equalsRational(new Rational(1, 6)) mustBe true
    (third * half).equalsRational(new Rational(1, 6)) mustBe true
    (minusHalf * half).equalsRational(new Rational(-1, 4)) mustBe true
    (minusHalf * minusHalf).equalsRational(new Rational(1, 4)) mustBe true
  }
  it must "multiply this by an Int" in new TestSetup {
    (half * 2).equalsRational(new Rational(1, 1)) mustBe true
    (minusHalf * 2).equalsRational(new Rational(-1, 1)) mustBe true
    (minusHalf * -2).equalsRational(new Rational(1, 1)) mustBe true
  }

  "(/)" must "divide this by another Rational" in new TestSetup {
    (half / third).equalsRational(new Rational(3, 2)) mustBe true
    (minusHalf / third).equalsRational(new Rational(-3, 2)) mustBe true
    (minusHalf / minusHalf).equalsRational(new Rational(1)) mustBe true
  }
  it must "divide this by an Int" in new TestSetup {
    (half / 2).equalsRational(new Rational(1, 4)) mustBe true
    (minusHalf / 2).equalsRational(new Rational(-1, 4)) mustBe true
    (minusHalf / -2).equalsRational(new Rational(1, 4)) mustBe true
    (half / half).equalsRational(new Rational(1, 1)) mustBe true
  }

  "(-)" must "subtract another Rational from this" in new TestSetup {
    (half - third).equalsRational(new Rational(1, 6)) mustBe true
    (minusHalf - third).equalsRational(new Rational(-5, 6)) mustBe true
    (minusHalf - minusHalf).equalsRational(new Rational(0)) mustBe true
  }
  it must "subtract an Int from this" in new TestSetup {
    (half - 1).equalsRational(new Rational(-1, 2)) mustBe true
    (minusHalf - 1).equalsRational(new Rational(-3, 2)) mustBe true
    (minusHalf - -1).equalsRational(new Rational(1, 2)) mustBe true
  }

  "sum" must "return the sum of a collection of Rationals" in new TestSetup {
    Rational.sum(half).toString mustBe half.toString
    Rational.sum(half, third, quarter).toString mustBe new Rational(13, 12).toString
    Rational.sum(List(half, third, quarter): _*).toString mustBe new Rational(13, 12).toString
  }

  "average" must "return the mean of a collection of Rationals" in new TestSetup {
    Rational.average(half).toString mustBe half.toString
    Rational.average(half, third, quarter).toString mustBe new Rational(13, 36).toString
    Rational.average(List(half, third, quarter): _*).toString mustBe new Rational(13, 36).toString
  }
}