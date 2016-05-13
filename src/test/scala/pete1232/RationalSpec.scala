package pete1232

import org.scalatest.{FlatSpec, MustMatchers}

class RationalSpec extends FlatSpec with MustMatchers{

  private class TestSetup {
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

  "+" must "return a new Rational from the current Rational added to the Rational on which + is invoked" in new TestSetup {
    (half + third).toString mustBe new Rational(5, 6).toString
    (third + half).toString mustBe new Rational(5, 6).toString
    (minusHalf + nineElevenths).toString mustBe new Rational(7, 22).toString
    (third + minusHalf).toString mustBe new Rational(-1, 6).toString
    (half + minusHalf).toString mustBe new Rational(0, 1).toString
  }

  "lessThan" must "return true if the current Rational is smaller than the Rational lessThan is invoked on" in new TestSetup {
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

  "equalsRational" must "return true if two Rationals are equal" in new TestSetup {
    half.equalsRational(half) mustBe true
    half.equalsRational(new Rational(1, 2)) mustBe true
    minusHalf.equalsRational(minusHalf) mustBe true
  }
  it must "return true if two Rationals are equal in normal form" in new TestSetup {
    half.equalsRational(threeSixths) mustBe true
    threeSixths.equalsRational(half) mustBe true
    minusHalf.equalsRational(minusThreeSixths) mustBe true
    minusThreeSixths.equalsRational(minusHalf) mustBe  true
  }
  it must "reuturn false otherwise" in new TestSetup {
    half.equalsRational(third) mustBe false
    third.equalsRational(quarter) mustBe false
    third.equalsRational(threeSixths) mustBe false
    half.equalsRational(minusHalf) mustBe false
    minusHalf.equals(half) mustBe false
  }

  "invoking Rational with one argument" must "return a Rational with the argument as the numerator" in {
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

  "*" must "return a new Rational from the current Rational multiplied by the Rational on which * is invoked" in new TestSetup {
    (half * third).toString mustBe new Rational(1, 6).toString
    (third * half).toString mustBe new Rational(1, 6).toString
    (minusHalf * half).toString mustBe new Rational(-1, 4).toString
    (minusHalf * minusHalf).toString mustBe new Rational(1, 4).toString
  }
}