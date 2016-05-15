package pete1232

/**
  * A model of a rational number
  * @param n a number
  * @param d a non-zero number
  */
class Rational(n: Int, d: Int) {
  require(d != 0)

  def this(n: Int) = this(n, 1)

  val num: Int = (n*d/d.abs)/gdc(n.abs, d.abs)
  val dom: Int = d.abs/gdc(n.abs, d.abs)

  def +(q: Rational): Rational = new Rational(num * q.dom + q.num * dom, dom * q.dom)

  def +(i: Int): Rational = new Rational(num + i*dom, dom)

  def -(q: Rational): Rational = this + new Rational(-q.num, q.dom)

  def -(i: Int): Rational = this + -i

  def *(q: Rational): Rational = new Rational(num * q.num, dom * q.dom)

  def *(i: Int): Rational = new Rational(num * i, dom)

  def /(q: Rational): Rational = this * new Rational(q.dom, q.num)

  def /(i: Int): Rational = new Rational(num, dom * i)

  def lessThan(q: Rational): Boolean = num * q.dom < q.num * dom

  def equalsRational(q: Rational): Boolean = num * q.dom == q.num * dom

  override def toString: String = num + "/" + dom

  private def gdc(x: Int, y: Int): Int = if(y == 0) x else gdc(y, x % y)
}