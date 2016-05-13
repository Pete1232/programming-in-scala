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

  def *(q: Rational): Rational = new Rational(num * q.num, dom * q.dom)

  def lessThan(q: Rational): Boolean = num * q.dom < q.num * dom

  def equalsRational(q: Rational): Boolean = num * q.dom == q.num * dom

  override def toString: String = num + "/" + dom

  private def gdc(x: Int, y: Int): Int = if(y == 0) x else gdc(y, x % y)
}