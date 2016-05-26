package pete1232

object Rational {
  /*TODO implment an implicit Numeric type for Rational
   * These act as a replacement for equivalent methods in scala.util.List that
   * expect an implicit Numeric type. In future a Numeric (Functional) type should
   * be made for Rational to implement these methods properly.
   */
  def average(q: Rational*): Rational = {
    sum(q: _*) / q.size
  }
  def sum(q: Rational*): Rational = {
    def total(q: Seq[Rational], value: Rational = new Rational(0)): Rational ={
      if(q.isEmpty) value else total(q.tail, value + q.head)
    }
    total(q)
  }
}

/**
  * A model of a rational number
  * @param n a number
  * @param d a non-zero number
  */
class Rational(n: Int, d: Int) extends Ordered[Rational] {
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

  def compare(q: Rational): Int = num * q.dom - q.num * dom

  /* TODO override standard equality methods
   * Eventually Rational will extend AnyVal and override the default ==
   * and != methods
   */
  def equalsRational(q: Rational): Boolean = (this compare q) == 0

  override def toString: String = num + "/" + dom

  private def gdc(x: Int, y: Int): Int = if(y == 0) x else gdc(y, x % y)
}