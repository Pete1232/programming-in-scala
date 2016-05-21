package pete1232

import Element.elem

object Element {
  private class ArrayElement(val conts: Array[String]) extends Element {
    require(conts.forall(_.length == width))
    def contents(): Array[String] = conts.clone()
  }
  private class LineElement(val s: String) extends Element {
    def contents: Array[String] = Array(s)
  }
  private class UniformElement(val c: Char, override val height: Int, override val width: Int) extends Element {
    require(height >= 0 && width >= 0)
    private val line = c.toString * width
    def contents: Array[String] = Array.fill(height)(line)
  }
  def elem(contents: Array[String]): Element = new ArrayElement(contents)
  def elem(line: String): Element = new LineElement(line)
  def elem(char: Char, height: Int, width: Int): Element = new UniformElement(char, height, width)
}

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = contents.headOption map(_.length) getOrElse(0)
  def above(that: Element): Element = elem(this.contents ++ that.contents)
  def beside(that: Element): Element = elem(
    for(
      (line1, line2) <- this.contents zip that.contents
    ) yield line1 + line2
  )
  override def toString: String = contents mkString "\n"
}
