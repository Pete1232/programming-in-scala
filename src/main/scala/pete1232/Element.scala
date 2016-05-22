package pete1232

import Element.elem
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

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

/* TODO add support for elements with content of varying width
 * height, width, above and beside assume every line of the Element has the same length
 */
abstract class Element {
  val logger = Logger(LoggerFactory.getLogger("Element"))
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if(!contents.isEmpty){(for(line <- contents) yield line.length).max} else 0
  def above(that: Element): Element = {
    logger.debug(s"calling above on this ${this.toString} with that ${that.toString}")
    val this1 = this.widen(that.width)
    val that1 = that.widen(this.width)
    elem(this1.contents ++ that1.contents)
  }
  def beside(that: Element): Element = {
    logger.debug(s"calling beside on this ${this.toString} with that ${that.toString}")
    val this1 = this.heighten(that.height)
    val that1 = that.heighten(this.height)
    elem(
      for (
        (line1, line2) <- this1.contents zip that1.contents
      ) yield line1 + line2
    )
  }
  def heighten(h: Int): Element = if(h <= height) this else {
    logger.debug(s"calling heighten with params ($h) on element of height $height")
    val top = elem(' ', (h - height)/2, width)
    logger.debug(s"heighten.top height: ${top.height}")
    val bot = elem(' ', h - height - top.height, width)
    logger.debug(s"heighten.bot height: ${bot.height}")
    top above this above bot
  }
  def widen(w: Int): Element = if(w <= width) this else {
    logger.debug(s"executing widen with params ($w) on element of width $width")
    val left = elem(' ', height, (w - width)/2)
    logger.debug(s"widen.left width: ${left.width}")
    val right = elem(' ', height, (w - width - left.width))
    logger.debug(s"widen.right width: ${right.width}")
    left beside this beside right
  }
  override def toString: String = contents mkString "\n"
}
