package pete1232

import org.scalatest.{FlatSpec, MustMatchers}
import Element.elem

class ElementSpec extends FlatSpec with MustMatchers{

  val contents = Array("Hello", "World")

  class TestElement (val testContents: Array[String]) extends Element {
    def contents = testContents
  }

  "height" must "return the length of contents" in new TestElement(contents) {
    height mustBe 2
  }
  "width" must "return the length of the first element of contents" in new TestElement(contents) {
    width mustBe 5
  }
  it must "return 0 if content is empty" in new TestElement(Array()){
    width mustBe 0
  }
  "above" must "compose two Elements" in new TestElement(contents) {
    above(new TestElement(contents)).contents mustBe new TestElement(contents ++ contents).contents
  }
  "below" must "concatenate the contents of two Elements" in new TestElement(contents) {
    beside(new TestElement(contents)).contents mustBe new TestElement(Array("HelloHello", "WorldWorld")).contents
  }
  "toString" must "return each line of contents seperated by a new line operator" in new TestElement(contents) {
    toString mustBe "Hello\nWorld"
  }
}

class ArrayElementSpec extends FlatSpec with MustMatchers {

  val contents = Array("Hello", "World")
  val badContents = Array("Hello", ", ", "World", "!")

  "ArrayElement" must "requre every element to have the same length" in {
    an [IllegalArgumentException] must be thrownBy elem(badContents)
  }
  "calling contents" must "not allow the user to modify the array" in {
    val element = elem(contents)
    element.contents.update(0, "Apple")
    element.contents.apply(0) mustBe "Hello"
  }
}

class LineElementSpec extends FlatSpec with MustMatchers {

  val content = "Hello"

  "height" must "return 1" in {
    elem(content).height mustBe 1
  }
  "width" must "return the length of s" in {
    elem(content).width mustBe 5
  }
}

class UniformElementSpec extends FlatSpec with MustMatchers {

  val content = 'I'
  val height = 3
  val width = 7

  "contents" must "return an array of the given height" in {
    elem(content, height, width).contents.length mustBe 3
  }
  it must "return an array of the given width" in {
    elem(content, height, width).contents.isEmpty mustBe false
    elem(content, height, width).contents.forall(_.length == 7) mustBe true
  }
  it must "hava a string filled with the given content on each row" in {
    elem(content, height, width).contents.isEmpty mustBe false
    elem(content, height, width).contents.forall(_ == "IIIIIII") mustBe true
  }
  it must "return an empty Array if height is 0" in {
    elem(content, 0, width).contents mustBe Array()
  }
  it must "return an Array of empty strings if width is 0" in {
    elem(content, height, 0).contents mustBe Array("", "", "")
  }
  it must "require height and width to be positive" in {
    an [IllegalArgumentException] must be thrownBy elem(content, height, -1)
    an [IllegalArgumentException] must be thrownBy elem(content, -1, width)
  }
}
