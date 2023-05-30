package br.unb.cic.parser

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  test("item on empty string") {
    assert(item("") == Nil)
  }

  test("item on nonempty string") {
    assert(item("abc") == List(('a', "bc")))
  }

  test("tests for the sat parser") {
    assert(sat ((c: Char) => c.isDigit) ("9abc") == List(('9', "abc")))
    assert(sat ((c: Char) => c == ',') ("9abc") == Nil)
    assert(sat ((c: Char) => c == ',') (",abc") == List((',', "abc")))
  }

  test("tests for the digit parser") {
    assert(digit("9abc") == List(('9', "abc")))
    assert(digit("") == Nil)
    assert(digit("abc") == Nil)
  }

  test("tests for the many digit parser") {
    assert(many(digit)("123abc") == List((List('1', '2', '3'), "abc")))
    assert(many(digit)("abc") == List((Nil, "abc")))
    assert(many(digit)("1a1bc") == List((List('1'), "a1bc")))
  }

  test("tests for the pure parser") {
    assert(pure(4)("abc") == List((4, "abc")))
    assert(pure(4)("") == List((4, "")))
    assert(pure("MODULE")("abc") == List(("MODULE", "abc")))
  }

  ignore("tests for the fail parser") {
    assert(fail("abc") == Nil)
    assert(fail("") == Nil)
  }

  test("tests for the choice combinator") {
    assert(choice(digit)(alpha)("abc") == List(('a', "bc")))
    assert(choice(digit)(alpha)("9abc") == List(('9', "abc")))
    assert(choice(alpha)(digit)("9abc") == List(('9', "abc")))
    assert(choice(alpha)(alpha)("9abc") == List())
  }

  test("tests for the bind combinator") {
      assert(bind(digit)((c: Char) => pure(c.toInt))("abc") == List())
      assert(bind(digit)((c: Char) => pure(c.asDigit))("9abc") == List((9, "abc")))
      assert(number("9abc") == List((9, "abc")))
      assert(number("923abc") == List((923,"abc")))
      assert(sum("4+3abc") == List((7, "abc")))
  }

  test("tests for the keyword primitive") {
    assert(keyword("MODULE")("MODULE foo") == List(("MODULE", " foo")))
    assert(keyword("BEGIN")("MODULE foo") == List())
  }

  test("tests for identifiers") {
    assert(identifier("9bc") == List())
    assert(identifier("a9bc 123") == List(("a9bc", " 123")))
    assert(identifier("a_bc 123") == List(("a_bc", " 123")))
    assert(identifier("a>bc 123") == List(("a", ">bc 123")))
    assert(identifier("_a>bc 123") == List())
  }

  ignore("tests for parsing expressions") {
    assert(expParser("x+y+z") == List((Variable("x"), "+y+z")))
    assert(expParser("99+y+z") == List((Const(99), "+y+z")))
    assert(add("99+y") == List((Add(Const(99), Variable("y")), "")))
    assert(expParser("99+y") == List((Add(Const(99), Variable("y")), "")))
  }

  def sum: Parser[Int] =
    bind(number)((x: Int) =>
      bind(symbol('+'))(_ =>
        bind(number)((y: Int) => pure(x + y))))
}
