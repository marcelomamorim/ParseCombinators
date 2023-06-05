package br.unb.cic.parser

type Parser[A] = String => List[(A, String)]

/* the first layer (more primitive) parsers */

def item: Parser[Char] = (input: String) => input.toList match {
  case Nil => Nil
  case h::rest => List((h, rest.mkString))
}

def sat(p : Char => Boolean) : Parser[Char] = (input : String) => input.toList match {
  case Nil => Nil
  case h::rest  => if p(h) then  List((h, rest.mkString)) else Nil
}

def digit = sat (c => c.isDigit)
def alpha = sat (c => c.isLetter)
def alphaOrDigit = sat (c => c.isLetterOrDigit)
def symbol(c1: Char) = sat ((c2: Char) => c1 == c2)

def many[A](p : Parser[A]) : Parser[List[A]] = (input : String) => p(input) match {
    case Nil => List((Nil, input))
    case List((o1, r1)) =>
      many(p)(r1) match {
        case List((o2, r2)) =>  List((o1::o2, r2))
        case _ => List()                                // we should reason about this case here
      }
    case _ => List()                                    // and also here.
}

/** Pure transforms a value into a parser that always succeed */
def pure[A](value: A): Parser[A] = (input: String) => List((value, input))

/** Another primitive parser that always fails */
def failed[A] : Parser[A] = (input: String) => List()

/** a combinator that receives two parsers and return the first one if it succeeds. Otherwise, returns the second one. */
infix def choice[A] (p: Parser[A])(q: Parser[A]): Parser[A] = (input: String) => p(input) match {
  case Nil => q(input)
  case List((a, s)) => List((a,s))
  case _ => ???                                   // ambiguity
}

/** a combinator that might be used for sequencing parsers
 *  examples:
 *    - digit * (c: Char) => return (c.toInt)
 *    - digit * (x: Integer) => (sat '+' * (_: char) => (digit * (y: Integer) => return x + y))
 */
def bind[A,B] (p: Parser[A])(f : A => Parser[B]): Parser [B] = (input: String) => p(input) match {
  case Nil => Nil       // we propagate the failure
  case List((a, r)) => f(a)(r)
  case _ =>  ???        // what the hell ... ambiguity
}

/* second layer parser definitions */

def space = symbol(' ')
def spaces = many(space)

def number: Parser[Int] = bind(digit)((c: Char) =>
  bind(many(digit))((cs: List[Char]) => pure((c::cs).mkString("").toInt)))

def keyword(k: String) : Parser[String] =
  bind(alpha)((c:Char) =>
    bind(many(alphaOrDigit))((cs: List[Char]) => if (c::cs).mkString("") == k then pure(k) else failed))

def identifier : Parser[String] =
  bind(alpha)((c:Char) =>
    bind(many(choice(alphaOrDigit)(symbol('_'))))((cs: List[Char]) => pure((c::cs).mkString(""))))

/* Lexical definitions */


// TODO: We should call and ignore spaces!

val module = keyword("MODULE")
val begin = keyword("begin")
val end = keyword("end")


/* Expressions */
type Identifier = String

sealed trait Expression

case class Variable(id: Identifier) extends Expression
case class Const(v: Int) extends Expression
case class Add(l: Expression, r: Expression) extends Expression
case class Mul(l: Expression, r: Expression) extends Expression

/* TODO: stack overflow here. we must rewrite this implementation using the left factor pattern */
def variable : Parser[Expression] = bind(identifier)((s: String) => pure(Variable(s)))
def const : Parser[Expression] = bind(number)((n: Int) => pure(Const(n)))

//TODO: expression parser using left factoring
def expParser(expression: String): Expression = {
  term(expression).headOption.map(_._1).getOrElse(throw new RuntimeException("testing..."))
}
def term: Parser[Expression] = input => {
  factor(input) match {
    case (left, rest) :: tail =>
      if (rest.nonEmpty && isArithmeticOperation(rest)) {
        val parsedChar = rest.head
        val remaining = trimLeft(rest.tail)
        val (right, newRest) = term(remaining).head
        val result = chooseArithmeticOperation(left.asInstanceOf[Const], parsedChar, right.asInstanceOf[Const])
        (Const(result), newRest) :: tail
      } else {
        (left, rest) :: tail
      }
    case _ => Nil
  }
}

def factor: Parser[Expression] = input => {
  if (input.nonEmpty && input.head.isDigit) {
    val (digit, remaining) = input.span(_.isDigit)
    (Const(digit.toInt), trimLeft(remaining)) :: Nil
  } else {
    Nil
  }
}

/**
 *
 * Métodos auxiliares ao parser de expressions
 *
 */
def trimLeft(input: Identifier): String = input.dropWhile(_.isWhitespace)
def isSumIdentifier(rest: Identifier) = rest.head == '+'
def isSubtractionIdentifier(rest : Identifier) = rest.head == '-'
def isArithmeticOperation(rest: Identifier) = isSumIdentifier(rest) || isSubtractionIdentifier(rest)
def sumTerms(left: Const, right: Const) = left.v + right.v
def subtractTerms(left: Const, right: Const) = left.v - right.v

def chooseArithmeticOperation(left: Const, parsedChar: Char, right: Const): Int = parsedChar match {
  case '+' => sumTerms(left, right)
  case '-' => subtractTerms(left, right)
  case _ => throw new IllegalArgumentException(s"Invalid operator: $parsedChar")
}
