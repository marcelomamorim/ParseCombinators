package br.unb.cic.parser

import scala.annotation.targetName

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
def fail[A] : Parser[A] = (input: String) => Nil

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
    bind(many(alphaOrDigit))((cs: List[Char]) => if (c::cs).mkString("") == k then pure(k) else fail))

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
def expParser: Parser[Expression] = choice(add)(choice(variable)(const))

def variable : Parser[Expression] = bind(identifier)((s: String) => pure(Variable(s)))
def const : Parser[Expression] = bind(number)((n: Int) => pure(Const(n)))
def add: Parser[Expression] =
  bind(expParser)((l: Expression) =>
    bind(symbol('+'))((c: Char) =>
      bind(expParser)((r: Expression) => pure(Add(l,r)))))
