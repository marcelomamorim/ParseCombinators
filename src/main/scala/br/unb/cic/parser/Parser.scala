package br.unb.cic.parser

//class Parser[A](val p: String => List[(A, String)])
//
//def run[A](parser: Parser[A], s : String) = parser.p(s)


type Parser[A] = String => List[(A, String)]

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

def many[A](p : Parser[A]) : Parser[List[A]] = (input : String) => p(input) match {
    case Nil => List((Nil, input))
    case List((output, tail)) =>
      many(p)(tail) match {
        case List((f, foo)) =>  List((output::f, foo))
        case _ => List()
      }
    case _ => List()
}
