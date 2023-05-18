package br.unb.cic.parser

//class Parser[A](val p: String => List[(A, String)])
//
//def run[A](parser: Parser[A], s : String) = parser.p(s)


type Parser[A] = String => List[(A, String)]

def item: Parser[Char] = (input: String) => input match {
  case "" => Nil
  case _ => List((input.head, input.tail))
}

def sat(p : Char => Boolean) : Parser[Char] = (input : String) => input match {
  case "" => Nil
  case _  => if(p(input.head)) then  List((input.head, input.tail)) else Nil
}

def digit = sat (c => c.isDigit)
def alpha = sat (c => c.isLetter)
def alphaOrDigit = sat (c => c.isLetterOrDigit)

def many[A](p : Parser[A]) : Parser[List[A]] = (input : String) => p(input) match {
    case Nil => List((Nil, input))
    case List((output, tail)) =>
      many(p)(tail) match {
        case List((f, foo)) =>  List((output::f, foo))
      }
}
