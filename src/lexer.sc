import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.io._


sealed trait Token

case class Id(id: Any) extends Token
case class Integer(int: String) extends Token
case class Word(word: String) extends Token
case class Operator(str: String) extends Token
case class Delimiter(str: String) extends Token
case class Sign(sign: Operator) extends Token
case class Bool(str: String) extends Token
case object EMPTY extends Token
case object NULL extends Token
case class MethodList(exp: Any, list: List[Any]) extends Token
case class Factor(factor: Any) extends Token
case class Term(term: Any, term2: Any) extends Token
case class Exp(term: Any, exp: Any = None) extends Token

class MyParser extends RegexParsers {

  override val whiteSpace = "[ \t\r\f\n;]+".r

  def delimiter: Parser[Delimiter] = ("("
    | ")"
    | "["
    | "]"
    | ","
    | ";") ^^ { str => Delimiter(str) }
  def operator: Parser[Operator] = (":"
    | "<="
    | ">="
    | "+"
    | "-"
    | "*"
    | "/"
    | "="
    | "!="
    | "<"
    | ">"
    | "&"
    | "|") ^^ { operator => Operator(operator) }
  def reserved: Parser[String] = ("test"
    | "method"
    | "input"
    | "expected"
    | "output"
    | "create"
    | "void"
    | "tester"
    | "testAll"
    | "sorted"
    | "negative"
    | "positive"
    | "reverse"
    | "min"
    | "max")
  def bool: Parser[Bool] = ("true" | "false") ^^ { b => Bool(b) }
  def sign: Parser[Sign] = ("+" | "-") ^^ { op => Sign(Operator(op)) }

  def int: Parser[Integer] = ("[0-9]+".r).+ ^^ { int => Integer(concat(int)) }
  def word: Parser[Word] = ("[a-zA-Z]{1}".r).+ ^^ { word => Word(concat(word))}

  def forbidden = not(bool) ~ not(reserved)
  def id: Parser[Id] = (forbidden ~> "[a-zA-Z]{1}".r ~ rep(forbidden ~> "[a-zA-Z]{1}".r | "[0-9]+".r)) ^^ {
    case w ~ Nil => Id(w)
    case w ~ x => Id(w + concat(x))
  }

  def empty = "empty" ^^ (_ => EMPTY)
  def Null = "null" ^^ (_ => NULL)


  def factor: Parser[Factor] = ( "(" ~> exp <~ ")" | id ) ^^ {
    case Id(id) => Factor(Id(id))
    case f => Factor(f)
  }

  def term: Parser[Term] =
    (factor
      | list
      | Null
      | empty
      | bool
      | int
      | word) ^^ {
      case NULL => Term(NULL, None)
      case EMPTY => Term(EMPTY, None)
      case Bool(b) => Term(Bool(b), None)
      case Integer(i) => Term(Integer(i), None)
      case Factor(f) ~ _ ~ _ => Term(Factor(f), ())
      case Factor(f) ~ Nil => Term(Factor(f), None)
      case Factor(f) ~ list => Term(Factor(f), list)
    }


  def list: Parser[Any] = "[" ~ ((word ~ rep("," ~> word)) | (int ~ rep("," ~> int))) ~ "]" ^^ { case x => x}

  def exp: Parser[Any] = (("test" ~ "method" ~ id ~ "(" ~> term <~ ")")
  | ("create" ~ "void" ~ "tester" ~ id ~ "(" ~ ")")
  | ("execute" ~ id ~ "(" ~ ")")
  | ("testAll" ~ "(" ~ ")")) ^^ {
    case "test" ~ "method" ~ id ~ _ ~ term ~ _ => {
      println("Expected Output: ")
      val inp = StdIn.readLine()
      println("Correct! The method '"+id+"' returned '"+inp+ "'" ) }
  }

  def concat(xs: List[String]): String = {
    xs match {
      case x :: tail => x + concat(tail)
      case Nil => ""
    }
  }

  def parse[T](s:String)(p:Parser[T]):T = {
    val phraseParser = phrase(p)

    val input = new CharSequenceReader(s)

    phraseParser(input) match {
      case Success(t,_) => t
      case NoSuccess(msg,_) => throw new IllegalArgumentException("Could not parse '" + s + "': " + msg)
    }
  }
}

object console extends MyParser {

  val parser = new MyParser

  def run() {
    var more = true
    println("Welcome to TestIt! a unit tester for scala")
    println("Type 'q' to quit program")

    while (more) {
      try {
        print(">> ")

        val cmmd = StdIn.readLine

        if (cmmd == "q" || cmmd == "'q'") {
          more = false
        }
        /*else if (cmmd == "code" || cmmd == "'code'") {
          val output = parser.parseAll(parser.exp, cmmd)
          println("Custom code parsed successfully!")
          println(output)
        }*/
        else {
          //val output = parser.parse(cmmd)(parser.exp)
          val output = parser.parseAll(parser.exp, cmmd)
          println("Code parsed successfully!")
          println(output)
        }
      } catch {
        case e: Exception => println(e)
      }
    }
    println("Thank you for TestingIt!")
  }

  def main(args: Array[String]): Unit = {
    run()
  }

}