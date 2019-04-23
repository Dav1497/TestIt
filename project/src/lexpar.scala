import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.io._
import java.io.File
import java.io.PrintWriter

//import scala.sys.process.processInternal.File

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

  def int: Parser[Integer] = "[0-9]+".r.+ ^^ { int => Integer(concat(int)) }
  def word: Parser[Word] = "[a-zA-Z]{1}".r.+ ^^ { word => Word(concat(word))}

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

  def term: Parser[Any] =
    (factor
      | list
      | Null
      | empty
      | bool
      | int
      | ("'" ~> word <~ "'")) ^^ {
      case NULL => Term(NULL, None)
      case EMPTY => Term(EMPTY, None)
      case Bool(b) => Term(Bool(b), None)
      case Integer(i) => Integer(i)
      case Word(w) => Term(Word(w), None)
      case Factor(f) ~ _ ~ _ => Term(Factor(f), ())
      case Factor(f) ~ Nil => Term(Factor(f), None)
      case Factor(f) ~ list => Term(Factor(f), list)
    }


  def list: Parser[Any] = "[" ~ ((word ~ rep("," ~> word)) | (int ~ rep("," ~> int))) ~ "]" ^^ { case x => x}

  //metodo para hacer files
  def createFile(name: String) = {

    val completePath = name +".txt"

    //val file = new File(completePath)
    //file.createNewFile();
    println(completePath)
    val mahFile = new File(completePath)

    val pw = new PrintWriter(mahFile)


  }

  def exp: Parser[Any] = (("test" ~ "method" ~ id ~ "(" ~ term ~ ")")
    | ("create" ~ "void" ~ "tester" ~ id ~ "(" ~ ")")
    | ("execute" ~ id ~ "(" ~ ")")
    | ("testAll" ~ "(" ~ ")")) ^^ {
    case "test" ~ "method" ~ id ~ _ ~ term ~ _ => {

      val mtdname = id.toString.substring(3, id.toString.length - 1)
      val param = term.toString.substring(8, term.toString.length - 1)

      val myClass = new A
      implicit def anyref2callable[T>:Null<:AnyRef](klass:T):Caller[T]= new Caller(klass)
      println("Expected Output: ")
      val inp = StdIn.readLine()
      println("Your output:")
      val yrout = myClass call(mtdname, param)

      if(inp == yrout.toString){
        println("Correct! The method '"+ mtdname +"' returned '"+ inp + "'" )
      } else{
        println("Incorrect! The output of '"+ mtdname +"' was not as expected")
      }

    }
    case "create" ~ "void" ~ "tester" ~ id ~ "(" ~ ")" => {
      val fileName = id.toString.substring(3, id.toString.length - 1)
      createFile(fileName)
    }
    case "testAll" ~ "(" ~ ")" =>{
      val myClasssss = new MyCases
      myClasssss.sayHi("holis bitch")
      println("Enter")
    }
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

case class Caller[T>:Null<:AnyRef](klass:T) {
  def call(methodName:String,args:AnyRef*):AnyRef = {
    def argtypes = args.map(_.getClass)
    def method = klass.getClass.getMethod(methodName, argtypes: _*)
    method.invoke(klass,args: _*)
  }
}


class A{
  def addInt( a:String ) : Int = {
    var sum:Int = 0
    sum = a.toInt + 5
    println(sum)
    return sum
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
        else {

          val output = parser.parseAll(parser.exp, cmmd)
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
//-----------------------------------------------------------------------------------------------------

class MyCases {

  def addInt(a : String, n : Int) : Unit = {
    var sum : Int = 0
    sum = a.toInt + n
    println(sum)
  }

  def sayHi(i: String) : Unit = {
    println(i)
  }

  def fibo(n: Int): Int = {
    if (n == 0) return 0
    else if (n == 1) return 1
    else fibo(n - 1) + fibo(n - 2)
  }

  def sortDecreasing(list: List[Int]) : List[Int] = {
    list.sorted
    return list.reverse
  }

  def multiply (list: List[Int]) : Int = {
    var product = 1
    list.foreach(product *= _)
    return product
  }


  addInt("1", 2)

  sayHi("Hello World!")

  var i = 0
  while (i < 11) {
    println("Fibonacci: " + i + " = " + fibo(i))
    i += 1
  }

  var toDo: List[Int] = List(1, 9, 33, 24)
  println(sortDecreasing(toDo))

  println(multiply(toDo))
}

//-----------------------------------------------------------------------------------------------------------


class TestingTools {
  def rand_list(n : Int) = {
    val r = new scala.util.Random
    1 to n map { _ => r.nextInt(100) }
  }

  def isPos(x : Int*) = {
    x.filter(x => (x > 0)).isEmpty == false
  }

  def isNeg(x : Int*) = {
    x.filter(x => (x < 0)).isEmpty == false
  }

  var x = rand_list(10)
  var xNegative = x.map(_ * -1)


  println("X : " + x)
  println("X min : " + x.min)
  println("X max : " + x.max)
  println("X reversed : " + x.reverse)
  println("X sorted in increasing order : " + x.sorted)
  println("Does X possess a positive number? : " + isPos(x: _*))
  println("Does X possess a negative number? : " + isNeg(x: _*))
  println("xNegative : " + xNegative)
  println("Does xNegative possess a positive number? : " + isPos(xNegative: _*))
  println("Does xNegative possess a negative number? : " + isNeg(xNegative: _*))
}
