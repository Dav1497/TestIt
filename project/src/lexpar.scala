import java.io

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.io._
import java.io.{File, PrintWriter}

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex


//import scala.sys.process.processInternal.File

sealed trait Token

case class Id(id: Any) extends Token
case class Integer(int: String) extends Token
case class Word(word: String) extends Token
case class Operator(str: String) extends Token
case class Delimiter(str: String) extends Token
case class Sign(sign: Operator) extends Token
case class Bool(str: String) extends Token
case class Proplist(listed: Any, list: List[Any]) extends Token
case class Mylist(list: Any) extends Token
case class PropParamlist(plisted: Any, list: List[Any]) extends Token
case class ParamList(plist: Any) extends Token
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

  def proplist: Parser[Any] = ("'" ~ word ~ "'" ~ rep("," ~> "'" ~ word ~ "'")) | (int ~ rep("," ~> int)) ^^ {
    case id ~ Nil => id
    case id ~ list => Proplist(id, list)
  }
  def mylist: Parser[Any] = rep(proplist) ^^ {
    case Nil => Mylist(())
    case p => Mylist(p)
  }

  def propparamlist: Parser[Any] = (term ~ rep("," ~> term)) ^^ {
    case t ~ Nil => t
    case t ~ list => Proplist(t, list)
  }
  def plist: Parser[Any] = rep(propparamlist) ^^ {
    case Nil => Mylist(())
    case p => Mylist(p)
  }

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
    ( "[" ~ mylist ~ "]"
      | (sign ~ int)
      | Null
      | empty
      | bool
      | int
      | ("'" ~> word <~ "'")) ^^ {
      case NULL => Term(NULL, None)
      case EMPTY => Term(EMPTY, None)
      case Bool(b) => Bool(b)
      case Operator(x)~ Integer(i) => Term(Operator(x), Integer(i))
      case Integer(i) => Integer(i)
      case Word(w) => Word(w)
      case "[" ~ Mylist(x) ~ "]" => Mylist(x)
    }



  //metodo para hacer files
  def createFile(name: String) = {

    val completePath = name +".txt"
    //val file = new File(completePath)
    //file.createNewFile();
    println(completePath)
    val mahFile = new File(completePath)
    val pw = new PrintWriter(mahFile)

  }

  def usrInput(mtdname:String, param:AnyRef): Unit ={
    val myClass = new MyCases
    implicit def anyref2callable[T>:Null<:AnyRef](klass:T):Caller[T]= new Caller(klass)


    println("Expected output:")
    val x = StdIn.readLine()

    println("Your output:")

    val yrout = {
      myClass call (mtdname, param)
      //myClass call(mtdname, param)
    }


    println(yrout.toString)

    if(param==(yrout.toString)){
      println("Correct! The method '"+ mtdname +"' returned '"+ param + "'" )
    } else{
      println("Incorrect! The output of '"+ mtdname +"' was not as expected")
    }
  }

  def paramMatch(ptype: String, param: String): AnyRef = {
    if (ptype.contentEquals("class scala.runtime.IntRef") | ptype.contentEquals("int")){
      return param
    }
    else if (ptype.contentEquals("class scala.collection.immutable.List")){
      println(param.toList.toString())
      return param.toList

    }
    else if (ptype.contentEquals("class Bool")){
      return param
    }
    else{
      return param
    }
  }

  def getParam(str: String): String = {
    val first = str.indexOf("(") + 1
    val last = str.indexOf(")")

    return str.substring(first, last)
  }

  def getIntList(str: String): List[Int] = {
    val pattern = new Regex("[0-9]+")
    val arr = (pattern findAllIn str).toList

    val intArr = new ArrayBuffer[Int]()
    for(x <- arr){
      intArr.+=(x.toInt)
    }

    println(intArr.toList)
    return intArr.toList
  }

  def getStrList(str:String):List[String] = {
    val notAllowd = List("Word", "List", "Mylist")

    val pattern = "[a-zA-Z]+".r

    val arr = pattern.findAllMatchIn(str)

    val strArr = new ArrayBuffer[String]()
    for(x <- arr){
      if (notAllowd.contains(x.toString())){}
      else{
        strArr.+=(x.toString)
      }
    }
    println(strArr.toList)
    return strArr.toList
  }




  def customParList(str: String): List[String] = {

    //val pattern = new Regex("[a-zA-Z]+")
    val a1 = str.trim.split(",")
    for(b <- a1){
      b.trim()
    }
    return a1.toList

    // val arr = (pattern findAllIn str).toList
    //return arr
  }

  def exp: Parser[Any] = (("test" ~ "method" ~ id ~ "(" ~ term ~ ")")
    | ("test" ~ "method" ~ id ~ "(" ~ plist ~ ")")
    | ("create" ~ "void" ~ "tester" ~ id ~ "(" ~ ")")
    | ("execute" ~ id ~ "(" ~ ")")
    | ("testAll" ~ "(" ~ ")")) ^^ {

    case "test" ~ "method" ~ id ~ "(" ~ term ~ ")" => {

      val mtdname = id.toString.substring(3, id.toString.length - 1)

      if(term.toString.contains("List")){
        println(term.toString)
        if (term.toString.contains("Integer")){
          val param = getIntList(term.toString)
          usrInput(mtdname, param)
        }
        else{
          val pstr = getStrList(term.toString)
          usrInput(mtdname, pstr)
        }
      }
      else{
        val param = getParam(term.toString)

        println(param)
        usrInput(mtdname, param)
      }


    }

    case "execute" ~ id ~ "(" ~ ")" => {
      val fileName = id.toString.substring(3, id.toString.length - 1)
      val fileContents = Source.fromFile(fileName + ".txt").getLines.mkString
      println(fileContents)
    }

    case "create" ~ "void" ~ "tester" ~ id ~ "(" ~ ")" => {
      val fileName = id.toString.substring(3, id.toString.length - 1)
      createFile(fileName)
    }


    case "testAll" ~ "(" ~ ")" =>{
      val myClasssss = new MyCases
      val allMetd = myClasssss.getClass.getDeclaredMethods
      for (i <- allMetd){

        try{
          if (i.getName != "$deserializeLambda$" && i.getName != "$anonfun$multiply$1") {
            println("Method: " + i.getName)
            val m = i.getParameterTypes.toString

            println("Specify parameters: ")
            val x = StdIn.readLine()

            val ptr = paramMatch(m, x)
            usrInput(i.getName, ptr)
          }
        }catch  {
          case e: Exception => println("Error with method")
        }
        println()

      }
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

    method.invoke(klass, args: _*)
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

  def add5Int( a:String ) : Int = {
    var sum:Int = 0
    sum = a.toInt + 5
    return sum
  }

  def isTrue(c: Bool):Boolean = {
    return false
  }

  /*def addInt(a : String, n : Int) : Unit = {
    var sum : Int = 0
    sum = a.toInt + n
    println(sum)
  }*/

  def sayHi(i: String) : String = {
    return i
  }

  def fibo(s: String): Int = {
    val n : Int = s.toInt
    if (n == 0) return 0
    else if (n == 1) return 1
    else fibo((n - 1).toString) + fibo((n - 2).toString)
  }

  def sortDecreasing(list: List[String]) : List[String] = {
    list.sorted
    return list.reverse
  }


  def concatenation(s1 : String) : String = {
    val r = s1.concat(" Friend")
    return r
  }

  def revStr(s : String) : String = {
    return  s.reverse
  }

  def firstCharInStr(s : String) : Char = {
    return  s.charAt(0)
  }

  def upStr(s : String) : String = {
    return  s.toUpperCase
  }

  def isEmptyStr(s : String) : Boolean = {
    return  s.isEmpty
  }

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