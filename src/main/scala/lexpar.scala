
import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io._
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader


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
    pw.print(name+"():")
    pw.close()
  }

  def usrInput(mtdname:String, param:AnyRef): Unit ={
    val myClass = new MyCases
    implicit def anyref2callable[T>:Null<:AnyRef](klass:T):Caller[T]=  Caller(klass)


    println("Expected output:")
    val x = StdIn.readLine()

    println("Your output:")

    paramMatch(mtdname,param.toString)
    val yrout = myClass call (mtdname, param)

    println(yrout.toString)

    if(x.trim==(yrout.toString.trim)){
      println("Correct! The method '"+ mtdname +"' passed the test" )
    } else{
      println("Incorrect! The output of '"+ mtdname +"' was not as expected")
    }
  }

  def paramMatch(ptype: String, param: String): AnyRef = {
    if (ptype.contentEquals("class scala.runtime.IntRef") | ptype.contentEquals("int")){
      Int.box(param.toInt)
    }
    else if (ptype.contains("mutable.ArrayBuffer")){
      val pattern = new Regex("[0-9]+")
      if(pattern.findAllMatchIn(param).toList.length > 0){
        val arb = new ArrayBuffer[Int]()
        val nopar = param.substring(1,param.length-1)
        val x = nopar.trim.split(",")
        for(i <- x){
          arb.+=(i.toInt)
        }
        arb
      }
      else{
        val arb = new ArrayBuffer[String]()
        val nopar = param.substring(2,param.length-2)
        val x = nopar.trim.split("','")
        for(i <- x){
          arb.+=(i.toString)
        }
        arb.toList
      }
    }
    else if (ptype.contentEquals("class Bool")){
      Boolean.box(param.toBoolean)
    }
    else{
      if(param.matches("[0-9]+")) param
      else param.substring(1,param.length-1)
    }
  }

  def getParam(str: String): String = {
    val first = str.indexOf("(") + 1
    val last = str.indexOf(")")

    str.substring(first, last)
  }

  def getIntList(str: String): ArrayBuffer[Int] = {
    val pattern = new Regex("[0-9]+")
    val arr = (pattern findAllIn str).toList

    val intArr = new ArrayBuffer[Int]()
    for(x <- arr){
      intArr.+=(x.toInt)
    }
    intArr
  }

  def getStrList(str:String): ArrayBuffer[String] = {
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

    strArr
  }

  def chkCondition(cnd: String, out: String): Boolean = {
    val arr = cnd.split(" ")
    if (arr(0) == "this") arr(0) = out
    println("Value of arr(0): "+ arr(0))
    val op = arr(1) match {
      case "<=" => {
        if(arr(0).toInt <= (arr(2)).toInt) true
        else false
      }
      case ">=" => {
        if(arr(0).toInt >= (arr(2)).toInt) true
        else false
      }
      case "<" => {
        if(arr(0) < (arr(2))) true
        else false
      }
      case ">" => {
        if(arr(0) > (arr(2))) true
        else false
      }
      case "==" => {
        if(arr(0) == (arr(2))) true
        else false
      }
      case "!=" => {
        if(arr(0) != (arr(2))) return true
        else false
      }
      case _ => false
    }
    op
  }


  def customParList(str: String): List[String] = {

    //val pattern = new Regex("[a-zA-Z]+")
    val a1 = str.trim.split(",")
    for(b <- a1){
      b.trim()
    }
     a1.toList

    // val arr = (pattern findAllIn str).toList
    //return arr
  }

  def exp: Parser[Any] = (("test" ~ "method" ~ id ~ "(" ~ term ~ ")")
    | ("test" ~ "method" ~ id ~ "(" ~ plist ~ ")")
    | ("create" ~ "void" ~ "tester" ~ id ~ "(" ~ ")")
    | ("execute" ~ id ~ "(" ~ ")")
    | "testAll" ) ^^ {

    case "test" ~ "method" ~ id ~ "(" ~ term ~ ")" => {

      val mtdname = id.toString.substring(3, id.toString.length - 1)

      if(term.toString.contains("List")){
        println(term.toString)
        if (term.toString.contains("Integer")){
          val param = getIntList(term.toString)
          usrInput(mtdname, param)
        }
        else{
          val pstr: ArrayBuffer[String] = getStrList(term.toString)
          usrInput(mtdname, pstr)
        }
      }
      else{
        val param = getParam(term.toString)

        println(param)
        val myClass = new MyCases
        var ptype = ""
        for(m <- myClass.getClass.getDeclaredMethods){
          if(m.getName==mtdname){
            val tp = m.getParameterTypes.mkString
            ptype = tp
          }
        }
        val ptr = paramMatch(ptype, param)
        usrInput(mtdname, param)
      }


    }

    case "execute" ~ id ~ "(" ~ ")" => {
      val fileName = id.toString.substring(3, id.toString.length - 1)
      val filesrc = Source.fromFile("src/main/scala/"+fileName + ".txt")
      val fileContents = filesrc.getLines.toList

      val myClass = new MyCases
      implicit def anyref2callable[T>:Null<:AnyRef](klass:T):Caller[T]=  Caller(klass)

      val mtdname = fileContents(1).substring(0, fileContents(1).indexOf("("))
      val param = getParam(fileContents(1))

      var ptype = ""
      for(m <- myClass.getClass.getDeclaredMethods){
        if(m.getName==mtdname){
          val tp = m.getParameterTypes.mkString
          ptype = tp
        }
      }
      val condition = getParam(fileContents(2))
      val ptr = paramMatch(ptype, param)
      val yrout = myClass call (mtdname, ptr)

      println("Method output: " + yrout.toString)

      if (condition.trim == "true") {
        println("Custom Tester: PASSED the test")
      }else if (condition.trim == "false"){
        println("Custom tester: FAILED the test")
      }else{
        if(chkCondition(condition, yrout.toString)) println("Custom Tester: PASSED the test")
        else println("Custom tester: FAILED the test")
      }

      filesrc.close()

    }

    case "create" ~ "void" ~ "tester" ~ id ~ "(" ~ ")" => {
      val fileName = id.toString.substring(3, id.toString.length - 1)
      createFile(fileName)
    }


    case "testAll" =>{
      val myClasssss = new MyCases
      val allMetd = myClasssss.getClass.getDeclaredMethods

      for (i <- allMetd){

        try{

          if (i.getName != "$deserializeLambda$" && i.getName != "$anonfun$multiply$1" && i.getName != "fiboHelp") {
            println("Method: " + i.getName)
            val m = i.getParameterTypes.mkString
           // println(m)

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
      println("Done!")
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
  def call(methodName:String,args: AnyRef*):AnyRef = {
    def argtypes = args.map(_.getClass)
    def method = klass.getClass.getMethod(methodName, argtypes: _*)
    method.invoke(klass,args: _*)
  }
}
