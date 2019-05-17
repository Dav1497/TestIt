import scala.collection.mutable.ArrayBuffer

class MyCases {

  def add5Int( a:String ) : Int = {
    var sum:Int = 0
    sum = a.toInt + 5
    sum
  }

  def isTrue(c: String):Boolean = {
    var boo =false

      if(c.toLowerCase()=="ru"){
        boo = true
      }
    println("Testing "+boo+ " "+ c)
      boo
  }

  /*def addInt(a : String, n : Int) : Unit = {
    var sum : Int = 0
    sum = a.toInt + n
    println(sum)
  }*/

  def sayHi(i: String) : String = {
     i
  }

  def fibo(n: String): Int = {

    //try {
      val i =  n.toInt
   // } catch {
  //    case e: Exception => 0
  //  }
    if (i == 0) return 0
    else if (i == 1) return 1
    else fiboHelp(i - 1) + fiboHelp(i - 2)
  }

  def fiboHelp(i: Int): Int ={
    if (i == 0) return 0
    else if (i == 1) return 1
    else fiboHelp(i - 1) + fiboHelp(i - 2)
  }

  def sortDecreasing(list: ArrayBuffer[Int]) : List[Int] = {
    list.sorted
    return list.toList.reverse
  }

  def multiply (list: ArrayBuffer[Int]) : Int = {
    var product = 1
    list.foreach(product *=_)
    return product
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