import scala.collection.mutable.ArrayBuffer

class MyCases {

  def add5Int( a:String ) : Int = {
    var sum:Int = 0
    sum = a.toInt + 5
    sum
  }

  def isTrue(c: Bool):Boolean = {
     false
  }

  /*def addInt(a : String, n : Int) : Unit = {
    var sum : Int = 0
    sum = a.toInt + n
    println(sum)
  }*/

  def sayHi(i: String) : String = {
     i
  }

  def fibo(n: Int): Int = {
    //val n : Int = s.toInt
    if (n == 0) return 0
    else if (n == 1) return 1
    else fibo(n - 1) + fibo(n - 2)
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