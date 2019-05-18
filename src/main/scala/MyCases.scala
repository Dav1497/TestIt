import scala.collection.mutable.ArrayBuffer

class MyCases {

  def sayHi(i: String) : String = {
    i
  }

  def add5Int( a:String ) : Int = {
    var sum:Int = 0
    sum = a.toInt + 5
    sum
  }

  def fibo(n: String): Int = {
    val i =  n.toInt


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
    return list.sorted.reverse.toList
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

  def isSorted(x : ArrayBuffer[Int]) :Boolean ={
    val y = x.sorted
    var boo = true
    for(i <- 0 until x.length){
      if(x(i).!=(y(i)))
        boo = false
    }
    boo
  }

  def isReversed(x : ArrayBuffer[Int]) :Boolean ={
    val y = x.reverse
    var boo = true
    for(i <- 0 until x.length){
      if(x(i).!=(y(i)))
        boo = false
    }
    boo
  }

  def isSorted(x : ArrayBuffer[Int]) :Boolean ={
    val y = x.sorted
    var boo = true
    for(i <- 0 until x.length){
      if(x(i).!=(y(i)))
        boo = false
    }
    boo
  }

}