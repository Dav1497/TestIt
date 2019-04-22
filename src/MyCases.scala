class MyCases {

  def addInt(a : String, n : Int) : Int = {
    var sum : Int = 0
    sum = a.toInt + n
    println(sum)
    return sum
  }

  def sayHi(i: String) = {
    println(i)
  }

  def fibo(n: Int): Int = {
    if (n == 0) return 0
    else if (n == 1) return 1
    else fibo(n - 1) + fibo(n - 2)
  }

  def sortDecreasing(list: List[Int]) = {
    list.sorted
    list.reverse
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