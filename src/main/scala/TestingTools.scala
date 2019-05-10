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
