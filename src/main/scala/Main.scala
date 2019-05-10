import scala.io.StdIn

object my_main extends MyParser{

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

          parser.parseAll(parser.exp, cmmd)
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


