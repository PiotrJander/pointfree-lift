package pointfree

import scala.io.StdIn

object Interactive {
  def main(args: Array[String]) {
    var program = Reduce(Plus) *: Map(Plus) *: Map(Plus) *: Map(Plus)
    while (true) {
      // foo
      var applicable = Equiv.applicableRules(program)
      var i = 0

      var c: Char = '\0'
      while (c != '\n' && c != 'j' && c != 'k') {
        c = StdIn.readChar()
        c match {
          case '\n' =>
            // apply rule to program
          case 'k' =>
            i = (i - 1) % applicable.length
          case 'j' =>
            i = (i + 1) % applicable.length
        }
      }

      // print the rules
      applicable.foreach(r => println(r._1))
    }
  }
}