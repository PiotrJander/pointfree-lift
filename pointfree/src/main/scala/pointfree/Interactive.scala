package pointfree

//import scala.io.StdIn
//
//object Interactive {
//  def main(args: Array[String]) {
//    var program = Reduce(Plus) *: Map(Plus) *: Map(Plus) *: Map(Plus)
//    while (true) {
//      // foo
//      var applicable = Equiv.applicableRules(program)
//      var i = 0
//
//      var c: Char = '\0'
//      while (c != '\n' && c != 'j' && c != 'k') {
//        c = StdIn.readChar()
//        c match {
//          case '\n' =>
//            // apply rule to program
//          case 'k' =>
//            i = (i - 1) % applicable.length
//          case 'j' =>
//            i = (i + 1) % applicable.length
//        }
//      }
//
//      // print the rules
//      applicable.foreach(r => println(r._1))
//    }
//  }
//}

import java.util.{TimerTask, Timer}

import net.team2xh.onions.{Palettes, Themes}
import net.team2xh.onions.components.Frame
import net.team2xh.onions.components.widgets._
import net.team2xh.onions.utils.{Varying, Lorem, TextWrap}
import net.team2xh.scurses.{Colors, Scurses}
import net.team2xh.scurses.RichText._

import scala.util.Random

object Interactive extends App {

  Scurses { implicit screen =>
    implicit val debug = true
    val frame = Frame(title = Some("Pointfree Lift derivations"),
      debug = true, theme = Themes.default)

    val colA = frame.panel
    val colB = colA.splitRight
    val colB2 = colB.splitDown

    colA.title = "Applicable rules"
    colB.title = "Derivation"
    colB2.title = "Current program and type"

    Label(colA, "foo").enabled = false

    colA.

//    Label(colA, )
//
//    val rewrites = List(
//      "map distributes through composition",
//      "commute filter reduce"
//    )
//
//    rewrites.foreach(Lab)

//    val foo = Label(colB, "foo\n= { ... }\nbar")
//
//    Label(colB2, "map f . map g")
//    Label(colB2, "[Float] -> Float")

    frame.show
  }
}
