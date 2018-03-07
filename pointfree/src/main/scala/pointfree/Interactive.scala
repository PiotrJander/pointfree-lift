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

import net.team2xh.onions.Themes
import net.team2xh.onions.components.Frame
import net.team2xh.onions.components.widgets._
import net.team2xh.onions.utils.Varying
import net.team2xh.scurses.{Colors, Scurses}
import net.team2xh.scurses.RichText._

object Interactive extends App {

  val programs: List[Expr] = List(
    Programs.csrMV,
    Programs.maxSegSum
  )
  val program_data = new Select.Data(programs.map(_.toString))

  var currentProgram: Varying[Expr] = new Varying(programs.head)
  var currentProgramTyp: Varying[Type] = new Varying(currentProgram.value.typ)
  var currentProgram_items: Varying[List[String]] = new Varying(getCurrentProgram_items)
  currentProgram.subscribe { () =>
    currentProgramTyp := currentProgram.value.typ
    currentProgram_items := getCurrentProgram_items
  }

  val applicableRules_data = new Select.Data(List("foo", "bar"))

  var derivation_items: Varying[List[String]] = new Varying(List("foo", "bar"))

  def programs_select(): Unit = {
    // TODO clear derivation
  }

  def getCurrentProgram_items: List[String] = List(
    currentProgram.value.toString,
    currentProgramTyp.value.toString
  )

  Scurses { implicit screen =>
    implicit val debug = true
    val frame = Frame(title = Some("Pointfree Lift derivations"),
      debug = true, theme = Themes.default)

    // split and name
    val colA = frame.panel
    val colB = colA.splitRight
    val colA2 = colA.splitDown
    val colB2 = colB.splitDown
    colA.title = "Programs"
    colA2.title = "Applicable rules"
    colB.title = "Derivation"
    colB2.title = "Current program and type"

    // programs
    Select(colA, program_data)

    // applicable rules
    Select(colA2, applicableRules_data)

    // derivation
    ListWidget(colB, derivation_items)

    // current and next program
    ListWidget(colB2, currentProgram_items)

    frame.show()
  }
}
