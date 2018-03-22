package pointfree

import net.team2xh.onions.Themes
import net.team2xh.onions.components.Frame
import net.team2xh.onions.components.widgets._
import net.team2xh.onions.utils.Varying
import net.team2xh.scurses.{Colors, Scurses}
import net.team2xh.scurses.RichText._
import scala.collection.immutable
import Console.{RED, BLUE, RESET}

object Interactive extends App {

  val programs: Varying[List[Expr]] = new Varying(List(
    Programs.denseMV,
    Programs.maxSegSum
  ))
  val program_data = new Select.Data[Expr](programs, e => e.toString)
  var currentProgram: Varying[Expr] = new Varying(Identity)


  // applicable rewrites
  val applicableRewrites: Varying[List[(Equiv, Expr, Expr)]] = new Varying(List())
  currentProgram.subscribe { () =>
    applicableRewrites := (for {
      rule <- Equiv.rewrites
      program = currentProgram.value.normalizeComposition.etaExpansion
      next <- program.rewrite(rule).zip(program.rewrite2(rule))
      (result, (_, change)) = next
    } yield (rule, result.etaReduction.normalizeComposition, change))
  }
  val applicableRules_data = new Select.Data[(Equiv, Expr, Expr)](
    applicableRewrites,
    { case (rule, next, change) =>
      s"${rule.name}: ${next.toString.replace(change.toString, s"[fg:red]$change[/fg]")}"
//      s"${rule.name}: ${next.toString.replaceFirst(change.toString, "FOO")}"
//      s"${rule.name}: $change"
    }
  )

  // derivation
  val derivation_items: Varying[immutable.List[String]] = new Varying(List())

  // current
  val currentProgramString: Varying[String] = "foo"
  var currentProgramTyp: Varying[String] = "bar"
  currentProgram.subscribe { () =>
    currentProgramString := currentProgram.value.toString
    currentProgramTyp := currentProgram.value.typ.toString
  }

  // SET PROGRAM
  currentProgram := programs.value.head

  def selectProgram(e: Expr): Unit = {
    currentProgram := e
    derivation_items := List(e.toString)
  }

  def selectRewriteRule(arg: (Equiv, Expr, Expr)): Unit = {
    val (rule, next, _) = arg
    currentProgram := next
    derivation_items := next.toString :: s"= { ${rule.name} }" :: derivation_items.value
  }

  Scurses { implicit screen =>
    implicit val debug = true
    val frame = Frame(title = Some("Pointfree Lift derivations"),
      debug = true, theme = Themes.light)

    // split and name
    val colA = frame.panel
    val colB = colA.splitDown
    val colB1 = colB.splitDown
    val colB2 = colB1.splitDown
    colA.title = "Programs"
    colB.title = "Applicable rules"
    colB1.title = "Derivation"
    colB2.title = "Current program and type"

    // programs
    Select(colA, program_data, selectProgram)

    // applicable rules
    Select(colB, applicableRules_data, selectRewriteRule)

    // derivation
    ListWidget(colB1, derivation_items)

    // current and next program
    Label(colB2, currentProgramString)
    Label(colB2, currentProgramTyp)

    frame.show()
  }
}
