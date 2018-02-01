import lambda._
import lambda.Var._
import lambda.TVar._
import lambda.Expr._
import Context._
import org.junit.Test

class LambdaTest {

  @Test
  def first(): Unit = {
    val expr = Identity *: Identity
    val e = b lambda v0
    println(expr.toStringDeBruijn(0))
    println(expr.typ(empty))
    println(e.typ(empty))
  }

  @Test
  def second(): Unit = {
//    println(flatten.typ(empty))
    println(initsOp.typ(empty))
//    println(tailsOp.typ(empty))
//    println(Catamorphism(Identity)(Identity))
  }

  @Test
  def deBruijnContext(): Unit = {
    println(empty + TInt + TFloat)
  }
}
