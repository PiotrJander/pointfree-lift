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
    val e = w of b lambda w
    println(expr)
    println(expr.typ(empty))
    println(e.typ(empty))
  }

  @Test
  def second(): Unit = {
//    println(flatten.typ(empty))
//    println(initsOp.typ(empty))
    println(tailsOp.typ(empty))
//    println(Catamorphism(Identity)(Identity))
  }
}
