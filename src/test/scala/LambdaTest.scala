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
    println(expr.typ(Nil))
    println(e.typ(Nil))
  }

  @Test
  def second(): Unit = {
//    println(flatten.typ(Nil))
//    println(initsOp.toStringDeBruijn(0))
//    println(initsOp.typ(Nil))
    println(tailsOp.typ(Nil))
//    println(Catamorphism(Identity)(Identity))
  }

  @Test
  def betaReduction(): Unit = {
    val expr = TInt lambda (TInt lambda (TInt lambda v0(v1(v2))))(Identity)
    println(expr.reduce.toStringDeBruijn(0))
  }
}
