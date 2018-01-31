package pointfree

import TVar._
import EVar._
import scala.collection.immutable

case class IdentityEquiv(t: Type, e: Expr)

object IdentityEquiv {
  def zipUnzip(a: Type, b: Type): IdentityEquiv =
    TList(TPair(a, b)) |- Uncurry(EZip) *: Unzip

}

