package lambda

import Type._
//import scala.collection.immutable.S

trait Substitutable[A] {
  def s_apply(s: Subst, a: A): A

  def free(a: A): Set[TVar]
}

object Substitutable {

  def apply[A](implicit s: Substitutable[A]): Substitutable[A] = s

  object ops {
    def s_apply[A: Substitutable](s: Subst, a: A): A = Substitutable[A].s_apply(s, a)

    def free[A: Substitutable](a: A): Set[TVar] = Substitutable[A].free(a)

    implicit class SubstitutableOps[A: Substitutable](a: A) {
      def s_apply(s: Subst): A = Substitutable[A].s_apply(s, a)

      def free: Set[TVar] = Substitutable[A].free(a)
    }

  }

  implicit val TypeSubstitutable: Substitutable[Type] =
    new Substitutable[Type] {

      override def s_apply(s: Subst, a: Type): Type = a match {
        case TList(b) => TList(b)
        case TArrow(b, c) => TArrow(Substitutable[Type].s_apply(s, b), Substitutable[Type].s_apply(s, c))
        case TPair(b, c) => TPair(Substitutable[Type].s_apply(s, b), Substitutable[Type].s_apply(s, c))
        case v: TVar => s.getOrElse(v, v)
        case _ => a
      }

      override def free(a: Type): Set[TVar] = a match {
        case TList(b) => Substitutable[Type].free(b)
        case TPair(b, c) => Substitutable[Type].free(b) union Substitutable[Type].free(c)
        case TArrow(b, c) => Substitutable[Type].free(b) union Substitutable[Type].free(c)
        case v: TVar => Set(v)
        case _ => Set.empty
      }
    }

  implicit val SchemeSubstitutable: Substitutable[Scheme] =
    new Substitutable[Scheme] {
      override def s_apply(s: Subst, a: Scheme): Scheme = {
        val Scheme(as, t) = a
        val s_free = as.foldRight(s) { case (v, sub) => sub - v }
        Scheme(as, Substitutable[Type].s_apply(s_free, t))
      }

      override def free(a: Scheme): Set[TVar] = Substitutable[Type].free(a.t) diff a.as.toSet
    }

  implicit val TypeEnvSubstitutable: Substitutable[TypeEnv] =
    new Substitutable[TypeEnv] {
      override def s_apply(s: Subst, a: TypeEnv): TypeEnv = a.mapValues(Substitutable[Scheme].s_apply(s, _))

      override def free(a: TypeEnv): Set[TVar] = a.values.foldRight(Set.empty[TVar]) { case (s, f) => f union Substitutable[Scheme].free(s) }
    }
}

trait Show[A] {
  def show(a: A): String
}


