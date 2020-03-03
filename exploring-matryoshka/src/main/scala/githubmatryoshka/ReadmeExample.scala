package githubmatryoshka

import scala.util.chaining._

import matryoshka._
import matryoshka.implicits._

object ReadmeExample extends util.App {

  // sealed abstract class Expr
  // final case class Num(value: Long)      extends Expr
  // final case class Mul(l: Expr, r: Expr) extends Expr

  // can be rewritten like this:

  sealed abstract class Expr[A]
  final case class Num[A](value: Long) extends Expr[A]
  final case class Mul[A](l: A, r: A)  extends Expr[A]

  implicit val exprFunctor = new scalaz.Functor[Expr] {

    override def map[A, B](fa: Expr[A])(f: (A) => B) = fa match {
      case Num(value) => Num[B](value)
      case Mul(l, r)  => Mul(f(l), f(r))
    }
  }

  val eval: Algebra[Expr, Long] = { // i.e. Expr[Long] => Long
    case Num(x)    => x
    case Mul(x, y) => x * y
  }

  def someExpr[T](implicit T: Corecursive.Aux[T, Expr]): T =
    Mul(Num[T](2).embed, Mul(Num[T](3).embed, Num[T](4).embed).embed).embed

  import matryoshka.data.Mu

  someExpr[Mu[Expr]].cata(eval) pipe println // ⇒ 24
}
