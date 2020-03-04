package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor._

/*
  Step 7 reformulates 'collapse' and calls it 'cata' to signify that we implemented a catamorphism.

    def cata[F[_]: Functor, A](algebra: Algebra[F, A])(fixedData: Fix[F]): A = {

      def cata_(algebra: Algebra[F, A]): Fix[F] => A = {
        val unfix: Fix[F] => F[Fix[F]] = _.unfix
        val recurse: F[Fix[F]] => F[A] = _.map(cata_(algebra))
        unfix andThen recurse andThen algebra
      }

      cata_(algebra)(fixedData)
    }

  The sequence of composed functions should make our steps clear:

    unfix andThen recurse andThen algebra
 */
object Cata07Calc extends util.App {

  import CalcF.{AddF, MulF, NumF}

  sealed trait CalcF[+A] extends Product with Serializable {

    def map[B](f: A => B): CalcF[B] =
      this match {
        case NumF(i)    => NumF(i)
        case AddF(a, b) => AddF(f(a), f(b))
        case MulF(a, b) => MulF(f(a), f(b))
      }
  }

  object CalcF {

    case class NumF[A](i: Int)     extends CalcF[A]
    case class AddF[A](a: A, b: A) extends CalcF[A]
    case class MulF[A](a: A, b: A) extends CalcF[A]

    implicit val functor: Functor[CalcF] = new Functor[CalcF] {
      override def map[A, B](fa: CalcF[A])(f: A => B): CalcF[B] =
        fa map f
    }
  }

  import CalcF.{AddF, MulF, NumF}

  // FORMAT: OFF
  // 1 + 2
  val calc1: Fix[CalcF] =
    Fix(AddF(
        Fix(NumF[Fix[CalcF]](1)),
        Fix(NumF[Fix[CalcF]](2))
      )).tap(println)

  // 3 * (1 + 2)
  val calc2: Fix[CalcF] =
    Fix(MulF(
        Fix(NumF[Fix[CalcF]](3)),
        Fix(AddF(
            Fix(NumF[Fix[CalcF]](1)),
            Fix(NumF[Fix[CalcF]](2))
          ))
      )).tap(println)
  // FORMAT: ON

  type Algebra[F[_], A]   = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  val eval: Algebra[CalcF, Int] = {
    case NumF(i)    => i
    case AddF(a, b) => a + b
    case MulF(a, b) => a * b
  }

  val show: Algebra[CalcF, String] = {
    case NumF(i)    => i.toString
    case AddF(a, b) => s"($a + $b)"
    case MulF(a, b) => s"$a * $b"
  }

  def cata[F[_]: Functor, A](algebra: Algebra[F, A])(fixedData: Fix[F]): A = {

    def cata_(algebra: Algebra[F, A]): Fix[F] => A = {
      val unfix: Fix[F] => F[Fix[F]] = _.unfix
      val recurse: F[Fix[F]] => F[A] = _.map(cata_(algebra))
      unfix andThen recurse andThen algebra
    }

    cata_(algebra)(fixedData)
  }

  println
  cata(show)(calc1) pipe (str => println(s"show: $str"))
  cata(eval)(calc1) pipe (res => println(s"eval: $res"))
  println
  cata(show)(calc2) pipe (str => println(s"show: $str"))
  cata(eval)(calc2) pipe (res => println(s"eval: $res"))
}
