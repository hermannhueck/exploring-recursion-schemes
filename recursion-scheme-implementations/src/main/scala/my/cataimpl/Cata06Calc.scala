package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor._

/*
  The implicit class FunctorSyntax has become generic in step 5.
  Here in step 6 we moved FunctorSyntax into the package object of my.functor.

  We also introduce the terms Algebra anc Coalgebra.
  Algebra is another name for a function F[A] => A.
  Coalgebra respectively is an alias for A => F[A]. (It is not used in this example.)

    type Algebra[F[_], A]   = F[A] => A
    type Coalgebra[F[_], A] = A => F[A]

  Subsequently I could replace every F[A] => A with Algebra[F[_], A].

  'collapseFA2A' is now just called 'collapse'.

  This step just beautifies and clarifies the code a bit.
 */
object Cata06Calc extends util.App {

  sealed trait CalcF[+A] extends Product with Serializable {

    import CalcF.{AddF, MulF, NumF}

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

  def collapse[F[_]: Functor, A](algebra: Algebra[F, A])(fixedData: Fix[F]): A = {
    val unfixed  = fixedData.unfix                // F[Fix[F]]
    val recursed = unfixed.map(collapse(algebra)) // F[A]
    val a        = algebra(recursed)              // A
    a
  }

  println
  collapse(show)(calc1) pipe (str => println(s"show: $str"))
  collapse(eval)(calc1) pipe (res => println(s"eval: $res"))
  println
  collapse(show)(calc2) pipe (str => println(s"show: $str"))
  collapse(eval)(calc2) pipe (res => println(s"eval: $res"))
}
