package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor.Functor

/*
  In step 4 our implicit class FunctorSyntax was specific for CalcF:

    final implicit class FunctorSyntax[A](private val calc: CalcF[A]) {
      def map[B](f: A => B): CalcF[B] =
        Functor[CalcF].map(calc)(f)
    }

  Here in step 5 we abstract over CalcF and implement a FunctorSyntax for any F[_].

    final implicit class FunctorSyntax[F[_]: Functor, A](private val fa: F[A]) {
      def map[B](f: A => B): F[B] =
        Functor[F].map(fa)(f)
    }

  This allows us to implement the collapse function generically.

  before:

     def collapseCalcFToA[A](calc2Value: CalcF[A] => A)(fixedCalc: Fix[CalcF])(implicit calcFunctor: Functor[CalcF]): A = {
      val unfixed: CalcF[Fix[CalcF]] = fixedCalc.unfix
      val recursed: CalcF[A]         = unfixed.map(collapseCalcFToA(calc2Value))
      val a: A                       = calc2Value(recursed)
      a
    }

  after:

    def collapseFA2A[F[_]: Functor, A](fa2a: F[A] => A)(fixedData: Fix[F]): A = {
      val unfixed: F[Fix[F]] = fixedData.unfix
      val recursed: F[A]     = unfixed.map(collapseFA2A(fa2a))
      val a: A               = fa2a(recursed)
      a
    }

  It is the same collapse function as  before. We just abstracted over CalcF[_] and made it an F[_].
  And we used a context bound for the Functor which is syntactic sugar for the implicit parameter list.
 */
object Cata05Calc extends util.App {

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

  final implicit class FunctorSyntax[F[_]: Functor, A](private val fa: F[A]) {
    @inline def map[B](f: A => B): F[B] =
      Functor[F].map(fa)(f)
  }

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

  val eval: CalcF[Int] => Int = {
    case NumF(i)    => i
    case AddF(a, b) => a + b
    case MulF(a, b) => a * b
  }

  val show: CalcF[String] => String = {
    case NumF(i)    => i.toString
    case AddF(a, b) => s"($a + $b)"
    case MulF(a, b) => s"$a * $b"
  }

  def collapseFA2A[F[_]: Functor, A](fa2a: F[A] => A)(fixedData: Fix[F]): A = {
    val unfixed: F[Fix[F]] = fixedData.unfix
    val recursed: F[A]     = unfixed.map(collapseFA2A(fa2a))
    val a: A               = fa2a(recursed)
    a
  }

  println
  collapseFA2A(show)(calc1) pipe (str => println(s"show: $str"))
  collapseFA2A(eval)(calc1) pipe (res => println(s"eval: $res"))
  println
  collapseFA2A(show)(calc2) pipe (str => println(s"show: $str"))
  collapseFA2A(eval)(calc2) pipe (res => println(s"eval: $res"))
}
