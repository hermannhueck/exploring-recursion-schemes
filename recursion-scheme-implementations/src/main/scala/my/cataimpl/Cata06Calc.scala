package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor._

object Cata06Calc extends util.App {

  sealed trait Calc[+A] extends Product with Serializable {

    import Calc.{Add, Mul, Num}

    def map[B](f: A => B): Calc[B] =
      this match {
        case Num(i)    => Num(i)
        case Add(a, b) => Add(f(a), f(b))
        case Mul(a, b) => Mul(f(a), f(b))
      }
  }

  object Calc {

    case class Num[A](i: Int)     extends Calc[A]
    case class Add[A](a: A, b: A) extends Calc[A]
    case class Mul[A](a: A, b: A) extends Calc[A]

    implicit val functor: Functor[Calc] = new Functor[Calc] {
      override def map[A, B](fa: Calc[A])(f: A => B): Calc[B] =
        fa map f
    }
  }

  import Calc.{Add, Mul, Num}

  // FORMAT: OFF
  // 1 + 2
  val calc1: Fix[Calc] =
    Fix(Add(
        Fix(Num[Fix[Calc]](1)),
        Fix(Num[Fix[Calc]](2))
      )).tap(println)

  // 3 * (1 + 2)
  val calc2: Fix[Calc] =
    Fix(Mul(
        Fix(Num[Fix[Calc]](3)),
        Fix(Add(
            Fix(Num[Fix[Calc]](1)),
            Fix(Num[Fix[Calc]](2))
          ))
      )).tap(println)
  // FORMAT: ON

  type Algebra[F[_], A]   = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  val eval: Algebra[Calc, Int] = {
    case Num(i)    => i
    case Add(a, b) => a + b
    case Mul(a, b) => a * b
  }

  val show: Algebra[Calc, String] = {
    case Num(i)    => i.toString
    case Add(a, b) => s"($a + $b)"
    case Mul(a, b) => s"$a * $b"
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