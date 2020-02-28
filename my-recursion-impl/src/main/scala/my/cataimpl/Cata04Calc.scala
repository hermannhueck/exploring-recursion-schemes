package my.cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor.Functor

object Cata04Calc extends util.App {

  sealed trait Calc[+A] extends Product with Serializable {

    import Calc.{Add, Mult, Num}

    def map[B](f: A => B): Calc[B] =
      this match {
        case Num(i)     => Num(i)
        case Add(a, b)  => Add(f(a), f(b))
        case Mult(a, b) => Mult(f(a), f(b))
      }
  }

  object Calc {

    case class Num[A](i: Int)      extends Calc[A]
    case class Add[A](a: A, b: A)  extends Calc[A]
    case class Mult[A](a: A, b: A) extends Calc[A]

    implicit val functor: Functor[Calc] = new Functor[Calc] {
      override def map[A, B](fa: Calc[A])(f: A => B): Calc[B] =
        fa map f
    }
  }

  import Calc.{Add, Mult, Num}

  final implicit class FunctorSyntax[A](private val calc: Calc[A]) {
    def map[B](f: A => B): Calc[B] =
      Functor[Calc].map(calc)(f)
  }

  // FORMAT: OFF
  // 1 + 2
  val calc1: Fix[Calc] =
    Fix(Add(
        Fix(Num[Fix[Calc]](1)),
        Fix(Num[Fix[Calc]](2))
      )).tap(println)

  // 3 * (1 + 2)
  val calc2: Fix[Calc] =
    Fix(Mult(
        Fix(Num[Fix[Calc]](3)),
        Fix(Add(
            Fix(Num[Fix[Calc]](1)),
            Fix(Num[Fix[Calc]](2))
          ))
      )).tap(println)
  // FORMAT: ON

  val eval: Calc[Int] => Int = {
    case Num(i)     => i
    case Add(a, b)  => a + b
    case Mult(a, b) => a * b
  }

  val show: Calc[String] => String = {
    case Num(i)     => i.toString
    case Add(a, b)  => s"($a + $b)"
    case Mult(a, b) => s"$a * $b"
  }

  def collapseCalcAToA[A](calc2Value: Calc[A] => A)(fixedCalc: Fix[Calc])(implicit calcFunctor: Functor[Calc]): A = {
    val unfixed: Calc[Fix[Calc]] = fixedCalc.unfix
    val recursed: Calc[A]        = unfixed.map(collapseCalcAToA(calc2Value))
    val a: A                     = calc2Value(recursed)
    a
  }

  println
  collapseCalcAToA(show)(calc1) pipe (str => println(s"show: $str"))
  collapseCalcAToA(eval)(calc1) pipe (res => println(s"eval: $res"))
  println
  collapseCalcAToA(show)(calc2) pipe (str => println(s"show: $str"))
  collapseCalcAToA(eval)(calc2) pipe (res => println(s"eval: $res"))
}
