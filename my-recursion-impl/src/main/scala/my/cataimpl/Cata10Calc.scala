package my.cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor._
import recursion._

object Cata10Calc extends util.App {

  sealed trait Calc[+A] extends Product with Serializable {

    import Calc.{Add, Mult, Num}

    def map[B](f: A => B): Calc[B] =
      this match {
        case Num(i)     => Num(i)
        case Add(a, b)  => Add(f(a), f(b))
        case Mult(a, b) => Mult(f(a), f(b))
      }

    def fix: Fix[Calc] = Calc.fix(this)
  }

  object Calc {

    case class Num[A](i: Int)      extends Calc[A]
    case class Add[A](a: A, b: A)  extends Calc[A]
    case class Mult[A](a: A, b: A) extends Calc[A]

    implicit val functor: Functor[Calc] = new Functor[Calc] {
      override def map[A, B](fa: Calc[A])(f: A => B): Calc[B] =
        fa map f
    }

    private def fixA[A](a: A): Fix[Calc] =
      fix apply a.asInstanceOf[Calc[A]]

    def fix[A]: Calc[A] => Fix[Calc] = {
      case Num(i) =>
        Fix(Num[Fix[Calc]](i)) // needs type param for type inference
      case Add(a, b) =>
        Fix(Add(fixA(a), fixA(b)))
      case Mult(a, b) =>
        Fix(Mult(fixA(a), fixA(b)))
    }
  }

  import Calc.{Add, Mult, Num}

  // 1 + 2
  val calc1 =
    Add(Num(1), Num(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2 =
    Mult(Num(3), Add(Num(1), Num(2)))
      .tap(println)

  val eval: Algebra[Calc, Int] = {
    case Num(i)     => i
    case Add(a, b)  => a + b
    case Mult(a, b) => a * b
  }

  val show: Algebra[Calc, String] = {
    case Num(i)     => i.toString
    case Add(a, b)  => s"($a + $b)"
    case Mult(a, b) => s"$a * $b"
  }

  final implicit class FixCalcSyntax(private val fix: Fix[Calc]) {
    @inline def cata[A](algebra: Algebra[Calc, A]): A =
      recursion.cata(algebra)(fix)
  }

  println
  calc1.fix.cata(show) pipe (str => println(s"show: $str"))
  calc1.fix.cata(eval) pipe (res => println(s"eval: $res"))
  println
  calc2.fix.cata(show) pipe (str => println(s"show: $str"))
  calc2.fix.cata(eval) pipe (res => println(s"eval: $res"))
}
