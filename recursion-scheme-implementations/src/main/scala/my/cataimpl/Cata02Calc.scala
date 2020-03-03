package my.cataimpl

import scala.util.chaining._

object Cata02Calc extends util.App {

  sealed trait Calc[+A] extends Product with Serializable
  object Calc {
    case class Num[A](i: Int)      extends Calc[A]
    case class Add[A](a: A, b: A)  extends Calc[A]
    case class Mult[A](a: A, b: A) extends Calc[A]
  }

  import Calc.{Add, Mult, Num}

  // 1 + 2
  val calc1: Calc[Calc[Nothing]] =
    Add(Num(1), Num(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2: Calc[Calc[Calc[Nothing]]] =
    Mult(Num(3), Add(Num(1), Num(2)))
      .tap(println)

  val eval: Calc[Nothing] => Int =
    _ match {
      case Num(i)     => i
      case Add(a, b)  => eval(a) + eval(b)
      case Mult(a, b) => eval(a) * eval(b)
    }

  val show: Calc[Nothing] => String =
    _ match {
      case Num(i)     => i.toString
      case Add(a, b)  => s"(${show(a)} + ${show(b)})"
      case Mult(a, b) => s"${show(a)} * ${show(b)}"
    }
}
