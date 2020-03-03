package my.cataimpl

import scala.util.chaining._

object Cata01Calc extends util.App {

  sealed trait Calc extends Product with Serializable
  object Calc {
    case class Num(i: Int)            extends Calc
    case class Add(a: Calc, b: Calc)  extends Calc
    case class Mult(a: Calc, b: Calc) extends Calc
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

  val eval: Calc => Int =
    _ match {
      case Num(i)     => i
      case Add(a, b)  => eval(a) + eval(b)
      case Mult(a, b) => eval(a) * eval(b)
    }

  def show: Calc => String =
    _ match {
      case Num(i)     => i.toString
      case Add(a, b)  => s"(${show(a)} + ${show(b)})"
      case Mult(a, b) => s"${show(a)} * ${show(b)}"
    }

  println
  calc1 pipe show pipe println
  calc1 pipe eval pipe println
  println
  calc2 pipe show pipe println
  calc2 pipe eval pipe println
}
