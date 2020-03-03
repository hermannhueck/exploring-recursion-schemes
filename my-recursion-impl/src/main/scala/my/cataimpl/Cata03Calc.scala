package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix

object Cata03Calc extends util.App {

  sealed trait Calc[+A] extends Product with Serializable
  object Calc {
    case class Num[A](i: Int)     extends Calc[A]
    case class Add[A](a: A, b: A) extends Calc[A]
    case class Mul[A](a: A, b: A) extends Calc[A]
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

  val eval: Fix[Calc] => Int =
    _.unfix match {
      case Num(i)    => i
      case Add(a, b) => eval(a) + eval(b)
      case Mul(a, b) => eval(a) * eval(b)
    }

  val show: Fix[Calc] => String =
    _.unfix match {
      case Num(i)    => i.toString
      case Add(a, b) => s"(${show(a)} + ${show(b)})"
      case Mul(a, b) => s"${show(a)} * ${show(b)}"
    }

  println
  show(calc1) pipe (str => println(s"show: $str"))
  eval(calc1) pipe (res => println(s"eval: $res"))
  println
  show(calc2) pipe (str => println(s"show: $str"))
  eval(calc2) pipe (res => println(s"eval: $res"))
}
