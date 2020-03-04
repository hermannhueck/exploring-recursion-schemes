package my
package cataimpl

import scala.util.chaining._
import recursion._

/*
  From step 2 to step 10 we worked with CalcF[+A] in order to be able to define
  a Pattern Functor and extract recursion based on that.

  Wouldn't it be nice to use our original Calc from step 1 again?

  Let's just implement a conversion function Calc => CalcF[A] in the cataimpl package object.

    def calc2CalcF[A]: Calc => CalcF[A] = ???

  The extension method 'toCalcF' can be invoked directly on a Calc instance:

    final implicit class CalcSyntax(calc: Calc) {
      def toCalcF[A]: CalcF[A] = calc2CalcF(calc)
    }

    val calc2: Calc =
      Mul(Num(3), Add(Num(1), Num(2)))
        .tap(println)
    val calc2F: CalcF[CalcF[CalcF[Nothing]]] =
      calc2.toCalcF

 */
object Cata11Calc extends util.App {

  import Calc._
  import CalcF._

  // 1 + 2
  val calc1: Calc =
    Add(Num(1), Num(2))
      .tap(println)
  val calc1F: CalcF[CalcF[Nothing]] =
    calc1
      .toCalcF
      .tap(println)

  // 3 * (1 + 2)
  val calc2: Calc =
    Mul(Num(3), Add(Num(1), Num(2)))
      .tap(println)
  val calc2F: CalcF[CalcF[CalcF[Nothing]]] =
    calc2
      .toCalcF
      .tap(println)

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

  println
  calc1F.fix.cata(show) pipe (str => println(s"show: $str"))
  calc1F.fix.cata(eval) pipe (res => println(s"eval: $res"))
  println
  calc2F.fix.cata(show) pipe (str => println(s"show: $str"))
  calc2F.fix.cata(eval) pipe (res => println(s"eval: $res"))
}
