package my
package cataimpl

import scala.util.chaining._
import recursion._

/*
  From step 2 to step 10 we worked with CalcF[+A] in order to be able to define
  a Pattern Functor and extract recursion based on that.

  Wouldn't it be nice to use our original Calc from step 1 again?

  Let's just implement a conversion function Calc => CalcF[A] in the cataimpl package object.

    def calc2CalcF[A]: Calc => CalcF[Calc] = ???

  This could also be seen as a Coalgebra[CalcF, Calc]:

    def calc2CalcF: Coalgebra[CalcF, Calc] = {
      case Num(i)    => NumF(i)
      case Add(a, b) => AddF(a, b)
      case Mul(a, b) => MulF(a, b)
    }

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
  val calc1F =
    calc1
      .toCalcF
      .tap(println)

  // 3 * (1 + 2)
  val calc2: Calc =
    Mul(Num(3), Add(Num(1), Num(2)))
      .tap(println)
  val calc2F =
    calc2
      .toCalcF
      .tap(println)

  println
  calc1F.fix.cata(show) pipe (str => println(s"show: $str"))
  calc1F.fix.cata(eval) pipe (res => println(s"eval: $res"))
  println
  calc2F.fix.cata(show) pipe (str => println(s"show: $str"))
  calc2F.fix.cata(eval) pipe (res => println(s"eval: $res"))
}
