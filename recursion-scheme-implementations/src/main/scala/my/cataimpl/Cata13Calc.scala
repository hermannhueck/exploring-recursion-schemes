package my
package cataimpl

import scala.util.chaining._
import recursion._

/*
  eval turns a CalcF[Int] into a Int.
  show turns a CalcF[String] into a String.

    val eval: Algebra[CalcF, Int] = ???
    val show: Algebra[CalcF, String] = ???

  Following the same pattern we can easily write
  another Algebra which turns a CalcF[Calc] back to a Calc.
  See CalcF.scala

    def calcF2Calc: CalcF[Calc] => Calc = {
      case NumF(i)    => Num(i)
      case AddF(a, b) => Add(a, b)
      case MulF(a, b) => Mul(a, b)
    }

  The implementaion too is the same pattern match we already used in eval and show.

  Not very useful in this example - but with this Algebra we now can use our cata
  to turn our CalcF structure back into a Calc:

    calc2.toCalcF.fix.cata(calcF2Calc)
 */
object Cata13Calc extends util.App {

  import Calc._
  import CalcF._

  // 1 + 2
  val calc1: Calc =
    Add(Num(1), Num(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2: Calc =
    Mul(Num(3), Add(Num(1), Num(2)))
      .tap(println)

  println
  calc1.toCalcF.fix.cata(show) pipe (str => println(s"show: $str"))
  calc1.toCalcF.fix.cata(eval) pipe (res => println(s"eval: $res"))
  println
  calc2.toCalcF.fix.cata(show) pipe (str => println(s"show: $str"))
  calc2.toCalcF.fix.cata(eval) pipe (res => println(s"eval: $res"))

  println
  calc1.toCalcF.fix.cata(calcF2Calc) pipe (calc => println(s"calcF2Calc: $calc"))
  calc2.toCalcF.fix.cata(calcF2Calc) pipe (calc => println(s"calcF2Calc: $calc"))
}
