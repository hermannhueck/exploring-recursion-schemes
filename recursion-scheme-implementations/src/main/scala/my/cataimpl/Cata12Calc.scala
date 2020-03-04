package my
package cataimpl

import scala.util.chaining._
import recursion._

/*
  With the Calc => CalcF[A] conversion of step 11 we can further simplify our code:
  We move the invocation of toCalcF to our final invocation chain:

    val calc2: Calc = Mul(Num(3), Add(Num(1), Num(2)))

    val res: Int = calc1.toCalcF.fix.cata(eval) // 9
 */
object Cata12Calc extends util.App {

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
}
