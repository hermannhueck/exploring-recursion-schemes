package my
package cataimpl

import scala.util.chaining._
import recursion._

/*
  The type aliases of Algebra and Coalgebra are generic as well as the definition of 'cata'.

  In step 0 we move them out into the my.recursion package object.

  We also move the CalcF trait and companion object into it's own file.
 */
object Cata09Calc extends util.App {

  import CalcF._

  // 1 + 2
  val calc1 =
    AddF(NumF(1), NumF(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2 =
    MulF(NumF(3), AddF(NumF(1), NumF(2)))
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
  cata(show)(CalcF.fix(calc1)) pipe (str => println(s"show: $str"))
  cata(eval)(CalcF.fix(calc1)) pipe (res => println(s"eval: $res"))
  println
  cata(show)(calc2.fix) pipe (str => println(s"show: $str"))
  cata(eval)(calc2.fix) pipe (res => println(s"eval: $res"))
}
