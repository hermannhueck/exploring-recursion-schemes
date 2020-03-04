package my
package cataimpl

import scala.util.chaining._
import recursion._

/*
  In step 10 we added the implicit class CataSyntax to recursion/package.scala.

    final implicit class CataSyntax[F[_]: Functor](private val fix: Fix[F]) {
      @inline def cata[A](algebra: Algebra[F, A]): A =
        self.cata(algebra)(fix)
    }

  Instead of:

    cata(eval)(calc2.fix)

  we can invoke cata directly on a Fix structure:

    calc2.fix.cata(eval)

  Now we can invoke our catamorphism conveniently:
  - We have a nested CalcF structure.
  - We then recursivly nest it int Fix invoking the 'fix' function on it.
  - Now we can invoke 'cata' on the Fix passing the 'eval' Algebra.
  - The hole expression gives us the Int result of the the evaluation of the original expression.
 */
object Cata10Calc extends util.App {

  import CalcF._

  // 1 + 2
  val calc1 =
    AddF(NumF(1), NumF(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2 =
    MulF(NumF(3), AddF(NumF(1), NumF(2)))
      .tap(println)

  println
  calc1.fix.cata(show) pipe (str => println(s"show: $str"))
  calc1.fix.cata(eval) pipe (res => println(s"eval: $res"))
  println
  calc2.fix.cata(show) pipe (str => println(s"show: $str"))
  calc2.fix.cata(eval) pipe (res => println(s"eval: $res"))
}
