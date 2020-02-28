package github

import scala.util.chaining._

import higherkindness.droste._
import higherkindness.droste.data._
import cats.implicits._

object ReadmeExample extends _root_.util.App {

  val natCoalgebra: Coalgebra[Option, BigDecimal] =
    Coalgebra(n => if (n > 0) Some(n - 1) else None)

  val fibAlgebra: CVAlgebra[Option, BigDecimal] = CVAlgebra {
    case Some(r1 :< Some(r2 :< _)) => r1 + r2
    case Some(_ :< None)           => 1
    case _                         => 0
  }

  val fib: BigDecimal => BigDecimal =
    scheme.ghylo(fibAlgebra.gather(Gather.histo), natCoalgebra.scatter(Scatter.ana))

  fib(0) pipe (x => println(s"fib(0) = $x"))
  fib(1) pipe (x => println(s"fib(1) = $x"))
  fib(2) pipe (x => println(s"fib(2) = $x"))
  fib(3) pipe (x => println(s"fib(3) = $x"))
  fib(4) pipe (x => println(s"fib(5) = $x"))
  fib(5) pipe (x => println(s"fib(5) = $x"))
  fib(10) pipe (x => println(s"fib(10) = $x"))
  fib(100) pipe (x => println(s"fib(100) = $x"))

  println

  val fibAlt: BigDecimal => BigDecimal =
    scheme.zoo.dyna(fibAlgebra, natCoalgebra)

  fibAlt(0) pipe (x => println(s"fibAlt(0) = $x"))
  fibAlt(1) pipe (x => println(s"fibAlt(1) = $x"))
  fibAlt(2) pipe (x => println(s"fibAlt(2) = $x"))
  fibAlt(3) pipe (x => println(s"fibAlt(3) = $x"))
  fibAlt(4) pipe (x => println(s"fibAlt(5) = $x"))
  fibAlt(5) pipe (x => println(s"fibAlt(5) = $x"))
  fibAlt(10) pipe (x => println(s"fibAlt(10) = $x"))
  fibAlt(100) pipe (x => println(s"fibAlt(100) = $x"))

  println

  val fromNatAlgebra: Algebra[Option, BigDecimal] = Algebra {
    case Some(n) => n + 1
    case None    => 0
  }

// note: n is the fromNatAlgebra helper value from the previous level of recursion
  val sumSquaresAlgebra: RAlgebra[BigDecimal, Option, BigDecimal] = RAlgebra {
    case Some((n, value)) => value + (n + 1) * (n + 1)
    case None             => 0
  }

  val sumSquares: BigDecimal => BigDecimal =
    scheme.ghylo(sumSquaresAlgebra.gather(Gather.zygo(fromNatAlgebra)), natCoalgebra.scatter(Scatter.ana))

  sumSquares(0) pipe (x => println(s"sumSquares(0) = $x"))
  sumSquares(1) pipe (x => println(s"sumSquares(1) = $x"))
  sumSquares(2) pipe (x => println(s"sumSquares(2) = $x"))
  sumSquares(3) pipe (x => println(s"sumSquares(3) = $x"))
  sumSquares(4) pipe (x => println(s"sumSquares(5) = $x"))
  sumSquares(5) pipe (x => println(s"sumSquares(5) = $x"))
  sumSquares(10) pipe (x => println(s"sumSquares(10) = $x"))
  sumSquares(100) pipe (x => println(s"sumSquares(100) = $x"))

  println

  val fused: BigDecimal => (BigDecimal, BigDecimal) =
    scheme.ghylo(
      fibAlgebra.gather(Gather.histo) zip
        sumSquaresAlgebra.gather(Gather.zygo(fromNatAlgebra)),
      natCoalgebra.scatter(Scatter.ana)
    )

  fused(0) pipe (x => println(s"fused(0) = $x"))
  fused(1) pipe (x => println(s"fused(1) = $x"))
  fused(2) pipe (x => println(s"fused(2) = $x"))
  fused(3) pipe (x => println(s"fused(3) = $x"))
  fused(4) pipe (x => println(s"fused(5) = $x"))
  fused(5) pipe (x => println(s"fused(5) = $x"))
  fused(10) pipe (x => println(s"fused(10) = $x"))
  fused(100) pipe (x => println(s"fused(100) = $x"))
}
