package my
package cataimpl

import scala.util.chaining._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

/*
  Now we want to do basically the same thing with Matryoshka.

  We cannot use our own Functor definition my.functor.Functor.
  Matryoshka is based on Scalaz. Hence we have to define an Instance of scalaz.Functor[CalcF].

  The structures Calc and CalcF con be resused.

  We can create a Fix structure by invoking the extension method 'embed' on envery component
  of a nested CalcF:

    def fixedCalc[T](implicit T: Corecursive.Aux[T, CalcF]): T =
      MulF(NumF[T](2).embed, MulF(NumF[T](3).embed, NumF[T](4).embed).embed).embed

    val fixed: Mu[CalcF] = fixedCalc[Mu[CalcF]]

  (Mu is one of Matryoshka's Fixpoint types.)

  Now we can invoke Matryoshka's catamorphism on the 'fixed':

    val res: Int = fixed.cata(eval) // 24

  We normally don't have a nested Fix structure in our application.
  Let's assume we have a Calc instance.

    val calc2 = Mul(Num(3), Add(Num(1), Num(2)))

  A Birecursive is an Isomorphism which can convert back and forth between Calc and CalcF[Calc].

  calcF2Calc: CalcF[Calc] => Calc      turns a CalcF[Calc] into a Calc
  calc2CalcF: Calc => CalcF[Calc]      turns a Calc into a CalcF[Calc]

  We define an implicit Birecursive passing these two isomorphic functions:

    implicit val calcBirecursive: Birecursive.Aux[Calc, CalcF] =
      Birecursive.fromAlgebraIso(calcF2Calc, calc2CalcF)

  In our case a Recursive would be sufficient, as we only have to turn Calc to CalcF[Calc].

    implicit val calcRecursive: Recursive.Aux[Calc, CalcF] =
      Recursive.fromCoalgebra(calc2CalcF)

  Now we create an instance of Recursive (by invoking it's apply method)
  and the invoke 'cata' on the instance:

    val res: Int = Recursive.apply[Calc].cata(calc2)(eval) // 9

  This works as well with our other Algebras 'show' and 'calcF2Calc'.
 */
object Cata14CalcMatryoshka extends util.App { self =>

  import Calc._
  import CalcF._

  implicit val calcFFunctor: scalaz.Functor[CalcF] = new scalaz.Functor[CalcF] {
    override def map[A, B](fa: CalcF[A])(f: A => B): CalcF[B] =
      fa map f
  }

  // 1 + 2
  val calc1 =
    Add(Num(1), Num(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2 =
    Mul(Num(3), Add(Num(1), Num(2)))
      .tap(println)

  def fixedCalc[T](implicit T: Corecursive.Aux[T, CalcF]): T =
    MulF(NumF[T](2).embed, MulF(NumF[T](3).embed, NumF[T](4).embed).embed).embed

  val fixed: Mu[CalcF] = fixedCalc[Mu[CalcF]]

  println
  fixed.cata(show) pipe (str => println(s"show: $str"))
  fixed.cata(eval) pipe (res => println(s"eval: $res"))

  // implicit val calcRecursive: Recursive.Aux[Calc, CalcF] =
  //   Recursive.fromCoalgebra(calc2CalcF)

  implicit val calcBirecursive: Birecursive.Aux[Calc, CalcF] =
    Birecursive.fromAlgebraIso(calcF2Calc, calc2CalcF)

  println
  Recursive.apply[Calc].cata(calc1)(show) pipe (str => println(s"show: $str"))
  Recursive.apply[Calc].cata(calc1)(eval) pipe (res => println(s"eval: $res"))

  println
  Recursive[Calc].cata(calc2)(show) pipe (str => println(s"show: $str"))
  Recursive[Calc].cata(calc2)(eval) pipe (res => println(s"eval: $res"))

  println
  Recursive[Calc].cata(calc1)(calcF2Calc) pipe (calc => println(s"calcF2Calc: $calc"))
  Recursive[Calc].cata(calc2)(calcF2Calc) pipe (calc => println(s"calcF2Calc: $calc"))
}
