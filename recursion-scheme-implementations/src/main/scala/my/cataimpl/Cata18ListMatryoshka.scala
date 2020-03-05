package my
package cataimpl

import scala.util.chaining._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

/*
  Matryoshka comes with it' own ListF implementation. Let's import

    import matryoshka.patterns.{ListF, NilF, ConsF}

  Matryoshka's ListF[E, A] (and NilF[E, A] and ConsF[E, A]) has two type parameters.
  E is the type parameter for the List element type (which is Int in our case).
  A is the type parameter, which makes the ListF higher-kinded (also A in our case)

  In step 18 replace our own ListF implemention with Matryoshka's implementation.

  To keep the changes minimal, we remove our own ListF ADT
  and just define three type aliases for ListF, NilF and ConsF:

    type ListF[A] = matryoshka.patterns.ListF[Int, A]
    type NilF[A]  = matryoshka.patterns.NilF[Int, A]
    type ConsF[A] = matryoshka.patterns.ConsF[Int, A]

  Besides that we have to change only one code location. That is where we define the
  nested Fix structure with .embed in line 100ff. Here the apply method of ConsF and NilF
  must be invoked with two type parameters instead of one:

    def fixedList[A](implicit corecursive: Corecursive.Aux[A, ListF]): A =
      ConsF[Int, A](
        1, ConsF[Int, A](
          2, ConsF[Int, A](
            3, ConsF[Int, A](
              4, ConsF[Int, A](
                5, NilF[Int, A]().embed).embed).embed).embed).embed).embed

  That is all. The rest of the code remains unchanged.
 */
object Cata18ListMatryoshka extends util.App {

  import matryoshka.patterns.{ConsF, NilF}

  type ListF[A] = matryoshka.patterns.ListF[Int, A]
  type NilF[A]  = matryoshka.patterns.NilF[Int, A]
  type ConsF[A] = matryoshka.patterns.ConsF[Int, A]

  // pattern functor for ListF
  implicit val functorListF: scalaz.Functor[ListF] = new scalaz.Functor[ListF] {
    override def map[A, B](fa: ListF[A])(f: A => B): ListF[B] =
      fa match {
        case NilF()            => NilF()
        case ConsF(head, tail) => ConsF(head, f(tail))
      }
  }

  // ----- Coalgebras ----------

  // converts   List[Int] => ListF[List[Int]]
  val list2ListF: Coalgebra[ListF, List[Int]] = {
    case Nil          => NilF()
    case head :: tail => ConsF(head, tail)
  }

  // ----- Algebras ----------

  // generic algebra function, that builds an Algebra from
  // a default value 'onNilF' and a function onConsF: (Int, A) => A
  def algebra[A](onNilF: A)(onConsF: (Int, A) => A): Algebra[ListF, A] = {
    case NilF()            => onNilF
    case ConsF(head, tail) => onConsF(head, tail)
  }

  // converts   ListF[List[Int]] => List[Int]
  val listF2List: Algebra[ListF, List[Int]] = algebra(List.empty[Int])(_ :: _)

  val sumAlgebra: Algebra[ListF, Int] = algebra(0)(_ + _)

  val productAlgebra: Algebra[ListF, Int] = algebra(1)(_ * _)

  val lengthAlgebra: Algebra[ListF, Int] = algebra(0)((_, t) => 1 + t)

  val concatSquaresAlgebra: Algebra[ListF, String] = algebra("") {
    case (head, tail) =>
      if (tail.isEmpty)
        (head * head).toString
      else
        s"${(head * head).toString} + $tail"
  }

  val sumOfSquaresAlgebra: Algebra[ListF, Int] = algebra(0) {
    case (head, tail) => head * head + tail
  }

  // zipAlgebras takes two Algebras and creates an new one with a tuple as result type.
  def zipAlgebras[A, B](algA: Algebra[ListF, A], algB: Algebra[ListF, B]): Algebra[ListF, (A, B)] =
    scalaz.Zip[Algebra[ListF, ?]].zip(algA, algB)

  val squares: Algebra[ListF, (String, Int)] =
    zipAlgebras(concatSquaresAlgebra, sumOfSquaresAlgebra)

  val sumLength: Algebra[ListF, (Int, Int)] =
    zipAlgebras(sumAlgebra, lengthAlgebra)

  // FORMAT: OFF
  def fixedList[A](implicit corecursive: Corecursive.Aux[A, ListF]): A =
    ConsF[Int, A](
      1, ConsF[Int, A](
        2, ConsF[Int, A](
          3, ConsF[Int, A](
            4, ConsF[Int, A](
              5, NilF[Int, A]().embed).embed).embed).embed).embed).embed
  // FORMAT: ON

  val fixed: Mu[ListF] = fixedList[Mu[ListF]]

  println
  fixed.cata(sumAlgebra) pipe (sum => println(s"sum: $sum"))
  fixed.cata(productAlgebra) pipe (res => println(s"product: $res"))
  fixed.cata(lengthAlgebra) pipe (res => println(s"length: $res"))

  val list =
    List(1, 2, 3, 4, 5)
      .tap(println)

  // implicit val listRecursive: Recursive.Aux[List[Int], ListF] =
  //   Recursive.fromCoalgebra(list2ListF)

  implicit val listBirecursive: Birecursive.Aux[List[Int], ListF] =
    Birecursive.fromAlgebraIso(listF2List, list2ListF)

  println
  Recursive.apply[List[Int]].cata(list)(sumAlgebra) pipe (sum => println(s"sum: $sum"))
  Recursive.apply[List[Int]].cata(list)(productAlgebra) pipe (product => println(s"product: $product"))
  Recursive.apply[List[Int]].cata(list)(lengthAlgebra) pipe (length => println(s"length: $length"))
  Recursive[List[Int]].cata(list)(concatSquaresAlgebra) pipe (concatSquares =>
    println(s"concatSquares: $concatSquares")
  )
  Recursive[List[Int]].cata(list)(sumOfSquaresAlgebra) pipe (sumOfSquares => println(s"sumOfSquares: $sumOfSquares"))
  Recursive[List[Int]].cata(list)(squares) pipe (squares => println(s"squares: $squares"))
  Recursive[List[Int]].cata(list)(squares) pipe (squares => println(s"squares: ${squares._1} = ${squares._2}"))
  Recursive[List[Int]].cata(list)(sumLength) pipe (sumLength => println(s"sumLength: $sumLength"))
  Recursive[List[Int]].cata(list)(sumLength) pipe {
    case (sum, length) => average(sum, length)
  } pipe println
  Recursive[List[Int]].cata(Nil)(sumLength) pipe {
    case (sum, length) => average(sum, length)
  } pipe println

  def average(sum: Int, length: Int): String =
    if (length > 0)
      s"average: ${sum / length}"
    else
      "empty List -> no average !!!"
}
