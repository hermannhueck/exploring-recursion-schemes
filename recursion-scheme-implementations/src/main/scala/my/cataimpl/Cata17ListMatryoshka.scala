package my
package cataimpl

import scala.util.chaining._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

/*
  In step 17 we replace our self-made cata implementation
  with the use of Matryoshka.

  We remove everyhting that has to do with Fix as we use the Matryoska
  fixpoint types (Mu in hist case)

  We define our Pattern Functor as scalaz.Functor, as Matryoshka is based on Scalaz.

  We again need our two conversion functions (list2ListF and listF2List)
  which convert from List[Int] to ListF[List[Int]] and vice versa.

  And we reuse the Algebras from the previous example.

  We can create a Fix structure by invoking the extension method 'embed' on envery component
  of a nested ListF:

    def fixedList[A](implicit corecursive: Corecursive.Aux[A, ListF]): A =
      ConsF[A](
        1, ConsF[A](
          2, ConsF[A](
            3, ConsF[A](
              4, ConsF[A](
                5, NilF[A]().embed).embed).embed).embed).embed).embed

    val fixed: Mu[ListF] = fixedList[Mu[ListF]]

  (Mu is one of Matryoshka's Fixpoint types.)

  Now we can invoke Matryoshka's catamorphism on the 'fixed':

    val sum: Int = fixed.cata(sumAlgebra) // 15

  We normally don't have a nested Fix structure in our application.
  Let's assume we have a List[Int] instance.

    val list = List(1, 2, 3, 4, 5)

  A Birecursive is an Isomorphism which can convert back and forth between List[Int] and ListF[List[Int]].

  listF2List: ListF[List[Int]] => List[Int]      turns a ListF[List[Int]] into a List[Int]
  .ist2ListF: List[Int] => ListF[List[Int]]      turns a List[Int] into a ListF[List[Int]]

  We define an implicit Birecursive passing these two isomorphic functions:

    implicit val ListBirecursive: Birecursive.Aux[List[Int], ListF] =
      Birecursive.fromAlgebraIso(listF2List, list2ListF)

  In our case a Recursive would be sufficient, as we only have to turn List[Int] to ListF[List[Int]].

    implicit val ListRecursive: Recursive.Aux[List[Int], ListF] =
      Recursive.fromCoalgebra(list2ListF)

  Now we create an instance of Recursive (by invoking it's apply method)
  and the invoke 'cata' on the instance:

    val res: Int = Recursive.apply[List[Int]].cata(list)(sumAlgebra) // 15

  This works as well with our other Algebras 'productAlgebra' etc.

  The function zipAlgebras con now be implemented with scalaz.Zip.

    def zipAlgebras[A, B](algA: Algebra[ListF, A], algB: Algebra[ListF, B]): Algebra[ListF, (A, B)] =
      scalaz.Zip[Algebra[ListF, ?]].zip(algA, algB)
 */
object Cata17ListMatryoshka extends util.App {

  sealed trait ListF[+A] extends Product with Serializable

  object ListF {

    case class NilF[A]()                    extends ListF[A]
    case class ConsF[A](head: Int, tail: A) extends ListF[A]

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
  }

  import ListF._

  // FORMAT: OFF
  def fixedList[A](implicit corecursive: Corecursive.Aux[A, ListF]): A =
    ConsF[A](
      1, ConsF[A](
        2, ConsF[A](
          3, ConsF[A](
            4, ConsF[A](
              5, NilF[A]().embed).embed).embed).embed).embed).embed
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
