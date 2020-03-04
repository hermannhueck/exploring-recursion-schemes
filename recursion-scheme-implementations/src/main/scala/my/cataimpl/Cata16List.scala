package my
package cataimpl

import scala.util.chaining._
import recursion._
import my.functor.Functor

/*
  In example 16 we refactor the implementation of the Algebras.

  If we look at the Algebras in example 15, we can see that they are all
  very similar in their signature and in their implemention. They all pattern match
  over the two ListF cases, providing a default value for the NilF case and applying
  an operation on head and tail in the ConsF case, e.g.:

    val sumAlgebra: ListF[Int] => Int = {
      case NilF()            => 0
      case ConsF(head, tail) => head + tail
    }

  Let's write a generic algebra function which returns an Algebra:

    def algebra[A](onNilF: A)(onConsF: (Int, A) => A): Algebra[ListF, A] = {
      case NilF()            => onNilF
      case ConsF(head, tail) => onConsF(head, tail)
    }

  Now we can provide a more terse definition for sumAlgebra.
  We pass 0 as default value as 1st argument, and the + function as 2nd argument.

    val sumAlgebra: Algebra[ListF, Int] = algebra(0)(_ + _)

  We also define zipAlgebras, which takes two Algebras and creates an new one with a tuple as result type.

    def zipAlgebras[A, B](algA: Algebra[ListF, A], algB: Algebra[ListF, B]): Algebra[ListF, (A, B)] = ???

  concatSquaresAlgebra is an Algebra[ListF, String].
  sumOfSquaresAlgebra  is an Algebra[ListF, Int].
  Zipped together we get an Algebra[ListF, (String, Int)].

    val squares: Algebra[ListF, (String, Int)] =
      zipAlgebras(concatSquaresAlgebra, sumOfSquaresAlgebra)

  cata computes the result:

    List(1, 2, 3, 4, 5).toListF.fix.cata(squares) pipe (squares => println(s"squares: $squares"))
    // squares: (1 + 4 + 9 + 16 + 25,55)
 */
object Cata16List extends util.App {

  import fixpoint._

  sealed trait ListF[+A] extends Product with Serializable {
    def fix: Fix[ListF] = ListF.fix(this)
  }

  object ListF {

    case class NilF[A]()                    extends ListF[A]
    case class ConsF[A](head: Int, tail: A) extends ListF[A]

    // converts a List[Int] into a ListF[A]
    def list2ListF[A]: List[Int] => ListF[A] = {
      case Nil          => NilF()
      case head :: tail => ConsF(head, tail.asInstanceOf[A])
    }

    final implicit class ListSyntax(li: List[Int]) {
      def toListF[A]: ListF[A] = list2ListF(li)
    }

    private def fixList[A](a: A): Fix[ListF] =
      fix apply a.asInstanceOf[List[Int]].toListF

    def fix[A]: ListF[A] => Fix[ListF] = {
      case NilF() =>
        Fix(NilF[Fix[ListF]]()) // needs type param for type inference
      case ConsF(head, tail) =>
        Fix(ConsF(head, fixList(tail)))
    }

    // pattern functor for ListF
    implicit val functorListF: Functor[ListF] = new Functor[ListF] {
      override def map[A, B](fa: ListF[A])(f: A => B): ListF[B] =
        fa match {
          case NilF()            => NilF()
          case ConsF(head, tail) => ConsF(head, f(tail))
        }
    }

    // Algebras

    // generic algebra function, that builds an Algebra from
    // a default value 'onNilF' and a function onConsF: (Int, A) => A
    def algebra[A](onNilF: A)(onConsF: (Int, A) => A): Algebra[ListF, A] = {
      case NilF()            => onNilF
      case ConsF(head, tail) => onConsF(head, tail)
    }

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
    def zipAlgebras[A, B](algA: Algebra[ListF, A], algB: Algebra[ListF, B]): Algebra[ListF, (A, B)] = {
      case NilF() =>
        val a = algA(NilF())
        val b = algB(NilF())
        (a, b)
      case ConsF(head, tail) =>
        val a = algA(ConsF(head, tail._1))
        val b = algB(ConsF(head, tail._2))
        (a, b)
    }

    val squares: Algebra[ListF, (String, Int)] =
      zipAlgebras(concatSquaresAlgebra, sumOfSquaresAlgebra)

    val sumLength: Algebra[ListF, (Int, Int)] =
      zipAlgebras(sumAlgebra, lengthAlgebra)
  }

  import ListF._

  val l1 =
    List(1, 2, 3, 4, 5)
      .tap(println)

  println
  l1.toListF.fix.cata(sumAlgebra) pipe (sum => println(s"sum: $sum"))
  l1.toListF.fix.cata(productAlgebra) pipe (product => println(s"product: $product"))
  l1.toListF.fix.cata(lengthAlgebra) pipe (length => println(s"length: $length"))
  l1.toListF.fix.cata(concatSquaresAlgebra) pipe (concatSquares => println(s"concatSquares: $concatSquares"))
  l1.toListF.fix.cata(sumOfSquaresAlgebra) pipe (sumOfSquares => println(s"sumOfSquares: $sumOfSquares"))
  l1.toListF.fix.cata(squares) pipe (squares => println(s"squares: $squares"))
  l1.toListF.fix.cata(squares) pipe (squares => println(s"squares: ${squares._1} = ${squares._2}"))
  l1.toListF.fix.cata(sumLength) pipe (sumLength => println(s"sumLength: $sumLength"))
  l1.toListF.fix.cata(sumLength) pipe {
    case (sum, length) => average(sum, length)
  } pipe println
  Nil.toListF.fix.cata(sumLength) pipe {
    case (sum, length) => average(sum, length)
  } pipe println

  def average(sum: Int, length: Int): String =
    if (length > 0)
      s"average: ${sum / length}"
    else
      "empty List -> no average !!!"
}
