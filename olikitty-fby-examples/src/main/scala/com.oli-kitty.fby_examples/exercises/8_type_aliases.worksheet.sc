import cats.Functor
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._

val list = 5 :: 4 :: 1 :: Nil
val seed = 1

def product(i: Option[(Int, Int)]): Int = i match {
  case None         => seed
  case Some((e, a)) => a * e
}

/**
  * Our goal is to get you accustomed to working with more abstract structures,
  * and develop the ability to recognize them. In the tail code snippet,
  * we’d like to introduce type aliases F[P] and S to decrease distraction and focus on an abstraction.
  */
def foldRight[A, E](op: Option[(E, A)] => A): List[E] => A = {

  // type F[P] = Option[(E, P)]
  // type S = List[E]

  new (List[E] => A) { self =>

    def unpack: List[E] => Option[(E, List[E])] = {
      case Nil          => None
      case head :: tail => Some((head, tail))
    }
    def recurse: Option[(E, List[E])] => Option[(E, A)] = {
      case None          => None
      case Some((x, xs)) => Some((x, self(xs)))
    }
    def compute: Option[(E, A)] => A = op

    def apply(xs: List[E]): A =
      compute(recurse(unpack(xs)))
  }
}

foldRight(product)(list)

// use type aliases
def foldRight1[A, E](op: Option[(E, A)] => A): List[E] => A = {

  type F[P] = Option[(E, P)]
  type S    = List[E]

  new (S => A) { self =>

    def project: S => F[S] = {
      case Nil          => None
      case head :: tail => Some((head, tail))
    }
    def recurse: F[S] => F[A] = {
      case None          => None
      case Some((x, xs)) => Some((x, self(xs)))
    }
    def compute: F[A] => A = op

    def apply(xs: List[E]): A = (project andThen recurse andThen compute)(xs)
  }
}

foldRight1(product)(list)

// use Functor
def foldRight2[A, E](op: Option[(E, A)] => A): List[E] => A = {

  type F[P] = Option[(E, P)]
  type S    = List[E]

  // composed Functor for Option of Pair
  implicit val F = Functor[Option].compose[(E, ?)]

  new (S => A) { self =>

    def project: S => F[S] = {
      case Nil          => None
      case head :: tail => Some((head, tail))
    }
    def recurse: F[S] => F[A] = _.fmap(self)
    def compute: F[A] => A    = op

    def apply(xs: List[E]): A = (project andThen recurse andThen compute)(xs)
  }
}

foldRight2(product)(list)

// simplified further
def foldRight3[A, E](op: Option[(E, A)] => A): List[E] => A = {

  type F[P] = Option[(E, P)]
  type S    = List[E]

  // composed Functor for Option of Pair
  implicit val F = Functor[Option].compose[(E, ?)]

  new (S => A) { self =>

    def project: S => F[S] = {
      case Nil          => None
      case head :: tail => Some((head, tail))
    }

    def apply(xs: List[E]): A = op(project(xs).fmap(self))
  }
}

foldRight3(product)(list)
