import cats.Functor
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.functor._

def range(x: Int): Option[(Int, Int)] = if (x > 0) Some((x, x - 1)) else None

/**
  * Our goal is to get you accustomed to working with more abstract structures,
  * and develop the ability to recognize them. In the next code snippet,
  *  we’d like to introduce type aliases F[P] and S to decrease distraction and focus on abstraction.
  */
def unfold0[E, B](f: B => Option[(E, B)]): B => List[E] = {

  type F[P] = Option[(E, P)]
  type S = List[E]

  new (B => List[E]) { self =>

    def compute: B => Option[(E, B)] = f
    def recurse: Option[(E, B)] => Option[(E, List[E])] = _ match {
      case None          => None
      case Some((x, xs)) => Some((x, self(xs)))
    }
    def embed: Option[(E, List[E])] => List[E] = _ match {
      case None          => Nil
      case Some((x, xs)) => x :: xs
    }

    def apply(b: B): List[E] =
      embed(recurse(compute(b)))
  }
}

unfold0(range)(10)

// use type aliases
def unfold1[E, B](f: B => Option[(E, B)]): B => List[E] = {

  type F[P] = Option[(E, P)]
  type S = List[E]

  new (B => S) { self =>

    def compute: B => F[B] = f
    def recurse: F[B] => F[S] = _ match {
      case None          => None
      case Some((x, xs)) => Some((x, self(xs)))
    }
    def embed: F[S] => S = _ match {
      case None          => Nil
      case Some((x, xs)) => x :: xs
    }

    def apply(b: B): S =
      embed(recurse(compute(b)))
  }
}

unfold1(range)(10)

// use Functor
def unfold2[E, B](f: B => Option[(E, B)]): B => List[E] = {

  type F[P] = Option[(E, P)]
  type S = List[E]

  implicit val F = Functor[Option].compose[(E, ?)]

  new (B => S) { self =>

    def compute: B => F[B] = f
    def recurse: F[B] => F[S] = _.fmap(self)
    def embed: F[S] => S = _ match {
      case None          => Nil
      case Some((x, xs)) => x :: xs
    }

    def apply(b: B): S =
      embed(recurse(compute(b)))
  }
}

unfold2(range)(10)

// simplify further
def unfold3[E, B](f: B => Option[(E, B)]): B => List[E] = {

  type F[P] = Option[(E, P)]
  type S = List[E]

  implicit val F = Functor[Option].compose[(E, ?)]

  new (B => S) { self =>

    def embed: F[S] => S = _ match {
      case None          => Nil
      case Some((x, xs)) => x :: xs
    }

    def apply(b: B): S = {
      val fb: F[B] = f(b) // intermediate step helps type interferencer
      embed(fb.fmap(self))
    }
  }
}

unfold3(range)(10)
