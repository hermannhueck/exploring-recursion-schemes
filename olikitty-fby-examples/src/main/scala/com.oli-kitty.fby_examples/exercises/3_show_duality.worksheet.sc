/**
  * It’s easy to see that foldRight takes more parameters than unfold. Can we do anything about it?
  *
  * def foldRight6[A, E](op: Option[(E, A)] => A): List[E] => A = ???
  * def unfold[B, E](f: B => Option[(B, E)]): B => List[E] = ???
  */
val list = 5 :: 4 :: 1 :: Nil
val seed = 1
def product(x: Int, y: Int) = x * y

// make z a function
def foldRight1[A, E](z: () => A)(op: (E, A) => A): List[E] => A = {
  new (List[E] => A) { self =>
    def apply(xs: List[E]): A =
      xs match {
        case Nil          => z()
        case head :: tail => op(head, self(tail))
      }
  }
}

foldRight1(() => seed)(product)(list)

// use Either, explain why it's equivalent
def foldRight2[A, E](op: Either[Unit, (E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>
    def apply(xs: List[E]): A =
      xs match {
        case Nil          => op(Left(()))
        case head :: tail => op(Right((head, self(tail))))
      }
  }
}

def product(i: Either[Unit, (Int, Int)]): Int = i match {
  case Left(())      => seed
  case Right((e, a)) => a * e
}

foldRight2(product)(list)

// use Option, explain why it's equivalent
def foldRight3[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>
    def apply(xs: List[E]): A =
      xs match {
        case Nil          => op(None)
        case head :: tail => op(Some((head, self(tail))))
      }
  }
}

def product(i: Option[(Int, Int)]): Int = i match {
  case None         => seed
  case Some((e, a)) => a * e
}

foldRight3(product)(list)
