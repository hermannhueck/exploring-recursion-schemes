/**
  * So far, our implementation is tightly coupled to a list—our data structure.
  * While we cleaned up the code a bit, we didn’t generalize our functions.
  * To better understand where the List type appears, let’s break down our function into steps:  *
  * Unpacking/projecting data structure
  * Recursion: call self for the nested structure
  * Computation
  */
val list = 5 :: 4 :: 1 :: Nil
val seed = 1
def product(i: Option[(Int, Int)]): Int = i match {
  case None         => seed
  case Some((e, a)) => a * e
}

def foldRight[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>
    def apply(xs: List[E]): A =
      xs match {
        case Nil          => op(None)
        case head :: tail => op(Some((head, self(tail))))
      }
  }
}

foldRight(product)(list)

// step 1: pattern match on list ~ unpack/project the data structure
def foldRight1[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>
    def apply(xs: List[E]): A =
      xs match {
        case Nil          => ???
        case head :: tail => ???
      }
  }
}

// step 2: recursion
def foldRight2[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>
    def apply(xs: List[E]): A =
      xs match {
        case ??? => ???
        case ??? => ??? self (???)
      }
  }
}

// step 3: computation
def foldRight3[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(xs: List[E]): A =
      xs match {
        case ??? => op(???)
        case ??? => op(???)
      }
  }
}
