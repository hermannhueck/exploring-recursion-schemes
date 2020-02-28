/**
  * Reimplement foldRight.
  * Instead of having one function inside `apply`, let's introduce three new functions: one per each step.
  *
  * def foldRight0[A, E](op: Option[(E, A)] => A): List[E] => A = {
  *
  *   new (List[E] => A) { kernel =>
  *
  *     def step1: ??? => ??? = ???
  *     def step2: ??? => ??? = ???
  *     def step3: ??? => ??? = ???

  *     def apply(v1: List[E]): A =
  *       v1 match {
  *         case Nil        => op(None)
  *         case head :: tl => op(Some((head, kernel(tl))))
  *       }
  *   }
  * }
  */
val list = 5 :: 4 :: 1 :: Nil
val seed = 1

def product(i: Option[(Int, Int)]): Int = i match {
  case None         => seed
  case Some((e, a)) => a * e
}

// Provide signatures
def foldRight[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>

    val step1: List[E] => Option[(E, List[E])] = ??? // unpack
    val step2: Option[(E, List[E])] => Option[(E, A)] = ??? // recurse
    val step3: Option[(E, A)] => A = ??? // compute

    def apply(xs: List[E]): A = (step1 andThen step2 andThen step3)(xs)
  }
}

// implement  step3
def foldRight2[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>

    val unpack: List[E] => Option[(E, List[E])] = ???
    val recurse: Option[(E, List[E])] => Option[(E, A)] = ???
    val compute: Option[(E, A)] => A = op

    def apply(xs: List[E]): A = (unpack andThen recurse andThen compute)(xs)
  }
}

// implement step1 & step2
def foldRight3[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>

    val unpack: List[E] => Option[(E, List[E])] = {
      case Nil          => None
      case head :: tail => Some(head -> tail)
    }
    val recurse: Option[(E, List[E])] => Option[(E, A)] = {
      case None               => None
      case Some(head -> tail) => Some(head -> self(tail))
    }
    val compute: Option[(E, A)] => A = op

    def apply(xs: List[E]): A = (unpack andThen recurse andThen compute)(xs)
  }
}

foldRight3(product)(list)

// introduce map and replace compute by op
def foldRight4[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { self =>

    val unpack: List[E] => Option[(E, List[E])] = {
      case Nil          => None
      case head :: tail => Some(head -> tail)
    }

    val recurse: Option[(E, List[E])] => Option[(E, A)] =
      opt => opt map { case head -> tail => head -> self(tail) }

    def apply(xs: List[E]): A = (unpack andThen recurse andThen op)(xs)
  }
}

foldRight4(product)(list)
