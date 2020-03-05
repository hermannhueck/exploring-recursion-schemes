def range(x: Int): Option[(Int, Int)] = if (x > 0) Some((x, x - 1)) else None

/*
def unfold0[E, B](f: B => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { self =>

    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(b: B): List[E] =
      f(b) match {
        case None          => Nil
        case Some((xs, x)) => xs :: self(x)
      }
  }
}

unfold0(range)(10)
 */

// provide signatures
def unfold1[E, B](f: B => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { self =>

    def step1: B => Option[(E, B)]                    = ??? // compute
    def step2: Option[(E, B)] => Option[(E, List[E])] = ??? // recurse
    def step3: Option[(E, List[E])] => List[E]        = ??? // embed ~= build List

    def apply(b: B): List[E] = (step1 andThen step2 andThen step3)(b)
  }
}

// apply step1
def unfold2[E, B](f: B => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { self =>

    def compute: B => Option[(E, B)]                    = f
    def recurse: Option[(E, B)] => Option[(E, List[E])] = ???
    def embed: Option[(E, List[E])] => List[E]          = ???

    def apply(b: B): List[E] = (compute andThen recurse andThen embed)(b)
  }
}

// apply the rest
def unfold3[E, B](f: B => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { self =>

    def compute: B => Option[(E, B)] = f
    def recurse: Option[(E, B)] => Option[(E, List[E])] = {
      case None         => None
      case Some((e, b)) => Some(e -> self(b))
    }
    def embed: Option[(E, List[E])] => List[E] = {
      case None            => Nil
      case Some((e, list)) => e :: list
    }

    def apply(b: B): List[E] = (compute andThen recurse andThen embed)(b)
  }
}

unfold3(range)(10)

// introduce map and replace compute by f
def unfold4[E, B](f: B => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { self =>

    def recurse: Option[(E, B)] => Option[(E, List[E])] =
      opt => opt map { case (e, b) => e -> self(b) }

    def embed: Option[(E, List[E])] => List[E] = {
      case None            => Nil
      case Some((e, list)) => e :: list
    }

    def apply(b: B): List[E] = (f andThen recurse andThen embed)(b)
  }
}

unfold4(range)(10)
