package freecofree.recursion

final case class Fix[F[_]](unfix: F[Fix[F]])
