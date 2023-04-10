object ListUtils:

  def subSequences[A](xs:List[A]):List[List[A]] = (0 to xs.length).iterator.flatMap(i => xs.combinations(i)).toList

  def subbags[A](xs:List[A]):List[List[A]] = for {
    ys <- subSequences(xs)
    zs <- ys.permutations
  } yield zs

  def split[A](xs:List[A]):List[(List[A], List[A])] =  xs match {
    case Nil => List((List(), List()))
    case x :: ls => (List(), x :: ls) :: (for {
      (ls, rs) <- split(ls)
    } yield (x :: ls, rs))
  }

  def notEmpty[A,B](n:(List[A], List[B])):Boolean = n match {
    case (xs, ys) =>  !(xs.isEmpty || ys.isEmpty)
  }

  def notEmptySplit[A](xs:List[A]):List[(List[A], List[A])] = split(xs).filter(x => notEmpty(x))
