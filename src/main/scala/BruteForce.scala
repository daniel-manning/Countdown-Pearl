import Countdown.{eval, exprs}
import ListUtils.*

object BruteForce:

  def solutions(ns:List[Int],n:Int):List[Expr] = for {
    nsDash <- subbags(ns)
    e <- exprs(nsDash)
    if eval(e) == List(n)
  } yield e
