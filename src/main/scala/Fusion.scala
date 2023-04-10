import Countdown.*
import ListUtils.*
import Expr.*

object Fusion:

  type Result = (Expr, Int)

  def combineFusion(lr:Result, rr:Result):List[Result] = (lr,rr) match {
    case ((le, x), (re, y)) => for{
      op <- ops
      if valid(op, x, y)
    } yield (App(op, le, re), apply(op, x, y))
  }


  def results(ns:List[Int]):List[Result] = ns match {

    case Nil => List()
    case List(n) => if(n>0) List((Val(n), n)) else List()
    case lns => for {
      /*e ← exprs(ns)
      n ← eval(e)*/
      (ls, rs) <- notEmptySplit(lns)
      lx <- results(ls)
      ry <- results(rs)
      res <- combineFusion(lx, ry)
    } yield res //(e, n)
  }

  def solutions(ns:List[Int], n:Int):List[Expr] = for {
    nsDash <- subbags(ns)
    (e, m) <- results(nsDash)
    if(m == n)
  } yield e
