import Countdown.{ops, apply}
import ListUtils.{notEmptySplit, subbags}
import Op.*
import Expr.*

object FusionArithmeticOrdering:
  type Result = (Expr, Int)

  def validDash(op:Op, x:Int, y:Int):Boolean = (op,x,y) match {
    case (Add, _, _) => x <= y
    case (Sub, _, _) => x > y
    case (Mul, _, _) => x != 1 && y != 1 && x <= y
    case (Div, _, _) => y != 1 && x % y == 0
  }

  def combineFusion(lr:Result, rr:Result):List[Result] = (lr,rr) match {
    case ((le, x), (re, y)) => for{
      op <- ops
      if validDash(op, x, y)
    } yield (App(op, le, re), apply(op, x, y))
  }


  def results(ns:List[Int]):List[Result] = ns match {

    case Nil => List()
    case List(n) => if(n>0) List((Val(n), n)) else List()
    case lns => for {
      (ls, rs) <- notEmptySplit(lns)
      lx <- results(ls)
      ry <- results(rs)
      res <- combineFusion(lx, ry)
    } yield res
  }

  def solutions(ns:List[Int], n:Int):List[Expr] = for {
    nsDash <- subbags(ns)
    (e, m) <- results(nsDash)
    if m == n
  } yield e

  def possibleTotals(ns:List[Int]):List[Int] = (for {
    nsDash <- subbags(ns)
    (e, m) <- results(nsDash)
  } yield m).distinct.sorted
