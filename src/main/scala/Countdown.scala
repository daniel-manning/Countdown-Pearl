import ListUtils._

sealed trait Op

case object Add extends Op
case object Sub extends Op
case object Mul extends Op
case object Div extends Op

sealed trait Expr
case class Val(x:Int) extends Expr
case class App(op:Op, le:Expr, re:Expr) extends Expr

object Countdown {

  def valid(op:Op, x:Int, y:Int):Boolean = (op,x,y) match {
    case (Add, _, _) => true
    case (Sub, x, y) => x > y
    case (Mul, _, _) => true
    case (Div, x, y) => x % y == 0
  }

  def apply(op:Op, x:Int, y:Int):Int = (op,x,y) match {
    case (Add, x, y) => x + y
    case (Sub, x, y) => x - y
    case (Mul, x, y) => x * y
    case (Div, x, y) => x / y
  }

  def values(expr:Expr):List[Int] = expr match {
    case Val(n) => List(n)
    case App(_, le, re) => values(le) ++ values(re)
  }

  def eval(expr:Expr):List[Int] = expr match {
    case Val(n) => if(n > 0) List(n) else List()
    case App(op, le, re) => for{
         x <- eval(le)
         y <- eval(re)
         if valid(op, x, y)
    } yield apply(op, x, y)
  }


  def solution(expr:Expr, ns:List[Int], n:Int):Boolean = {
    subbags(ns).contains(values(expr)) && (eval(expr) == List(n))
  }


  def exprs(ns:List[Int]):List[Expr] = ns match {
    case Nil => List()
    case n::Nil => List(Val(n))
    case xs => for {
      (ls, rs) <- notEmptySplit(xs)
      l <- exprs(ls)
      r <- exprs(rs)
      e <- combine(l, r)
    } yield e
  }

  def combine(le:Expr, re:Expr):List[Expr] =  ops.map(op => App(op, le, re))
  def ops = List(Add, Sub, Mul, Div)

  def show(expr:Expr):String = expr match {
    case Val(n) => s"$n"
    case App(op:Op, le:Expr, re:Expr) => s"(${show(le)} ${show(op)} ${show(re)})"
  }

  def show(op:Op):String = op match {
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
    case Div => "/"
  }

  def main(args:Array[String]): Unit = {
    if(args.head.contains("[")) {
      //no target is selected
      val chosenNumbers: List[Int] =
        args
        .map(_.filter(_.toString.matches("[0-9]+")).toInt)
        .sortBy(identity)
        .reverse
        .toList
      
      println(s"Your ${chosenNumbers.size} numbers are: $chosenNumbers")
      
      //profile run
      val t0 = System.nanoTime()
      val targets = FusionArithmeticOrdering.possibleTotals(chosenNumbers).filter(_ < 1000)
      val t1 = System.nanoTime()
      println(s"Elapsed time: ${(t1 - t0) / 1000000000}s")

      //display solutions
      println(s"${targets.length} possible Targets: $targets")
    }else{
      //target is selected
      val targetNumber: Int = args.head.toInt
      val chosenNumbers: List[Int] =
        args
        .tail
        .map(_.filter(_.toString.matches("[0-9]+")).toInt)
        .sortBy(identity)
        .reverse
        .toList

      println(s"Your ${chosenNumbers.size} numbers are: $chosenNumbers for a target of: $targetNumber")

      //profile run
      val t0 = System.nanoTime()
      val sols = FusionArithmeticOrdering.solutions(chosenNumbers, targetNumber)
      val t1 = System.nanoTime()
      println(s"Elapsed time: ${(t1 - t0) / 1000000000}s")
      
      //display solutions
      println(s"${sols.size} solutions")
      sols.foreach(expr => println(show(expr)))
    }
  }
}




