import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should.*
import Expr.*
import Op.*

class CountdownSpec extends AnyFlatSpec with Matchers {

  "An integer list" should "split correctly" in {
    ListUtils.split(List(1,2)) should be (List((List(), List(1, 2)), (List(1), List(2)), (List(1, 2), List())))
  }

  "An integer list" should "nesplit correctly" in {
    ListUtils.notEmptySplit(List(1,2)) should be (List((List(1), List(2))))
  }

  "An pair of empty lists" should "be false" in {
    ListUtils.notEmpty((List(),List())) should be (false)
  }

  "A right empty list" should "be false" in {
    ListUtils.notEmpty((List(1,2),List())) should be (false)
  }

  "A left empty list" should "be false" in {
    ListUtils.notEmpty((List(),List(1,2))) should be (false)
  }

  "An pair of non-empty lists" should "be true" in {
    ListUtils.notEmpty((List(1,2),List(1,2))) should be (true)
  }

  "An integer list" should "create a list of expressions" ignore {
    Countdown.exprs(List(3,4)) should be (List(Val(1)))
  }

  "An valid solution" should "evaluate correctly" in {
    Countdown.solution(App(Add, Val(3), Val(4)), List(3,4), 7) should be (true)
  }

  "An integer list" should "find a BruteForce solution to a solvable case" in {
    val solutions = BruteForce.solutions(List(3,4), 7)
    assert(solutions.nonEmpty)
  }

  "An integer list" should "find a Fusion solution to a solvable case" in {
    val solutions = Fusion.solutions(List(3,4), 7)
    assert(solutions.nonEmpty)
  }

  "An integer list" should "find a FusionArithmeticOrdering solution to a solvable case" in {
    val solutions = FusionArithmeticOrdering.solutions(List(3,4), 7)
    assert(solutions.nonEmpty)
  }

  "An integer list" should "find a unique FusionArithmeticOrdering solution to a solvable case" in {
    val solutions = FusionArithmeticOrdering.solutions(List(3,4), 7)
    assert(solutions.lengthCompare(1) == 0)
  }

  "An integer list" should "give every possible total" in {
    val solutions = FusionArithmeticOrdering.possibleTotals(List(1,2,3,4))
    assert(solutions.lengthCompare(31) == 0)
    assert(solutions == List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,32,36))
  }
}
