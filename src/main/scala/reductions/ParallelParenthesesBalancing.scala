package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceReq(chars: List[Char], acc: Int): Boolean = {
      if (acc < 0) return false

      chars match {
        case Nil => acc == 0
        case el :: tl => {
          val updAcc = el match {
            case '(' => acc + 1
            case ')' => acc - 1
            case _ => acc
          }
          balanceReq(tl, updAcc)
        }
      }
    }

    balanceReq(chars.toList, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def compose(left: (Int, Int), right: (Int, Int)): (Int, Int) = {
      val leftUpd = left._1 + right._1 - right._2
      if (leftUpd < 0) (0, left._2 - leftUpd)
      else (leftUpd, left._2)
    }

    def traverse(idx: Int, until: Int, disbalanceL: Int, disbalanceR: Int): (Int, Int) = {
      if (idx >= until) return (disbalanceL, disbalanceR)

      chars(idx) match {
        case '(' => traverse(idx + 1, until, disbalanceL + 1, disbalanceR)
        case ')' => if (disbalanceL < 1) traverse(idx + 1, until, disbalanceL, disbalanceR + 1)
          else traverse(idx + 1, until, disbalanceL - 1, disbalanceR)
        case _ => traverse(idx + 1, until, disbalanceL, disbalanceR)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if ((until - from) <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid + 1, until))

        compose(left, right)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
