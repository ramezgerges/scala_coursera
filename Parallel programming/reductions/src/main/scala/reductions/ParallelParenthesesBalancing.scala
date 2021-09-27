package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    @tailrec def balanceIter(chars: Array[Char], acc: Int, from: Int, until: Int): Boolean = {
      if from == until then acc == 0
      else if chars(from) == ')' then if acc == 0 then false else balanceIter(chars, acc - 1, from + 1, until)
      else if chars(from) == '(' then balanceIter(chars, acc + 1, from + 1, until)
      else balanceIter(chars, acc, from + 1, until)
    }

    balanceIter(chars, 0, 0, chars.length)


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    @tailrec def traverse(idx: Int, until: Int, closingParens: Int, openingParens: Int): (Int, Int) =
      if idx == until then (closingParens, openingParens)
      else if chars(idx) == '(' then traverse(idx + 1, until, closingParens, openingParens + 1)
      else if chars(idx) == ')' then
        if openingParens == 0 then traverse(idx + 1, until, closingParens + 1, openingParens)
        else traverse(idx + 1, until, closingParens, openingParens - 1)
      else traverse(idx + 1, until, closingParens, openingParens)

    def reduce(from: Int, until: Int): (Int, Int) =
      if until - from < threshold then traverse(from, until, 0, 0)
      else
        val midpoint = (from + until) / 2
        val ((c1, o1), (c2, o2)) = parallel(reduce(from, midpoint), reduce(midpoint, until))
        if o1 > c2 then (c1, o1 - c2 + o2)
        else (c1 + c2 - o1, o2)

    reduce(0, chars.length) == (0, 0)

// For those who want more:
  // Prove that your reduction operator is associative!

