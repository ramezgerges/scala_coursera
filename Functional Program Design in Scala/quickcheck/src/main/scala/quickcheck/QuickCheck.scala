package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for
      node <- arbitrary[A]                                    // gives random element of type A
      heap <- genHeap  // either empty, or a heap from a recursive call to this function
    yield insert(node, heap)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (p: (Int, Int)) =>
    val h = insert(p._1, insert(p._2, empty))
    findMin(h) == (if p._1 < p._2 then p._1 else p._2)
  }

  property("gen2") = forAll { (a: Int) =>
    deleteMin(insert(a, empty))  == empty
  }
/*
  property("meld1") = forAll { (h1: H, h2: H) =>
    println(s"${findMin(h1)} ${findMin(h2)} ${findMin(meld(h1, h2))} ${if findMin(h1) < findMin(h2) then findMin(h1) else findMin(h2)}")

    !isEmpty(h1) && !isEmpty(h2) && findMin(meld(h1, h2)) == (if findMin(h1) < findMin(h2) then findMin(h1) else findMin(h2))
  }

  property("meld2") = forAll { (p: (H, H)) =>
    val (h1, h2) = p
    followsOrder(meld(h1, h2), Nil)
  }
*/
  property("insdel") = forAll { (h: H, n: Int) =>
    if isEmpty(h) then true
    else
      val min = findMin(h)
      val less = if n < min then n else min
      findMin(deleteMin(insert(min, insert(less, deleteMin(h))))) == min
  }

  private def followsOrder(h: H, acc: List[A]): Boolean =
    if isEmpty(h) then true
    else
      val newH = deleteMin(h)
      val min = findMin(h)
      acc match
        case Nil => followsOrder(newH, List(min))
        case head :: _ => if min >= head then followsOrder(newH, min :: acc) else false
/*
  property("ins2") = forAll { (h: H) =>
    if isEmpty(h) then true
    else
      val min = findMin(h)
      contains(min, ins(min, h, 20), 20)
  }

  def ins(a: A, h: H, times: Int): H =
    if times == 0 then h
    else
      ins(a - 1, insert(a - 1, h), times - 1)

  def contains(a: A, h: H, times: Int): Boolean =
    if times == 0 then true
    else
      val min = findMin(h)
      min == a - times && contains(a, deleteMin(h), times - 1)
*/
  property("seq") = forAll { (h: H) =>
    followsOrder(h, Nil)
  }
