package MatrixMultiplication

abstract class Strip[T](elems: Seq[T]) extends Serializable {
  private val elements = elems
  private type t[M] = this.type

  val length: Int = elements.length

  def apply(n: Int): T =
    if (n < 0) throw new IndexOutOfBoundsException(s"Trying to access split by  negative index: $n")
    else if (n >= length) throw new IndexOutOfBoundsException(s"Trying to access split of length $length by index: $n")
    else elements(n)

}


case class Column[T](elements: Seq[T]) extends Strip[T](elements) with Serializable {
  def split(n: Int): (Column[T], Column[T]) = {
    if (n < 0) throw new IndexOutOfBoundsException(s"Trying to access column by  negative index: $n")
    else if (n >= length) throw new IndexOutOfBoundsException(s"Trying to access column of length $length by index: $n")
    else {
      val (s1, s2) = elements.splitAt(n)
      (Column(s1), Column(s2))
    }
  }

  def ==(that: Column[T]): Boolean = this.elements == that.elements
}

case class Row[T](elements: Seq[T]) extends Strip[T](elements) with Serializable {
  def split(n: Int): (Row[T], Row[T]) = {
    if (n < 0) throw new IndexOutOfBoundsException(s"Trying to access column by  negative index: $n")
    else if (n >= length) throw new IndexOutOfBoundsException(s"Trying to access column of length $length by index: $n")
    else {
      val (s1, s2) = elements.splitAt(n)
      (Row(s1), Row(s2))
    }
  }
  def ==(that: Row[T]): Boolean = this.elements == that.elements
}

object Row extends Serializable {
  implicit class RowHelperInt(r: Row[Int]) extends Serializable {
    def *(that: Column[Int]): Int = {
      require(r.length == that.length, s"Strips don't have same length: ${r.length}, ${that.length}")
      var res = 0
      (0 until r.length).foreach(i => res += r(i) * that(i))
      res
    }
  }

  implicit class RowHelperDouble(r: Row[Double]) extends Serializable {
    def *(that: Column[Double]): Double = {
      require(r.length == that.length, s"Strips don't have same length: ${r.length}, ${that.length}")

      var res = 0.0
      (0 until r.length).foreach(i => res += r(i) * that(i))
      res
    }
  }
}