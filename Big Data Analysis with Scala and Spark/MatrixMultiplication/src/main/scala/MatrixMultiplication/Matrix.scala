package MatrixMultiplication

trait Matrix[T] extends Serializable {
  val length: Int
  val width: Int

  def apply(r: Int, c: Int): T

  def trans: Matrix[T]

  override def toString: String = {
    (
      for (
        r <- 0 until length
      ) yield
        (
          for (
            c <- 0 until width
          ) yield
            this (r, c).toString).mkString("[", ", ", "]")
      ).mkString("\n")
  }
}

class RowMatrix[T](private val rows: Seq[Seq[T]]) extends Matrix[T] with Serializable {
  require(rows.forall(r => r.length ==  rows.head.length), "All rows must have the same length")

  //def this(rowObjs: Seq[Row[T]]) =  this(rowObjs.map(r => r.elements))

  override val length: Int =  rows.length
  override val width: Int = rows.head.length

  require(length > 0, "Matrix must have positive length")
  require(width > 0, "Matrix must have positive width")

  override def apply(r: Int, c:Int): T = {
    require(r >= 0, "Row must be nonnegative")
    require(c >= 0, "Column must be nonnegative")
    require(r < rows.length, s"Row must be within bounds: no. of rows: ${rows.length}, query row: $r")
    require(c < rows.head.length, s"Column must be within bounds: no. of columns: ${rows.head.length}, query column: $c")
    rows(r)(c)
  }

  def getRow(r: Int): Row[T] = {
    require(r >= 0, "Row must be nonnegative")
    require(r < rows.length, s"Row must be within bounds: no. of rows: ${rows.length}, query row: $r")
    Row(rows(r))
  }

  def trans: ColumnMatrix[T] = new ColumnMatrix(rows)
}

class ColumnMatrix[T](private val cols: Seq[Seq[T]]) extends Matrix[T]  with Serializable {
  require(cols.forall(c => c.length == cols.head.length), "All columns must have the same length")

  override val width: Int = cols.length
  override val length: Int = cols.head.length

  require(length > 0, "Matrix must have positive length")
  require(width > 0, "Matrix must have positive width")

  override def apply(r: Int, c: Int): T = {
    require(r >= 0, "Row must be nonnegative")
    require(c >= 0, "Column must be nonnegative")
    require(r < cols.head.length, s"Row must be within bounds: no. of rows: ${cols.head.length}, query row: $r")
    require(c < cols.length, s"Column must be within bounds: no. of columns: ${cols.length}, query column: $c")
    cols(c)(r)
  }

  def getColumn(c: Int): Column[T] = {
    require(c >= 0, "Column must be nonnegative")
    require(c < cols.length, s"Column must be within bounds: no. of columns: ${cols.length}, query column: $c")
    Column(cols(c))
  }

  def trans: RowMatrix[T] = new RowMatrix(cols)
}