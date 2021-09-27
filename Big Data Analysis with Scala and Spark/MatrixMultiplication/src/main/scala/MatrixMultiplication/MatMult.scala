package MatrixMultiplication

import org.apache.spark.rdd.RDD

import scala.util.Random
import org.apache.spark.{SparkConf, SparkContext}

object MatMult {
  val conf: SparkConf = new SparkConf().setMaster("local[2]").setAppName("Wikipedia")
  val sc: SparkContext = new SparkContext(conf)

  def generateIntRowMatrix(length: Int, width:Int): RowMatrix[Int] = {
    val rows = for (
      r <- 0 until length
    ) yield for (
      c <- 0 until width
    ) yield Random.nextInt(5)

    new RowMatrix(rows)
  }

  def generateIntColMatrix(length: Int, width:Int): ColumnMatrix[Int] = {
    val cols = for (
      c <- 0 until width
    ) yield for (
      c <- 0 until length
    ) yield Random.nextInt(5)

    new ColumnMatrix(cols)
  }

  def rddFromRowMat[T](rowMatrix: RowMatrix[T]): RDD[(Int, Row[T])] = {
    val rows = for (i <- 0 until rowMatrix.length) yield (i, rowMatrix.getRow(i))
    sc.parallelize(rows).cache()
  }

  def rddFromColMat[T](colMatrix: ColumnMatrix[T]): RDD[(Int, Column[T])] = {
    val cols = for (i <- 0 until colMatrix.width) yield (i, colMatrix.getColumn(i))
    sc.parallelize(cols).cache()
  }

  def multRowsByCols(rows: RDD[(Int, Row[Int])], cols: RDD[(Int, Column[Int])]): RDD[Seq[Int]] = {
    rows.cartesian(cols)
      .map {case ((r, row), (c, col)) => (r, (c, row * col))}
      .groupByKey()
      .mapValues(iter => iter.toVector.sortBy(_._1).map(_._2))
      .sortByKey()
      .map(_._2)
  }

  def distrubutedMult(rowMatrix: RowMatrix[Int], colMatrix: ColumnMatrix[Int]): RowMatrix[Int] = {
    new RowMatrix(multRowsByCols(rddFromRowMat(rowMatrix), rddFromColMat(colMatrix)).collect())
  }

  def sequentialMult(rowMat: RowMatrix[Int], colMat: ColumnMatrix[Int]): RowMatrix[Int] = {
    new RowMatrix(for (
      r <- 0 until rowMat.length
    ) yield
      for (
        c <- 0 until colMat.width
      ) yield rowMat.getRow(r) * colMat.getColumn(c)
    )
  }

  def main(args: Array[String]): Unit = {
    val rowMat = generateIntRowMatrix(1000, 10000)
    val colMat = generateIntColMatrix(10000, 1000)
    /*println(rowMat)
    println()
    println(colMat)
    println()*/

    val start1 = System.currentTimeMillis()
    val res1 = sequentialMult(rowMat, colMat)
    val stop1 = System.currentTimeMillis()

    val start2 = System.currentTimeMillis()
    val res2 = distrubutedMult(rowMat, colMat)
    val stop2 = System.currentTimeMillis()

    println(s"Sequential Multiplication: ${stop1 - start1} ms")
    println(s"Distributed Multiplication: ${stop2 - start2} ms")

    /*println(res1)
    println()
    println(res2)
    assert(res2 == res1)
    println("Results are the same!")*/
  }
}
