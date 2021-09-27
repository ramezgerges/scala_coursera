package MatrixMultiplication

class TestSuite extends munit.FunSuite {

  trait TestSamples {
    val strip1: Row[Int] = Row(Seq(1, 2, 3, 4))
    val strip2: Column[Int] = Column(Seq(5, 6, 7, 8))

    val mat = Seq(
      Seq(46, 87, 31),
      Seq(28, 49, 78),
      Seq(3, 6, 48),
      Seq(69, 58, 39)
    )
    val matTransposed = Seq(
      Seq(46, 28, 3, 69),
      Seq(87, 49, 6, 58),
      Seq(31, 78, 48, 39)
    )
  }

  test("Multiplication returns correct answers") {
    new TestSamples {
      assertEquals(strip1 * strip2, 70)
    }
  }

  test("apply returns correct element") {
    new TestSamples {
      assertEquals(strip2(0), 5)
      assertEquals(strip2(1), 6)
      assertEquals(strip2(2), 7)
      assertEquals(strip2(3), 8)
    }
  }

  test("Row matrix cell query returns correct elements") {
    new TestSamples {
      val m = new RowMatrix(mat)
      for (
        r <- mat.indices
      ) for (
        c <- mat.head.indices
      ) assertEquals(mat(r)(c), m(r, c))
    }
  }

  test("Column matrix cell query returns correct elements") {
    new TestSamples {
      val m = new ColumnMatrix(matTransposed)
      for (
        r <- mat.indices
      ) for (
        c <- mat.head.indices
      ) assertEquals(mat(r)(c), m(r, c), s"query: $r,$c")
    }
  }

  test("Column matrix getColumn returns correct column") {
    new TestSamples {
      val m = new ColumnMatrix(matTransposed)
      for (
        c <- mat.head.indices
      ) assertEquals(Column(matTransposed(c)), m.getColumn(c))
    }
  }

  test("Row matrix getRow returns correct column") {
    new TestSamples {
      val m = new RowMatrix(mat)
      for (
        r <- mat.indices
      ) assertEquals(Row(mat(r)), m.getRow(r))
    }
  }
}