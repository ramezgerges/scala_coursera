package kmeans

import java.util.concurrent.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.{Map, Seq, mutable}
import scala.math.*

class KMeansSuite extends munit.FunSuite :
  object KM extends KMeans

  import KM.*

  def checkParClassify(points: ParSeq[Point], means: ParSeq[Point], expected: ParMap[Point, ParSeq[Point]]): Unit =
    assertEquals(classify(points, means), expected, s"classify($points, $means) should equal to $expected")

  test("'classify' should work for empty 'points' and empty 'means'") {
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq().par
    val expected = ParMap[Point, ParSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test(" 'kMeans' should work for 'points' == ParSeq((0, 0, 1), (0,0, -1), (0,1,0), (0,10,0)) and 'oldMeans' == ParSeq((0, -1, 0), (0, 2, 0)) and 'eta' == 12,25") {
    println(kMeans(ParSeq(Point(0, 0, 1), Point(0, 0, -1), Point(0, 1, 0), Point(0, 10, 0)), ParSeq(Point(0, -1, 0), Point(0, 2, 0)), 12.25))
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds


