package stackoverflow

import org.apache.spark.{SparkConf, SparkContext}
import org.junit._

object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
}

class StackOverflowSuite {

  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}
