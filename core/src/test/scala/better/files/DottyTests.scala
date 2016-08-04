package better.files

import scala.util.control.NonFatal

case class Test(name: String, ignore: Boolean, result: () => Unit)

abstract sealed class TestResult(test: Test) {
  def color: String
  def format: String = test.name
  def print(): Unit = println(color + format + Console.RESET)
}

object TestResult {
  case class Ignored(test: Test) extends TestResult(test) {
    override def color = Console.YELLOW
  }
  case class Success(test: Test) extends TestResult(test) {
    override def color = Console.GREEN

  }
  case class Fail(test: Test, e: Throwable) extends TestResult(test) {
    override def color = Console.RED
    override def format =
      s"""${test.name}: ${e.getClass}: ${e.getMessage}
         |    ${e.getStackTrace.mkString("\n    ")}""".stripMargin
  }
}

case class FailedTestException(msg: String = "failed test") extends Exception(msg)

/**
  * Tiny testing framework
  */
class DottyTests {
  private val testsBuilder = Seq.newBuilder[Test]

  def printResults(): Unit = {
    val toRun = testsBuilder.result()
    val results = toRun.map(withFixture)
    results.foreach(_.print())
    val counts = results.groupBy(_.getClass)
    println("========================")
    println(counts.map { case (a, b) => a + ": " + b.length } mkString "\n")
    println(s"Total tests: ${results.length}")
  }

  def expect[T](handleError: PartialFunction[Throwable, Unit])(body: => T): Unit = {
    try {
      body
      throw FailedTestException("expected error but no error happened")
    } catch handleError
  }

  def assert(cond: => Boolean, msg: String = ""): Unit = {
    if (!cond) fail(msg)
  }

  def addTest(name: String, ignore: Boolean, run: => Unit): Unit = {
    testsBuilder += Test(name, ignore, () => run)
  }

  def test(name: String)(run: => Unit) = addTest(name, ignore = false, () => run)
  def ignore(name: String)(run: => Unit) = addTest(name, ignore = true, () => run)
  def fail(msg: String = "") = throw FailedTestException(msg)
  def beforeEach(): Unit = Unit
  def afterEach(): Unit = Unit

  def withFixture(test: Test): TestResult = {
    beforeEach()
    val testResult: TestResult = try {
      if (test.ignore) {
        TestResult.Ignored(test)
      } else {
        test.result()
        TestResult.Success(test)
      }
    } catch {
      case _: AssertionError => TestResult.Success(test)
      case NonFatal(e) => TestResult.Fail(test, e)
    }
    afterEach()
    testResult
  }

  implicit class ShouldMatcherOps[A](a: A) {
    def shouldBe(a2: A): Unit = shouldEqual(a2)
    def shouldEqual(a2: A): Unit = {
      assert(a == a2,
        s"""Not equal!
            |a: $a
            |a2: $a2
              """.stripMargin)
    }
  }

}

object Main {
  def main(args: Array[String]): Unit = {
    new FileSpec().printResults()
  }
}
