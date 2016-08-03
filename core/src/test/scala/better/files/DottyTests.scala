package better.files

import scala.util.control.NonFatal

/**
  * Tiny testing framework
  */
class DottyTests {
  private val testsBuilder = Seq.newBuilder[TestResult]

  def printResults(): Unit = {
    val results = testsBuilder.result()
    results.foreach(_.print())
    val counts = results.groupBy(_.getClass)
    println("========================")
    println(counts.map { case (a, b) => a + ": " + b.length } mkString "\n")
    println(s"Total tests: ${results.length}")
  }
  case class FailedTestException(msg: String = "failed test") extends Exception(msg)
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
  case class Test(name: String, ignore: Boolean = false) {
    def apply[T](f: => Unit): Unit = {
      beforeEach()
      val result: TestResult = try {
        if (ignore) {
          TestResult.Ignored(this)
        } else {
          f
          TestResult.Success(this)
        }
      } catch {
        case _: AssertionError => TestResult.Success(this)
        case NonFatal(e) => TestResult.Fail(this, e)
      }
      afterEach()
      testsBuilder += result
    }
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

  def assert(cond: => Boolean, msg: String = ""): Unit = {
    if (!cond) fail(msg)
  }

  def expect[T](handleError: PartialFunction[Throwable, Unit])(body: => T): Unit = {
    try {
      body
      throw FailedTestException("expected error but no error happened")
    } catch handleError
  }

  def test(name: String) = Test(name)
  def ignore(name: String) = Test(name, ignore = true)

  def beforeEach(): Unit = Unit
  def afterEach(): Unit = Unit
  def withFixture(test: Test): Unit = Unit

  def fail(msg: String = "") = throw FailedTestException(msg)
}

object Main {
  def main(args: Array[String]): Unit = {
    new FileSpec().printResults()
  }
}
