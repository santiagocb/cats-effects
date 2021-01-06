package co.com.catseffects.datatypes

import org.scalatest.FunSuite

import scala.concurrent.{Await, Future}
import scala.io.StdIn

class ConceptsSuite extends FunSuite {

  test("Referential transparency") {

    val x = "Hello".reverse
    assert(x ++ x == "Hello".reverse ++ "Hello".reverse)
  }

  test("Broken Referential transparency") {

    val x: Int = StdIn.readInt()

    assert(x + x == StdIn.readInt() + StdIn.readInt())
  }

  test("Check if Future[_] has referential transparency property") {

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration._

    //Same behaviour like above
    val x: Future[Int] = Future(StdIn.readInt())
    val operation = for {
      a <- x
      b <- x
      c <- Future(StdIn.readInt())
      d <- Future(StdIn.readInt())
    } yield a + b == c + d

    assert(Await.result(operation, Inf))
  }


}
