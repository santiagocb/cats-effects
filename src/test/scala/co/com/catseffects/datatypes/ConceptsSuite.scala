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
    val x: Int = {
      println(s"Input a number: ")
      StdIn.readInt()
    }
    assert(x + x == { println(s"Input a number: "); StdIn.readInt() } + {
      println(s"Input a number: ")
      StdIn.readInt()
    })
  }

  test("Check if Future[_] has referential transparency property") {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration._

    //Same behaviour like above
    val x: Future[Int] = Future({
      println(s"Input a number: ")
      StdIn.readInt()
    })
    val operation = for {
      a <- x
      b <- x
      c <- Future({
        println(s"Input a number: ")
        StdIn.readInt()
      })
      d <- Future({
        println(s"Input a number: ")
        StdIn.readInt()
      })
    } yield a + b == c + d

    assert(Await.result(operation, Inf))
  }

  test("By name & by value parameter") {
    def byNameParameter(condition: => Boolean)(x: => Int) = if(condition) x else 0
    def byValueParameter(condition: Boolean)(x: Int) = if(condition) x else 0

    assert(byNameParameter(condition = false)(5) == 0)  //In this case de parameter x is not evaluated (this makes this parameter lazy)
    assert(byNameParameter(condition = true)(2) == 2)   //x is evaluated
    assert(byValueParameter(condition = false)(5) == 0)   //By value parameters are always evaluated but just once
    assert(byValueParameter(condition = true)(2) == 2)   //x is evaluated once

  }


}
