package co.com.catseffects.datatypes

import cats.effect.IO
import org.scalatest.FunSuite

import scala.io.StdIn

class IOSuite extends FunSuite {

  /* IO Computation
   * Side effect as values
   * Referential transparency
   * True laziness
   * Compositionality
   * Cancelability
   * Stack-safety (trampolining)
   * Lightweight threads
   * Dependency injection */

  test("IO Monad") {
    val ioa = IO { println("hey!") }
    println("This message must be printed before hey!")   //this is a side-effect, but it is there just for testing

    val program: IO[Unit] =
      for {
        _ <- ioa
        _ <- ioa
      } yield ()

    program.unsafeRunSync()
    assert(true)
  }

  test("Referential transparency IO") {
    val number = IO {
      println(s"Input a number: ")
      StdIn.readInt()
    }

    val sumProgram: IO[Boolean] =
      for {
        x <- number
        y <- number
        w <- IO {
          println(s"Input a number: ")
          StdIn.readInt()
        }
        z <- IO {
          println(s"Input a number: ")
          StdIn.readInt()
        }
      } yield x + y == w + z

    assert(sumProgram.unsafeRunSync())
  }


}
