package co.com.catseffects.datatypes

import cats.effect.IO
import org.scalatest.FunSuite

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

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
    println("This message must be printed before hey!")   // this is a side-effect, but it is there just for testing

    val program: IO[Unit] =
      for {
        _ <- ioa
        _ <- ioa
      } yield ()

    program.unsafeRunSync()
    assert(true)
  }

  test("IO Referential transparency") {
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

  test("IO stack safety support - fibonacci serie") {
    def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] = {
      IO(a + b).flatMap { b2 =>
        if (n > 0)
          fib(n - 1, b, b2)
        else
          IO.pure(a)
      }
    }

    assert(fib(5).unsafeRunSync() == 5)
  }

  test("Plus two values, one already into IO monad and other using pure function") {
    val x = 2
    val y = IO.pure(x)    //eager evaluated
    val program = for {
      a <- y
      b <- IO(3)
    } yield a + b

    assert(program.unsafeRunSync() == 5)
    assert(IO.unit == IO.pure(()))
  }

  test("Async function - convertion a future manually") {
    def convert[A](fa: => Future[A])(implicit ec: ExecutionContext): IO[A] =
      IO.async { cb =>
        fa.onComplete {
          case Success(a) => cb(Right(a))
          case Failure(e) => cb(Left(e))
        }
      }
    import scala.concurrent.ExecutionContext.Implicits.global

    assert(convert(Future.successful(2)).unsafeRunSync() == 2)
    assert(Try(convert(Future.failed(new RuntimeException("Runtime e"))).unsafeRunSync()).isFailure)
    // Even if we wrap the onComplete method in IO Monad, we can have a runtime exception.
    // In this case IO Monad help us to manage side effects that onComplete method carries.
    // For that, we wrap the entire result in a Try.
  }
}
