package co.com.catseffects.datatypes

import cats.effect.IO
import org.scalatest.FunSuite
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.util.concurrent.ScheduledExecutorService
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

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

  implicit val CS = IO.contextShift(global)
  implicit val timer = IO.timer(global)

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

  test("IO Plus two values, one already into IO monad and other using pure function") {
    val x = 2
    val y = IO.pure(x)    //eager evaluated
    val program = for {
      a <- y
      b <- IO(3)
    } yield a + b

    assert(program.unsafeRunSync() == 5)
    assert(IO.unit == IO.pure(()))
  }

  test("IO Async function - convertion a future manually") {
    def convert[A](fa: => Future[A]): IO[A] =
      IO.async { cb =>
        fa.onComplete {
          case Success(a) => cb(Right(a))
          case Failure(e) => cb(Left(e))
        }
      }

    assert(convert(Future.successful(2)).unsafeRunSync() == 2)
    assert(Try(convert(Future.failed(new RuntimeException("Runtime e"))).unsafeRunSync()).isFailure)
    // Even if we wrap the onComplete method in IO Monad, we can have a runtime exception.
    // In this case IO Monad help us to manage side effects that onComplete method carries.
    // For that, we wrap the entire result in a Try.
  }

  test("IO can run process sequencially & concurrency") {
    val jobOne = IO(100)
    val jobTwo = IO("Hello world")

    // Sequencially
    val seqOne: IO[(Int, String)] = jobOne.flatMap(i => jobTwo.map(s => (i, s)))
    val seqTwo: IO[(Int, String)] = (jobOne, jobTwo).tupled

    // Concurrently (manually)
    val conOne: IO[(Int, String)] = for {
      j1Fiber <- jobOne.start
      j2Fiber <- jobTwo.start
      i <- j1Fiber.join
      s <- j2Fiber.join
    } yield (i, s)

    // A fiber can be described as a lightweighted thread
    // Concurrently (higher level)
    val conTwo: IO[(Int, String)] = (jobOne, jobTwo).parTupled
  }


  test("IO can cancel a process with both methods runCancelable & unsafeRunCancelable") {

    implicit val timer = IO.timer(ExecutionContext.global)

    val io: IO[Unit] = IO.sleep(10.seconds) *> IO(println("Hello!"))
    val cancelUnSafe: IO[Unit] = io.unsafeRunCancelable(r => println(s"Done unsafely: $r"))

    val cancelSafe = io.runCancelable(r => IO(println(s"Done safely: $r")))
    cancelSafe.unsafeRunSync()

  }

  test("IO has a mark to put an IO process uncancelable") {

    implicit val timer = IO.timer(ExecutionContext.global)
    val io: IO[Unit] = IO.sleep(10.seconds) *> IO(println("Hello!"))
    io.uncancelable
  }

  

}
