package co.com.catseffects.datatypes

import cats.data.NonEmptyList
import cats.effect.{ExitCase, IO}
import cats.implicits._
import org.scalatest.FunSuite

import java.io.{BufferedReader, FileReader}
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
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
    } // this is not granular

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

  test("IO brack method to catch exceptions") {

    val x = IO(new BufferedReader(new FileReader("hola.txt"))).bracket { in =>
      IO {
        val content = new StringBuilder()
        var line: String = null
        do {
          // Synchronized access to isCanceled and to the reader
          line = in.readLine()
          if (line != null) content.append(line)
        } while (line != null)
        content.toString()
      }
    } { in =>
      IO(in.close()).void
    }

    println(x.unsafeRunSync())
  }

  test("IO has a fromEither method to convert Either to IO") {

    val e = Right[Throwable, Int](2)
    val ex = Left[Throwable, Int](new IllegalArgumentException("Raising an expection"))

    assert(IO.fromEither(e).unsafeRunSync() == 2)
    Try(IO.fromEither(ex).unsafeRunSync()) match {
      case Failure(_) => succeed
      case _ => fail()
    }
  }

  test("IO attempt method to catch exceptions") {

    val boom: IO[Unit] = IO.raiseError(new Exception("boom"))
    boom.attempt.map({
      case Left(_) => succeed
      case Right(_) => fail()
    })
  }

  test("IO allows us to change execution context pool") {
    val cachedThreadPool = Executors.newCachedThreadPool()
    val BlockingFileIO   = ExecutionContext.fromExecutor(cachedThreadPool)
    implicit val Main = ExecutionContext.global

    val ioa: IO[Unit] =
      for {
        _     <- IO(println("Enter your name: "))
        _     <- IO.shift(BlockingFileIO)
        name  <- IO(scala.io.StdIn.readLine())
        _     <- IO.shift(Main)
        _     <- IO(println(s"Welcome $name!"))
        _     <- IO(cachedThreadPool.shutdown())
      } yield ()

    ioa.unsafeRunSync()
  }

  test("IO has parMapN method which allows us to execute multiple tasks parallel") {

    val ioA = IO(println("Running ioA"))
    val ioB = IO(println("Running ioB"))
    val ioC = IO(println("Running ioC"))

    val program = (ioA, ioB, ioC).parMapN { (x, y, z) => () }

    program.unsafeRunSync()
    ()
  }

  test("IO if any parallel tasks completes with a failure, the other ones get cancelled") {
    val a = IO.raiseError[Unit](new Exception("boom")) <* IO(println("Running ioA"))

    val b = (IO.sleep(10.second) *> IO(println("Running ioB")))
      .guaranteeCase {
        case ExitCase.Canceled => IO(println("ioB was canceled!"))
        case _ => IO.unit
      }

    val parFailure = (a, b).parMapN { (_, _) => () }

    parFailure.attempt.unsafeRunSync()
    ()
  }

  test("IO parSequence method allows us to convert List[IO] to IO[List] and execute those tasks parallel") {
    val anIO = IO(1)

    val aLotOfIOs =
      List(anIO, anIO)

    val ioOfList: IO[List[Int]] = aLotOfIOs.parSequence

    ioOfList.unsafeRunSync()
  }

  test("IO parTraverse allows us to map every list member into IO and sequence the result (execution in parallel)") {
    val results: IO[NonEmptyList[Int]] = NonEmptyList.of(1, 2, 3).parTraverse { i =>
      IO(i)
    }

    results.unsafeRunSync()
  }

}
