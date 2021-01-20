package co.com.catseffects.datatypes

import cats.effect.{Fiber, IO}
import org.scalatest.FunSuite

class FiberSuite extends FunSuite {

  // A fiber represents the result of an Async data type being started concurrently and
  // that can be either joined or canceled.

  test("Fiber say hello! in another execution thread (lighter than the main one)") {
    import scala.concurrent.ExecutionContext.Implicits.global
    // Needed for `start`
    implicit val ctx = IO.contextShift(global)

    val io = IO(println("Hello!"))
    val fiber: IO[Fiber[IO, Unit]] = io.start
  }

  test("Fiber composition and forking light threads to execute multiple tasks") {
    import cats.effect.{ContextShift, IO}

    import scala.concurrent.ExecutionContext

    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

    val launchMissiles: IO[Unit] = IO.raiseError(new Exception("boom!"))
    val runToBunker = IO(println("To the bunker!!!"))

    val x = for {
      fiber <- launchMissiles.start
      _ <- runToBunker.handleErrorWith { error =>
        fiber.cancel *> IO.raiseError(error)
      }
      aftermath <- fiber.join
    } yield aftermath

    x.unsafeRunAsync(r => assert(r.isLeft))
  }


}
