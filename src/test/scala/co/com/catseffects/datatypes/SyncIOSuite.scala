package co.com.catseffects.datatypes

import cats.Eval
import cats.effect.SyncIO
import org.scalatest.FunSuite
import cats.effect.IO

import scala.io.StdIn

class SyncIOSuite extends FunSuite {

  // SyncIO is used for sync computations

  test("SyncIO constructor similar to IO") {

    def putStrLn(str: String): SyncIO[Unit] = SyncIO(println(str))

    SyncIO.pure("Hello SyncIO Cats!").flatMap(putStrLn).unsafeRunSync()
  }

  test("SyncIO has interoperability with Eval (cats)") {
    val eval = Eval.now("Hello eval now!")

    val result = SyncIO.eval(eval).unsafeRunSync()
    assert(result == "Hello eval now!")
  }

  test("SyncIO 'to' method allows us to lift SyncIO to another monad") {

    val ioa: SyncIO[Unit] = SyncIO(println("Hello SyncIO world!"))

    val iob: IO[Unit] = ioa.to[IO]

    iob.unsafeRunAsync(_ => ())
  }

  test("SyncIO referential transparency") {
    val number = SyncIO {
      println(s"Input SyncIO number: ")
      StdIn.readInt()
    } // this is not granular

    val sumProgram: SyncIO[Boolean] =
      for {
        x <- number
        y <- number
        w <- SyncIO {
          println(s"Input SyncIO number: ")
          StdIn.readInt()
        }
        z <- SyncIO {
          println(s"Input SyncIO number: ")
          StdIn.readInt()
        }
      } yield x + y == w + z

    assert(sumProgram.unsafeRunSync())
  }
}
