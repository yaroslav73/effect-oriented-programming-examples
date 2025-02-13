package ce.helpers

import cats.effect.{IO, IOApp}

trait IOAppDebug:
  self =>

  def run: IO[Any]

  def main(args: Array[String]): Unit =
    val app = new IOApp.Simple:
      def run: IO[Unit] =
        self.run.flatMap {
          case result if !result.isInstanceOf[Unit] =>
            IO.println(s"Result: $result")
          case _ => IO.unit
        }

    app.main(args)
