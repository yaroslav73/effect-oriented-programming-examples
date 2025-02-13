package ce.extensions

import cats.effect.IO

import scala.language.postfixOps

extension [A](io: IO[A])
  def retryN(n: => Int): IO[A] =
    io.handleErrorWith { error =>
      if (n > 0) io.retryN(n - 1)
      else IO.raiseError(error)
    }

  def repeatN(n: => Int): IO[A] =
    IO.defer {
      def loop(n: Int): IO[A] =
        io.flatMap(a => if (n <= 0) IO(a) else loop(n - 1))

      loop(n)
    }

  def when(p: => Boolean): IO[Option[A]] =
    if (p) io.map(Some(_)) else IO.none