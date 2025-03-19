package ce.chapters.ch04.kleisli

// Kleisli[F[_], A, B] is just a wrapper around the function A => F[B]

val twice: Int => Int =
  x => x * 2

val countCats: Int => String =
  x => if (x == 1) "1 cat" else s"$x cats"

// We can compose two functions twice and countCats
val twiceAsManyCats: Int => String =
  twice andThen countCats // equivalent to: countCats compose twice

object App01 extends App:
  println(twiceAsManyCats(2)) // 4 cats

val parse: String => Option[Int] =
  s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

val reciprocal: Int => Option[Double] =
  i => if (i != 0) Some(1.0 / i) else None

// We can't compose parse and reciprocal because the types don't match
val parseAndReciprocal: String => Option[Double] =
  // parse andThen reciprocal
  // we can do it in this way
  parse andThen (_.flatMap(reciprocal))

object App02 extends App:
  println(parseAndReciprocal("4")) // Some(0.25)
  println(parseAndReciprocal("0")) // None
  println(parseAndReciprocal("a")) // None

// As it stands we cannot use Function1.compose (or Function1.andThen) to compose these two functions.
// The output type of parse is Option[Int] whereas the input type of reciprocal is Int.
//
// This is where Kleisli comes into play.

import cats.data.Kleisli

val parseK: Kleisli[Option, String, Int] =
  Kleisli(s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None)

val reciprocalK: Kleisli[Option, Int, Double] =
  Kleisli(i => if (i != 0) Some(1.0 / i) else None)

val parseAndReciprocalK: Kleisli[Option, String, Double] =
  parseK andThen reciprocalK

object App03 extends App:
  println(parseAndReciprocalK("4")) // Some(0.25)
  println(parseAndReciprocalK("0")) // None