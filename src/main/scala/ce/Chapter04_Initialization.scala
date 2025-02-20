package ce.Chapter04_Initialization

import cats.effect.{IO, Ref, Resource}
import ce.extensions.*
import pureconfig.*
import pureconfig.generic.derivation.*

trait Bread:
  val eat: IO[Unit] = IO.println("Bread: Eating")

class BreadStoreBought extends Bread

val purchaseBread =
  IO.println("Buying bread").as(BreadStoreBought())

val storeBoughtBread =
  purchaseBread

val eatBread: IO[Bread] => IO[Unit] =
  bread => bread.flatMap(_.eat)

object App0 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    eatBread:
      storeBoughtBread

class Dough:
  val letRise: IO[Unit] = IO.println("Dough: rising")

object Dough:
  val fresh: IO[Dough] = IO.println("Dough: Mixed").as(Dough())

trait HeatSource

class Oven extends HeatSource

object Oven:
  val heated: IO[Oven] = IO.println("Oven: Heated").as(Oven())

class BreadHomeMade(heat: HeatSource, dough: Dough) extends Bread

object BreadHomeMade:
  def make(heat: IO[HeatSource], dough: IO[Dough]): IO[BreadHomeMade] =
    for
      d <- dough
      h <- heat
      _ <- IO.println("BreadHomeMade: Baked")
    yield BreadHomeMade(h, d)

val homeMadeBread: IO[BreadHomeMade] =
  for
    heat <- Oven.heated
    dough <- Dough.fresh
    _ <- IO.println("BreadHomeMade: Baked")
  yield BreadHomeMade(heat, dough)

object App1 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    eatBread(homeMadeBread)

object Bread:
  val storeBought: IO[BreadStoreBought] = storeBoughtBread
  val homeMade: IO[BreadHomeMade] = homeMadeBread

trait Toast:
  val bread: Bread
  val heat: HeatSource
  val eat: IO[Unit] = IO.println("Toast: Eating")

final case class ToastFromHeatSource(bread: Bread, heat: HeatSource) extends Toast

object ToastFromHeatSource:
  val toasted: (IO[HeatSource], IO[Bread]) => IO[ToastFromHeatSource] =
    (heat, bread) =>
      for
        bread <- bread
        heat <- heat
        _ <- IO.println("Toast: Made")
      yield ToastFromHeatSource(bread, heat)

class Toaster extends HeatSource

object Toaster:
  val ready: IO[Toaster] = IO.println("Toaster: Ready").as(Toaster())

final case class ToastFromToaster(bread: Bread, heat: HeatSource) extends Toast

object ToastFromToaster:
  val toasted: (IO[HeatSource], IO[Bread]) => IO[ToastFromToaster] =
    (heat, bread) =>
      for
        h <- heat
        b <- bread
        _ <- IO.println("Toast: Made")
      yield ToastFromToaster(b, h)

object App2 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    ToastFromToaster.toasted(Toaster.ready, Bread.homeMade).flatMap(_.eat)

object OvenSafe:
  val heated: IO[Oven] =
    IO.println("Oven: Heated")
      .as(Oven())
      .guarantee(IO.println("Oven: Turning off"))

// Skip App3 zio, it's the same as App2 but with ZLayer.Debug.Tree
object App3 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    eatBread(BreadHomeMade.make(OvenSafe.heated, Dough.fresh))
// Oven: Heated
// Dough: Mixed
// BreadHomeMade: Baked
// Bread: Eating
// Oven: Turning off


class BreadFromFriend extends Bread

object Friend:
  def forcedFailure(invocations: Int): IO[BreadFromFriend] =
    IO.println(s"Attempt $invocations: Failure(Friend Unreachable)") *>
      IO.raiseError(new Exception("Friend Unreachable"))
        .as(BreadFromFriend())

  def requestBread(retry: Ref[IO, Int]): IO[BreadFromFriend] =
    val worksOnAttempt = 4
    for
      curInvocations <- retry.updateAndGet(_ + 1)
      bread <- if curInvocations < worksOnAttempt then forcedFailure(curInvocations)
      else IO.println(s"Attempt $curInvocations: Succeeded").as(BreadFromFriend())
    yield bread
  end requestBread
end Friend

object App4 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    for
      retry <- Ref.of[IO, Int](0)
      bread = Friend.requestBread(retry)
      _ <- eatBread(bread)
    yield ()

object App5 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    for
      retry <- Ref.of[IO, Int](0)
      bread = Friend.requestBread(retry).orElse(Bread.storeBought)
      _ <- eatBread(bread)
    yield ()

object App6 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    for
      retry <- Ref.of[IO, Int](0)
      bread = Friend.requestBread(retry)
      _ <- eatBread(bread).retryN(1)
    yield ()

final case class RetryConfig(times: Int)derives ConfigReader

val configurableBread: (Ref[IO, Int], RetryConfig) => IO[Bread] =
  (retry, config) => Friend.requestBread(retry).retryN(config.times)

object App7 extends ce.helpers.IOAppDebug:
  val retryTwice: RetryConfig = RetryConfig(2)

  def run: IO[Any] =
    for
      retry <- Ref.of[IO, Int](0)
      bread = configurableBread(retry, retryTwice)
      _ <- eatBread(bread)
    yield ()

val configSource =
  ConfigSource
    .string("{ times: 3 }")
    .load[RetryConfig]

val configuration =
  configSource match
    case Right(config) => IO(config)
    case Left(failure) => IO.raiseError(new Exception(failure.toString))

object App8 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    for
      retry <- Ref.of[IO, Int](0)
      config <- configuration
      bread = configurableBread(retry, config)
      _ <- eatBread(bread)
    yield ()

