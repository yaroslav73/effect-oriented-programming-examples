package ce.chapters.ch04.jam

import cats.effect.{IO, Ref, Resource}
import ce.*
import ce.chapters.ch04.res.*
import ce.extensions.*
import pureconfig.*
import pureconfig.generic.derivation.*

trait Bread:
  val eat: IO[Unit] = IO.println("Bread: Eating")

class BreadStoreBought extends Bread

val purchaseBread =
  IO.println("Buying bread").as(BreadStoreBought())

val storeBoughtBread =
  Resource.eval(purchaseBread)

val eatBread: Bread => IO[Unit] =
  bread => bread.eat

object App0 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    storeBoughtBread.use(eatBread)

class Dough:
  val letRise: IO[Unit] = IO.println("Dough: rising")

object Dough:
  val fresh: Resource[IO, Dough] =
    Resource.eval(IO.println("Dough: Mixed").as(Dough()))

trait HeatSource

class Oven extends HeatSource

object Oven:
  val heated: Resource[IO, Oven] =
    Resource.eval(IO.println("Oven: Heated").as(Oven()))

class BreadHomeMade(heat: HeatSource, dough: Dough) extends Bread

object BreadHomeMade:
  def make(heat: Oven, dough: Dough): Resource[IO, BreadHomeMade] =
    for
      _ <- IO.println("BreadHomeMade: Baked").toResource
    yield BreadHomeMade(heat, dough)

object App1 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    val breadHomeMade = for
      oven <- Oven.heated
      dough <- Dough.fresh
      bread <- BreadHomeMade.make(oven, dough)
    yield bread

    breadHomeMade.use(eatBread)

object Bread:
  val storeBought: Resource[IO, BreadStoreBought] = storeBoughtBread
  val homeMade: (Oven, Dough) => Resource[IO, BreadHomeMade] =
    (oven, dough) => BreadHomeMade.make(oven, dough)

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
  val ready: Resource[IO, Toaster] = Resource.eval(IO.println("Toaster: Ready").as(Toaster()))

final case class ToastFromToaster(bread: Bread, heat: HeatSource) extends Toast

object ToastFromToaster:
  val toasted: (Bread, HeatSource) => Resource[IO, ToastFromToaster] =
    (bread, heat) =>
      for
        _ <- IO.println("Toast: Made").toResource
      yield ToastFromToaster(bread, heat)

object App2 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    val homeMadeToast = for
      toaster <- Toaster.ready
      dough <- Dough.fresh
      heat <- Oven.heated
      bread <- Bread.homeMade(heat, dough)
      toast <- ToastFromToaster.toasted(bread, toaster)
    yield toast

    homeMadeToast.use(_.eat)

object OvenSafe:
  val heated: Resource[IO, Oven] =
    Resource.make(IO.println("Oven: Heated").as(Oven()))(_ => IO.println("Oven: Turning off"))

// Skip App3 zio, it's the same as App2 but with ZLayer.Debug.Tree
object App3 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    val homeMadeBread = for
      heat <- OvenSafe.heated
      dough <- Dough.fresh
      bread <- Bread.homeMade(heat, dough)
    yield bread

    homeMadeBread.use(eatBread)

class BreadFromFriend extends Bread

object Friend:
  def forcedFailure(invocations: Int): Resource[IO, BreadFromFriend] =
    Resource.eval(
      IO.println(s"Attempt $invocations: Failure(Friend Unreachable)") *>
        IO.raiseError(new Exception("Friend Unreachable")).as(BreadFromFriend())
    )

  def requestBread(retry: Ref[IO, Int]): Resource[IO, BreadFromFriend] =
    val worksOnAttempt = 4
    for
      curInvocations <- retry.updateAndGet(_ + 1).toResource
      bread <- if curInvocations < worksOnAttempt then forcedFailure(curInvocations)
      else IO.println(s"Attempt $curInvocations: Succeeded").as(BreadFromFriend()).toResource
    yield bread
  end requestBread
end Friend

object RetryCounter:
  def apply(): Resource[IO, Ref[IO, Int]] =
    Resource.eval(Ref.of[IO, Int](0))

object App4 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    val breadFromFriend =
      for
        retry <- RetryCounter()
        bread <- Friend.requestBread(retry)
      yield bread

    breadFromFriend.use(eatBread)

object App5 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    val breadFromFriend =
      for
        retry <- RetryCounter()
        bread <- Friend.requestBread(retry).handleErrorWith[Bread, Throwable](_ => Bread.storeBought)
      yield bread

    breadFromFriend.use(eatBread)

object App6 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    val breadFromFriend = for
      retry <- RetryCounter()
      bread <- Friend.requestBread(retry).retryN(1)
    yield bread

    breadFromFriend.use(eatBread)

final case class RetryConfig(times: Int)derives ConfigReader

val configurableBread: (Ref[IO, Int], RetryConfig) => Resource[IO, Bread] =
  (retry, config) => Friend.requestBread(retry).retryN(config.times)

object App7 extends ce.helpers.IOAppDebug:
  val retryTwice: RetryConfig = RetryConfig(2)

  def run: IO[Any] =
    val configBread = for
      retry <- RetryCounter()
      bread <- configurableBread(retry, retryTwice)
    yield bread

    configBread.use(eatBread)

val configSource =
  Resource.eval(
    IO(
      ConfigSource
        .string("{ times: 3 }")
        .load[RetryConfig]
    )
  )

val configuration =
  configSource.flatMap {
    case Right(config) => Resource.pure(config)
    case Left(failure) => Resource.eval(IO.raiseError(new Exception(failure.toString)))
  }

object App8 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    val configBread = for
      retry <- RetryCounter()
      config <- configuration
      bread <- configurableBread(retry, config)
    yield bread

    configBread.use(eatBread)

