package ce.chapters.ch04.jam

import cats.Id
import cats.effect.{IO, Ref, Resource}
import ce.*
import ce.extensions.*
import jam.monad.Reval
import pureconfig.*
import pureconfig.generic.derivation.*

trait Bread:
  def eat: IO[Unit] = IO.println("Bread: Eating")

class BreadStoreBought extends Bread:
  println("Buying bread")

def storeBoughtBread = jam.brew[BreadStoreBought]

def eatBread: Bread => IO[Unit] = _.eat

object App0 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    eatBread:
      storeBoughtBread

class Dough:
  println("Dough: Mixed")

trait HeatSource

class Oven extends HeatSource:
  println("Oven: Heated")

class BreadHomeMade(heat: HeatSource, dough: Dough) extends Bread:
  println("BreadHomeMade: Baked")

def homeMadeBread =
  BreadHomeMade(jam.brew[Oven], jam.brew[Dough])

object App1 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    eatBread:
      homeMadeBread

//object Bread:
//  val storeBought: Resource[IO, BreadStoreBought] = storeBoughtBread
//  val homeMade: (Oven, Dough) => Resource[IO, BreadHomeMade] =
//    (oven, dough) => BreadHomeMade.make(oven, dough)
//
//trait Toast:
//  val bread: Bread
//  val heat: HeatSource
//  val eat: IO[Unit] = IO.println("Toast: Eating")
//
//final case class ToastFromHeatSource(bread: Bread, heat: HeatSource) extends Toast
//
//object ToastFromHeatSource:
//  val toasted: (IO[HeatSource], IO[Bread]) => IO[ToastFromHeatSource] =
//    (heat, bread) =>
//      for
//        bread <- bread
//        heat <- heat
//        _ <- IO.println("Toast: Made")
//      yield ToastFromHeatSource(bread, heat)
//
//class Toaster extends HeatSource
//
//object Toaster:
//  val ready: Resource[IO, Toaster] = Resource.eval(IO.println("Toaster: Ready").as(Toaster()))
//
//final case class ToastFromToaster(bread: Bread, heat: HeatSource) extends Toast
//
//object ToastFromToaster:
//  val toasted: (Bread, HeatSource) => Resource[IO, ToastFromToaster] =
//    (bread, heat) =>
//      for
//        _ <- IO.println("Toast: Made").toResource
//      yield ToastFromToaster(bread, heat)
//
//object App2 extends ce.helpers.IOAppDebug:
//  def run: IO[Any] =
//    val homeMadeToast = for
//      toaster <- Toaster.ready
//      dough <- Dough.fresh
//      heat <- Oven.heated
//      bread <- Bread.homeMade(heat, dough)
//      toast <- ToastFromToaster.toasted(bread, toaster)
//    yield toast
//
//    homeMadeToast.use(_.eat)
//
//object OvenSafe:
//  val heated: Resource[IO, Oven] =
//    Resource.make(IO.println("Oven: Heated").as(Oven()))(_ => IO.println("Oven: Turning off"))
//
//// Skip App3 zio, it's the same as App2 but with ZLayer.Debug.Tree
//object App3 extends ce.helpers.IOAppDebug:
//  def run: IO[Any] =
//    val homeMadeBread = for
//      heat <- OvenSafe.heated
//      dough <- Dough.fresh
//      bread <- Bread.homeMade(heat, dough)
//    yield bread
//
//    homeMadeBread.use(eatBread)
//
//class BreadFromFriend extends Bread
//
//object Friend:
//  def forcedFailure(invocations: Int): Resource[IO, BreadFromFriend] =
//    Resource.eval(
//      IO.println(s"Attempt $invocations: Failure(Friend Unreachable)") *>
//        IO.raiseError(new Exception("Friend Unreachable")).as(BreadFromFriend())
//    )
//
//  def requestBread(retry: Ref[IO, Int]): Resource[IO, BreadFromFriend] =
//    val worksOnAttempt = 4
//    for
//      curInvocations <- retry.updateAndGet(_ + 1).toResource
//      bread <- if curInvocations < worksOnAttempt then forcedFailure(curInvocations)
//      else IO.println(s"Attempt $curInvocations: Succeeded").as(BreadFromFriend()).toResource
//    yield bread
//  end requestBread
//end Friend
//
//object RetryCounter:
//  def apply(): Resource[IO, Ref[IO, Int]] =
//    Resource.eval(Ref.of[IO, Int](0))
//
//object App4 extends ce.helpers.IOAppDebug:
//  def run: IO[Any] =
//    val breadFromFriend =
//      for
//        retry <- RetryCounter()
//        bread <- Friend.requestBread(retry)
//      yield bread
//
//    breadFromFriend.use(eatBread)
//
//object App5 extends ce.helpers.IOAppDebug:
//  def run: IO[Any] =
//    val breadFromFriend =
//      for
//        retry <- RetryCounter()
//        bread <- Friend.requestBread(retry).handleErrorWith[Bread, Throwable](_ => Bread.storeBought)
//      yield bread
//
//    breadFromFriend.use(eatBread)
//
//object App6 extends ce.helpers.IOAppDebug:
//  def run: IO[Any] =
//    val breadFromFriend = for
//      retry <- RetryCounter()
//      bread <- Friend.requestBread(retry).retryN(1)
//    yield bread
//
//    breadFromFriend.use(eatBread)
//
//final case class RetryConfig(times: Int)derives ConfigReader
//
//val configurableBread: (Ref[IO, Int], RetryConfig) => Resource[IO, Bread] =
//  (retry, config) => Friend.requestBread(retry).retryN(config.times)
//
//object App7 extends ce.helpers.IOAppDebug:
//  val retryTwice: RetryConfig = RetryConfig(2)
//
//  def run: IO[Any] =
//    val configBread = for
//      retry <- RetryCounter()
//      bread <- configurableBread(retry, retryTwice)
//    yield bread
//
//    configBread.use(eatBread)
//
//val configSource =
//  Resource.eval(
//    IO(
//      ConfigSource
//        .string("{ times: 3 }")
//        .load[RetryConfig]
//    )
//  )
//
//val configuration =
//  configSource.flatMap {
//    case Right(config) => Resource.pure(config)
//    case Left(failure) => Resource.eval(IO.raiseError(new Exception(failure.toString)))
//  }
//
//object App8 extends ce.helpers.IOAppDebug:
//  def run: IO[Any] =
//    val configBread = for
//      retry <- RetryCounter()
//      config <- configuration
//      bread <- configurableBread(retry, config)
//    yield bread
//
//    configBread.use(eatBread)
//
