package ce.Chapter04_Initialization

import cats.effect.{IO, Ref, Resource}

trait Bread:
  val eat: IO[Unit] = IO.println("Bread: Eating")

class BreadStoreBought extends Bread

val purchaseBread =
  IO.println("Buying bread").as(BreadStoreBought())

val storeBoughtBread =
  purchaseBread

val eatBread: IO[Bread] => IO[Unit] =
  bread => bread.flatMap(_.eat)

// or with def
//def eatBread(bread: IO[Bread]): IO[Unit] =
//  bread.flatMap(_.eat)

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
        bread <- bread //Bread.storeBought // TODO: pass bread home or bought
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
// TODO: try to use Resource

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

  def requestBread: IO[BreadFromFriend] =
    val worksOnAttempt = 4
    for
      invocations <- Ref.of[IO, Int](0)
      curInvocations <- invocations.updateAndGet(_ + 1)
      bread <- if curInvocations < worksOnAttempt then forcedFailure(curInvocations)
      else IO.println(s"Attempt $curInvocations: Succeeded").as(BreadFromFriend())
    yield bread
  end requestBread
end Friend

object App4 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    eatBread(Friend.requestBread)

object App5 extends ce.helpers.IOAppDebug:
  def run: IO[Any] =
    eatBread(Friend.requestBread.orElse(Bread.storeBought))
