package ce.chapters.ch03

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Ref}
import ce.extensions.*
import ce.helpers.IOAppDebug

import scala.concurrent.duration.DurationInt

var currentScenario: Scenario = Scenario.NeverWorks

enum Scenario:
  case Successful
  case NeverWorks
  case Slow
  case WorksOnTryInner(ref: Ref[IO, Int])

  def simulate[A](effect: IO[A]): IO[A] =
    for
      _ <- IO {
        currentScenario = this
      }
      result <- effect
    yield result
//    this match
//      case Successful =>
//        effect
//      case NeverWorks =>
//        IO.raiseError(new Exception("Never works"))
//      case Slow =>
//        IO.sleep(100.millis) *> effect
//      case WorksOnTryInner(ref) =>
//        ref.get.flatMap {
//          case 2 => effect
//          case _ => ref.update(_ + 1) *> IO.raiseError(new Exception("Try again"))
//        }

object Scenario:
  //  def WorksOnThirdTry: IO[WorksOnTryInner] =
  //    Ref.of[IO, Int](2).map(WorksOnTryInner(_))

  def WorksOnThirdTry: Scenario =
    Ref.of[IO, Int](2).map(WorksOnTryInner(_)).unsafeRunSync()

def saveUser(username: String): IO[String] =
  val succeed = IO.pure(s"User $username saved")

  val fail: IO[String] =
    IO.raiseError(new Exception("**Database crashed!!**"))
      .handleErrorWith(error =>
        IO.println(s"Log: ${error.getMessage}") *>
          IO.raiseError(error)
      )

  for
    scenario <- IO(currentScenario)
    _ <- IO.println("Attempting to save user")
    result <- scenario match
      case Scenario.NeverWorks => fail
      case Scenario.Successful => succeed
      case Scenario.Slow => IO.sleep(10.seconds) *> succeed
      case Scenario.WorksOnTryInner(ref) =>
        ref.get.flatMap {
          case 0 => succeed
          case _ => ref.update(_ - 1) *> fail
        }
  yield result
end saveUser

def sendToManualQueue(username: String): IO[Either[Throwable, String]] =
  IO(s"Sent $username to manual queue").attempt

def logUserSignup(username: String): IO[Unit] =
  IO.println(s"Log: Signup initiated for $username")

val userName = "Morty"

val effect0: IO[String] = saveUser(userName)

object MyApp extends IOApp.Simple:
  def run: IO[Unit] =
    Scenario.Successful.simulate(effect0).void

object App0 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Successful.simulate(effect0)

object App1 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.WorksOnThirdTry.simulate(effect0)

val effect1 = effect0.retryN(2)

object App2 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.WorksOnThirdTry.simulate(effect1)

object App3 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.NeverWorks.simulate:
      effect1

val effect2 =
  effect1.handleErrorWith { _ =>
    IO.raiseError(new Exception("FAILURE: User not saved"))
  }

object App4 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.NeverWorks.simulate:
      effect2

val effect3 =
  effect2
    .timeout(5.seconds)
    .handleErrorWith(_ => IO.raiseError(new Exception("** Save timed out **")))

object App5 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Slow.simulate(effect3)

val effect4 =
  effect3.orElse(sendToManualQueue(userName))

object App6 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.NeverWorks.simulate(effect4)

val effect5 =
  effect4.guarantee(logUserSignup(userName))

object App7 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Successful.simulate(effect5)

val effect6 = effect5.timed

object App8 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Successful.simulate(effect6)

val effect7 = effect6.when(userName != "Morty")

object App9 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Successful.simulate(effect7)

object App10 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Successful.simulate:
      IO.println("Before save")
      effect1

val effect8 =
  IO.println("Before save") *> effect1

object App11 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Successful.simulate:
      effect8

object App12 extends IOAppDebug:
  def run: IO[Any] =
    Scenario.Successful.simulate:
      IO.println("**Before**") *>
        effect8.debug().repeatN(1) *>
        IO.println("**After**")
