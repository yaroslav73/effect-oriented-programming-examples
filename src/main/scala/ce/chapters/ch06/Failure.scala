package ce.chapters.ch06

import cats.effect.*
import ce.extensions.*
import ce.helpers.IOAppDebug

case object FailObject extends Throwable

class FailException extends Exception:
  override def toString: String =
    "FailException"

def failureTypes(n: Int): IO[Unit] =
  n match
    case 0 =>
      IO.raiseError(new Throwable("String fail")) // CE doesn't have IO.fail
    case 1 =>
      IO.raiseError(FailObject)
    case _ =>
      IO.raiseError(FailException())

object App0 extends IOAppDebug:
  def run =
    for
      _ <- IO.println("Begin failing")
      _ <- failureTypes(0).attempt.debug() // CE doesn't have flip
      _ <- failureTypes(1).attempt.debug()
      _ <- failureTypes(2).attempt.debug()
      _ <- IO.println("Done failing")
    yield ()
  // Begin failing
  // DEBUG: Succeeded: Left(java.lang.Throwable: String fail)
  // DEBUG: Succeeded: Left(ce.chapters.ch06.FailObject$)
  // DEBUG: Succeeded: Left(FailException)
  // Done failing

def limitFail(n: Int, limit: Int): IO[String] =
  IO.println(s"Executing step $n") *> {
    if n < limit then IO(s"Completed step $n")
    else IO.raiseError(Throwable(s"Failed at step $n"))
  }

def shortCircuit(limit: Int): IO[String] =
  limitFail(0, limit) *>
    limitFail(1, limit) *>
    limitFail(2, limit)

object App1 extends IOAppDebug:
  def run =
    shortCircuit(0).flip
  // Executing step 0
  // Result: Failed at step 0

object App2 extends IOAppDebug:
  def run =
    shortCircuit(1).flip
  // Executing step 0
  // Executing step 1
  // Result: Failed at step 1

object App3 extends IOAppDebug:
  def run =
    shortCircuit(2).flip
  // Executing step 0
  // Executing step 1
  // Executing step 2
  // Result: Failed at step 2

object App4 extends IOAppDebug:
  def run =
    shortCircuit(3)
  // Executing step 0
  // Executing step 1
  // Executing step 2
  // Result: Completed step 2

import Scenario.*

var currentScenario: Scenario = GPSFailure

enum Scenario:
  case Successful,
    TooCold,
    NetworkFailure,
    GPSFailure

  def simulate[A](effect: Scenario => IO[A]): IO[A] =
    for
      _      <- IO { currentScenario = this }
      result <- effect(currentScenario)
    yield result

case object GpsException     extends Exception("GPS Failure")
case object NetworkException extends Exception("Network Failure")

final case class Temperature(degrees: Int)

val getTemperature: Scenario => IO[Temperature] = scenario =>
  IO.println("Getting Temperature") *> {
    scenario match
      case Scenario.GPSFailure     => IO.raiseError(GpsException)
      case Scenario.NetworkFailure => IO.raiseError(NetworkException)
      case Scenario.TooCold        => IO(Temperature(-20))
      case Scenario.Successful     => IO(Temperature(35))
    end match
  }

object App5 extends IOAppDebug:
  def run =
    Successful.simulate:
      getTemperature
  // Getting Temperature
  // Result: Temperature(35)

object App6 extends IOAppDebug:
  def run =
    NetworkFailure.simulate:
      getTemperature
  // Getting Temperature
  // Defect: NetworkException: Network Failure

object App7 extends IOAppDebug:
  def run =
    NetworkFailure
      .simulate:
        getTemperature
      .debug("Succeeded")
  // Getting Temperature
  // Defect: NetworkException: Network Failure

object App8 extends IOAppDebug:
  val displayTemperature: Scenario => IO[Any] = scenario =>
    getTemperature(scenario).handleErrorWith:
      case _: Exception => IO("getTemperature failed")

  def run =
    NetworkFailure.simulate:
      displayTemperature
  // Getting Temperature
  // getTemperature failed

val temperatureAppComplete: Scenario => IO[Any] = scenario =>
  getTemperature(scenario).handleErrorWith:
    case NetworkException =>
      IO:
        "Network Unavailable"
    case GpsException     =>
      IO:
        "GPS Hardware Failure"

object App9 extends IOAppDebug:
  def run =
    GPSFailure.simulate:
      temperatureAppComplete
  // Getting Temperature
  // GPS Hardware Failure

// Note - the error below does not get properly replaced when building on windows
val x = 0

def check(t: Temperature) =
  IO.println("Checking Temperature") *> {
    if t.degrees > 0 then
      IO:
        "Comfortable Temperature"
    else
      IO.raiseError:
        ClimateFailure("**Too Cold**")
  }

final case class ClimateFailure(message: String) extends Throwable(message)

object App10 extends IOAppDebug:
  def run =
    check(Temperature(-20))
  // Checking Temperature
  // Error: ClimateFailure(**Too Cold**)

val weatherReportFaulty: Scenario => IO[String] = scenario => getTemperature(scenario).flatMap(check)

val weatherReport: Scenario => IO[String] = scenario =>
  weatherReportFaulty(scenario).handleErrorWith:
    case exception: Exception    =>
      IO(exception.getMessage)
    case failure: ClimateFailure =>
      IO(failure.message)

object App11 extends IOAppDebug:
  def run =
    TooCold.simulate:
      weatherReport
  // Getting Temperature
  // Checking Temperature
  // **Too Cold**

def getTemperatureOrThrow: String =
  currentScenario match
    case Scenario.GPSFailure     => throw GpsException
    case Scenario.NetworkFailure => throw NetworkException
    case _                       => "35 degrees"

object App12 extends IOAppDebug:
  def run =
    NetworkFailure.simulate: _ =>
      IO(getTemperatureOrThrow)
  // Defect: NetworkException: Network Failure

def safeTemperatureApp =
  IO:
    getTemperatureOrThrow

object App13 extends IOAppDebug:
  def run =
    NetworkFailure.simulate: _ =>
      safeTemperatureApp.orElse:
        IO:
          "Could not get temperature"
  // Result: Could not get temperature
