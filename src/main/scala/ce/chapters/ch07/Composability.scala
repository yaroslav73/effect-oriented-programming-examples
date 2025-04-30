package ce.chapters.ch07

import cats.syntax.all.*

import scala.concurrent.{Future, TimeoutException}
import Scenario.*
import cats.effect.{IO, Resource}
import ce.helpers.IOAppDebug

import scala.util.Using

var currentScenario: Scenario =
  DiskFull

enum Scenario:
  case Successful
  case HeadlineError
  case BoringTopic
  case FileSystemError
  case WikiSystemError
  case AISlow
  case DiskFull

  def simulate[A](effect: IO[A]): IO[A] =
    for
      _      <- IO {
                  currentScenario = this
                }
      result <- effect
    yield result

def getHeadline: Future[String] =
  println("Network - Getting headline")
  currentScenario match
    case Scenario.HeadlineError   =>
      Future.failed:
        new Exception("Headline not available")
    case Scenario.Successful      =>
      Future
        .successful("stock market rising!")
    case Scenario.WikiSystemError =>
      Future.successful("Fred built a barn.")
    case Scenario.AISlow          =>
      Future.successful("space is big!")
    case Scenario.FileSystemError =>
      Future
        .successful("new unicode released!")
    case Scenario.BoringTopic     =>
      Future.successful("boring content")
    case Scenario.DiskFull        =>
      Future
        .successful("human genome sequenced")
  end match
end getHeadline

def findTopicOfInterest(content: String): Option[String] =
  println("Analytics - Scanning for topic")
  val topics =
    List(
      "stock market",
      "space",
      "barn",
      "unicode",
      "genome",
    )
  val res    = topics.find(content.contains)
  println(s"Analytics - topic: $res")
  res

final case class NoWikiArticle() extends Throwable

def wikiArticle(topic: String): Either[NoWikiArticle, String] =
  println(s"Wiki - articleFor($topic)")
  // simulates that this takes some time
  Thread.sleep(1_000)
  topic match
    case "stock market" | "space" | "genome" =>
      Right:
        s"detailed history of $topic"
    case "barn"                              =>
      Left:
        NoWikiArticle()

final case class HeadlineNotAvailable()

// Either[HeadlineNotAvailable, String]
val getHeadlineCE: IO[String] =
  IO.fromFuture:
    IO.delay:
      getHeadline
  .onError:
    e => IO.println(s"We got an error: $e")
//      IO.pure:
//        HeadlineNotAvailable()

object App0 extends IOAppDebug:
  def run: IO[Any] =
    Successful.simulate:
      getHeadlineCE
// Network - Getting headline
// Result: stock market rising!

object App1 extends IOAppDebug:
  def run: IO[Any] =
    HeadlineError.simulate:
      getHeadlineCE

val result: Option[String] =
  findTopicOfInterest:
    "a boring headline"

final case class NoInterestingTopic(headline: String)

final case class NoInterestingTopicException(headline: String) extends Throwable

def topicOfInterestCE(headline: String) =
  IO.fromOption(
    findTopicOfInterest:
      headline,
  )(throw NoInterestingTopicException(headline))

object App2 extends IOAppDebug:
  def run: IO[Any] =
    topicOfInterestCE:
      "stock market rising!"
// Analytics - Scanning for topic
// Analytics - topic: None
// Analytics - Scanning for topic
// Analytics - topic: Some(stock market)
// Result: stock market

object App3 extends IOAppDebug:
  def run: IO[Any] =
    topicOfInterestCE:
      "boring and inane"

def wikiArticleCE(topic: String): IO[String] =
  IO.fromEither:
    wikiArticle:
      topic

object App4 extends IOAppDebug:
  def run: IO[Any] =
    wikiArticleCE:
      "stock market"
// Analytics - Scanning for topic
// Analytics - topic: None
// Wiki - articleFor(stock market)
// Result: detailed history of stock market

object App5 extends IOAppDebug:
  def run: IO[Any] =
    wikiArticleCE:
      "barn"
// Analytics - Scanning for topic
// Analytics - topic: None
// Wiki - articleFor(barn)
// Error: ce.chapters.ch07.NoWikiArticle

import scala.util.Try

trait File extends AutoCloseable:
  def contains(searchTerm: String): Boolean
  def write(entry: String): Try[String]
  def summaryFor(searchTerm: String): String
  def content(): String

def sameContents(files: List[File]): Boolean =
  println:
    "side-effect print: comparing content"

  files.tail
    .forall:
      _.content() == files.head.content()

def openFile(path: String) =
  new File:
    var contents: List[String] =
      List("Medical Breakthrough!")
    println(s"File - OPEN: $path")

    def content(): String =
      path match
        case "file1" | "file2" | "file3" | "summaries" =>
          "hot dog"
        case _                                         =>
          "not hot dog"

    def close(): Unit =
      println:
        s"File - CLOSE: $path"

    def contains(searchTerm: String): Boolean =
      val result =
        searchTerm match
          case "wheel" | "unicode" =>
            true
          case _                   =>
            false
      println:
        s"File - contains($searchTerm) => $result"
      result

    def summaryFor(searchTerm: String): String =
      println(s"File - summaryFor($searchTerm)")
      if searchTerm == "unicode" then
        println("File - * Threw Exception *")
        throw Exception(s"FileSystem error")
      else if searchTerm == "stock market"
      then "stock markets are neat"
      else if searchTerm == "space" then "space is huge"
      else ???

    def write(entry: String): Try[String] =
      if entry.contains("genome") then
        println("File - disk full!")
        Try(throw new Exception("Disk is full!"))
      else
        println("File - write: " + entry)
        contents = entry :: contents
        Try(entry)

// Only via Resources
def openFileCE(path: String): Resource[IO, File] =
  Resource.fromAutoCloseable:
    IO:
      openFile(path)

object App6 extends IOAppDebug:
  def run: IO[Any] =
    openFileCE("file1").use: file =>
      IO(file.contains("topicOfInterest"))
// Analytics - Scanning for topic
// Analytics - topic: None
// File - OPEN: file1
// File - contains(topicOfInterest) => false
// File - CLOSE: file1
// Result: false

object App7 extends IOAppDebug:
  def run: IO[Any] =
    IO:
      Using(openFile("file1")):
        file1 =>
          Using(openFile("file2")):
            file2 =>
              Using(openFile("file3")):
                file3 =>
                  sameContents:
                    List(file1, file2, file3)
          .get
        .get
      .get
// Analytics - Scanning for topic
// Analytics - topic: None
// File - OPEN: file1
// File - OPEN: file2
// File - OPEN: file3
// side-effect print: comparing content
// File - CLOSE: file3
// File - CLOSE: file2
// File - CLOSE: file1
// Result: true

object App8 extends IOAppDebug:
  def run: IO[Any] =
    val result = for
      file1 <- openFileCE("file1")
      file2 <- openFileCE("file2")
      file3 <- openFileCE("file3")
    yield List(file1, file2, file3)

    result.use:
      files =>
        IO:
          sameContents:
            files

object App9 extends IOAppDebug:
  def run: IO[Any] =
    List("file1", "file2", "file3")
      .traverse(openFileCE)
      .use(files => IO(sameContents(files)))

final case class FileWriteFailure() extends Throwable

def writeToFileCE(file: File, content: String): IO[String] =
  IO.fromTry:
    file.write:
      content
  .onError:
    e => IO.println(s"Write fail: $e")

object App10 extends IOAppDebug:
  def run: IO[Any] =
    openFileCE("file1").use: file =>
      writeToFileCE(file, "New Data")

object App11 extends IOAppDebug:
  def run: IO[Any] =
    IO:
      openFile("file1").summaryFor("space")

object App12 extends IOAppDebug:
  def run: IO[Any] =
    IO:
      openFile("file1").summaryFor("unicode")

case class FileReadFailure(topic: String) extends Throwable

def summarize(article: String): String =
  println(s"AI - summarize - start")
  // Represents the AI taking a long time to
  // summarize the content
  if article.contains("space") then
    println("AI - taking a long time")
    Thread.sleep(5_000)

  println(s"AI - summarize - end")
  if article.contains("stock market") then
    s"market is moving"
  else if article.contains("genome") then
    "The human genome is huge!"
  else if article.contains("long article")
  then
    "short summary"
  else
    ???

def summaryForCE(file: File, topic: String): IO[String] =
  IO:
    file.summaryFor(topic)

object App13 extends IOAppDebug:
  def run: IO[Any] =
    IO:
      summarize("long article")

import scala.concurrent.duration.DurationInt

def summarizeCE(article: String) =
  IO.blocking:
    summarize(article)
  .timeout(4.seconds)
  .onError { case _: TimeoutException  => IO.println("AI **INTERRUPTED**") }

object App14 extends IOAppDebug:
  def run: IO[Any] =
    summarizeCE("long article")

object App15 extends IOAppDebug:
  def run: IO[Any] =
    summarizeCE("space")
    
val researchHeadline: IO[String] =
  val result = for
    headline <- getHeadlineCE.toResource
    topic <- topicOfInterestCE(headline).toResource
    summaryFile <- openFileCE("summaries")
    summary <- 
      if summaryFile.contains(topic) then summaryForCE(summaryFile, topic).toResource
      else
        for
            article <- wikiArticleCE(topic).toResource
            summary <- summarizeCE(article).toResource
            _       <- writeToFileCE(summaryFile, summary).toResource
        yield summary
  yield summary

  result.use { headline => IO(headline) }

object App16 extends IOAppDebug:
  def run: IO[Any] =
    HeadlineError.simulate:
      researchHeadline

object App17 extends IOAppDebug:
  def run: IO[Any] =
    BoringTopic.simulate:
      researchHeadline

object App18 extends IOAppDebug:
  def run: IO[Any] =
    FileSystemError.simulate:
      researchHeadline

object App19 extends IOAppDebug:
  def run: IO[Any] =
    WikiSystemError.simulate:
      researchHeadline

object App20 extends IOAppDebug:
  def run: IO[Any] =
    AISlow.simulate:
      researchHeadline

object App21 extends IOAppDebug:
  def run: IO[Any] =
    DiskFull.simulate:
      researchHeadline

object App22 extends IOAppDebug:
  def run: IO[Any] =
    Successful.simulate:
      researchHeadline
      
val quickResearch =
  researchHeadline
    .timeout(100.milliseconds)

object App23 extends IOAppDebug:
  def run: IO[Any] =
    Successful.simulate:
      quickResearch