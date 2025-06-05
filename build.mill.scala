import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.1`
import de.tobiasroeser.mill.vcs.version._

import mill._
import mill.scalalib._

import scala.concurrent.duration.{Duration, DurationInt}

def scala213 = "2.13.16"
def scala212 = "2.12.20"

object publish extends Cross[Publish](scala213, scala212)

object Deps {

  object Version {
    def coursier      = "2.1.24"
    def jsoniterScala = "2.36.2"
  }

  def collectionCompat = ivy"org.scala-lang.modules::scala-collection-compat::2.13.0"
  def coursierCache    = ivy"io.get-coursier::coursier-cache:${Version.coursier}"
  def coursierCore     = ivy"io.get-coursier::coursier-core:${Version.coursier}"
  def jsoniterCore     =
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:${Version.jsoniterScala}"
  def jsoniterMacros =
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:${Version.jsoniterScala}"
  def sttp  = ivy"com.softwaremill.sttp.client3::core:3.11.0"
  def utest = ivy"com.lihaoyi::utest::0.8.5"
}

trait Publish extends CrossScalaModule with Published {
  def ivyDeps: Target[Agg[Dep]] = super.ivyDeps() ++ Seq(
    Deps.coursierCache,
    Deps.coursierCore,
    Deps.collectionCompat,
    Deps.jsoniterCore,
    Deps.sttp
  )
  def compileIvyDeps: Target[Agg[Dep]] = super.compileIvyDeps() ++ Seq(
    Deps.jsoniterMacros
  )
  def javacOptions: Target[Seq[String]] = super.javacOptions() ++ Seq(
    "--release",
    "8"
  )
  object test extends ScalaTests {
    def ivyDeps: Target[Agg[Dep]] = super.ivyDeps() ++ Seq(
      Deps.utest
    )
    def testFramework = "utest.runner.Framework"
  }
}

def publishSonatype(tasks: mill.main.Tasks[PublishModule.PublishData]): Command[Unit] =
  Task.Command {
    val timeout     = 10.minutes
    val credentials = sys.env("SONATYPE_USERNAME") + ":" + sys.env("SONATYPE_PASSWORD")
    val pgpPassword = sys.env("PGP_PASSWORD")
    val data        = Task.sequence(tasks.value)()

    doPublishSonatype(
      credentials = credentials,
      pgpPassword = pgpPassword,
      data = data,
      timeout = timeout,
      workspace = Task.workspace,
      env = Task.env,
      log = Task.ctx().log
    )
  }

private def doPublishSonatype(
  credentials: String,
  pgpPassword: String,
  data: Seq[PublishModule.PublishData],
  timeout: Duration,
  workspace: os.Path,
  env: Map[String, String],
  log: mill.api.Logger
): Unit = {

  val artifacts = data.map {
    case PublishModule.PublishData(a, s) =>
      (s.map { case (p, f) => (p.path, f) }, a)
  }

  val isRelease = {
    val versions = artifacts.map(_._2.version).toSet
    val set      = versions.map(!_.endsWith("-SNAPSHOT"))
    assert(
      set.size == 1,
      s"Found both snapshot and non-snapshot versions: ${versions.toVector.sorted.mkString(", ")}"
    )
    set.head
  }
  val publisher = new mill.scalalib.publish.SonatypePublisher(
    uri = "https://oss.sonatype.org/service/local",
    snapshotUri = "https://oss.sonatype.org/content/repositories/snapshots",
    credentials = credentials,
    signed = true,
    gpgArgs = Seq(
      "--detach-sign",
      "--batch=true",
      "--yes",
      "--pinentry-mode",
      "loopback",
      "--passphrase",
      pgpPassword,
      "--armor",
      "--use-agent"
    ),
    readTimeout = timeout.toMillis.toInt,
    connectTimeout = timeout.toMillis.toInt,
    log = log,
    workspace = workspace,
    env = env,
    awaitTimeout = timeout.toMillis.toInt,
    stagingRelease = isRelease
  )

  publisher.publishAll(isRelease, artifacts: _*)
}

trait Published extends PublishModule {
  import mill.scalalib.publish._
  def pomSettings: Target[PomSettings] = PomSettings(
    description = artifactName(),
    organization = "io.get-coursier.publish",
    url = s"https://github.com/coursier/publish",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("coursier", "publish"),
    developers = Seq(
      Developer("alexarchambault", "Alex Archambault", "https://github.com/alexarchambault")
    )
  )
  def publishVersion: Target[String] = finalPublishVersion()
}

private def computePublishVersion(state: VcsState, simple: Boolean): String =
  if (state.commitsSinceLastTag > 0)
    if (simple) {
      val versionOrEmpty = state.lastTag
        .filter(_ != "latest")
        .filter(_ != "nightly")
        .map(_.stripPrefix("v"))
        .flatMap { tag =>
          if (simple) {
            val idx = tag.lastIndexOf(".")
            if (idx >= 0)
              Some(tag.take(idx + 1) + (tag.drop(idx + 1).toInt + 1).toString + "-SNAPSHOT")
            else
              None
          }
          else {
            val idx = tag.indexOf("-")
            if (idx >= 0) Some(tag.take(idx) + "+" + tag.drop(idx + 1) + "-SNAPSHOT")
            else None
          }
        }
        .getOrElse("0.0.1-SNAPSHOT")
      Some(versionOrEmpty)
        .filter(_.nonEmpty)
        .getOrElse(state.format())
    }
    else {
      val rawVersion = os.proc("git", "describe", "--tags").call().out.text().trim
        .stripPrefix("v")
        .replace("latest", "0.0.0")
        .replace("nightly", "0.0.0")
      val idx = rawVersion.indexOf("-")
      if (idx >= 0) rawVersion.take(idx) + "+" + rawVersion.drop(idx + 1) + "-SNAPSHOT"
      else rawVersion
    }
  else {
    val fromTag = state
      .lastTag
      .getOrElse(state.format())
      .stripPrefix("v")
    if (fromTag == "0.0.0") "0.0.1-SNAPSHOT"
    else fromTag
  }

def finalPublishVersion: Target[String] = {
  val isCI = System.getenv("CI") != null
  if (isCI)
    Task(persistent = true) {
      val state = VcsVersion.vcsState()
      computePublishVersion(state, simple = false)
    }
  else
    Task {
      val state = VcsVersion.vcsState()
      computePublishVersion(state, simple = true)
    }
}
