import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.1`
import $ivy.`com.goyeau::mill-scalafix::0.5.1`
import de.tobiasroeser.mill.vcs.version._
import mill._
import mill.scalalib._

import scala.concurrent.duration.{Duration, DurationInt}
import com.goyeau.mill.scalafix.ScalafixModule
import com.lumidion.sonatype.central.client.core.{PublishingType, SonatypeCredentials}

object Versions {
  def scala3        = "3.3.6"
  def coursier      = "2.1.24"
  def jsoniterScala = "2.36.7"
}

object publish extends Publish

object Deps {
  def coursierCache = ivy"io.get-coursier:coursier-cache_2.13:${Versions.coursier}"
  def coursierCore  = ivy"io.get-coursier:coursier-core_2.13:${Versions.coursier}"
  def jsoniterCore  =
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:${Versions.jsoniterScala}"
  def jsoniterMacros =
    ivy"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:${Versions.jsoniterScala}"
  def sttp  = ivy"com.softwaremill.sttp.client3::core:3.11.0"
  def utest = ivy"com.lihaoyi::utest::0.8.9"
}

trait Publish extends ScalaModule with Published with ScalafixModule {
  def scalaVersion: Target[String]                = Versions.scala3
  override def scalacOptions: Target[Seq[String]] = Task {
    super.scalacOptions() ++ Seq("-Wunused:all")
  }

  def ivyDeps: Target[Agg[Dep]] = super.ivyDeps() ++ Seq(
    Deps.coursierCache,
    Deps.coursierCore,
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

def publishOrg: String = "io.get-coursier.publish"
def ghOrg: String      = "coursier"
def ghName: String     = "publish"

def publishSonatype(tasks: mill.main.Tasks[PublishModule.PublishData]): Command[Unit] =
  Task.Command {
    val taskNames = tasks.value.map(_.toString())
    System.err.println(
      s"""Tasks producing artifacts to be included in the bundle:
         |  ${taskNames.mkString("\n  ")}""".stripMargin
    )
    val publishVersion = finalPublishVersion()
    System.err.println(s"Publish version: $publishVersion")
    val bundleName = s"$publishOrg-$ghName-$publishVersion"
    System.err.println(s"Publishing bundle: $bundleName")
    val timeout     = 10.minutes
    val credentials = SonatypeCredentials(
      username = sys.env("SONATYPE_USERNAME"),
      password = sys.env("SONATYPE_PASSWORD")
    )
    val pgpPassword = sys.env("PGP_PASSWORD")
    val data        = Task.sequence(tasks.value)()

    doPublishSonatype(
      bundleName = bundleName,
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
  bundleName: String,
  credentials: SonatypeCredentials,
  pgpPassword: String,
  data: Seq[PublishModule.PublishData],
  timeout: Duration,
  workspace: os.Path,
  env: Map[String, String],
  log: mill.api.Logger
): Unit = {
  System.err.println("Actual artifacts included in the bundle:")
  val artifacts = data.map {
    case PublishModule.PublishData(a, s) =>
      System.err.println(s"  ${a.group}:${a.id}:${a.version}")
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
  System.err.println(s"Is release: $isRelease")
  val publisher = new SonatypeCentralPublisher(
    credentials = credentials,
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
    awaitTimeout = timeout.toMillis.toInt
  )

  val publishingType = if (isRelease) PublishingType.AUTOMATIC else PublishingType.USER_MANAGED
  System.err.println(s"Publishing type: $publishingType")
  val finalBundleName = if (bundleName.nonEmpty) Some(bundleName) else None
  System.err.println(s"Final bundle name: $finalBundleName")
  publisher.publishAll(
    publishingType = publishingType,
    singleBundleName = finalBundleName,
    artifacts = artifacts: _*
  )
}

trait Published extends SonatypeCentralPublishModule {
  import mill.scalalib.publish._
  def pomSettings: Target[PomSettings] = PomSettings(
    description = artifactName(),
    organization = publishOrg,
    url = s"https://github.com/$ghOrg/$ghName",
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
