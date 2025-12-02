//| mvnDeps:
//| - com.goyeau::mill-scalafix::0.6.0
//| - com.lumidion::sonatype-central-client-requests:0.6.0
package build
import mill.*
import mill.scalalib.*
import mill.api.{BuildCtx, Task}
import mill.util.{Tasks, VcsVersion}

import scala.concurrent.duration.{Duration, DurationInt}
import com.goyeau.mill.scalafix.ScalafixModule
import com.lumidion.sonatype.central.client.core.{PublishingType, SonatypeCredentials}

object Versions {
  def scala3        = "3.3.7"
  def coursier      = "2.1.24"
  def jsoniterScala = "2.36.7"
}

object publish extends Publish

object Deps {
  def coursierCache = mvn"io.get-coursier:coursier-cache_2.13:${Versions.coursier}"
  def coursierCore  = mvn"io.get-coursier:coursier-core_2.13:${Versions.coursier}"
  def jsoniterCore  =
    mvn"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:${Versions.jsoniterScala}"
  def jsoniterMacros =
    mvn"com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:${Versions.jsoniterScala}"
  def sttp  = mvn"com.softwaremill.sttp.client3::core:3.11.0"
  def utest = mvn"com.lihaoyi::utest::0.8.9"
}

trait Publish extends ScalaModule with Published with ScalafixModule {
  def scalaVersion: T[String]                = Versions.scala3
  override def scalacOptions: T[Seq[String]] = Task {
    super.scalacOptions() ++ Seq("-Wunused:all")
  }

  def mvnDeps: T[Seq[Dep]] = super.mvnDeps() ++ Seq(
    Deps.coursierCache,
    Deps.coursierCore,
    Deps.jsoniterCore,
    Deps.sttp
  )
  def compileMvnDeps: T[Seq[Dep]] = super.compileMvnDeps() ++ Seq(
    Deps.jsoniterMacros
  )
  def javacOptions: T[Seq[String]] = super.javacOptions() ++ Seq(
    "--release",
    "8"
  )
  object test extends ScalaTests {
    def mvnDeps: T[Seq[Dep]] = super.mvnDeps() ++ Seq(
      Deps.utest
    )
    def testFramework = "utest.runner.Framework"
  }
}

def publishOrg: String = "io.get-coursier.publish"
def ghOrg: String      = "coursier"
def ghName: String     = "publish"

trait Published extends SonatypeCentralPublishModule {
  import mill.scalalib.publish.*
  def pomSettings: T[PomSettings] = PomSettings(
    description = artifactName(),
    organization = publishOrg,
    url = s"https://github.com/$ghOrg/$ghName",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("coursier", "publish"),
    developers = Seq(
      Developer("alexarchambault", "Alex Archambault", "https://github.com/alexarchambault")
    )
  )
  def publishVersion: T[String] = finalPublishVersion()
}

private def computePublishVersion(state: VcsVersion.State, simple: Boolean): String =
  if state.commitsSinceLastTag > 0 then
    if simple then {
      val versionOrEmpty = state.lastTag
        .filter(_ != "latest")
        .filter(_ != "nightly")
        .map(_.stripPrefix("v"))
        .flatMap { tag =>
          if simple then {
            val idx = tag.lastIndexOf(".")
            if idx >= 0 then
              Some(tag.take(idx + 1) + (tag.drop(idx + 1).toInt + 1).toString + "-SNAPSHOT")
            else None
          }
          else {
            val idx = tag.indexOf("-")
            if idx >= 0 then Some(tag.take(idx) + "+" + tag.drop(idx + 1) + "-SNAPSHOT") else None
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
      if idx >= 0 then rawVersion.take(idx) + "+" + rawVersion.drop(idx + 1) + "-SNAPSHOT"
      else rawVersion
    }
  else {
    val fromTag = state
      .lastTag
      .getOrElse(state.format())
      .stripPrefix("v")
    if fromTag == "0.0.0" then "0.0.1-SNAPSHOT"
    else fromTag
  }

def finalPublishVersion: T[String] = {
  val isCI = System.getenv("CI") != null
  if isCI then
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
