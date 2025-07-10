package coursier.publish

import coursier.maven.MavenRepository
import coursier.publish.fileset.FileSet
import coursier.publish.sonatype.SonatypeApi
import coursier.publish.sonatype.logger.{BatchSonatypeLogger, InteractiveSonatypeLogger}
import coursier.util.Task

import java.io.PrintStream
import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.duration.DurationInt

trait Hooks {
  type T >: Null

  def beforeUpload(fileSet: FileSet, isSnapshot: Boolean): Task[T] =
    Task.point(null)
  def repository(t: T, repo: PublishRepository, isSnapshot: Boolean): Option[MavenRepository] =
    None
  def afterUpload(t: T): Task[Unit] =
    Task.point(())

}

object Hooks {
  final case class Sonatype(
    repo: PublishRepository.Sonatype,
    api: SonatypeApi,
    out: PrintStream,
    verbosity: Int,
    batch: Boolean,
    es: ScheduledExecutorService
  ) extends Hooks {
    private def logger =
      if batch then new BatchSonatypeLogger(out, verbosity)
      else InteractiveSonatypeLogger.create(out, verbosity)

    type T = Option[(SonatypeApi.Profile, String)]

    override def beforeUpload(fileSet0: FileSet, isSnapshot: Boolean): Task[T] =
      if isSnapshot then Task.point(None)
      else
        for (p <- PublishTasks.sonatypeProfile(fileSet0, api, logger))
          yield {
            if verbosity >= 2 then
              out.println(s"Selected Sonatype profile ${p.name} (id: ${p.id}, uri: ${p.uri})")
            else if verbosity >= 1 then
              out.println(s"Selected Sonatype profile ${p.name} (id: ${p.id})")
            else if verbosity >= 0 then
              out.println(s"Selected Sonatype profile ${p.name}")

            val id = api.createStagingRepository(p, "create staging repository")
            Some((p, id))
          }

    override def repository(
      t: T,
      repo0: PublishRepository,
      isSnapshot: Boolean
    ): Option[MavenRepository] =
      t.map {
        case (_, repoId) =>
          repo.releaseRepoOf(repoId)
      }

    override def afterUpload(profileRepoIdOpt: T): Task[Unit] =
      profileRepoIdOpt match {
        case None                    => Task.point(())
        case Some((profile, repoId)) =>
          // TODO Print sensible error messages if anything goes wrong here (commands to finish promoting, etc.)
          api.sendCloseStagingRepositoryRequest(profile, repoId, "closing repository")
          for {
            _ <- api.waitForStatus(
              profileId = profile.id,
              repositoryId = repoId,
              status = "closed",
              checkActivityOpt = Some("close"),
              maxAttempt = 20,
              initialDelay = 3.seconds,
              backoffFactor = 1.5,
              es = es
            )
            _ = api.promoteStagingRepository(repoId, "promoting repository")
            _ <- api.waitForStatus(
              profileId = profile.id,
              repositoryId = repoId,
              status = "released",
              checkActivityOpt = Some("release"),
              maxAttempt = 20,
              initialDelay = 3.seconds,
              backoffFactor = 1.5,
              es = es
            )
          } yield ()
      }
  }

  def dummy: Hooks =
    new Hooks {
      type T = Object
    }

  def sonatype(
    repo: PublishRepository.Sonatype,
    api: SonatypeApi,
    out: PrintStream,
    verbosity: Int,
    batch: Boolean,
    es: ScheduledExecutorService
  ): Hooks = Sonatype(repo, api, out, verbosity, batch, es)
}
