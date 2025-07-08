package coursier.publish

import coursier.core.Authentication
import coursier.maven.MavenRepository

sealed abstract class PublishRepository extends Product with Serializable {
  def snapshotRepo: MavenRepository
  def releaseRepo: MavenRepository
  def readSnapshotRepo: MavenRepository
  def readReleaseRepo: MavenRepository

  final def repo(isSnapshot: Boolean): MavenRepository =
    if isSnapshot then snapshotRepo else releaseRepo
  final def readRepo(isSnapshot: Boolean): MavenRepository =
    if isSnapshot then readSnapshotRepo else readReleaseRepo
  def checkResultsRepo(isSnapshot: Boolean): MavenRepository =
    readRepo(isSnapshot)

  def withAuthentication(auth: Authentication): PublishRepository
}

object PublishRepository {
  final case class Simple(
    snapshotRepo: MavenRepository,
    readRepoOpt: Option[MavenRepository] = None
  ) extends PublishRepository {
    def releaseRepo: MavenRepository      = snapshotRepo
    def readSnapshotRepo: MavenRepository = readRepoOpt.getOrElse(snapshotRepo)
    def readReleaseRepo: MavenRepository  = readSnapshotRepo

    def withAuthentication(auth: Authentication): Simple =
      copy(
        snapshotRepo = snapshotRepo.withAuthentication(Some(auth)),
        readRepoOpt = readRepoOpt.map(_.withAuthentication(Some(auth)))
      )
  }

  final case class GitHub(
    username: String,
    repo: String,
    token: String,
    overrideAuthOpt: Option[Authentication]
  ) extends PublishRepository {
    def releaseRepo: MavenRepository =
      MavenRepository(
        s"https://maven.pkg.github.com/$username/$repo",
        authentication = overrideAuthOpt.orElse(Some(Authentication(username, token)))
      )
    def snapshotRepo: MavenRepository =
      releaseRepo

    def readReleaseRepo: MavenRepository =
      releaseRepo
    def readSnapshotRepo: MavenRepository =
      releaseRepo

    def withAuthentication(auth: Authentication): GitHub =
      copy(overrideAuthOpt = Some(auth))

    override def toString: String =
      Iterator(username, repo, "****", overrideAuthOpt)
        .mkString("GitHub(", ", ", ")")
  }

  /** Represents Sonatype Central Portal implementation for publishing artifacts.
    * @param base
    *   the base for the Sonatype Maven repository, defaults to the Central Portal OSSRH Staging API
    * @param centralPortalBase
    *   The base URL for the Sonatype Central Portal
    * @param useLegacySnapshots
    *   enables deprecated legacy snapshot repository path, kept for historical purposes
    */
  final case class Sonatype(
    base: MavenRepository = MavenRepository("https://ossrh-staging-api.central.sonatype.com"),
    centralPortalBase: String = "https://central.sonatype.com",
    useLegacySnapshots: Boolean = false
  ) extends PublishRepository {
    @deprecated(
      message = "Snapshot repo as per the old OSSRH purposes, kept for historical purposes",
      since = "0.4.0"
    )
    def legacySnapshotRepo: MavenRepository =
      base.withRoot(s"${base.root}/content/repositories/snapshots")

    def snapshotRepo: MavenRepository =
      if useLegacySnapshots then legacySnapshotRepo
      else base.withRoot(s"$centralPortalBase/repository/maven-snapshots/")

    def releaseRepo: MavenRepository =
      base.withRoot(s"$restBase/staging/deploy/maven2")
    def releaseRepoOf(repoId: String): MavenRepository =
      base.withRoot(s"$restBase/staging/deployByRepositoryId/$repoId")
    def readSnapshotRepo: MavenRepository =
      snapshotRepo
    def readReleaseRepo: MavenRepository =
      base.withRoot(s"${base.root}/content/repositories/releases")

    override def checkResultsRepo(isSnapshot: Boolean): MavenRepository =
      if isSnapshot then super.checkResultsRepo(isSnapshot)
      else base.withRoot(s"${base.root}/content/repositories/public")

    def restBase: String =
      s"${base.root}/service/local"

    def withAuthentication(auth: Authentication): Sonatype =
      copy(
        base = base.withAuthentication(Some(auth))
      )
  }

  def gitHub(username: String, repo: String, token: String): PublishRepository =
    GitHub(username, repo, token, None)
}
