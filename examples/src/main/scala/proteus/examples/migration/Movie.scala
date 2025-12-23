package proteus.examples.migration

import java.time.{Duration, OffsetDateTime}

case class Movie(
  id: Int,
  title: String,
  genres: List[Genre],
  duration: Duration,
  releasedStatus: ReleaseStatus
)

enum Genre {
  case Comedy, Drama, Horror
}

enum ReleaseStatus {
  case Released(releaseDate: OffsetDateTime)
  case Unreleased(plannedReleaseDate: Option[OffsetDateTime])
}
