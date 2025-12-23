package proteus.examples.migration

import java.time.{Instant, OffsetDateTime, ZoneOffset}

import io.scalaland.chimney.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.Result
import proto.migration.Genre.Unrecognized

given Transformer[Movie, proto.migration.Movie] =
  Transformer.define[Movie, proto.migration.Movie].buildTransformer

given Transformer[Genre, proto.migration.Genre] = {
  case Genre.Comedy => proto.migration.Genre.GENRE_COMEDY
  case Genre.Drama  => proto.migration.Genre.GENRE_DRAMA
  case Genre.Horror => proto.migration.Genre.GENRE_HORROR
}

given Transformer[ReleaseStatus, proto.migration.ReleaseStatus] =
  value => proto.migration.ReleaseStatus(value.transformInto[proto.migration.ReleaseStatus.Value])

given Transformer[java.time.Duration, Int] = _.toMillis.toInt

given Transformer[java.time.OffsetDateTime, Long] = _.toInstant().toEpochMilli().toLong

given PartialTransformer[proto.migration.Movie, Movie] =
  PartialTransformer.define[proto.migration.Movie, Movie].buildTransformer

given PartialTransformer[proto.migration.Genre, Genre] =
  PartialTransformer {
    case proto.migration.Genre.GENRE_COMEDY => Result.fromValue(Genre.Comedy)
    case proto.migration.Genre.GENRE_DRAMA  => Result.fromValue(Genre.Drama)
    case proto.migration.Genre.GENRE_HORROR => Result.fromValue(Genre.Horror)
    case Unrecognized(unrecognizedValue)    => Result.fromErrorString(s"Unrecognized genre: $unrecognizedValue")
  }

given PartialTransformer[Int, java.time.Duration] =
  PartialTransformer(value => Result.fromValue(java.time.Duration.ofMillis(value)))

given PartialTransformer[Long, java.time.OffsetDateTime] =
  PartialTransformer(value => Result.fromValue(java.time.OffsetDateTime.ofInstant(Instant.ofEpochMilli(value), ZoneOffset.UTC)))

given PartialTransformer[proto.migration.ReleaseStatus, ReleaseStatus] =
  PartialTransformer(
    _.value
      .intoPartial[ReleaseStatus]
      .withSealedSubtypeHandledPartial[proto.migration.ReleaseStatus.Value.Empty.type](_ => Result.fromValue(ReleaseStatus.Unreleased(None)))
      .transform
  )
