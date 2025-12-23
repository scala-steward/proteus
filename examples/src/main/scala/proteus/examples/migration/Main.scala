package proteus.examples.migration

import java.time.{Duration, OffsetDateTime, ZoneOffset}

import io.scalaland.chimney.dsl.*

import proteus.*

@main
def main = {
  val movie = Movie(
    1,
    "The Godfather",
    List(Genre.Drama),
    Duration.ofMinutes(175),
    ReleaseStatus.Released(OffsetDateTime.of(1972, 3, 24, 0, 0, 0, 0, ZoneOffset.UTC))
  )

  // encode and decode using ScalaPB and Chimney
  val encoded = movie.transformInto[proto.migration.Movie].toByteArray
  val decoded = proto.migration.Movie.parseFrom(encoded).transformIntoPartial[Movie].asOption.get
  assert(decoded == movie)

  // generate a proto file with proteus
  val protoFile = Dependency("migration", "proto").add[Movie].render(Nil)
  println(protoFile)

  // encode and decode using Proteus
  val encoded2 = ProtobufCodec[Movie].encode(movie)
  val decoded2 = ProtobufCodec[Movie].decode(encoded)
  assert(decoded2 == movie)

  // decode with Proteus an object with no release status
  val encoded3 = movie
    .transformInto[proto.migration.Movie]
    .withReleasedStatus(proto.migration.ReleaseStatus(proto.migration.ReleaseStatus.Value.Empty))
    .toByteArray
  val decoded3 = ProtobufCodec[Movie].decode(encoded3)
  assert(decoded3 == movie.copy(releasedStatus = ReleaseStatus.Unreleased(None)))
}
