package proteus.examples.migration

import java.time.{Duration, Instant, OffsetDateTime, ZoneOffset}

import zio.blocks.schema.*

import proteus.*
import proteus.Modifiers.*

given Schema[Movie]         = Schema.derived[Movie]
given Schema[ReleaseStatus] = Schema.derived[ReleaseStatus].defaultValue(ReleaseStatus.Unreleased(None))

val deriver =
  ProtobufDeriver
    .enable(ProtobufDeriver.DerivationFlag.AutoPrefixEnums)
    .enable(ProtobufDeriver.DerivationFlag.NestedOneOf)
    .instance(timeCodec)
    .instance(durationCodec)
    .modifier[Movie](reserved(2))
    .modifier[Movie]("title", reserved(4))

lazy val timeCodec: ProtobufCodec[OffsetDateTime] =
  Schema[Long]
    .derive(deriver)
    .transform[OffsetDateTime](
      millis => OffsetDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC),
      _.toInstant().toEpochMilli().toLong
    )

lazy val durationCodec: ProtobufCodec[Duration] =
  Schema[Int].derive(deriver).transform[Duration](Duration.ofMillis, _.toMillis.toInt)

given ProtobufCodec[Movie] = Schema[Movie].derive(deriver)
