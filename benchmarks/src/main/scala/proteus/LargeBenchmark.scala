package proteus

import java.util.concurrent.TimeUnit

import Data.*
import io.bullet.borer.Cbor
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import test.test as scalapb
import upickle.default.*

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class LargeBenchmark {
  private val proteusEnc = proteusCodec.encode(largeData)
  private val scalapbEnc = domainToScalaPB(largeData).toByteArray
  private val zioEnc     = zioSchemaCodec.encode(largeData)
  private val upickleEnc = writeBinary(largeData)
  private val borerEnc   = Cbor.encode(largeData).toByteArray
  private val kyoEnc     = _root_.kyo.Protobuf.encode(largeData)

  @Benchmark
  def proteus_encode(bh: Blackhole): Unit =
    bh.consume(proteusCodec.encode(largeData))

  @Benchmark
  def proteus_decode(bh: Blackhole): Unit =
    bh.consume(proteusCodec.decode(proteusEnc))

  @Benchmark
  def scalapb_chimney_encode(bh: Blackhole): Unit =
    bh.consume(domainToScalaPB(largeData).toByteArray)

  @Benchmark
  def scalapb_chimney_decode(bh: Blackhole): Unit =
    bh.consume(scalaPBToDomain(scalapb.A.parseFrom(scalapbEnc)))

  @Benchmark
  def zio_schema_protobuf_encode(bh: Blackhole): Unit =
    bh.consume(zioSchemaCodec.encode(largeData))

  @Benchmark
  def zio_schema_protobuf_decode(bh: Blackhole): Unit =
    bh.consume(zioSchemaCodec.decode(zioEnc))

  @Benchmark
  def upickle_encode(bh: Blackhole): Unit =
    bh.consume(writeBinary(largeData))

  @Benchmark
  def upickle_decode(bh: Blackhole): Unit =
    bh.consume(readBinary[A](upickleEnc))

  @Benchmark
  def borer_encode(bh: Blackhole): Unit =
    bh.consume(Cbor.encode(largeData).toByteArray)

  @Benchmark
  def borer_decode(bh: Blackhole): Unit =
    bh.consume(Cbor.decode(borerEnc).to[A].value)

  @Benchmark
  def kyo_encode(bh: Blackhole): Unit =
    bh.consume(_root_.kyo.Protobuf.encode(largeData))

  @Benchmark
  def kyo_decode(bh: Blackhole): Unit =
    bh.consume(_root_.kyo.Protobuf.decode[A](kyoEnc).getOrThrow)
}
