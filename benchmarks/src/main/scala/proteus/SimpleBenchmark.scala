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
class SimpleBenchmark {
  private val proteusEnc = proteusCodec.encode(simpleData)
  private val scalapbEnc = domainToScalaPB(simpleData).toByteArray
  private val zioEnc     = zioSchemaCodec.encode(simpleData)
  private val upickleEnc = writeBinary(simpleData)
  private val borerEnc   = Cbor.encode(simpleData).toByteArray
  private val kyoEnc     = _root_.kyo.Protobuf.encode(simpleData)

  @Benchmark
  def proteus_encode(bh: Blackhole): Unit =
    bh.consume(proteusCodec.encode(simpleData))

  @Benchmark
  def proteus_decode(bh: Blackhole): Unit =
    bh.consume(proteusCodec.decode(proteusEnc))

  @Benchmark
  def scalapb_chimney_encode(bh: Blackhole): Unit =
    bh.consume(domainToScalaPB(simpleData).toByteArray)

  @Benchmark
  def scalapb_chimney_decode(bh: Blackhole): Unit =
    bh.consume(scalaPBToDomain(scalapb.A.parseFrom(scalapbEnc)))

  @Benchmark
  def zio_schema_protobuf_encode(bh: Blackhole): Unit =
    bh.consume(zioSchemaCodec.encode(simpleData))

  @Benchmark
  def zio_schema_protobuf_decode(bh: Blackhole): Unit =
    bh.consume(zioSchemaCodec.decode(zioEnc))

  @Benchmark
  def upickle_encode(bh: Blackhole): Unit =
    bh.consume(writeBinary(simpleData))

  @Benchmark
  def upickle_decode(bh: Blackhole): Unit =
    bh.consume(readBinary[A](upickleEnc))

  @Benchmark
  def borer_encode(bh: Blackhole): Unit =
    bh.consume(Cbor.encode(simpleData).toByteArray)

  @Benchmark
  def borer_decode(bh: Blackhole): Unit =
    bh.consume(Cbor.decode(borerEnc).to[A].value)

  @Benchmark
  def kyo_encode(bh: Blackhole): Unit =
    bh.consume(_root_.kyo.Protobuf.encode(simpleData))

  @Benchmark
  def kyo_decode(bh: Blackhole): Unit =
    bh.consume(_root_.kyo.Protobuf.decode[A](kyoEnc).getOrThrow)
}
