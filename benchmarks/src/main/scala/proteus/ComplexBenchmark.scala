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
class ComplexBenchmark {
  private val proteusEnc = proteusCodec.encode(complexData)
  private val scalapbEnc = domainToScalaPB(complexData).toByteArray
  private val zioEnc     = zioSchemaCodec.encode(complexData)
  private val upickleEnc = writeBinary(complexData)
  private val borerEnc   = Cbor.encode(complexData).toByteArray
  private val kyoEnc     = _root_.kyo.Protobuf.encode(complexData)

  @Benchmark
  def proteus_encode(bh: Blackhole): Unit =
    bh.consume(proteusCodec.encode(complexData))

  @Benchmark
  def proteus_decode(bh: Blackhole): Unit =
    bh.consume(proteusCodec.decode(proteusEnc))

  @Benchmark
  def scalapb_chimney_encode(bh: Blackhole): Unit =
    bh.consume(domainToScalaPB(complexData).toByteArray)

  @Benchmark
  def scalapb_chimney_decode(bh: Blackhole): Unit =
    bh.consume(scalaPBToDomain(scalapb.A.parseFrom(scalapbEnc)))

  @Benchmark
  def zio_schema_protobuf_encode(bh: Blackhole): Unit =
    bh.consume(zioSchemaCodec.encode(complexData))

  @Benchmark
  def zio_schema_protobuf_decode(bh: Blackhole): Unit =
    bh.consume(zioSchemaCodec.decode(zioEnc))

  @Benchmark
  def upickle_encode(bh: Blackhole): Unit =
    bh.consume(writeBinary(complexData))

  @Benchmark
  def upickle_decode(bh: Blackhole): Unit =
    bh.consume(readBinary[A](upickleEnc))

  @Benchmark
  def borer_encode(bh: Blackhole): Unit =
    bh.consume(Cbor.encode(complexData).toByteArray)

  @Benchmark
  def borer_decode(bh: Blackhole): Unit =
    bh.consume(Cbor.decode(borerEnc).to[A].value)

  @Benchmark
  def kyo_encode(bh: Blackhole): Unit =
    bh.consume(_root_.kyo.Protobuf.encode(complexData))

  @Benchmark
  def kyo_decode(bh: Blackhole): Unit =
    bh.consume(_root_.kyo.Protobuf.decode[A](kyoEnc).getOrThrow)
}
