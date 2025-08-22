package proteus

import java.time.*

import scala.util.Try

import zio.blocks.schema.{Schema, TypeName}
import zio.test.*
import zio.test.Assertion.*

object ProtobufCodecSpec extends ZIOSpecDefault {

  type DateTime = OffsetDateTime

  object DateTime {
    def unsafeSystemNow(): DateTime = OffsetDateTime.now()

    val timeZoneId: ZoneId = ZoneId.of("Asia/Seoul")
    val min: DateTime      = OffsetDateTime.ofInstant(Instant.EPOCH, timeZoneId)

    def ofEpochMilli(millis: Long, zoneId: ZoneId = timeZoneId): DateTime =
      OffsetDateTime.ofInstant(Instant.ofEpochMilli(millis), zoneId)
  }

  extension (dateTime: DateTime) {
    def toEpochMilli: Long =
      dateTime.toInstant.toEpochMilli
  }

  case class DateTimeWrapper(currentTimeMillis: Long) derives Schema

  opaque type UserId = String
  object UserId {
    def apply(value: String): UserId = value
    extension (userId: UserId) def value: String = userId
  }

  val deriver                    = ProtobufDeriver()
  val deriverWithOptionalAsOneOf = ProtobufDeriver(Set(ProtobufDeriver.DerivationFlag.OptionalAsOneOf))

  val testDateTimeSchema: ProtobufCodec[OffsetDateTime] =
    Schema[DateTimeWrapper]
      .derive(deriver)
      .transform[OffsetDateTime](
        wrapper => {
          val millis = wrapper.currentTimeMillis
          if (millis == 0) DateTime.min else DateTime.ofEpochMilli(millis)
        },
        dt => DateTimeWrapper(dt.toEpochMilli)
      )

  def spec = suite("ProtobufCodecSpec")(
    suite("Simple Types")(
      test("basic message with primitives") {
        case class SimpleMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[SimpleMessage].derive(deriver)

        val original = SimpleMessage(42, "test", true)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with default values") {
        case class DefaultMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[DefaultMessage].derive(deriver)

        val original = DefaultMessage(0, "", false)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Enum Tests")(
      test("message with enum field") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(id: Int, status: Status) derives Schema
        val codec = Schema[StatusMessage].derive(deriver)

        val variants = List(Status.Active, Status.Inactive, Status.Pending)
        val results  = variants.map { status =>
          val original = StatusMessage(1, status)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.status))(equalTo(variants))
      },
      test("oneOf field variants") {
        enum ContactInfo derives Schema {
          case Email(address: String)
          case Phone(number: String)
          case Social(platform: String, handle: String)
        }
        case class ContactMessage(contact: ContactInfo) derives Schema
        val codec = Schema[ContactMessage].derive(deriver)

        val variants = List(
          ContactInfo.Email("test@example.com"),
          ContactInfo.Phone("555-0123"),
          ContactInfo.Social("twitter", "@testuser")
        )

        val results = variants.map { contact =>
          val original = ContactMessage(contact)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.contact))(equalTo(variants))
      }
    ),
    suite("DateTime Tests")(
      test("message with DateTime field") {
        case class TimeMessage(id: Int, timestamp: DateTime) derives Schema

        val codec = Schema[TimeMessage].deriving(deriver).instance(TypeName.offsetDateTime, testDateTimeSchema).derive

        val timestamps = List(
          DateTime.min,
          DateTime.ofEpochMilli(DateTime.unsafeSystemNow().toEpochMilli),
          DateTime.ofEpochMilli(0L),
          DateTime.ofEpochMilli(1000000000000L)
        )

        val results = timestamps.map { timestamp =>
          val original = TimeMessage(1, timestamp)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.timestamp))(equalTo(timestamps))
      }
    ),
    suite("Optional Fields")(
      test("message with Option field") {
        case class OptionalMessage(id: Int, value: Option[String]) derives Schema
        val codec = Schema[OptionalMessage].derive(deriver)

        val withSome = OptionalMessage(1, Some("present"))
        val withNone = OptionalMessage(2, None)

        val encodedSome = codec.encode(withSome)
        val decodedSome = codec.decode(encodedSome)

        val encodedNone = codec.encode(withNone)
        val decodedNone = codec.decode(encodedNone)

        assert(decodedSome)(equalTo(withSome)) &&
          assert(decodedNone)(equalTo(withNone))
      },
      test("optional field None vs Some empty string encoding") {
        case class OptionalFieldMessage(id: Int, optionalValue: Option[String]) derives Schema
        val codec = Schema[OptionalFieldMessage].derive(deriver)

        val messageWithNone           = OptionalFieldMessage(1, None)
        val messageWithEmptyString    = OptionalFieldMessage(1, Some(""))
        val messageWithNonEmptyString = OptionalFieldMessage(1, Some("test"))

        val encodedNone        = codec.encode(messageWithNone)
        val encodedEmptyString = codec.encode(messageWithEmptyString)
        val encodedNonEmpty    = codec.encode(messageWithNonEmptyString)

        val decodedNone        = codec.decode(encodedNone)
        val decodedEmptyString = codec.decode(encodedEmptyString)
        val decodedNonEmpty    = codec.decode(encodedNonEmpty)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedEmptyString)(equalTo(messageWithEmptyString)) &&
          assert(decodedNonEmpty)(equalTo(messageWithNonEmptyString)) &&
          assert(encodedNone.length)(isLessThan(encodedEmptyString.length)) &&
          assert(encodedEmptyString.length)(isLessThan(encodedNonEmpty.length))
      },
      test("optional primitives None vs Some default values") {
        case class OptionalPrimitivesMessage(
          optInt: Option[Int],
          optLong: Option[Long],
          optBool: Option[Boolean],
          optDouble: Option[Double],
          optFloat: Option[Float]
        ) derives Schema
        val codec = Schema[OptionalPrimitivesMessage].derive(deriver)

        val messageWithNone     = OptionalPrimitivesMessage(None, None, None, None, None)
        val messageWithDefaults = OptionalPrimitivesMessage(Some(0), Some(0L), Some(false), Some(0.0), Some(0.0f))

        val encodedNone     = codec.encode(messageWithNone)
        val encodedDefaults = codec.encode(messageWithDefaults)

        val decodedNone     = codec.decode(encodedNone)
        val decodedDefaults = codec.decode(encodedDefaults)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedDefaults)(equalTo(messageWithDefaults)) &&
          assert(encodedNone.length)(isLessThan(encodedDefaults.length))
      },
      test("optional enum None vs Some default value") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class OptionalEnumMessage(id: Int, status: Option[Status]) derives Schema
        val codec = Schema[OptionalEnumMessage].derive(deriver)

        val messageWithNone    = OptionalEnumMessage(1, None)
        val messageWithDefault = OptionalEnumMessage(1, Some(Status.Active))

        val encodedNone    = codec.encode(messageWithNone)
        val encodedDefault = codec.encode(messageWithDefault)

        val decodedNone    = codec.decode(encodedNone)
        val decodedDefault = codec.decode(encodedDefault)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedDefault)(equalTo(messageWithDefault)) &&
          assert(encodedNone.length)(isLessThan(encodedDefault.length))
      }
    ),
    suite("Optional Fields as OneOf")(
      test("message with Option field derived as OneOf") {
        case class OptionalAsOneOfMessage(id: Int, value: Option[String]) derives Schema
        val codec = Schema[OptionalAsOneOfMessage].derive(deriverWithOptionalAsOneOf)

        val withSome = OptionalAsOneOfMessage(1, Some("present"))
        val withNone = OptionalAsOneOfMessage(2, None)

        val encodedSome = codec.encode(withSome)
        val decodedSome = codec.decode(encodedSome)

        val encodedNone = codec.encode(withNone)
        val decodedNone = codec.decode(encodedNone)

        assert(decodedSome)(equalTo(withSome)) &&
          assert(decodedNone)(equalTo(withNone))
      },
      test("OneOf optional encoding vs standard optional encoding different") {
        case class StandardOptional(id: Int, value: Option[String]) derives Schema
        case class OneOfOptional(id: Int, value: Option[String]) derives Schema

        val standardCodec = Schema[StandardOptional].derive(deriver)
        val oneOfCodec    = Schema[OneOfOptional].derive(deriverWithOptionalAsOneOf)

        val messageWithSome = (StandardOptional(1, Some("test")), OneOfOptional(1, Some("test")))
        val messageWithNone = (StandardOptional(1, None), OneOfOptional(1, None))

        val standardEncodedSome = standardCodec.encode(messageWithSome._1)
        val oneOfEncodedSome    = oneOfCodec.encode(messageWithSome._2)

        val standardEncodedNone = standardCodec.encode(messageWithNone._1)
        val oneOfEncodedNone    = oneOfCodec.encode(messageWithNone._2)

        assert(standardEncodedSome)(not(equalTo(oneOfEncodedSome))) &&
          assert(standardEncodedNone)(not(equalTo(oneOfEncodedNone)))
      },
      test("multiple optional fields as OneOf") {
        case class MultipleOptionalMessage(
          id: Int,
          name: Option[String],
          count: Option[Int],
          active: Option[Boolean]
        ) derives Schema
        val codec = Schema[MultipleOptionalMessage].derive(deriverWithOptionalAsOneOf)

        val testCases = List(
          MultipleOptionalMessage(1, Some("test"), Some(42), Some(true)),
          MultipleOptionalMessage(2, None, Some(0), None),
          MultipleOptionalMessage(3, Some(""), None, Some(false)),
          MultipleOptionalMessage(4, None, None, None)
        )

        val results = testCases.map { original =>
          val encoded = codec.encode(original)
          val decoded = codec.decode(encoded)
          decoded
        }

        assert(results)(equalTo(testCases))
      },
      test("optional nested message as OneOf") {
        case class NestedMessage(value: String, count: Int) derives Schema
        case class OptionalNestedAsOneOf(id: Int, nested: Option[NestedMessage]) derives Schema
        val codec = Schema[OptionalNestedAsOneOf].derive(deriverWithOptionalAsOneOf)

        val withNested    = OptionalNestedAsOneOf(1, Some(NestedMessage("test", 42)))
        val withoutNested = OptionalNestedAsOneOf(2, None)

        val encodedWithNested = codec.encode(withNested)
        val decodedWithNested = codec.decode(encodedWithNested)

        val encodedWithoutNested = codec.encode(withoutNested)
        val decodedWithoutNested = codec.decode(encodedWithoutNested)

        assert(decodedWithNested)(equalTo(withNested)) &&
          assert(decodedWithoutNested)(equalTo(withoutNested))
      },
      test("optional enum as OneOf") {
        enum Priority derives Schema { case Low, Medium, High }
        case class OptionalEnumAsOneOf(id: Int, priority: Option[Priority]) derives Schema
        val codec = Schema[OptionalEnumAsOneOf].derive(deriverWithOptionalAsOneOf)

        val withPriority    = OptionalEnumAsOneOf(1, Some(Priority.High))
        val withoutPriority = OptionalEnumAsOneOf(2, None)

        val encodedWithPriority = codec.encode(withPriority)
        val decodedWithPriority = codec.decode(encodedWithPriority)

        val encodedWithoutPriority = codec.encode(withoutPriority)
        val decodedWithoutPriority = codec.decode(encodedWithoutPriority)

        assert(decodedWithPriority)(equalTo(withPriority)) &&
          assert(decodedWithoutPriority)(equalTo(withoutPriority))
      }
    ),
    suite("Nested Messages")(
      test("message with nested message") {
        case class Inner(value: String) derives Schema
        case class Outer(id: Int, inner: Inner) derives Schema
        val outerCodec = Schema[Outer].derive(deriver)

        val original = Outer(1, Inner("nested"))
        val encoded  = outerCodec.encode(original)
        val decoded  = outerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("optional message None vs Some with default values") {
        case class InnerMessage(value: String, count: Int) derives Schema
        case class OptionalMessageMessage(id: Int, inner: Option[InnerMessage]) derives Schema
        val codec = Schema[OptionalMessageMessage].derive(deriver)

        val messageWithNone    = OptionalMessageMessage(1, None)
        val messageWithDefault = OptionalMessageMessage(1, Some(InnerMessage("", 0)))

        val encodedNone    = codec.encode(messageWithNone)
        val encodedDefault = codec.encode(messageWithDefault)

        val decodedNone    = codec.decode(encodedNone)
        val decodedDefault = codec.decode(encodedDefault)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedDefault)(equalTo(messageWithDefault)) &&
          assert(encodedNone.length)(isLessThan(encodedDefault.length))
      }
    ),
    suite("Collections")(
      test("message with List[Int]") {
        case class NumberMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[NumberMessage].derive(deriver)

        val original = NumberMessage("test", List(1, 2, 3, 4, 5))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with empty List[Int]") {
        case class EmptyListMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[EmptyListMessage].derive(deriver)

        val original = EmptyListMessage("empty", List.empty)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with List[Int] containing zeros") {
        case class ZeroListMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[ZeroListMessage].derive(deriver)

        val original = ZeroListMessage("zeros", List(0, 1, 0, 2, 0))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with List of messages") {
        case class Item(name: String, value: Int) derives Schema
        case class Container(items: List[Item]) derives Schema
        val containerCodec = Schema[Container].derive(deriver)

        val original = Container(List(Item("a", 1), Item("b", 2), Item("c", 3)))
        val encoded  = containerCodec.encode(original)
        val decoded  = containerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with Map field") {
        case class MapMessage(data: Map[String, Int]) derives Schema
        val codec = Schema[MapMessage].derive(deriver)

        val original = MapMessage(Map("key1" -> 1, "key2" -> 2, "key3" -> 3))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with empty objects in list") {
        case class EmptyItem(name: String, value: String) derives Schema
        case class EmptyContainer(items: List[EmptyItem]) derives Schema
        val emptyContainerCodec = Schema[EmptyContainer].derive(deriver)

        val original = EmptyContainer(List(EmptyItem("", "")))
        val encoded  = emptyContainerCodec.encode(original)
        val decoded  = emptyContainerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("String Handling")(
      test("message with special characters") {
        case class TextMessage(text: String) derives Schema
        val codec = Schema[TextMessage].derive(deriver)

        val specialTexts = List(
          "Normal text",
          "Unicode测试",
          "Special!@#$%^&*()Characters",
          "Very Long String " * 100
        )

        val results = specialTexts.map { text =>
          val original = TextMessage(text)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.text))(equalTo(specialTexts))
      },
      test("message with empty string fields") {
        case class EmptyStrings(name: String, description: String) derives Schema
        val codec = Schema[EmptyStrings].derive(deriver)

        val original = EmptyStrings("", "")
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Opaque Types")(
      test("opaque type wrapper") {
        case class UserMessage(id: UserId, name: String) derives Schema
        val codec = Schema[UserMessage].derive(deriver)

        val original = UserMessage(UserId("user-123"), "John Doe")
        val encoded = codec.encode(original)
        val decoded = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Error Handling")(
      test("decode invalid data returns error") {
        case class TestMessage(id: Int) derives Schema
        val codec = Schema[TestMessage].derive(deriver)

        val invalidBytes = Array[Byte](1, 2, 3, 4, 5)
        val decoded      = Try(codec.decode(invalidBytes)).toEither

        assert(decoded)(isLeft)
      },
      test("empty byte array decode returns defaults") {
        case class TestMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[TestMessage].derive(deriver)

        val emptyBytes = Array.empty[Byte]
        val decoded    = codec.decode(emptyBytes)

        assert(decoded)(equalTo(TestMessage(0, "", false)))
      }
    )
  )
}
