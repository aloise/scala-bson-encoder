package name.aloise

import name.aloise.bson._
import org.mongodb.scala.bson.{BsonInt64, BsonString}
import name.aloise.bson.BsonEncoder._
import name.aloise.bson.BsonDecoder._
import name.aloise.bson.derivation.decoder.auto._
import name.aloise.bson.derivation.encoder.auto._

object Main extends App {

  final class ObjectId(val id: Int) extends AnyVal
  case class Test2(x: Option[String], next: Option[Test1] = None)
  case class Test1(name: String, value2: Int)

  sealed trait AdtTest
  case class AdtTest1(name: String) extends AdtTest
  case class AdtTest2(name1: String) extends AdtTest
  case class AdtTest3(name2: List[String], lng: Option[Test1], x: Test1, y: List[Test2] = Nil) extends AdtTest
  case class AdtTest4Primitive(x: Long) extends AdtTest
  case object AdtObject extends AdtTest

  implicit val adtEncoder = implicitly[BsonEncoder[AdtTest]]

  // custom encoder
  implicit def encAdtTest4Primitive: BsonEncoder[AdtTest4Primitive] = (a: AdtTest4Primitive) => new BsonInt64(a.x)

  println(Test1("Hello World", 2).toBson)
  println((AdtTest1("Hello World")).toBson)
  println((AdtTest3(List("Hello World", "Hello Again"), None, Test1("name1", Int.MaxValue))).toBson)
  println((AdtTest4Primitive(5L)).toBson)
  println((AdtObject).toBson)
  println((new ObjectId(4)).toBson)

  // Adt Encoder
  println(adtEncoder(AdtTest1("Hello World")))

  // Decoders
  println(Test1("Hello World", 2).toBson)

  println(Test1("Hello World", 2).toBson.as[Test1])
  println((AdtTest3(List("Hello World", "Hello Again"), None, Test1("name1", Int.MaxValue))).toBson.as[AdtTest3])
}
