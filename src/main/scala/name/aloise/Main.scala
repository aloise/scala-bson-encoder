package name.aloise

import name.aloise.bson._
import org.mongodb.scala.bson.BsonInt64

object Main extends App {

  final class ObjectId(val id: Int) extends AnyVal

  val anyValEncoder: Encoder[ObjectId] = EncoderDerivation.gen[ObjectId]

  case class Test1(name: String, value2: Int)

  val encoder: Encoder[Test1] = EncoderDerivation.gen[Test1]

  sealed trait AdtTest
  case class AdtTest1(name: String) extends AdtTest
  case class AdtTest2(name1: String) extends AdtTest
  case class AdtTest3(name2: List[String], lng: Option[Double], x: Long) extends AdtTest
  case class AdtTest4Primitive(x: Long) extends AdtTest
  case object AdtObject extends AdtTest

  implicit def encAdtTest4Primitive: Encoder[AdtTest4Primitive] =
    (a: AdtTest4Primitive) => new BsonInt64(a.x)

  val adtEncoder: Encoder[AdtTest] = EncoderDerivation.gen[AdtTest]

  println(encoder(Test1("Hello World", 2)))
  println(adtEncoder(AdtTest1("Hello World")))
//  println(adtEncoder(AdtTest3(List("Hello World", "Hello Again"), None, Long.MaxValue)))
  println(adtEncoder(AdtTest4Primitive(5L)))
  println(adtEncoder(AdtObject))
  println(anyValEncoder(new ObjectId(4)))
}
