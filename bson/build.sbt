name := "magnolia-bson"

version := "0.1"

libraryDependencies ++= Seq(
  "org.mongodb.scala" %% "mongo-scala-driver" % "4.0.1",
  "com.propensive" %% "magnolia" % "0.12.7",
  "org.typelevel" %% "cats-core" % "2.2.0-M1"
//  "com.chuusai" % "shapeless_2.13" % "2.4.0-M1"
)
