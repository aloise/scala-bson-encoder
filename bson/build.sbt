name := "magnolia-bson"

version := "0.1"

libraryDependencies ++= Seq(
  "org.mongodb.scala" %% "mongo-scala-driver" % "4.0.1",
  "com.propensive" %% "magnolia" % "0.12.2",
  "com.chuusai" % "shapeless_2.13" % "2.4.0-M1"
)
