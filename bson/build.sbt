name := "magnolia-bson"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.mongodb" % "bson" % "4.1.0",
  "com.propensive" %% "magnolia" % "0.17.0",
  "org.typelevel" %% "cats-core" % "2.2.0-RC3",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)
