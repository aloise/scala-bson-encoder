name := "magnolia-bson"

version := "0.2"

ThisBuild / scalaVersion := "2.13.3"

lazy val bson = (project in file("bson"))

lazy val test = (project in file("test")).dependsOn(bson).settings(
  libraryDependencies ++= Seq(
    "org.mongodb.scala" %% "mongo-scala-driver" % "4.0.3"
  )
)

mainClass in (Compile, run) := Some("name.aloise.Main")


