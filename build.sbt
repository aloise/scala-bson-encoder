name := "magnolia-bson"

version := "0.1"

ThisBuild / scalaVersion := "2.13.1"

lazy val bson = (project in file("bson"))

lazy val root = (project in file(".")).dependsOn(bson).settings(
  libraryDependencies ++= Seq(
    "org.mongodb.scala" %% "mongo-scala-driver" % "4.0.1"
  )
)


