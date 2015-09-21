logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("play" % "sbt-plugin" % "2.1.3")

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "reactivemongo" % "0.11.7"
)
