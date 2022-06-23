libraryDependencies += "org.scala-sbt" %% "io" % "1.6.0"

scalaVersion := "2.13.8"

// simply run `sbt assembly` and it will bundle the jar file somewhere in `target/scala-VERSION/enter.jar`.
assembly / mainClass := Some("Enter")
assembly / assemblyJarName := "enter.jar"
