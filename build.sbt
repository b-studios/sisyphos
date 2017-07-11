import com.github.retronym.SbtOneJar._

oneJarSettings

resolvers += Resolver.url("typesafe", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)

libraryDependencies += "org.scala-sbt" %% "io" % "0.13.9"

scalaVersion := "2.11.7"
