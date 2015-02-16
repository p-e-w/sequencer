libraryDependencies ++= Seq(
	"com.github.scopt" %% "scopt" % "3.3.0",
	"log4j" % "log4j" % "1.2.17",
	"axelclk" % "symja" % "2015-02-08" from "https://bitbucket.org/axelclk/symja_android_library/downloads/symja-2015-02-08.jar"
)

resolvers += Resolver.sonatypeRepo("public")

assemblyJarName in assembly := "sequencer.jar"

