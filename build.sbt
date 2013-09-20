retrieveManaged := true

initialCommands in console := """import com.DouglasCodes.CryptogramScala._, ru.circumflex.orm._"""
     
libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-compiler" % "2.10.2",
//	"org.scalatest" % "scalatest_2.10" % "2.0.M8",
	    "ru.circumflex" % "circumflex-orm" % "2.1",
    "org.specs2" % "specs2_2.10" % "2.0-RC1",
		"junit" % "junit" % "4.7"
		)
