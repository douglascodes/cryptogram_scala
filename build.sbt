retrieveManaged := true

initialCommands in console := """import com.DouglasCodes.CryptogramScala._, com.mysql.jdbc.Driver
"""

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-compiler" % "2.10.2",
		"mysql" % "mysql-connector-java" % "5.1.26",
    "org.specs2" % "specs2_2.10" % "2.0-RC1",
		"junit" % "junit" % "4.7"
		)
