package com.DouglasCodes.CryptogramScala

object CryptogramSolver extends App {

  val OrderedUniqueString: Function1[String, String] =
    (name: String) =>
      name.foldLeft(new String)((a, b) =>
        { if (a.contains(b)) a else a + b} )

  val NumericPatternOfString: Function1[String, String] =
    (str: String) => {
      var s: String = str
      val g: Map[Char, Int] = { for ( c: Char <- s)
        yield (c -> s.indexOf(c)) }.toMap
      for ( (k,v) <- g )
        s = s.replaceAll(k.toString, v.toString)
      s
    }

}
