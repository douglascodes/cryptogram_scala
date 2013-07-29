package com.DouglasCodes.CryptogramScala.UnitOfLanguage

trait UnitOfLanguage[T] {
    protected var possibles: Set[T]

    def possibilities: Int =
      possibles.size

    def singular: Boolean =
      possibilities == 1

    def canBe(x: T): Boolean =
      possibles.contains(x)

    def reassign(p: Set[T] ) =
      possibles = p

}

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