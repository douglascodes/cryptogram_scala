package com.DouglasCodes.CryptogramScala
import scala.io.Source

trait Entry {
  val name: String
  val OrderedUniqueString: Function1[String, String] = {
    (str: String) =>
      str.foldLeft(new String)((a, b) =>
        { if (a.contains(b)) a else a + b} ) }

  val NumericPatternOfString: Function1[String, String] =
    (str: String) => {
      var s: String = str
      val g: Map[Char, Int] = { for ( c: Char <- s)
        yield (c -> s.indexOf(c)) }.toMap
      for ( (k,v) <- g )
        s = s.replaceAll(k.toString, v.toString)
      s
    }

  val uName: String = OrderedUniqueString(name)
  val pattern: String = NumericPatternOfString(name)
  val size: Int = name.size
  val uSize: Int = uName.size

  def fits(that: Entry): Boolean = {
    this.uSize == that.uSize &&
    this.pattern == that.pattern &&
    this.size == that.size
  }

  override def toString: String = name
}

class DictionaryEntry(val name: String ) extends Entry {

}

class Dictionary(val name: String, srcfile: String ) {

  val words: Map[Int, Set[DictionaryEntry]] =
    { for (l <- Source.fromFile(srcfile).getLines() )
      yield (new DictionaryEntry(l)) }.toSet.groupBy( _.size )

  def matches(entry: Entry): Set[DictionaryEntry] = words(entry.size).filter(entry.fits(_) )



  override def toString: String =
    name + " dictionary: " + { for ( s <- words.values)
      yield s.size }.sum + " entries."
}
