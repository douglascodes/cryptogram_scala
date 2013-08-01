package com.DouglasCodes.CryptogramScala
import scala.io.Source

class DictionaryEntry(val name: String) {
  val uName: String = CryptogramSolver.OrderedUniqueString(name)
  val pattern: String = CryptogramSolver.NumericPatternOfString(name)
  def size: Int =
    name.size
  def uSize: Int =
    uName.size
  def mkString: String=
    name
}
class Dictionary(val name: String, srcfile: String ) {
  var words: Map[Int, Set[DictionaryEntry]] =
    { for (l <- Source.fromFile(srcfile).getLines() )
      yield new DictionaryEntry(l) }.toSet[DictionaryEntry].groupBy(_.size )

  override def toString: String =
    name + ": " +
    words.foldLeft(0)( (a, b) =>
    a + b._2.size) + " words."
}
