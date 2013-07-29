package com.DouglasCodes.CryptogramScala.Dictionary
import com.DouglasCodes.CryptogramScala.UnitOfLanguage
import scala.collection.immutable.HashMap
import scala.io.Source

class Dictionary(val name: String, srcfile: String ){
  var words =
    { for (l <- Source.fromFile(srcfile).getLines() )
      yield (l -> Tuple2[Int, String](OrderedUniqueString(l).size,
      NumericPatternOfString(l) ) ) }.toMap.
      groupBy(f = (a) => a._1.size  )

  override def toString: String =
    name + ": " +
    words.foldLeft(0)( (a, b) =>
    a + b._2.size) + " words."
}