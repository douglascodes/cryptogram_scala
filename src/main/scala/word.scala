package com.DouglasCodes.CryptogramScala.Word
import com.DouglasCodes.CryptogramScala.Dictionary
import com.DouglasCodes.CryptogramScala.LetterPool

class Word(val name: String, val dictionary: Dictionary ) extends UnitOfLanguage[String] {
  val uName: String = OrderedUniqueString(name)
  val value = for (i: Char <- uName.toVector)
    yield LetterPool.refer(i)

  protected var possibles: Set[String] =
    dictionary.words(name.size).filter( (a) => a._2 == uName.size ).keySet

  override def toString: String =
    name + "\n" +
      { for (p <- value)
          yield p
      }.mkString("","","") +
     possibles.mkString("{ ", " ", " }")
}