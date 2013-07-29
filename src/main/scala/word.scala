package com.DouglasCodes.CryptogramScala.Word
import com.DouglasCodes.CryptogramScala.Dictionary
import com.DouglasCodes.CryptogramScala.LetterPool

class Word(val name: String, val dictionary: Dictionary ) extends UnitOfLanguage[String] {
  val pattern: String = NumericPatternOfString(name)
  val uName: String = OrderedUniqueString(name)
  val value =
    for (i: Char <- uName.toVector)
      yield LetterPool.refer(i)

  protected var possibles: Set[String] =
    dictionary.words(name.size).filter( (a) =>
      a._2._1 == uName.size &&
      a._2._2 == pattern ).keySet

  def update(): Unit =
    possibles = possibles.filter( (testWord: String) => this.canBe(OrderedUniqueString(testWord) ) )

  override def canBe( testWord: String ): Boolean = {
    if ( testWord.size != this.uName.size )
      return false
    for ( i: Int <- (0 until testWord.size) )
      if ( !(value(i).canBe(testWord(i) ) ) )
        return false
    return true
  }

  override def toString: String =
    name + "\n" + { for (p <- value) yield p }.mkString("","","") + possibles.mkString("{ ", " ", " }")

}
