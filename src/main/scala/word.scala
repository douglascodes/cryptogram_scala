package com.DouglasCodes.CryptogramScala

class Word(val name: String, val dictionary: Dictionary) extends UnitOfLanguage[DictionaryEntry] with Entry {

  val value =
    for (i: Char <- uName.toVector)
      yield LetterPool.refer(i)

  override protected var possibles: Set[DictionaryEntry] =
    dictionary.matches(this)

  def update(): Unit =
    possibles = possibles.filter( this.canBe(_) )

  override def canBe( testWord: DictionaryEntry ): Boolean = {
    for ( i: Int <- (0 until testWord.uSize) )
      if ( !(value(i).canBe(testWord.uName(i) ) ) )
        return false
    true
  }

  override def toString: String =
    name + "\n" + { for (p <- value) yield p }.mkString("","","") + possibles.mkString("{ ", " ", " }")

}
