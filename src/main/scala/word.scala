package com.DouglasCodes.CryptogramScala

class Word(val name: String, val dictionary: Dictionary ) extends UnitOfLanguage[DictionaryEntry] {
  val pattern: String = CryptogramSolver.NumericPatternOfString(name)
  val uName: String = CryptogramSolver.OrderedUniqueString(name)
  val uSize = uName.size
  val value =
    for (i: Char <- uName.toVector)
      yield LetterPool.refer(i)

  override protected var possibles: Set[DictionaryEntry] =
    dictionary.words(name.size).filter( (a) =>
      a.uSize == uSize &&
      a.pattern == pattern )

  def update(): Unit =
    possibles = possibles.filter( ( testWord ) => this.canBe(testWord) )

  override def canBe( testWord: DictionaryEntry ): Boolean = {
    if ( testWord.size != this.uName.size )
      return false
    for ( i: Int <- (0 until testWord.size) )
      if ( !(value(i).canBe(testWord.name(i) ) ) )
        return false
    true
  }

  override def toString: String =
    name + "\n" + { for (p <- value) yield p }.mkString("","","") + possibles.mkString("{ ", " ", " }")

}
