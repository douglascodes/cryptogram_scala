package com.DouglasCodes.CryptogramScala

class Word(val name: String, val dictionary: Dictionary) extends UnitOfLanguage[DictionaryEntry] with Entry {

  val value =
    for (i: Char <- name.toVector)
      yield LetterPool.refer(i)

  override protected var possibles: Set[DictionaryEntry] =
    dictionary.matches(this).filter(canBe(_))

  def update(): Unit =
    possibles = possibles.filter( canBe(_) )

  //def +(that: Word): Map[Letter, Set[Char]] =

  def reverse(): Unit =
    for ( rev <- 0 until this.size )
      this.value(rev).trim({for (p <- possibles )
        yield p.name(rev) }.toSet )


  override def canBe( testWord: DictionaryEntry ): Boolean = {
    for ( i: Int <- (0 until testWord.size) )
      if ( !(value(i).canBe(testWord.name(i) ) ) )
        return false
    true
  }

  override def toString: String =
    name + "\n" +
    possibilities + ": possibilities\n" +
    { for (p <- value) yield p }.mkString("","","") +
    possibles.mkString("{ ", " ", " }")

}
