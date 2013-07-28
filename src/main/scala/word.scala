package com.DouglasCodes.CryptogramScala.Word
import com.DouglasCodes.CryptogramScala.LetterPool

class Word(val name: String ) extends UnitOfLanguage[String] {
  val uName: String = name.foldLeft(new String)((a, b) => { if (a.contains(b)) a else a + b} )
  val value = for (i: Char <- uName.toVector)
    yield LetterPool.refer(i)

  protected var possibles: Set[String] = Set(name)

  override def toString: String =
    name + "\n" +
      { for (p <- value)
          yield p
      }.mkString("","","") +
     possibles.mkString("{ ", " ", " }")
}