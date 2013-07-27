package com.DouglasCodes.Cryptogram_scala.Word

object LetterPool {
  private val alphabet: Set[Char] = ('A' to 'Z').toSet
  private val pool: Map[Char, Letter] =
  { for ( a <- alphabet)
    yield (a, new Letter(a) )}.toMap

  def refer(x: Char): Letter =
    pool(x)
}

trait UnitOfLanguage[T] {
    var possibles: Set[T]

    def possibilities: Int =
      possibles.size

    def singular: Boolean =
      possibilities == 1

    def canBe(x: T): Boolean =
      possibles.contains(x)

    def reassign(p: Set[T] ) =
      possibles = p

}

class Letter(val name: Char, val initial: Set[Char] ) extends UnitOfLanguage[Char] {
  def this(name: Char) =
    this(name: Char, ('A' to 'Z').toSet - name)

  var possibles: Set[Char] =
    initial

  override def toString: String =
    name + possibles.mkString(": { ", ", ", " }\n")
}

class Word(val name: String ) extends UnitOfLanguage[String] {
  val value = for (i: Char <- name.toSet)
    yield LetterPool.refer(i)

  var possibles: Set[String] = Set(name)

  override def toString: String =
    name + "\n" +
      { for (p <- value)
          yield p
      }.mkString("","","") +
     possibles.mkString("{ ", " ", " }")
}