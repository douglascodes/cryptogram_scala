package com.DouglasCodes.Cryptogram_scala.Word


class Letter(val name: Char, val initial: Set[Char] ) {
  def this(name: Char) =
    this(name: Char, ('A' to 'Z').toSet - name)

  private var possibles: Set[Char] =
    initial

  def possibilities: Int =
    possibles.size

  def singular: Boolean =
    possibilities == 1

  def canBe(x: Char): Boolean =
    possibles.contains(x)

  def reassign(p: Set[Char] ) =
    possibles = p

  override def toString: String =
    name + possibles.mkString(": { ", ", ", " }\n")
}

class Word(val name: String ) {
  val value: Set[Letter] = name.toSet.map( (x: Char)=> LetterPool.refer(x) )

  private var possibles: Set[String] = Set(name)

  def possibilities: Int =
    possibles.size

  def singular: Boolean =
    possibilities == 1

  def canBe(x: String): Boolean =
    possibles.contains(x)

  def reassign(p: Set[String] ) =
    possibles = p

  override def toString: String =
    name + "\n" +
      { for (p <- value)
          yield p
      }.mkString("","","") +
     possibles.mkString("{ ", " ", " }")
}

object LetterPool {
   val pool: Hash[Char, Letter] =

}
