package com.DouglasCodes.Cryptograma_scala.Letter

class Letter(val name: Char ) {
  private var possibles: Set[Char] =
    if ( "'-".contains(name) )
      Set(name)
    else
      ('A' to 'Z').toSet - name

  def possibilities: Int =
    possibles.size

  def singular: Boolean =
    possibilities == 1

  def canBe(x: Char): Boolean =
    possibles.contains(x)

  def reassign(p: Set[Char] ) =
    possibles = p

  override def toString: String =
    name + possibles.mkString(": { ", ", ", " }")
}
