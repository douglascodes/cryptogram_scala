package com.DouglasCodes.CryptogramScala.Letter
import com.DouglasCodes.CryptogramScala.UnitOfLanguage

class Letter(val name: Char, val initial: Set[Char] ) extends UnitOfLanguage[Char] {
  def this(name: Char) =
    this(name: Char, ('A' to 'Z').toSet - name)

  protected var possibles: Set[Char] =
    initial

  override def toString: String =
    name + possibles.mkString(": { ", ", ", " }\n")
}
