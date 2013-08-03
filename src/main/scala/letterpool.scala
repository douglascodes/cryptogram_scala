package com.DouglasCodes.CryptogramScala

object LetterPool {
  private val alphabet: Set[Char] = ('A' to 'Z').toSet
  private val pool: Map[Char, Letter] =
  { for ( a <- alphabet)
    yield (a, new Letter(a) )}.toMap

  def refer(x: Char): Letter =
    pool(x)

}
