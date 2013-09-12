package com.DouglasCodes.CryptogramScala

object LetterPool {
  private val alphabet: Set[Char] = ('A' to 'Z').toSet
  private val pool: Map[Char, Letter] =
  { for ( a <- alphabet)
    yield (a, new Letter(a) )}.toMap +
    (39.toChar -> new Letter(39.toChar, Set(39.toChar))) 

  def refer(x: Char): Letter =
    pool(x)

  def reassignMap(assignment: Map[Letter, Set[Char]] ): Unit = {
    for ((l, assign) <- assignment)
      if ( assign.isEmpty != true )
        l.reassign(assign)
  }

  def killSingles(): Unit = {
    val singleOrNot = pool.values.toSet.groupBy[Boolean]( _.singular )
    for ( notSingle <- singleOrNot(false) )
      for ( single <- singleOrNot(true) )
        notSingle.removePossibilities(single.referPossibilities)
  }

}
