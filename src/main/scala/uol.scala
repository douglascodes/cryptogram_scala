package com.DouglasCodes.CryptogramScala.UnitOfLanguage

trait UnitOfLanguage[T] {
    protected var possibles: Set[T]

    def possibilities: Int =
      possibles.size

    def singular: Boolean =
      possibilities == 1

    def canBe(x: T): Boolean =
      possibles.contains(x)

    def reassign(p: Set[T] ) =
      possibles = p

}
