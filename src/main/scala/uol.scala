package com.DouglasCodes.CryptogramScala

trait UnitOfLanguage[T] {
    protected var possibles: Set[T]

    def possibilities: Int =
      possibles.size

    def singular: Boolean =
      possibilities == 1

    def canBe(x: T): Boolean =
      possibles.contains(x)

    def reassign(p: Set[T] ): Int = {
      possibles = p
      possibilities
    }

    def trim(p: Set[T] ): Int = {
      reassign( possibles & p )
      possibilities
    }

    def removePossibility(p: T ): Int = {
      reassign( possibles - p )
      possibilities
    }

    def removePossibilities(p: Set[T] ): Int = {
      reassign( possibles -- p )
      possibilities
    }

    def referPossibilities: Set[T] =
      possibles
}

