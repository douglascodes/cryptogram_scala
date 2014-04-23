package com.DouglasCodes.CryptogramScala

class Word(
  val name: String,
  val dictionary: Dictionary,
    var initial: Set[DictionaryEntry] ) extends
    UnitOfLanguage[DictionaryEntry] with
    Entry {

  def this(name: String, dictionary: Dictionary) =
    this(name, dictionary,
    dictionary.matches(new DictionaryEntry(name))
    )

  protected var possibles: Set[DictionaryEntry] =
    initial

  val value =
    for (i: Char <- uName.toVector)
      yield LetterPool.refer(i)


  def update(): Word = {
    possibles = possibles.filter( canBe(_) )
    this
  }

  // Too many side effects to use this. Takes the Values and reassigns the letter objects
  // according to current possibilities. This is superceded by the efficiency of the plus operator.
  // def reverse(): Unit =
  //   for ( rev <- 0 until this.uSize )
  //     this.value(rev).trim({for (p <- possibles )
  //       yield p.name(rev) }.toSet )

  // Combination operator. Returns Map of letter objects and the suggested reassignment Set[Char]
  def +(that: Word): Map[Letter, Set[Char]] = {

    val similarKey: Vector[Letter] = hasSimilar(that)
    val myLocations: Vector[Int] = locationOfLetters(similarKey)
    val thatLocations: Vector[Int] = that.locationOfLetters(similarKey)
    val acceptableCombinations: Set[String] = getStringSet(myLocations) & that.getStringSet(thatLocations)
    // acceptableCombinations
    val temp = for (position <- 0 until myLocations.size )
      yield Map( value( myLocations(position) ) -> { for ( possible <- acceptableCombinations )
          yield possible(position)
        }.toSet)
    temp.foldLeft[Map[Letter, Set[Char]]]( Map() )((B, A) => A ++ B) //Join the maps together

  }

  def hasSimilar(that: Word): Vector[Letter] =
    this.value.filter( that.value.contains(_))

  def locationOfLetters(key: Vector[Letter] ): Vector[Int] =
    {for (l <- key )
      yield this.value.indexOf( l ) }

  def getStringSet(locations: Vector[Int] ): Set[String] =
    for (p <- possibles )
     yield { for ( l <- locations )
     yield {p.uName(l) } }.mkString

  override def canBe( testWord: DictionaryEntry ): Boolean = {
    if ( this.unfits(testWord)  ) return false

    for ( i: Int <- (0 until testWord.uSize) )
      if ( value(i).cantBe(testWord.uName(i)  ) )
        return false
    true
  }

  override def toString: String =
    name + "\n" +
    possibilities + ": possibilities\n" +
    { for (p <- value) yield p }.mkString("","","") +
    possibles.mkString("{ ", " ", " }")

}
