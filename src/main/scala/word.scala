package com.DouglasCodes.CryptogramScala

class Word(val name: String, val dictionary: Dictionary) extends UnitOfLanguage[DictionaryEntry] with Entry {

  val value =
    for (i: Char <- uName.toVector)
      yield LetterPool.refer(i)

  override protected var possibles: Set[DictionaryEntry] =
    dictionary.matches(this).filter(canBe(_))

  def update(): Unit =
    possibles = possibles.filter( canBe(_) )

  //def +(that: Word): Map[Letter, Set[Char]] =

  def reverse(): Unit =
    for ( rev <- 0 until this.size )
      this.value(rev).trim({for (p <- possibles )
        yield p.name(rev) }.toSet )

  def +(that: Word) = {
    val similarKey: Vector[Letter] = hasSimilar(that)
    val myLocations: Vector[Int] = locationOfLetters(similarKey)
    val thatLocations: Vector[Int] = that.locationOfLetters(similarKey)
    val acceptableCombinations: Set[String] = getStringSet(myLocations) & getStringSet(thatLocations)
    acceptableCombinations
    // getStringSet(myLocations)
// res0: scala.collection.immutable.Set[String] = Set(BREL, ESNT, READ, IAND, CKLE, ANST, RNED, NDAY, CHAT, DOES, TREA, ARCE, SEAL, ULSE, SCET, KENS, MSAT, CERS, THEY, RNES, TEAL, TSAM, SHOT, ULNA, ENSO, SCLE, SELY, TERY, ARFS, ENDA, NROY, BEAR, DSET, CHLY, IRDS, REAL, CROS, TROD, EARD, MOAT, BULE, ANZE, TEAR, NEOL, EUND, RCES, OAMS, UALS, BERS, OMPE, IRNY, UAME, HNES, COAL, CAYS, ERPA, BERY, RSEY, UANT, SLEY, URLY, RNET, GEAT, ONCS, RDOS, ROAM, GLED, GEOT, TLEY, CORE, PURE, ERMA, PARL, BLET, ODGY, PULE, PERT, TORS, ARMY, TRES, BUED, ARVY, UMLE, PNOS, TERS, CSLE, SOAR, GURA, RDOY, NLEY, SNET, GNET, AFLY, HOED, ILDS, PLAS, ASTY, WLEY, ALGY, IRNS, ARNS, ACLE, PUTS, RNEA, GAOL, HEAD, RLET, IOMS, NOES, EWLY, AUDS, TRAY, BRAS, PLAY, ANLE, CKLY, CURS, ORGE, DLEY, MRAI, IEND, HROT, PERY, GERS, RC...
// scala> res0.size
// res1: Int = 249

// scala> li.possibilities
// res2: Int = 7555

// scala> fi.possibilities
// res3: Int = 33

// scala> val letter0 = { for ( w <- res0)
//      | yield w(2) }
// letter0: scala.collection.immutable.Set[Char] = Set(E, N, T, Y, F, A, M, G, V, L, B, P, C, H, R, O, D, Z, S)


  }

  def getStringSet(locations: Vector[Int] ): Set[String] =
    for (p <- possibles )
     yield { for ( l <- locations )
     yield {p.uName(l) } }.mkString

  def locationOfLetters(key: Vector[Letter] ): Vector[Int] =
    {for (l <- key )
      yield this.value.indexWhere(_ == l) }

  def hasSimilar(that: Word): Vector[Letter] =
    value.filter( that.value.contains(_))

  override def canBe( testWord: DictionaryEntry ): Boolean = {
    for ( i: Int <- (0 until testWord.uSize) )
      if ( !(value(i).canBe(testWord.uName(i) ) ) )
        return false
    true
  }

  override def toString: String =
    name + "\n" +
    possibilities + ": possibilities\n" +
    { for (p <- value) yield p }.mkString("","","") +
    possibles.mkString("{ ", " ", " }")

}
