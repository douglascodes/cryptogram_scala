package com.DouglasCodes.CryptogramScala


object Solver {
	val d = new Dictionary("Full", "./src/resources/wordlist.txt")
	val smith = new Dictionary("names", "./src/resources/smith.txt")
	val splitREX = """[.?!]"* - """

	val cryptogram: Set[Word] = string2Puzzle("LZ WDPQ P VZZC JZW LAD JEWIL LERD EI RPCD LAD PHYSPETLPTHD ZJ P TDO JWEDTQ LZ WDPQ EL P IDHZTQ LERD EI LZ RDDL PT ZXQ ZTD. - IDXOMT HAPRUEZT")
	
	def string2Puzzle(str: String): Set[Word] = {
		val puzzleParts = str.split(splitREX)
		val cryptoRaw = puzzleParts(0).split(' ').toSet
		val authorRaw = puzzleParts(1).split(' ').toSet
		cryptoRaw.map[Word, Set[Word]]( new Word(_, d) )  ++  authorRaw.map[Word, Set[Word]]( new Word(_, smith ) ) 
	}

	def updateAll(wordList: Set[Word] ): Unit = {
		for (w <- wordList )
			w.update()
	} 
	
	def cycleWords(crypt: Set[Word] ): Unit = {
		for ( x <- crypt.repr )
			for ( y <- crypt ) {
				LetterPool.reassignMap( x + y )
				x.update()
				y.update()
			}
	}

	def singleToStringNum( s: Set[DictionaryEntry] ): String =
		if (s.size == 1) s.mkString else s.size.toString

	def displayAll(wordList: Set[Word] ): Unit = {
		for ( w <- wordList )
			println( w.name + ": " + singleToStringNum(w.referPossibilities) )
	}
	
	def cycle(crypt: Set[Word]): Unit ={
		cycleWords(crypt)
		updateAll(crypt)
		LetterPool.killSingles()
		updateAll(crypt)
	}

}

object Main extends App {
	Solver.displayAll(Solver.cryptogram)	
}







