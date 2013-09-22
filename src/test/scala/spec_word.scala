package com.DouglasCodes.CryptogramScala
import org.junit.runner.RunWith
// import org.specs2._
import org.specs2.mutable._

class WordTest extends SpecificationWithJUnit { override def is = s2"""

'HELLO' should
 	exist as a Word object  																        $w1
 	contain a set of possibilities													        $w2
 	not be singular																					        $w3
 	'fit' each possibility																	        $w4
 	accept reassignment of Set[DictionaryEntry]							        $w5

'SAFETY' should
 	initialize with a determined set 												        $saf1
	have 1 possibilities 																		        $saf2
  not be singular                                                 $saf3
  is singular after update                                        $saf5

'SAFETY' combined with 'HELLO' should
  return a Map[Letter, Set[Char] ]                                $ws1
  not return an empty Map                                         $ws2
  have the letter object 'E' as a key                             $ws3
  contain a single pair                                           $ws4

 	                                                                 """
	object wordSetup  {
	  val exampleDictionary = { new Dictionary("Full", "./src/resources/wordlist.txt") }
	  lazy val hello = { new Word("HELLO", exampleDictionary) }
		lazy val a = { new Word("A", exampleDictionary) }
		lazy val safety = { new Word("SAFETY", exampleDictionary, Set(new DictionaryEntry("COUPLE"), new DictionaryEntry("PEPPER") ) ) }
		}

  val h = wordSetup.hello
  val a = wordSetup.a
  val s = wordSetup.safety

  def w1 = h.isInstanceOf[Word]
  def w2 = h.possibilities must be_>(0)
  def w3 = !(h.singular)
  def w4 = h.referPossibilities.forall(_ fits h)
  def w5 = {
  	val newSet = h.referPossibilities.take(2)
  	h.reassign(newSet)
  	h.possibilities must be_==(2)
  }

  def saf1 = s must haveClass[Word]
  def saf2 = s.possibilities must be_==(2)
  def saf3 = !s.singular
  def saf5 = {
    val ss = new Word("SAFETY", wordSetup.exampleDictionary, Set(new DictionaryEntry("COUPLE"), new DictionaryEntry("PEPPER") ) )
    ss.update()
    ss.singular
  }

  def ws1 = ( s.update() + h) must haveSuperclass[Map[Letter, Set[Char]]]
  def ws2 = ( s.update() + h) must not beEmpty
  def ws3 = ( s.update() + h) must haveKey(LetterPool.refer('E'))
  def ws4 = ( s.update() + h).size mustEqual 1
  def ws5 = ( s.update() + h).apply(LetterPool('E')).size mustEqual 1

}