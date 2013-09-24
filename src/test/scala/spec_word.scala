package com.DouglasCodes.CryptogramScala
import org.junit.runner.RunWith
// import org.specs2._
import org.specs2.mutable._

class WordTest extends SpecificationWithJUnit { override def is = s2"""

Word objects should
  return a similarity vector when combined                        $word1
  should point to location letters with locationOfLetters         $word2
  get possiblity strings from locations Vector                    $word3

'B'
  should have two possibilities                                   $b1

Five letter words with unique chars
  must have 200 possibilities                                     $l1
  must have pattern = "01234"                                     $l2
  have uName equal to Name                                        $l3

'HELLO' should
 	exist as a Word object  																        $w1
 	contain a set of possibilities													        $w2
 	not be singular																					        $w3
 	'fit' each possibility																	        $w4
  accept reassignment of Set[DictionaryEntry]                     $w5
  return its value when running hasSimilar with itself            $w6

'SAFETY' should
 	initialize with a determined set 												        $saf1
	have 1 possibilities 																		        $saf2
  not be singular                                                 $saf3
  get reversed 'string set' should get inverted possibilities     $saf4

'COUPLE'
  is singular after update                                        $coup1

'SAFETY' combined with 'HELLO' should
  return a Map[Letter, Set[Char] ]                                $ws1
  not return an empty Map                                         $ws2
  have the letter object 'E' as a key                             $ws3
  contain a single pair                                           $ws4

 	                                                                 """
	object wordSetup  {
	  val exampleDictionary = { new Dictionary("Full", "./src/resources/wordlist.txt") }
	  lazy val hello = { new Word("HELLO", exampleDictionary) }
    lazy val letter5 = { new Word("ABCDE", exampleDictionary) }
		lazy val b = { new Word("B", exampleDictionary) }
    lazy val safety = { new Word("SAFETY", exampleDictionary, Set(new DictionaryEntry("COUPLE"), new DictionaryEntry("DANCER") ) ) }
    lazy val couple = { new Word("COUPLE", exampleDictionary, Set(new DictionaryEntry("SAFETY"), new DictionaryEntry("PEPPER")  ) ) }
		}

  val h = wordSetup.hello
  val b = wordSetup.b
  val c = wordSetup.couple
  val s = wordSetup.safety
  val l5 = wordSetup.letter5

  def w1 = h.isInstanceOf[Word]
  def w2 = h.possibilities must be_>(0)
  def w3 = !(h.singular)
  def w4 = h.referPossibilities.forall(_ fits h)
  def w5 = {
    val newSet = h.referPossibilities.take(2)
    h.reassign(newSet)
    h.possibilities must be_==(2)
  }
  def w6 = h.hasSimilar(h) mustEqual h.value

  def b1 = b.possibilities must be_==(2)

  def l1 = l5.update().possibilities must be_>=(5000)
  def l2 = l5.pattern must beEqualTo("01234")
  def l3 = l5.uName must beEqualTo( l5.name )

  def saf1 = s must haveClass[Word]
  def saf2 = s.possibilities must be_==(2)
  def saf3 = !s.singular
  def saf4 = s.getStringSet( Vector(5,4,3,2,1,0) ) must beEqualTo(Set("ELPUOC", "RECNAD"))

  def coup1 = {
    c.update()
    c.singular
  }

  def ws1 = ( s + h) must haveSuperclass[Map[Letter, Set[Char]]]
  def ws2 = ( s + h) must not beEmpty
  def ws3 = ( s + h) must haveKey(LetterPool.refer('E'))
  def ws4 = ( s + h).size mustEqual 1
  def ws5 = ( s + h).apply(LetterPool('E')).size mustEqual 1

  def word1 = s.hasSimilar(h) mustEqual Vector(LetterPool('E'))
  def word2 = ( h.locationOfLetters(Vector(LetterPool('E') ) ) mustEqual Vector(1) ) and
    (s.locationOfLetters(Vector(LetterPool('E') ) ) mustEqual Vector(3))
  def word3 = s.getStringSet( Vector(3) ) mustEqual Set("P", "C")

}