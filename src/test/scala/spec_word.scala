package com.DouglasCodes.CryptogramScala
import org.junit.runner.RunWith
// import org.specs2._
import org.specs2.mutable._

class WordTest extends SpecificationWithJUnit { override def is = s2"""

 	'HELLO' should
 		exist as a Word object  																$w1
 		contain a set of possibilities													$w2
 		not be singular																					$w3
 		'fit' each possibility																	$w4
 		accept reassignment of Set[DictionaryEntry]							$w5

 	'SAFETY' should
 		initialize with a determined set 												$saf1
		have 1 possibilities 																		$saf2
    be singular                                             $saf3
    is empty after update                                   $saf5
		
 	                                                                 """
	object wordSetup  {
	  val exampleDictionary = { new Dictionary("Full", "./src/resources/wordlist.txt") }
	  lazy val hello = { new Word("HELLO", exampleDictionary) }
		lazy val a = { new Word("A", exampleDictionary) }
		lazy val safety = { new Word("SAFETY", exampleDictionary, Set(new DictionaryEntry("CUDDLE")) ) }
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
  def saf2 = s.possibilities must be_==(1)
  def saf3 = s.singular

  def saf5 = {
    s.update()
    s.possibilities must be_==(0)
  }

}