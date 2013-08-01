package com.DouglasCodes.CryptogramScala
import org.junit.runner.RunWith
import org.specs2.mutable._

class LetterTest extends SpecificationWithJUnit {

  "Letter" should {

    "be instantiated with a set, or without a set of possibles" in {
      val h = new Letter('H')
      h.possibilities must be_==(25)
      val b = new Letter('B', Set('V','R'))
      b.possibilities must be_==(2)
    }

    "not include self if instantiated without possibles" in {
      val h = new Letter('H')
      h.canBe('H') must beFalse
    }

    "can include self if instantiated with possibles" in {
      val h = new Letter('H', Set('H','K','L'))
      h.canBe('H') must beTrue
    }

    "accept reassignment of a set" in {
      val h = new Letter('H')
      h.reassign(Set('H','K','L'))
      h.possibilities must be_==(3)
    }

  }
}