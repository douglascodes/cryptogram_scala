package com.DouglasCodes.CryptogramScala

import org.junit.runner.RunWith
import org.specs2.mutable._

class LetterPoolTest extends SpecificationWithJUnit {
  object LPTester extends LetterPool{

  }

  "LetterPool" should {

    "refer Letter objects where name is referring character" in {
      val v = Vector[Char]('A', 'B', 'C', 'D')
      v.forall( (c: Char) => (
        LPTester.refer(c).name == c ) )
    }

    "throw a NoSuchElementException when char is not in pool" in {
      LPTester(']') must throwA[NoSuchElementException]
    }

    "use Apply or Refer for retrieving Letter objects" in {
      LPTester('H') mustEqual LPTester.refer('H')
    }

  }
}