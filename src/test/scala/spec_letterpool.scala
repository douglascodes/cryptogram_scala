package com.DouglasCodes.CryptogramScala

import org.junit.runner.RunWith
import org.specs2.mutable._

class LetterPoolTest extends SpecificationWithJUnit {

  "LetterPool" should {

    "refer Letter objects where name is referring character" in {
      val v = Vector[Char]('A', 'B', 'C', 'D')        
      v.forall( (c: Char) => (
        LetterPool.refer(c).name == c ) )
    }

  }
}