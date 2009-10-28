package com.codahale.bert.tests

import com.codahale.bert.Tuple

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
 
@RunWith(classOf[JUnitRunner])
class TupleSpec extends Spec with MustMatchers {
  val t = Tuple(1, 'a, "yay")
  
  describe("A tuple") {
    it("is human readable") {
      t.toString must equal ("""Tuple(1,'a,yay)""")
    }
    
    it("allows for pattern matching individual elements") {
      t match {
        case Tuple(_, _, s: String) => s must equal ("yay")
        case e => fail("can't parse " + e)
      }
    }
    
    it("iterates through all elements") {
      val l = (for (i <- t) yield i).toList
      l must equal (List(1, 'a, "yay"))
    }
  }
}
