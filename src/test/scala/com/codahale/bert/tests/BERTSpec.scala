package com.codahale.bert.tests

import com.codahale.bert.BERT

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
 
@RunWith(classOf[JUnitRunner])
class BERTSpec extends Spec with MustMatchers {
  describe("an encoded 8-bit integer") {
    it("is the same value as the Ruby gem") {
      BERT(22).toList must equal (List(131, 97, 22))
    }
    
    it("decodes to be an 8-bit integer") {
      BERT.parse(BERT(22).toArray) match {
        case BERT(i: Int) => i must equal (22)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded 16-bit integer") {
    it("is the same value as the Ruby gem") {
      BERT(0xBAD).toList must equal (List(131, 98, 0, 0, 11, 173))
    }
    
    it("decodes to be an 16-bit integer") {
      BERT.parse(BERT(0xBAD).toArray) match {
        case BERT(i: Int) => i must equal (0xBAD)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded, unsigned 32-bit integer") {
    it("is the same value as the Ruby gem") {
      BERT(0xDECAFBADL).toList must equal (List(131, 110, 4, 0, 173, 251, 202, 222))
    }
    
    it("decodes to be an unsigned 32-bit integer") {
      BERT.parse(BERT(0xDECAFBADL).toArray) match {
        case BERT(i: Long) => i must equal (0xDECAFBADL)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded, unsigned 64-bit integer") {
    it("is the same value as the Ruby gem") {
      BERT(0x00FDECAFBADL).toList must equal (
        List(131, 110, 5, 0, 173, 251, 202, 222, 15)
      )
    }
    
    it("decodes to be an unsigned 64-bit integer") {
      BERT.parse(BERT(0x00FDECAFBADL).toArray) match {
        case BERT(i: Long) => i must equal (0x00FDECAFBADL)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded string") {
    it("is the same value as the Ruby gem") {
      BERT("Bert & Ernie").toList must equal (
        List(131, 109, 0, 0, 0, 12, 66, 101, 114, 116, 32, 38, 32, 69, 114,
             110, 105, 101)
      )
    }
    
    it("decodes to be a string") {
      BERT.parse(BERT("Bert & Ernie").toArray) match {
        case BERT(s: String) => s must equal ("Bert & Ernie")
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded symbol") {
    it("is the same value as the Ruby gem") {
      BERT('bert).toList must equal (List(131, 100, 0, 4, 98, 101, 114, 116))
    }
    
    it("decodes to be a symbol") {
      BERT.parse(BERT('bert).toArray) match {
        case BERT(s: Symbol) => s must equal ('bert)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded double") {
    it("is the same value as the Ruby gem") {
      BERT(0.22).toList must equal (
        List(131, 99, 50, 46, 50, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
             48, 48, 48, 101, 45, 48, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      )
    }
    
    it("decodes to be a double") {
      BERT.parse(BERT(0.22).toArray) match {
        case BERT(d: Double) => d must equal (0.22)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded boolean true") {
    it("is the same value as the Ruby gem") {
      BERT(true).toList must equal (
        List(131, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 114,
             117, 101)
      )
    }
    
    it("decodes to be a boolean true") {
      BERT.parse(BERT(true).toArray) match {
        case BERT(b: Boolean) => b must equal (true)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded boolean false") {
    it("is the same value as the Ruby gem") {
      BERT(false).toList must equal (
        List(131, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 5, 102, 97,
             108, 115, 101)
      )
    }
    
    it("decodes to be a boolean false") {
      BERT.parse(BERT(false).toArray) match {
        case BERT(b: Boolean) => b must equal (false)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded tuple") {
    it("is the same value as the Ruby gem") {
      BERT(1, 'two, "three").toList must equal (
        List(131, 104, 3, 97, 1, 100, 0, 3, 116, 119, 111, 109, 0, 0, 0, 5,
             116, 104, 114, 101, 101)
      )
    }
    
    it("decodes to be a tuple") {
      BERT.parse(BERT(1, 'two, "three").toArray) match {
        case BERT(Tuple(i: Int, a: Symbol, s: String)) =>
          i must equal (1)
          a must equal ('two)
          s must equal ("three")
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded list") {
    it("is the same value as the Ruby gem") {
      BERT(List(1, 2, 3)).toList must equal (
        List(131, 108, 0, 0, 0, 3, 97, 1, 97, 2, 97, 3, 106)
      )
    }
    
    it("decodes to be a list") {
      BERT.parse(BERT(List(1, 2, 3)).toArray) match {
        case BERT(List(a, b, c)) =>
          a must equal (1)
          b must equal (2)
          c must equal (3)
        case e => fail("unable to decode: " + e)
      }
    }
  }
  
  describe("an encoded empty list") {
    it("is the same value as the Ruby gem") {
      BERT(Nil).toList must equal (List(131, 106))
    }
    
    it("decodes to be an empty list") {
      BERT.parse(BERT(Nil).toArray) match {
        case BERT(x) => x must equal (Nil)
      }
    }
  }
  
  describe("an encoded null") {
    it("is the same value as the Ruby gem") {
      BERT(null).toList must equal (
        List(131, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 3, 110,
             105, 108)
      )
    }
    
    it("decodes to be a null value") {
      BERT.parse(BERT(null).toArray) match {
        case BERT(x) => x must equal (null)
      }
    }
  }
  
  describe("an encoded map") {
    it("is the same value as the Ruby gem") {
      BERT(Map(1 -> 2, 3 -> 4)).toList must equal (
        List(131, 104, 3, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 100, 105,
             99, 116, 108, 0, 0, 0, 2, 104, 2, 97, 1, 97, 2, 104, 2, 97, 3,
             97, 4, 106)
      )
    }
    
    it("decodes to be a map") {
      BERT.parse(BERT(Map(1 -> 2, 3 -> 4)).toArray) match {
        case BERT(x) => x must equal (Map(1 -> 2, 3 -> 4))
      }
    }
  }
  
  describe("a BERT without a valid header") {
    it("throws an EncodingException") {
      val badData: Array[Byte] = Array(1, 2, 3)
      evaluating { BERT.parse(badData) } must produce [EncodingException]
    }
  }
  
  describe("a BERT with an unknown term") {
    it("throws an EncodingException") {
      val badData: Array[Byte] = Array(131, 3, 0, 0).map { _.byteValue }
      evaluating { BERT.parse(badData) } must produce [EncodingException]
    }
  }
  
  // TODO: bigint
  // TODO: really big int
  // TODO: large tuple
  // TODO: slightly larger int than normal
}
