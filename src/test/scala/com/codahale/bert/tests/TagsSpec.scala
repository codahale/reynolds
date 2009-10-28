package com.codahale.bert.tests

import com.codahale.bert.Tags

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Spec
import org.scalatest.matchers.MustMatchers
 
@RunWith(classOf[JUnitRunner])
class TagsSpec extends Spec with MustMatchers {
  describe("a BERT header") {
    it("is 131") {
      Tags.Header.id must equal (131)
    }
  }
  
  describe("a BERT BINARY_EXT") {
    it("is 109") {
      Tags.Binary.id must equal (109)
    }
  }
  
  describe("a BERT SMALL_INTEGER_EXT") {
    it("is 97") {
      Tags.SmallInteger.id must equal (97)
    }
  }
  
  describe("a BERT INTEGER_EXT") {
    it("is 98") {
      Tags.Integer.id must equal (98)
    }
  }
  
  describe("a BERT SMALL_BIG_EXT") {
    it("is 110") {
      Tags.SmallBigInt.id must equal (110)
    }
  }
  
  describe("a BERT LARGE_BIG_EXT") {
    it("is 111") {
      Tags.LargeBigInt.id must equal (111)
    }
  }
  
  describe("a BERT ATOM_EXT") {
    it("is 100") {
      Tags.Atom.id must equal (100)
    }
  }
  
  describe("a BERT FLOAT_EXT") {
    it("is 99") {
      Tags.Float.id must equal (99)
    }
  }
  
  describe("a BERT SMALL_TUPLE_EXT") {
    it("is 104") {
      Tags.SmallTuple.id must equal (104)
    }
  }
  
  describe("a BERT LARGE_TUPLE_EXT") {
    it("is 105") {
      Tags.LargeTuple.id must equal (105)
    }
  }
  
  describe("a BERT LIST_EXT") {
    it("is 108") {
      Tags.List.id must equal (108)
    }
  }
  
  describe("a BERT NIL_EXT") {
    it("is 106") {
      Tags.Nil.id must equal (106)
    }
  }
}
