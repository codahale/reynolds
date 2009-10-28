package com.codahale.bert

import java.io.{InputStream, OutputStream, ByteArrayOutputStream,
                ByteArrayInputStream, DataOutputStream, DataInputStream,
                IOException}
import java.util.NoSuchElementException

class EncodingException(msg: String) extends IOException(msg)

private object Tags extends Enumeration {
  type Tags = Value
  val Header = Value(131)
  val Binary = Value(109)
  val SmallInteger = Value(97)
  val Integer = Value(98)
  val SmallBigInt = Value(110)
  val LargeBigInt = Value(111)
  val Atom = Value(100)
  val Float = Value(99)
  val SmallTuple = Value(104)
  val LargeTuple = Value(105)
  val List = Value(108)
  val Nil = Value(106)
}

private class TagInputStream(input: InputStream) extends DataInputStream(input) {
  def readTag: Tags.Value = {
    val id = readByte
    try {
      return Tags(id)
    } catch {
      case e: NoSuchElementException => throw new EncodingException("Unknown tag id: " + id)
    }
  }
}

private class TagOutputStream(output: OutputStream) extends DataOutputStream(output) {
  def writeTag(tag: Tags.Value) = writeByte(tag.id)
}

object BERT {
  def parse(input: BERT) = input
  
  def parse(input: InputStream): BERT = {
    val in = new TagInputStream(input)
    if (in.readUnsignedByte() != Tags.Header.id) {
      throw new EncodingException("Not a valid BERT message.")
    }
    BERT(readObject(in))
  }
  
  def parse(input: Array[Byte]): BERT = {
    parse(new ByteArrayInputStream(input))
  }
  
  private def readObject(in: TagInputStream): Any = {
    in.readTag match {
      case Tags.Binary => readString(in)
      case Tags.SmallInteger => readSmallInt(in)
      case Tags.Integer => readInt(in)
      case Tags.SmallBigInt => readSmallBigInt(in)
      case Tags.LargeBigInt => readLargeBigInt(in)
      case Tags.Atom => readAtom(in)
      case Tags.Float => readFloat(in)
      case Tags.SmallTuple => readSmallTuple(in)
      case Tags.LargeTuple => readLargeTuple(in)
      case Tags.List => readList(in)
      case Tags.Nil => Nil
      case unknown => throw new IOException("Unrecognized tag: " + unknown)
    }
  }
  
  private def readSmallInt(in: TagInputStream) = in.readUnsignedByte
  private def readInt(in: TagInputStream) = in.readInt
  
  private def readSmallBigInt(in: TagInputStream) = readBigInt(in, in.readUnsignedByte)
  private def readLargeBigInt(in: TagInputStream) = readBigInt(in, in.readInt)
  private def readBigInt(in: TagInputStream, length: Int): Number = {
    val sign = in.readUnsignedByte
    val buffer: Array[Byte] = Array.make(length, 0)
    in.read(buffer)
    
    val result = buffer.zipWithIndex.map { x =>
      x match {
        case (digit, place) => BigInt(digit & 0xFF) * BigInt(256).pow(place)
      }
    }.reduceLeft { _ + _ } * BigInt(-1).pow(sign)
    
    if (result.bitLength <= 64) {
      return result.longValue
    }
    
    return result
  }
  
  private def readAtom(in: TagInputStream): Symbol = {
    val length = in.readShort
    val buffer: Array[Byte] = Array.make(length, 0)
    in.read(buffer)
    return Symbol(new String(buffer))
  }
  
  private def readSmallTuple(in: TagInputStream) = readTuple(in, in.readUnsignedByte)
  private def readLargeTuple(in: TagInputStream) = readTuple(in, in.readInt)
  private def readTuple(in: TagInputStream, length: Int): Any = {
    var items: List[Any] = Nil
    for (i <- 0 until length) {
      items = readObject(in) :: items
    }
    
   new Tuple(items.reverse) match {
      case Tuple('bert, 'true) => true
      case Tuple('bert, 'false) => false
      case Tuple('bert, 'nil) => null
      case Tuple('bert, 'dict, x:  List[_]) => x.foldLeft(Map[Any, Any]()) { (m, s) =>
        val inner = s.asInstanceOf[Tuple[Any]]
        m(inner(0)) = inner(1)
      }
      case t => t
    }
  }
  
  private def readList(in: TagInputStream): List[Any] = {
    var items: List[Any] = Nil
    var length = in.readInt
    return (for (i <- 0 until length) yield readObject(in)).toList
  } 
  
  private def readFloat(in: TagInputStream): Double = {
    val buffer: Array[Byte] = Array.make(31, 0)
    in.read(buffer)
    return new String(buffer).trim.toDouble
  }
  
  private def readString(in: TagInputStream): String = {
    val length = in.readInt
    val buffer: Array[Byte] = Array.make(length, 0)
    in.read(buffer)
    return new String(buffer)
  }
}

case class BERT(item: Any) {
  val MinByte = 0
  val MaxByte = 256
  val MinInt  = -(1 << 27)
  val MaxInt  = (1 << 27) - 1
  val MaxSmallBigInt = 257
  val MaxSmallTuple = 256
  
  def write(output: OutputStream) {
    val out = new TagOutputStream(output)
    out.writeTag(Tags.Header)
    writeObject(out, item)
  }
  
  private def writeObject(out: TagOutputStream, o: Any) {
    o match {
      case m: Map[_,_] => writeProduct(out, ('bert, 'dict, m.toList))
      case null => writeProduct(out, ('bert, 'nil))
      case l: Seq[_] => writeList(out, l)
      case t: Product => writeProduct(out, t)
      case n: BigInt => writeBigInt(out, n)
      case b: Boolean => writeProduct(out, ('bert, Symbol(b.toString)))
      case n: Double => writeFloat(out, n)
      case i: Int => writeInt(out, i)
      case l: Long => writeBigInt(out, BigInt(l))
      case s: String => writeString(out, s)
      case s: Symbol => writeSymbol(out, s)
      case e => throw new EncodingException("unknown item: " + e)
    }
  }
  
  private def writeList(out: TagOutputStream, l: Seq[_]) {
    l match {
      case Nil => out.writeTag(Tags.Nil)
      case _   =>
        out.writeTag(Tags.List)
        out.writeInt(l.size)
        for (o <- l) {
          writeObject(out, o)
        }
        out.writeTag(Tags.Nil)
    }
  }
  
  private def writeProduct(out: TagOutputStream, t: Product) {
    if (t.productArity < MaxSmallTuple) {
      out.writeTag(Tags.SmallTuple)
      out.writeByte(t.productArity)
    } else {
      out.writeTag(Tags.LargeTuple)
      out.writeInt(t.productArity)
    }
    for (i <- 0 until t.productArity) {
      writeObject(out, t.productElement(i))
    }
  }
  
  private def writeFloat(out: TagOutputStream, n: Double) {
    out.writeTag(Tags.Float)
    val s = n.formatted("%15.15e")
    out.writeBytes(s)
    out.writeBytes("\0" * (31 - s.length))
  }
  
  private def writeSymbol(out: TagOutputStream, s: Symbol) {
    out.writeTag(Tags.Atom)
    out.writeShort(s.name.length)
    out.writeBytes(s.name)
  }
  
  private def writeInt(out: TagOutputStream, i: Int) {
    if (i >= MinByte && i < MaxByte) {
      out.writeTag(Tags.SmallInteger)
      out.writeByte(i)
    } else if (i >= MinInt && i < MaxInt) {
      out.writeTag(Tags.Integer)
      out.writeInt(i)
    } else {
      writeBigInt(out, BigInt(i))
    }
  }
  
  private def writeBigInt(out: TagOutputStream, n: BigInt) {
    val length = Math.ceil(n.toString(2).length() / 8.0).intValue
    if (length <= MaxSmallBigInt) {
      out.writeTag(Tags.SmallBigInt)
      out.writeByte(length)
    } else {
      out.writeTag(Tags.LargeBigInt)
      out.writeInt(length)
    }
    out.writeByte(if (n >= 0) 0 else 1)
    for (i <- 0 until length) {
      out.writeByte(((n >> (i * 8)) % 256).intValue)
    }
  }
  
  private def writeString(out: TagOutputStream, s: String) {
    out.writeTag(Tags.Binary)
    out.writeInt(s.length)
    out.writeBytes(s)
  }
  
  def toArray: Array[Byte] = {
    val output = new ByteArrayOutputStream()
    write(output)
    output.close
    output.toByteArray
  }
  
  def toList: List[Int] = List.fromArray(toArray).map { _ & 0xFF }
}