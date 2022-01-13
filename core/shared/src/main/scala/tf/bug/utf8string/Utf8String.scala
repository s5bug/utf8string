package tf.bug.utf8string

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

final class Utf8String private[utf8string] (private val bytes: Array[Byte]) {

  def byteAt(index: Int): Byte = bytes(index)
  def getBytes(dst: ByteBuffer): Unit = dst.put(bytes)

  def length: Int = bytes.length
  def size: Int = bytes.length

  def toCodepointString: CodepointString = {
    var codepointCount = bytes.length
    var byteIndex = 0
    while(byteIndex < bytes.length) {
      val here = bytes(byteIndex)
      if((here & 0xf8) == 0xf0) {
        byteIndex += 4
        codepointCount -= 3
      } else if((here & 0xf0) == 0xe0) {
        byteIndex += 3
        codepointCount -= 2
      } else if((here & 0xe0) == 0xc0) {
        byteIndex += 2
        codepointCount -= 1
      } else if((here & 0x80) == 0x00) {
        byteIndex += 1
      } else {
        throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
      }
    }

    val array = new Array[Int](codepointCount)
    var codepointIndex = 0
    byteIndex = 0
    while(codepointIndex < codepointCount) {
      val here = bytes(byteIndex)
      var num = 0
      if((here & 0xf8) == 0xf0) {
        num |= ((here & 0x07) << 18)
        val second = bytes(byteIndex + 1)
        if((second & 0xc0) != 0x80) throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
        num |= ((second & 0x3f) << 12)
        val third = bytes(byteIndex + 2)
        if((third & 0xc0) != 0x80) throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
        num |= ((third & 0x3f) << 6)
        val fourth = bytes(byteIndex + 3)
        if((fourth & 0xc0) != 0x80) throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
        num |= ((fourth & 0x3f) << 0)
        byteIndex += 4
      } else if((here & 0xf0) == 0xe0) {
        num |= ((here & 0x0f) << 12)
        val second = bytes(byteIndex + 1)
        if((second & 0xc0) != 0x80) throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
        num |= ((second & 0x3f) << 6)
        val third = bytes(byteIndex + 2)
        if((third & 0xc0) != 0x80) throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
        num |= ((third & 0x3f) << 0)
        byteIndex += 3
      } else if((here & 0xe0) == 0xc0) {
        num |= ((here & 0x1f) << 6)
        val second = bytes(byteIndex + 1)
        if((second & 0xc0) != 0x80) throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
        num |= ((second & 0x3f) << 0)
        byteIndex += 2
      } else if((here & 0x80) == 0x00) {
        num = here
        byteIndex += 1
      } else {
        throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
      }
      array(codepointIndex) = num
      codepointIndex += 1
    }

    new CodepointString(array)
  }

  override def toString: String = new String(bytes, 0, bytes.length, StandardCharsets.UTF_8)

  override def equals(obj: Any): Boolean =
    obj match {
      case null => false
      case u8s: Utf8String => java.util.Arrays.equals(bytes, u8s.bytes)
      case _ => false
    }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

}

object Utf8String {

  def apply(str: String): Utf8String = {
    val bytes: Array[Byte] = str.getBytes(StandardCharsets.UTF_8)
    new Utf8String(bytes)
  }

  def parse(buf: ByteBuffer): Utf8String = {
    var completeStringLen = buf.limit() - buf.position()
    var idx = buf.position()
    var done = false
    while(!done && idx < completeStringLen) {
      val here = buf.get(idx)
      if((here & 0xf8) == 0xf0) {
        if(idx + 3 < completeStringLen) {
          idx += 4
        } else {
          completeStringLen = idx
          done = true
        }
      } else if((here & 0xf0) == 0xe0) {
        if(idx + 2 < completeStringLen) {
          idx += 3
        } else {
          completeStringLen = idx
          done = true
        }
      } else if((here & 0xe0) == 0xc0) {
        if(idx + 1 < completeStringLen) {
          idx += 2
        } else {
          completeStringLen = idx
          done = true
        }
      } else if((here & 0x80) == 0x00) {
        if(idx < completeStringLen) {
          idx += 1
        } else {
          completeStringLen = idx
          done = true
        }
      } else {
        throw new IllegalStateException("Invalid UTF-8 sequence in Utf8String")
      }
    }

    val bytes = new Array[Byte](completeStringLen)
    buf.get(bytes, 0, completeStringLen)
    buf.position(idx)
    new Utf8String(bytes)
  }

}
