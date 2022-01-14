package tf.bug.utf8string

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

final class Utf8String private[utf8string] (private val bytes: Array[Byte]) {

  def byteAt(index: Int): Byte = bytes(index)
  def getBytes(dst: ByteBuffer): Unit = dst.put(bytes)
  def getBytes(dst: Array[Byte], offset: Int): Unit = java.lang.System.arraycopy(bytes, 0, dst, offset, bytes.length)

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

  def apply(bytes: Array[Byte]): Utf8String = {
    val buf = ByteBuffer.wrap(bytes)

    var result = -1
    var len = 0
    while(len < buf.limit() && result != 0) {
      result = validationBufferLength(buf, consume = true)
      len += result
    }

    buf.position(0)

    val res = new Array[Byte](len)

    result = -1
    len = 0
    while(len < res.length) {
      result = validationBufferCopy(buf, res, len)
      len += result
    }

    new Utf8String(res)
  }

  val errorReplacement: Utf8String = Utf8String("\uFFFD")

  @tailrec private def continuationLenOrError(buf: ByteBuffer, ex: Int, tg: Int, consume: Boolean): Int = {
    if(ex == 0) {
      tg
    } else if(buf.position() < buf.limit()) {
      val here = buf.get()
      if((here & 0xc0) == 0x80) continuationLenOrError(buf, ex - 1, tg, consume)
      else errorReplacement.length
    } else {
      if(consume) errorReplacement.length
      else {
        buf.position(buf.position() - (tg - ex))
        0
      }
    }
  }

  private def validationBufferLength(buf: ByteBuffer, consume: Boolean = true): Int = {
    val here = buf.get()

    if((here & 0x80) == 0x00) {
      // 0xxxxxxx
      1
    } else if((here & 0xc0) == 0xc0) {
      // 11xxxxxx
      val seqLength = java.lang.Integer.numberOfLeadingZeros((~here) & 0x000000ff) - 24
      continuationLenOrError(buf, seqLength - 1, seqLength, consume)
    } else {
      errorReplacement.length
    }
  }

  @tailrec private def continuationSuccess(buf: ByteBuffer, ex: Int): Boolean = {
    if(ex == 0) {
      true
    } else if(buf.position() < buf.limit()) {
      val here = buf.get()
      if((here & 0xc0) == 0x80) continuationSuccess(buf, ex - 1)
      else false
    } else {
      false
    }
  }

  private def validationBufferCopy(buf: ByteBuffer, out: Array[Byte], offset: Int): Int = {
    val pos = buf.position()
    val here = buf.get()

    if((here & 0x80) == 0x00) {
      // 0xxxxxxx
      out(offset) = here
      1
    } else if((here & 0xc0) == 0xc0) {
      // 11xxxxxx
      val seqLength = java.lang.Integer.numberOfLeadingZeros((~here) & 0x000000ff) - 24
      if(continuationSuccess(buf, seqLength - 1)) {
        buf.position(pos)
        buf.get(out, offset, seqLength)
        seqLength
      } else {
        errorReplacement.getBytes(out, offset)
        errorReplacement.length
      }
    } else {
      errorReplacement.getBytes(out, offset)
      errorReplacement.length
    }
  }

  def parse(buf: ByteBuffer): Utf8String = {
    val originalPosition = buf.position()

    var result = -1
    var len = 0
    while(len < buf.limit() && result != 0) {
      result = validationBufferLength(buf, consume = false)
      len += result
    }

    buf.position(originalPosition)

    val bytes = new Array[Byte](len)

    result = -1
    len = 0
    while(len < bytes.length) {
      result = validationBufferCopy(buf, bytes, len)
      len += result
    }

    new Utf8String(bytes)
  }

}
