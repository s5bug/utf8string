package tf.bug.utf8string

final class CodepointString private[utf8string] (private val ints: Array[Int]) {

  def codepointAt(index: Int): Int = ints(index)

  def length: Int = ints.length
  def size: Int = ints.length

  def toUtf8String: Utf8String = {
    var byteCount = ints.length
    var codepointIndex = 0
    while(codepointIndex < ints.length) {
      val here = ints(codepointIndex)
      if(here >= 0x80) byteCount += 1
      if(here >= 0x800) byteCount += 1
      if(here >= 0x10000) byteCount += 1
      if(here >= 0x200000) throw new IllegalArgumentException("Codepoints above U+200000 are not yet handled")
      codepointIndex += 1
    }

    val bytes = new Array[Byte](byteCount)
    var byteIndex = 0
    codepointIndex = 0
    while(byteIndex < byteCount) {
      val here = ints(codepointIndex)
      if(here >= 0x10000) {
        bytes(byteIndex) = (0xf0 | ((here >> 18) & 0x07)).toByte
        bytes(byteIndex + 1) = (0x80 | ((here >> 12) & 0x3f)).toByte
        bytes(byteIndex + 2) = (0x80 | ((here >> 6) & 0x3f)).toByte
        bytes(byteIndex + 3) = (0x80 | ((here >> 0) & 0x3f)).toByte
        byteIndex += 4
      } else if(here >= 0x800) {
        bytes(byteIndex) = (0xe0 | ((here >> 12) & 0x0f)).toByte
        bytes(byteIndex + 1) = (0x80 | ((here >> 6) & 0x3f)).toByte
        bytes(byteIndex + 2) = (0x80 | ((here >> 0) & 0x3f)).toByte
        byteIndex += 3
      } else if(here >= 0x80) {
        bytes(byteIndex) = (0xc0 | ((here >> 6) & 0x1f)).toByte
        bytes(byteIndex + 1) = (0x80 | ((here >> 0) & 0x3f)).toByte
        byteIndex += 2
      } else {
        bytes(byteIndex) = here.toByte
        byteIndex += 1
      }
      codepointIndex += 1
    }

    new Utf8String(bytes)
  }

  override def toString: String = new String(ints, 0, ints.length)

  override def equals(obj: Any): Boolean =
    obj match {
      case null => false
      case cps: CodepointString => java.util.Arrays.equals(ints, cps.ints)
      case _ => false
    }

  override def hashCode(): Int = java.util.Arrays.hashCode(ints)

}

object CodepointString {

  def apply(str: String): CodepointString = {
    val array = new Array[Int](str.codePointCount(0, str.length))
    var cpi = 0
    var chi = 0
    while(cpi < array.length) {
      if(Character.isHighSurrogate(str.charAt(chi)) && Character.isLowSurrogate(str.charAt(chi + 1))) {
        array(cpi) = Character.toCodePoint(str.charAt(chi), str.charAt(chi + 1))
        cpi += 1
        chi += 2
      } else {
        array(cpi) = str.charAt(chi)
        cpi += 1
        chi += 1
      }
    }
    new CodepointString(array)
  }

}
