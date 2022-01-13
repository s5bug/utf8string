package tf.bug.utf8string.fs2

import fs2._
import tf.bug.utf8string.Utf8String

import java.nio.ByteBuffer

object utf8text {

  def decodeUtf8[F[_]]: Pipe[F, Byte, Utf8String] =
    _.chunks.mapAccumulate(ByteBuffer.allocate(0)) {
      (carry, chunk) =>
        val next = ByteBuffer.allocate((carry.limit() - carry.position()) + chunk.size)
        next.put(carry)
        next.put(chunk.toByteBuffer)
        next.flip()
        val str = Utf8String.parse(next)
        (next, str)
    }.map(_._2)

  def encodeUtf8[F[_]]: Pipe[F, Utf8String, Byte] =
    _.mapChunks(_.flatMap(str => {
      val bb = ByteBuffer.allocate(str.length)
      str.getBytes(bb)
      bb.flip()
      Chunk.byteBuffer(bb)
    }))

}
