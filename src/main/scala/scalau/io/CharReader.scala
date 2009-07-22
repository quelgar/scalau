package scalau.io

import collection.mutable
import java.io.EOFException
import java.nio.CharBuffer
import java.nio.charset.{CharsetDecoder, Charset, CoderResult}
import Misc._
import runtime.RichString


trait CharReader {

  /**
   * Reads the next character from this reader. This method will block until
   * a character is read, or end of stream is reached.
   *
   * @return The character read (as an Int in the range 0-0xFFFF) or -1 if
   *  end of stream is reached.
   */
  def read(): Int

  /**
   * Reads a number of characters from this reader.
   * The number of characters to read is determined by the initial number
   * of characters remaining in {@code dst}. Characters are written to
   * {@code dst} at its position and the position is incremented in the
   * usual fashion. This method will block until {@code dst} is filled or
   * end of stream is reached.
   *
   * @param dst The buffer to write characters to.
   * @return True if end of stream is reached.
   */
  def read(dst: CharBuffer): Boolean

  def skip(n: Int): Boolean = {
    var i = 1
    while (i <= n) {
      if (read() < 0)
        return true
    }
    false
  }

  def read[A <: Seq[Char]](dst: mutable.Buffer[Char], delimiters: A*): Option[A]

  def readLine(): String = {
    val line = new StringBuilder(200)
    val found = read[RichString](line, "\n", "\r\n")
    if (found.isDefined) {
      skip(found.get.length)
    }
    line.toString
  }

}

class CharReaderFromBytes(initByteInput: ByteBufferedInput, decoder: CharsetDecoder) extends CharReader with BufferedInput {

  type BufferType = CharBuffer

  def charset = decoder.charset

  private val byteInput = new ByteBufferedInput {
    def fillBuffer(): Boolean = {
      if (!buffer.hasRemaining) {
        buffer.clear()
//        initByteInput.fillBuffer()
        true  // XXX
      }
      else {
        false
      }
    }

 //    def buffer = initByteInput.buffer
    def buffer: java.nio.ByteBuffer = null


    def close = initByteInput.close()
  }

  protected val buffer = CharBuffer.allocate(
    (decoder.averageCharsPerByte * byteInput.bufferSize).ceil.toInt)

  private var flushOverflow = false

  def this(initByteInput: ByteBufferedInput, charset: Charset) = this(initByteInput, charset.newDecoder())

  protected def fillBuffer(): Boolean = {
    if (flushOverflow) {
      val flushResult = decoder.flush(buffer)
      assert(!flushResult.isError)
      return flushResult.isUnderflow
    }
//    val eofByteBuf = byteInput.fillBuffer()
    val eofByteBuf = false
//    val result = decoder.decode(byteInput.buffer, buffer, eof)
    val result: CoderResult = null
    if (result.isError) {
      result.throwException()
    }
    val eof = if (result.isUnderflow && eofByteBuf) {
      val flushResult = decoder.flush(buffer)
      if (flushResult.isOverflow) {
        flushOverflow = true
        false
      }
      else {
        true
      }
    }
    else {
      false
    }
    buffer.flip()
    eof
  }

  def read: Int = {
    if (!buffer.hasRemaining) {
      buffer.clear()
      fillBuffer()
    }
    if (!buffer.hasRemaining) {
      -1
    }
    buffer.get()
  }


  def read(dst: CharBuffer): Boolean = true

  def read[A <: Seq[Char]](dst: mutable.Buffer[Char], delimiters: A*): Option[A] = {
    while (true) {
      if (!buffer.hasRemaining) {
        buffer.clear()
        fillBuffer()
      }
      if (!buffer.hasRemaining) {
        return None
      }
      while (buffer.hasRemaining) {
        buffer.mark()
        for (delim <- delimiters) {
          val found = buffer.remaining >= delim.length && delim.forall(c => c == buffer.get())
          buffer.reset()
          if (found) {
            val cp = buffer.asReadOnlyBuffer
            cp.flip()
            while (cp.hasRemaining) {
              dst += cp.get()
            }
            return Some(delim)
          }
        }
        dst += buffer.get()
      }
    }
    error("Unreachable")
  }

  def close {
    byteInput.close()
  }
}
