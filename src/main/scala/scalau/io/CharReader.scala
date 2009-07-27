package scalau.io

import collection.mutable
import java.io.{ EOFException, File, FileInputStream }
import java.nio.{ ByteBuffer, CharBuffer }
import java.nio.charset.{CharsetDecoder, Charset, CoderResult}
import Misc._
import runtime.RichString


object CharReader {
  
  def reader(input: CharBuffer): CharReader = new StandardBufferCharReader(input)
  
  def reader(input: CharSequence): CharReader = {
    val len = input.length
    val buf = CharBuffer.allocate(len)
    (0 until len).foreach(i => buf.put(input.charAt(i)))
    buf.flip()
    reader(buf)
  }

}

trait CharReader {

  /**
   * Reads the next character from this reader. This method will block until
   * a character is read, or end of stream is reached.
   *
   * @return The character read (as an Int in the range 0-0xFFFF) or -1 if
   *  end of stream is reached.
   */
  def read(): Int
  
  def readOption(): Option[Char] = {
    val r = read()
    if (r < 0)
      None
    else {
      assert(r <= Math.MAX_CHAR)
      Some(r.toChar)
    }
  }

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

  def read[A <: Seq[Char]](dst: mutable.Buffer[Char], delimiters: A*): Option[A]

  def skip(n: Int): Boolean = {
    var i = 1
    while (i <= n) {
      if (read() < 0)
        return true
      i += 1
    }
    false
  }

  def readLine(): String = {
    val line = new StringBuilder(200)
    val found = read[RichString](line, "\n", "\r\n")
    if (found.isDefined) {
      skip(found.get.length)
    }
    line.toString
  }

}

trait BufferCharReader extends CharReader {
  
  protected val charBuffer: CharBuffer
  
  protected def fillCharBuffer(): Boolean = {
    charBuffer.limit(charBuffer.position)
    true
  }

  def read(): Int = {
    if (!charBuffer.hasRemaining) {
      charBuffer.clear()
      fillCharBuffer()
    }
    if (!charBuffer.hasRemaining) {
      -1
    }
    else {
      charBuffer.get()
    }
  }

  def read(dst: CharBuffer): Boolean = {
    var eof = false
    while (!eof && dst.hasRemaining) {
      if (!charBuffer.hasRemaining) {
        charBuffer.clear()
        fillCharBuffer()
      }
      if (!charBuffer.hasRemaining) {
        eof = true
      }
      else {
        val savedLimit = charBuffer.limit
        if (dst.remaining < charBuffer.remaining) {
          charBuffer.limit(charBuffer.limit - (charBuffer.remaining - dst.remaining))
        }
        dst.put(charBuffer)
        charBuffer.limit(savedLimit)
      }
    }
    eof
  }

  def read[A <: Seq[Char]](dst: mutable.Buffer[Char], delimiters: A*): Option[A] = {
    while (true) {
      if (!charBuffer.hasRemaining) {
        charBuffer.clear()
        fillCharBuffer()
      }
      if (!charBuffer.hasRemaining) {
        return None
      }
      while (charBuffer.hasRemaining) {
        charBuffer.mark()
        for (delim <- delimiters) {
          val found = charBuffer.remaining >= delim.length && delim.forall(c => c == charBuffer.get())
          charBuffer.reset()
          if (found) {
            return Some(delim)
          }
        }
        dst += charBuffer.get()
      }
    }
    error("Unreachable")
  }


}

trait ByteBufferCharDecoder extends BufferCharReader {

  this: ByteBufferedInput =>

  protected val decoder: CharsetDecoder

  def charset = decoder.charset

  protected lazy val charBuffer = CharBuffer.allocate(
    (decoder.averageCharsPerByte * bufferSize).ceil.toInt)

  private var flushOverflow = false

  protected override final def fillCharBuffer(): Boolean = {
    if (flushOverflow) {
      val flushResult = decoder.flush(charBuffer)
      assert(!flushResult.isError)
      return flushResult.isUnderflow
    }
    val eofByteBuffer = if (!buffer.hasRemaining) {
      buffer.clear()
      fillBuffer()
    }
    else {
      false
    }
    val result = decoder.decode(buffer, charBuffer, eofByteBuffer)
    if (result.isError) {
      result.throwException()
    }
    val eof = if (result.isUnderflow && eofByteBuffer) {
      val flushResult = decoder.flush(charBuffer)
      assert(!flushResult.isError)
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
  
}


class FileCharReader(val file: File, charset: Charset) extends ByteBufferCharDecoder with BlockingChannelInput {
    
  protected val buffer = ByteBuffer.allocate(IO.defaultBufferSize)
  buffer.limit(0)
    
  protected val channel = new FileInputStream(file).getChannel
    
  protected val decoder = charset.newDecoder()
}

class StandardBufferCharReader(protected val charBuffer: CharBuffer) extends BufferCharReader
