package scalau.io

import java.io.{ File, FileOutputStream }
import java.nio.{ ByteBuffer, CharBuffer }
import java.nio.charset.{ CharsetEncoder, CoderResult, Charset }


trait CharWriter {

  def writeChars(chars: Char*): Unit

  def write(string: CharSequence): Unit

  def writeLine(string: CharSequence): Unit = {
    write(string)
    write(IO.lineSeparator)
  }

}

class BufferCharWriter(initSize: Int) extends CharWriter {
  
  private var charBuffer = CharBuffer.allocate(initSize)
  
  def output: CharBuffer = {
    val copy = charBuffer.asReadOnlyBuffer
    copy.flip()
    copy
  }
  
  def writeChars(chars: Char*): Unit = {
    var pos = 0
    val len = chars.length
    while (pos < len) {
      if (!charBuffer.hasRemaining) {
        val newBuf = CharBuffer.allocate(charBuffer.capacity * 2)
        charBuffer.flip()
        newBuf.put(charBuffer)
        charBuffer = newBuf
      }
      charBuffer.put(chars(pos))
      pos += 1
    }
  }

  def write(string: CharSequence): Unit = {
    var pos = 0
    val len = string.length
    while (pos < len) {
      if (!charBuffer.hasRemaining) {
        val newBuf = CharBuffer.allocate(charBuffer.capacity * 2)
        charBuffer.flip()
        newBuf.put(charBuffer)
        charBuffer = newBuf
      }
      charBuffer.put(string.charAt(pos))
      pos += 1
    }
  }
  
}


trait CharWriterToBytes extends CharWriter with ByteBufferedOutput {
  
/*  this: ByteBufferedOutput =>*/
  
  protected val encoder: CharsetEncoder
  
  def charset = encoder.charset

  protected lazy val charBuffer = CharBuffer.allocate(
    (encoder.averageBytesPerChar * bufferSize).ceil.toInt)

  protected final def flushCharBuffer(endOfInput: Boolean): Unit = {
    while (charBuffer.hasRemaining) {
      val result = encoder.encode(charBuffer, buffer, endOfInput)
      if (result.isError)
        result.throwException()
      if (result.isOverflow) {
        flush()
      }
    }
    charBuffer.clear()
  }
  
  def writeChars(chars: Char*): Unit = {
    var pos = 0
    val len = chars.length
    while (pos < len) {
      if (!charBuffer.hasRemaining) {
        charBuffer.flip()
        flushCharBuffer(false)
      }
      charBuffer.put(chars(pos))
      pos += 1
    }
  }

  def write(string: CharSequence): Unit = {
    var pos = 0
    val len = string.length
    while (pos < len) {
      if (!charBuffer.hasRemaining) {
        charBuffer.flip()
        flushCharBuffer(false)
      }
      charBuffer.put(string.charAt(pos))
      pos += 1
    }
  }
  
  abstract override def close() {
    flushCharBuffer(true)
    var result = CoderResult.OVERFLOW
    do {
      result = encoder.flush(buffer)
      assert(!result.isError)
      flush()
    } while (result.isOverflow)
    super.close()
  }

}

class FileCharWriter(val file: File, charset: Charset, append: Boolean) extends BlockingChannelOutput with CharWriterToBytes  {

  protected val buffer = ByteBuffer.allocate(IO.defaultBufferSize)
  buffer.limit(0)
  
  protected val channel = new FileOutputStream(file, append).getChannel
  
  protected val encoder = charset.newEncoder()

  def this(file: File, charset: Charset) = this(file, charset, false)

}
