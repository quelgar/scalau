package scalau.io

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel


trait BufferedInput extends Buffered {

  protected def fillBuffer(): Boolean

}

trait ByteBufferedInput extends BufferedInput {

  type BufferType = ByteBuffer

}


trait BlockingChannelInput extends ByteBufferedInput {

  protected val channel: ReadableByteChannel

  protected def fillBuffer(): Boolean = {
    val bytesToRead = buffer.remaining
    var bytesRead = 0
    var eof = false
    while (!eof && bytesRead < bytesToRead) {
      val readResult = channel.read(buffer)
      if (readResult < 0) {
        eof = true
      }
      else {
        bytesRead += readResult
      }
    }
    buffer.flip()
    eof
  }

  def close() {
    channel.close()
  }

}


trait ByteReader {

  /**
   * Reads the next byte from this reader. This method will block until a
   * byte is read, or end of stream is reached.
   *
   * @return The byte read, or -1 if end of stream is reached.
   */
  def read(): Byte

  /**
   * Returns the next byte, or {@code None} if end of stream is reached.
   * This is more convenient that fooling around with special values returned
   * by the {@link #read()} method, but less efficient due to wrapping of
   * the byte.
   */
  def readOption(): Option[Byte] = {
    val b = read()
    if (b < 0)
      None
    else
      Some(b)
  }

  /**
   * Skips over bytes.
   *
   * @param n The number of bytes to skip.
   * @return True if end of stream is reached.
   */
  def skip(n: Int): Boolean = {
    var i = 1;
    while (i <= n) {
      if (read() < 0)
        return true
      i += 1
    }
    false
  }

  /**
   * Reads a number of bytes from this reader. The bytes read are added at
   * the position of { @code dst } and the position is incremented by the number
   * of bytes read (as per usual fashion). The number of bytes to read is
   * determined by the initial remaining bytes in   { @code dst }. This method
   * will block until the { @code dst } buffer is filled, or
   * end of stream is reached.
   *
   * @param dst The buffer to hold the bytes read.
   * @return True if end of stream has been reached.
   */
  def read(dst: ByteBuffer): Boolean

}

trait SynchronousReader extends ByteReader {

  this: ByteBufferedInput =>

  def read(): Byte = {
    if (!buffer.hasRemaining) {
      buffer.clear()
      fillBuffer()
    }
    if (!buffer.hasRemaining) {
      -1
    }
    else {
      buffer.get()
    }
  }

  def read(dst: ByteBuffer): Boolean = {
    var eof = false
    while (!eof && dst.hasRemaining) {
      if (!buffer.hasRemaining) {
        buffer.clear()
        fillBuffer()
      }
      if (!buffer.hasRemaining) {
        eof = true
      }
      else {
        val savedLimit = buffer.limit
        if (dst.remaining < buffer.remaining) {
          buffer.limit(buffer.limit - (buffer.remaining - dst.remaining))
        }
        dst.put(buffer)
        buffer.limit(savedLimit)
      }
    }
    eof
  }
}

class BufferReader(protected val buffer: ByteBuffer) extends SynchronousReader with ByteBufferedInput {

  protected def fillBuffer(): Boolean = {
    buffer.limit(buffer.position)
    true
  }

  def close { }

}

class FileReader(file: File) extends SynchronousReader with BlockingChannelInput {

  protected val channel = new FileInputStream(file).getChannel

  protected val buffer = ByteBuffer.allocate(32000)

  buffer.limit(0)

}


