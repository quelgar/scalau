package scalau.io


import java.nio.CharBuffer
import java.nio.charset.CharsetEncoder

trait CharWriter {

  def writeChars(chars: Char*): Unit

  def write(string: CharSequence): Unit

  def writeLine(string: CharSequence): Unit = {
    write(string)
    write(IO.lineSeparator)
  }

}


class CharWriterToBytes(initByteOutput: ByteBufferedOutput, encoder: CharsetEncoder) extends CharWriter with BufferedOutput {

  type BufferType = CharBuffer

  def charset = encoder.charset

  protected def buffer = null

  protected def flushImpl() = null

  def writeChars(chars: Char*) = null

  def write(string: CharSequence) = null

  def close = {}
}
