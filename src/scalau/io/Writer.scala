package scalau.io

import java.io._
import java.io.{Writer => JWriter, PrintWriter => JPrintWriter}
import java.nio.channels.WritableByteChannel
import java.nio.charset.Charset
import java.nio.{Buffer, ByteBuffer}


object Writer {

}

trait Buffered {

  type BufferType <: Buffer

  protected def buffer: BufferType

  final def bufferSize = buffer.capacity

  def processBuffer(): Unit

  def flush() {
    buffer.flip()
    processBuffer()
  }

}

trait ByteBuffered extends Buffered {

  type BufferType = ByteBuffer

}

trait BlockingChannelOutput extends ByteBuffered {

  protected val channel: WritableByteChannel

  def processBuffer(): Unit = write(buffer)

  def write(data: ByteBuffer): Unit = {
    val sizeToWrite = data.remaining
    val written = channel.write(data)
    assert(written == sizeToWrite && !data.hasRemaining)
    data.clear()
  }

  def close(): Unit = {
    flush()
    channel.close()
  }

}


trait ByteWriter {

  final def writeBytes(b: Byte*): Unit = write(b)

  def write(data: Seq[Byte]): Unit

}

trait SynchronousWriter extends ByteWriter {

  this: ByteBuffered =>

  def write(data: Seq[Byte]) {
    var remainingData = data
    while (!remainingData.isEmpty) {
      val writeData = remainingData.take(buffer.remaining)
      val sizeWrite = writeData.size
      for (b <- writeData) {
        buffer.put(b)
      }
      if (!buffer.hasRemaining) {
        buffer.flip()
        processBuffer()
      }
      remainingData = remainingData.drop(sizeWrite)
    }
  }

}

class BufferWriter(initSize: Int) extends SynchronousWriter with ByteBuffered {

  protected var currentBuffer = ByteBuffer.allocate(initSize)

  protected def buffer = currentBuffer

  def output: ByteBuffer = {
    val readOnly = currentBuffer.asReadOnlyBuffer
    readOnly.flip
    readOnly
  }

  def currentCapacity: Int = currentBuffer.capacity

  def processBuffer() {
    if (currentBuffer.limit == currentBuffer.capacity) {
      val newBuf = ByteBuffer.allocate(currentBuffer.capacity * 2)
      newBuf.put(currentBuffer)
      currentBuffer = newBuf
    }
    else {
      // just undo flip
      currentBuffer.position(currentBuffer.limit)
      currentBuffer.limit(currentBuffer.capacity)
    }
  }
}

class FileWriter(file: File, append: Boolean) extends SynchronousWriter with BlockingChannelOutput {

  def this(file: File) = this(file, false)

  protected val buffer = ByteBuffer.allocate(32000)

  protected val channel = new FileOutputStream(file, append).getChannel
  
}


trait ByteReader {

  def read: Byte

  def read(minToRead: Int): ByteBuffer = {
    val buf = ByteBuffer.allocate(minToRead)
    while (buf.hasRemaining) {
      
    }
  }

  def read(dst: Array[Byte], offset: Int, length: Int): Unit

}

object Test {

  def main(args: Array[String]) {
    val w = new BufferWriter(8)
    w.writeBytes(1, 2, 3, 4, 5)
    assert(w.currentCapacity == 8)
    w.writeBytes(6, 7, 8, 9, 10)
    val output = w.output
    assert(output.remaining == 10)
    assert(output.get(9) == 10)
    println(w.currentCapacity)
  }
}
