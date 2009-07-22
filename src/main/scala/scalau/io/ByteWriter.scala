package scalau.io

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.WritableByteChannel


trait BufferedOutput extends Buffered {

  def flush() {
    buffer.flip()
    flushImpl()
  }

  protected def flushImpl(): Unit

}

trait ByteBufferedOutput extends BufferedOutput {

  type BufferType = ByteBuffer

}

trait BlockingChannelOutput extends ByteBufferedOutput {

  protected val channel: WritableByteChannel

  def flushImpl(): Unit = write(buffer)

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

  this: ByteBufferedOutput =>

  def write(data: Seq[Byte]) {
    var remainingData = data
    while (!remainingData.isEmpty) {
      val writeData = remainingData.take(buffer.remaining)
      val sizeWrite = writeData.size
      for (b <- writeData) {
        buffer.put(b)
      }
      if (!buffer.hasRemaining) {
        flush()
      }
      remainingData = remainingData.drop(sizeWrite)
    }
  }

}

class BufferWriter(initSize: Int) extends SynchronousWriter with ByteBufferedOutput {

  protected var currentBuffer = ByteBuffer.allocate(initSize)

  protected def buffer = currentBuffer

  def output: ByteBuffer = {
    val readOnly = currentBuffer.asReadOnlyBuffer
    readOnly.flip
    readOnly
  }

  def currentCapacity: Int = currentBuffer.capacity

  def flushImpl() {
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

  def close = {}
}

class FileWriter(file: File, append: Boolean) extends SynchronousWriter with BlockingChannelOutput {

  def this(file: File) = this(file, false)

  protected val buffer = ByteBuffer.allocate(32000)

  protected val channel = new FileOutputStream(file, append).getChannel
  
}

