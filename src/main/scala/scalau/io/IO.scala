package scalau.io

import java.io.{Closeable}
import java.nio.charset.Charset
import java.nio.{Buffer}


object IO {

  private[io] val defaultBufferSize = 32000

  val lineSeparator = System.getProperty("line.separator")

  def using[R, T <% Closeable](resource: T)(block: T => R): R = {
    try {
      block(resource)
    }
    finally {
      resource.close()
    }
  }
  
}


trait Buffered extends Closeable {

  type BufferType <: Buffer

  protected def buffer: BufferType

  final def bufferSize = buffer.capacity

}
