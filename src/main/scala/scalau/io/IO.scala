package scalau.io

import java.io.{File, Closeable}
import java.nio.charset.Charset
import java.nio.{Buffer}
import scalau.Misc._


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

  def UTF8 = Charset.forName("UTF-8")

  def fsTreeStream(dir: File): Stream[File] = {
    val children = ?(dir.listFiles()).getOrElse(Nil: Seq[File])
    Stream.cons(dir,
      Stream.concat(Stream(children: _*), Stream.concat(children.map(fsTreeStream(_)))))
  }
}


trait Buffered extends Closeable {

  type BufferType <: Buffer

  protected def buffer: BufferType

  final def bufferSize = buffer.capacity

}
