package scalau.io

import java.io.{File, Closeable}
import java.nio.charset.Charset
import java.nio.{ByteBuffer, Buffer}


object IO {

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

    val reader = new BufferReader(output)
    for (i <- 1 to 10) {
      assert(reader.read() == i)
    }
    assert(reader.read() < 0)
    assert(reader.read() < 0)
  }

  val file = new File("junk")
  val testMessage = "Hello, world".getBytes
  IO.using(new FileWriter(file)) {
    w => w.write(testMessage)
  }
  val fromFile = IO.using(new FileReader(file)) {
    r => val buf = ByteBuffer.allocate(5000)
    val eof = r.read(buf)
    assert(eof)
    buf.flip()
    val array = new Array[Byte](testMessage.length)
    buf.get(array)
    array
  }
  assert(testMessage.sameElements(fromFile))

  val charReader = new CharReaderFromBytes(new FileReader(file), Charset.forName("UTF-8"))
  
}
