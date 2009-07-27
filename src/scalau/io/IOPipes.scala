package scalau.io

import java.io._
import java.nio.charset.Charset
import logging.Logger


trait Pipeable[+O] {

  def resource: O

  final def |[O2](pipe: Pipe[O, O2]): Pipeable[O2] = pipe.connect(resource)

}

trait Pipe[-L, +R] {

  def connect(left: => L): Pipeable[R]

}

object IOPipes {

  val logger = Logger.logger("scalau.io.IOOp")

  def byteArrayOutputStream(initialSize: Int) = new Pipeable[ByteArrayOutputStream]() {
    def resource = {
      println("Constructed ByteArrayOutputStream")
      new ByteArrayOutputStream(initialSize)
    }
  }

  def outputStream(file: File) = new Pipeable[FileOutputStream]() {
    def resource = new FileOutputStream(file)
  }

  def streamWriter(charset: Charset) = new Pipe[OutputStream, OutputStreamWriter]() {
    def connect(left: => OutputStream) = new Pipeable[OutputStreamWriter]() {
      def resource = { println("Constructed OutputStreamWriter") ; new OutputStreamWriter(left, charset) }
    }
  }

  def streamWriterUTF8 = streamWriter(Charset.forName("UTF-8"))

  def printWriter = new Pipe[Writer, PrintWriter]() {
    def connect(left: => Writer) = new Pipeable[PrintWriter]() {
      def resource = new PrintWriter(left)
    }
  }

  def bufferedOutputStream(size: Int) = new Pipe[OutputStream, BufferedOutputStream]() {
    def connect(left: => OutputStream) = new Pipeable[BufferedOutputStream]() {
      def resource = new BufferedOutputStream(left, size)
    }
  }

  def inputStream(file: File) = new Pipeable[FileInputStream]() {
    def resource = new FileInputStream(file)
  }

  def streamReader(charset: Charset) = new Pipe[InputStream, InputStreamReader]() {
    def connect(left: => InputStream) = new Pipeable[InputStreamReader]() {
      def resource = new InputStreamReader(left, charset)
    }
  }

  def streamReaderUTF8 = streamReader(Charset.forName("UTF-8"))

  def bufferedReader(size: Int) = new Pipe[Reader, BufferedReader]() {
    def connect(left: => Reader) = new Pipeable[BufferedReader]() {
      def resource = new BufferedReader(left, size)
    }
  }

}
