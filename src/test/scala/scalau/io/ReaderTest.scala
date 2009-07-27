package scalau.io

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import java.nio.ByteBuffer


final class ByteReaderSpec extends Spec with ShouldMatchers {

  describe("A BufferReader") {
    
    it("should return bytes one by one") {
      val br = ByteReader.reader(1, 2, 3, 4)
      br.read() should equal (1)
      br.read() should equal (2)
      br.read() should equal (3)
      br.read() should equal (4)
      br.read() should equal (-1)
      br.read() should equal (-1)
    }
  
    it("should return eof for an empty buffer") {
      val br = ByteReader.reader()
      br.read() should equal (-1)
      br.readOption() should be (None)
      val dummy = ByteBuffer.allocate(100)
      br.read(dummy) should be (true)
      br.skip(1) should be (true)
    }

  }

}

final class CharReaderSpec extends Spec with ShouldMatchers {
  
  describe("A BufferCharReader") {
    
    it("should return bytes one by one") {
      val cr = CharReader.reader("test")
      cr.read() should equal ('t')
      cr.read() should equal ('e')
      cr.read() should equal ('s')
      cr.read() should equal ('t')
      cr.read() should equal (-1)
      cr.read() should equal (-1)
    }
    
    it("should return a single line") {
      val line = "first line"
      var cr = CharReader.reader(line)
      cr.readLine() should equal (line)
      cr.read() should equal (-1)
    }
    
    def multipleLines(eol: String) {
      val lines = List("line one", "line two", "line three")
      val cr = CharReader.reader(lines.mkString(eol))
      for (line <- lines) {
        cr.readLine() should equal (line)
      }
      cr.read() should equal (-1)
    }
    
    it("should return multiple lines") {
      multipleLines("\n")
      multipleLines("\r\n")
    }
    
    it("should read a line with eol as last character") {
      val cr = CharReader.reader("line\n")
      cr.readLine() should equal ("line")
      cr.read() should equal (-1)
    }
    
    it("should return eof for an empty input") {
      var cr = CharReader.reader("")
      cr.read() should equal (-1)
      cr.read() should equal (-1)
    }

  }
}