package scalau.io

import java.nio.charset.Charset
import java.nio.{CharBuffer, ByteBuffer}


object RichByteBuffer {

	implicit def richByteBuffer(buf: ByteBuffer) = new RichByteBuffer(buf)

}

final class RichByteBuffer private (underlying: ByteBuffer) {

	def putUByte(ubyte: Int) = {
		underlying.put((ubyte & 0xFF).toByte)
		underlying
	}

	def getUByte: Int = underlying.get & 0xFF

	def putUByte(index: Int, ubyte: Int) = {
		underlying.put((ubyte & 0xFF).toByte)
		underlying
	}

	def getUByte(index: Int): Int = underlying.get(index) & 0xFF

	def putUShort(ushort: Int) = {
		underlying.putShort((ushort & 0xFFFF).toShort)
		underlying
	}

	def getUShort: Int = underlying.getShort & 0xFFFF

	def putUShort(index: Int, ushort:Int) = {
		underlying.putShort(index, (ushort & 0xFFFF).toShort)
		underlying
	}

	def getUShort(index: Int): Int = underlying.getShort(index) & 0xFFFF

	def putUInt(uint: Long) = {
		underlying.putInt((uint & 0xFFFFFFFF).toInt)
		underlying
	}

	def getUInt: Int = {
		underlying.getInt & 0xFFFFFFFF
	}

	def decode(charset: Charset): CharBuffer = charset.decode(underlying)

	def decodeToString(charset: Charset): String = decode(charset).toString

	def limit = underlying.limit
	
	def limit_=(limit: Int): Unit = underlying.limit(limit)

}
