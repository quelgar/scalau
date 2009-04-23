package scalau.io

import java.nio.ByteBuffer
import java.nio.charset.Charset


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

	def getString(charset: Charset): String = {
		val bytes = new Array[Byte](underlying.remaining)
		underlying.get(bytes)
		assert(!underlying.hasRemaining)
		new String(bytes, charset)
	}

}
