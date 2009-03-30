package scalau

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.Charset


final class ByteData private(private val buffer: ByteBuffer) extends RandomAccessSeq[Byte] {

	def apply(index: Int) = buffer.get(index)

	def length = buffer.remaining

	def asReadOnlyBuffer = buffer.asReadOnlyBuffer

	override def toArray[B >: Byte]: Array[B] = {
		val dst = new Array[B](length)
		copyToArray(dst, 0)
		dst
	}

	override def copyToArray[B >: Byte](dst: Array[B], start: Int): Unit = {
		val tmpBuf = buffer.asReadOnlyBuffer
		if (dst.isInstanceOf[Array[Byte]]) {
			tmpBuf.get(dst.asInstanceOf[Array[Byte]], start, length)
		}
		else {
			val tmpArray = new Array[Byte](dst.length)
			tmpBuf.get(tmpArray, start, length)
			for (i <- 0 until dst.length) {
				dst(i) = tmpArray(i)
			}
		}
	}

	def copyToArray(dst: Array[Byte]): Unit = copyToArray(dst, 0)

	def toString(charset: Charset) = {
		val tmpBuf = buffer.asReadOnlyBuffer
		if (tmpBuf.hasArray) {
			new String(tmpBuf.array, charset)
		}
		else {
			val tmpArray = new Array[Byte](tmpBuf.remaining)
			tmpBuf.get(tmpArray)
			new String(tmpArray, charset)
		}
	}

	override def toString() = {
		val tmpBuf = buffer.asReadOnlyBuffer
		val cutoff = tmpBuf.remaining > ByteData.TOSTRING_LIMIT;
		if (cutoff) {
			tmpBuf.limit(tmpBuf.position + ByteData.TOSTRING_LIMIT)
		}
		val cb = ByteData.toHex(tmpBuf)
		if (cutoff) cb.put(cb.limit - 1, '…')
		cb.toString
	}

	override def hashCode: Int = buffer.hashCode

	override def equals(o: Any): Boolean = {
		if (!o.isInstanceOf[ByteData]) {
			false
		}
		else {
			buffer.equals(o.asInstanceOf[ByteData].buffer)
		}
	}

}

object ByteData {
	private[ByteData] val TOSTRING_LIMIT = 40;

	def wrap(buffer: ByteBuffer) = new ByteData(buffer.asReadOnlyBuffer)

	def wrap(bytes: Byte*) = new ByteData(ByteBuffer.wrap(bytes.toArray).asReadOnlyBuffer)

	def wrap(bytes: Array[Byte]) = new ByteData(ByteBuffer.wrap(bytes).asReadOnlyBuffer)

	def wrap(bytes: Array[Byte], offset: Int, length: Int) =
		new ByteData(ByteBuffer.wrap(bytes, offset, length).asReadOnlyBuffer)

	def wrap(s: String, charset: Charset): ByteData = wrap(s.getBytes(charset))

	def toHex(bytes: ByteBuffer): CharBuffer = {
		val cb = CharBuffer.allocate(bytes.remaining * 2)
		while (bytes.hasRemaining) {
			val (hi, lo) = toHex(bytes.get())
			cb.put(hi)
			cb.put(lo)
		}
		assert(!cb.hasRemaining)
		cb.flip
		cb
	}

	def toHex(b: Byte): (Char, Char) = {
		(Character.toUpperCase(Character.forDigit((b & 0xF0) >> 4, 16)),
				Character.toUpperCase(Character.forDigit(b & 0x0F, 16)))
	}

}
