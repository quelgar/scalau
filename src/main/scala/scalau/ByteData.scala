package scalau

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.Charset


final class ByteData private(private val buffer: ByteBuffer) {

	def apply(index: Int) = buffer.get(index)

	def length = buffer.remaining

	def isEmpty = length == 0

	def asReadOnlyBuffer() = buffer.asReadOnlyBuffer

	def toArray[B >: Byte : Manifest]: Array[B] = {
		val dst = new Array[B](length)
		copyToArray(dst, 0)
		dst
	}

	def copyToArray[B >: Byte](dst: Array[B], start: Int): Unit = {
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

	def decode(charset: Charset): CharBuffer = charset.decode(buffer.asReadOnlyBuffer)

	def decodeToString(charset: Charset): String = decode(charset).toString

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

	implicit def toSeq = new collection.immutable.IndexedSeq[Byte] {

		val length = ByteData.this.length

		def apply(index: Int): Byte = ByteData.this(index)
	}

	def drop(n: Int): ByteData = {
		require(n >= 0)
		if (n >= length)
			ByteData.empty
		else {
			val buf = asReadOnlyBuffer()
			buf.position(buf.position + n)
			new ByteData(buf)
		}
	}

	def take(n: Int): ByteData = {
		require(n >= 0)
		if (n >= length)
			this
		else {
			val buf = asReadOnlyBuffer()
			buf.limit(buf.position + n)
			new ByteData(buf)
		}
	}

	def slice(from: Int, to: Int): ByteData = {
		val buf = asReadOnlyBuffer()
		val pos = buf.position
		buf.position(pos + from)
		buf.limit(pos + to)
		new ByteData(buf)
	}

	def foreach(f: Byte => Unit): Unit = {
		val buf = asReadOnlyBuffer()
		while (buf.hasRemaining) {
			f(buf.get)
		}
	}

	def toStream: Stream[Byte] = {
		def toStreamFrom(start: Int): Stream[Byte] = {
			require(start >= 0)
			if (start >= length)
				Stream.empty
			else
				Stream.cons(apply(start), toStreamFrom(start + 1))
		}
		toStreamFrom(0)
	}

}

object ByteData {

	private[ByteData] val TOSTRING_LIMIT = 40;

	val empty = new ByteData(ByteBuffer.wrap(new Array[Byte](0)))

	def wrap(buffer: ByteBuffer) = new ByteData(buffer.asReadOnlyBuffer)

	def wrap(bytes: Byte*) = new ByteData(ByteBuffer.wrap(bytes.toArray).asReadOnlyBuffer)

	def wrap(bytes: Array[Byte]) = new ByteData(ByteBuffer.wrap(bytes).asReadOnlyBuffer)

	def wrap(bytes: Array[Byte], offset: Int, length: Int) =
		new ByteData(ByteBuffer.wrap(bytes, offset, length).asReadOnlyBuffer)

	def wrap(s: String, charset: Charset): ByteData = wrap(s.getBytes(charset))

	def copy(buffer: ByteBuffer, length: Int) = {
		val array = new Array[Byte](length)
		buffer.get(array)
		wrap(array)
	}

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
