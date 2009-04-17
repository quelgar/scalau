package scalau

import java.util.Properties


object Misc {

	def repeat(block: => Unit)(count: Int) {
		var i = 1
		while (i <= count) {
			block
			i += 1
		}
	}

	// manual currying because of Scala #302
	def repeatGen[A](generator: => A): (Int) => Array[A] = (count: Int) => {
		var i = 0
		val result = new Array[A](count)
		while (i < count) {
			result(i) = generator
			i += 1
		}
		result
	}

	def fromNullable[A](nullable: A): Option[A] = if (nullable == null) None else Some(nullable)

	def toNullable[A](option: Option[A]): A = option.getOrElse(null.asInstanceOf[A])

	implicit def mapToProperties(map: Map[String, String]): Properties = {
		val p = new Properties()
		for ((k, v) <- map) {
			p.setProperty(k, v)
		}
		p
	}

//	implicit def richReference[A](ref: A) = new RichReference(ref)

}

//
//final class RichReference[A] private[scalau] (val ref: A) {
//
//	def unary_~ = Misc.fromNullable(ref)
//
//}
