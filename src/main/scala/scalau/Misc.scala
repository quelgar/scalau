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

	def ?[A <: AnyRef](nullable: A): Option[A] = if (nullable eq null) None else Some(nullable)

  // XXX - Not sure this is a good idea
  def ??[A <: AnyRef](block: => A): Option[A] = try {
    ?(block)
  }
  catch {
    case ex: NullPointerException => None
  }

	def toNullable[A](option: Option[A]): A = option.getOrElse(null.asInstanceOf[A])

	def ?[A <: AnyRef](nullable: A, default: => A) = if (nullable eq null) default else nullable

	implicit def mapToProperties(map: Map[String, String]): Properties = {
		val p = new Properties()
		for ((k, v) <- map) {
			p.setProperty(k, v)
		}
		p
	}

	def anyToJavaObject(any: Any): Object = any match {
		case x: Boolean => java.lang.Boolean.valueOf(x)
		case x: Byte => java.lang.Byte.valueOf(x)
		case x: Short => java.lang.Short.valueOf(x)
		case x: Char => java.lang.Character.valueOf(x)
		case x: Int => java.lang.Integer.valueOf(x)
		case x: Long => java.lang.Long.valueOf(x)
		case x: Float => java.lang.Float.valueOf(x)
		case x: Double => java.lang.Double.valueOf(x)
		case x: AnyRef => x
	}

	def anyToJavaObjectArray(any: Any*): Array[Object] = {
		(for (a <- any) yield anyToJavaObject(a)).toArray
	}

    //	implicit def richReference[A](ref: A) = new RichReference(ref)

  implicit def javaCharSequence2RandomAccessSeq(charSeq: CharSequence): RandomAccessSeq[Char] = new RandomAccessSeq[Char] {
    def length = charSeq.length

    def apply(t: Int) = charSeq.charAt(t)
  }

  def promoteLeft[L, R](i: Iterable[Either[L, R]]): Either[L, List[R]] =
    i.foldRight(Right(Nil): Either[L, List[R]]) {
      (item, either) => item match {
        case Left(left) => Left(left)
        case Right(right) => either match {
          case x@Left(_) => x
          case Right(xs) => Right(right :: xs)
        }
      }
    }

}

//
//final class RichReference[A] private[scalau] (val ref: A) {
//
//	def unary_~ = Misc.fromNullable(ref)
//
//}
