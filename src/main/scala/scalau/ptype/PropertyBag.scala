package scalau.ptype

import Function.tupled
import reflect.Manifest
import scalau.Misc._
import scalau.Misc


final class PropertyValue(val rawValue: Any) {

  def int: Option[Int] = value[Int]

  def boolean: Option[Boolean] = value[Boolean]

  def string: Option[String] = value[String]

  def bag: Option[PropertyBag] = value[PropertyBag]

  def value[B](implicit manifest: Manifest[B]): Option[B] = {
    if (isInstance[B](rawValue)) Some(rawValue.asInstanceOf[B]) else None
  }

  override def toString = "[Property Value %s]".format(rawValue)

}

trait PropertyUser[-F, +T] extends (F => T) {

  def keyNotFound(key: Symbol): Unit = ()

  def unexpectedType(key: Symbol, expected: Manifest[_], actual: Class[_]): Unit = ()

}

object PropertyUser {

  implicit def functionToPropUser[F, T](f: F => T) = new PropertyUser[F, T] {
    def apply(from: F) = f(from)
  }

  def functionToStdOutPropUser[F, T](f: F => T) = new PropertyUser[F, T] {

    def apply(from: F) = f(from)

    override def unexpectedType(key: Symbol, expected: Manifest[_],
        actual: Class[_]) {
      printf("Expected type '%s' for key %s: %s%n", expected, key, actual.getName)
    }

    override def keyNotFound(key: Symbol) {
      printf("Key %s not found%n", key)
    }
  }

}

trait PropertyBag {

  type Prototype <: PropertyBag

  val prototype: Option[Prototype]

  final def apply(key: Symbol): Option[PropertyValue] = {
    get(key).orElse(prototype.flatMap(_(key)))
  }

  def get(key: Symbol): Option[PropertyValue]

  def properties: Collection[(Symbol, PropertyValue)]

  def using[F, T](key: Symbol)(block: PropertyUser[F, T])(implicit m: Manifest[F]): Option[T] = {
    apply(key) match {
      case None => block.keyNotFound(key); None
      case Some(pv) => pv.value[F] match {
        case None => block.unexpectedType(key, m, Misc.getClass(pv.rawValue)); None
        case Some(value) => Some(block(value))
      }
    }
  }

}

trait ListBackedBag extends PropertyBag {

  val properties: List[(Symbol, PropertyValue)]


  def get(key: Symbol) = properties.find(_._1 == key).map(_._2)

}


class ValueExtract {

  type T

  def unapply(pv: PropertyValue)(implicit manifest: Manifest[T])
      : Option[T] = pv.value(manifest)

}

object StringProp extends ValueExtract {

  type T = String

}

object IntProp extends ValueExtract {

  type T = Int

}

object BooleanProp extends ValueExtract {

  type T = Boolean

}

object BagProp extends ValueExtract {

  type T = PropertyBag

}

object PropSet {

  def unapply(pv: PropertyValue): Boolean = pv.boolean.getOrElse(false)

}


object Prototype {

  def valueExtract[A] = new ValueExtract { type T = A }

  def bag[A <: PropertyBag](aprototype: Option[A], props: (Symbol, Any)*)
          : PropertyBag { type Prototype = A } = new ListBackedBag {
    val properties = props.map(tupled((s, v) => (s, new PropertyValue(v)))).toList
    val prototype = aprototype
    type Prototype = A
  }

  def bag(props: (Symbol, Any)*): PropertyBag = bag(None, props: _*)

}

object Test {

  def test {
    val bag: PropertyBag = Prototype.bag('name -> "fred",
      'more -> Prototype.bag('val -> 5, 'boolean -> true))
    val bag2: PropertyBag = Prototype.bag('fred -> 666)

    (bag('name): @unchecked) match {
      case Some(StringProp(v)) => println(v)
    }
    val r = for {
      StringProp(value) <- bag('name)
      IntProp(intValue) <- bag2(Symbol(value))
    }
    yield (value, intValue)
    println(r)

    val r2 = for {
      StringProp(value) <- bag2('fred)
      IntProp(intValue) <- bag2(Symbol(value))
    }
    yield (value, intValue)
    println(r2)

    val r3 = for {
      StringProp(value) <- bag('notReal)
      IntProp(intValue) <- bag2(Symbol(value))
    }
    yield (value, intValue)
    println(r3)

    (bag2('fred): @unchecked) match {
      case Some(_) => println("exists")
    }
    (bag('fred): @unchecked) match {
      case None => println("doesn't exist")
    }

    (bag('more): @unchecked) match {
      case Some(BagProp(inner)) => (inner('boolean): @unchecked) match {
        case Some(PropSet()) => println("prop is set")
      }
    }

    implicit def convert[F, T](f: F => T) = PropertyUser.functionToStdOutPropUser(f)

    printf("withProp1 = %d%n", (bag2.using('fred) {(_ : Int) + 2}).get)
    printf("withProp2 = %s%n", bag2.using('unknown) { i: Int => i + 3})
    printf("withProp3 = %s%n", bag2.using('fred) {s: String => "found string: [%s]".format(s)})
  }
}
