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

  def child(props: (Symbol, Any)*): PropertyBag

  def set(props: (Symbol, Any)*): PropertyBag

  def delete(keys: Symbol*): PropertyBag

  override def toString = "[Property Bag %s]".format(
    Stream.fromIterator(properties.elements).take(3).map(p => (p._1, p._2.rawValue)).mkString(", "))
}

object PropertyBag {

  def makeValues(props: Seq[(Symbol, Any)]): Seq[(Symbol, PropertyValue)] = {
    props.map(tupled((k, v) => (k, new PropertyValue(v))))
  }

  def apply(props: (Symbol, Any)*): PropertyBag = {
    new ListBackedBag {
      val properties = makeValues(props).toList
      val prototype = None
      type Prototype = Nothing
    }
  }

  def valueExtract[A] = new ValueExtract { type T = A }
  
}

trait ListBackedBag extends PropertyBag {

  import PropertyBag.makeValues

  val properties: List[(Symbol, PropertyValue)]

  def get(key: Symbol) = properties.find(_._1 == key).map(_._2)

  def delete(keys: Symbol*) = new ListBackedBag {
    val properties = ListBackedBag.this.properties.remove(p => keys.exists(_ == p._1))
    val prototype = ListBackedBag.this.prototype
    type Prototype = ListBackedBag.this.Prototype
  }

  def set(props: (Symbol, Any)*) = new ListBackedBag {
    val properties = makeValues(props).toList :::
        ListBackedBag.this.properties.remove(p => props.exists(_._1 == p._1))
    val prototype = ListBackedBag.this.prototype
    type Prototype = ListBackedBag.this.Prototype
  }

  def child(props: (Symbol, Any)*) = new ListBackedBag {
    type Prototype = ListBackedBag
    val properties = makeValues(props).toList
    val prototype = Some(ListBackedBag.this)
  }
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

}

object Test {

  def test {
    val bag: PropertyBag = PropertyBag('name -> "fred",
      'more -> PropertyBag('val -> 5, 'boolean -> true))
    val bag2: PropertyBag = PropertyBag('fred -> 666)

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

    val sub = bag.child('name -> "bill",
      'another -> 55D)
    printf("prototype prop = %s%n", sub('more))
    printf("child name = %s%n", sub('name).get)
  }
}
