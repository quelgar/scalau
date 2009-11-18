package scalau.ptype

import Function.tupled
import reflect.Manifest
import scalau.Misc._


sealed abstract class PropertyValue {

  val rawValue: Any

  val isDefined: Boolean

  def int: Option[Int]

  def boolean: Option[Boolean]

  def string: Option[String]

  def bag: Option[HasProperties]

  def value[A](implicit manifest: Manifest[A]): Option[A]

  def filter(extract: PropertyValue => Boolean): PropertyValue = {
    if (isDefined && extract(this)) this else NullPropertyValue
  }

  def map[A](m: PropertyValue => A): PropertyValue

  def flatMap[A](m: PropertyValue => PropertyValue): PropertyValue = {
    if (isDefined) m(this) else NullPropertyValue
  }
  
}

object NullPropertyValue extends PropertyValue {


  val rawValue = ()

  val isDefined = false

  def bag = None

  def string = None

  def boolean = None

  def int = None

  def value[A](implicit manifest: Manifest[A]) = None

  def map[A](m: (PropertyValue) => A) = this

  override def toString = "[Null Property Value]"

}

final class PropertyValueDefined(val rawValue: Any) extends PropertyValue {

  def value[A](implicit manifest: Manifest[A]) = {
    if (isInstance[A](rawValue)) Some(rawValue.asInstanceOf[A]) else None
  }

  def bag = value[HasProperties]

  def string = value[String]

  def boolean = value[Boolean]

  def int = value[Int]

  val isDefined = true

  def map[A](m: (PropertyValue) => A) = new PropertyValueDefined(m(this))

  override def toString = "[Property Value %s]".format(rawValue)

}

trait HasProperties {

  type Prototype <: HasProperties

  val prototype: Option[Prototype]

  def apply(key: Symbol): PropertyValue

  def properties: Collection[(Symbol, PropertyValue)]

}

trait ListBackedBag extends HasProperties {

  val properties: List[(Symbol, PropertyValue)]


  def apply(key: Symbol) = properties.find(_._1 == key).map(_._2).getOrElse(NullPropertyValue)

}

class ValueExtract {

  type T

  def unapply(pv: PropertyValue)(implicit manifest: Manifest[T]): Option[T] = pv.value(manifest)

}

object StringProp extends ValueExtract {

  type T = String

}

object IntProp extends ValueExtract {

  type T = Int

}

object PropExists {

  def unapply(pv: PropertyValue): Boolean = pv.isDefined

}

object NullProperty {

  def unapply(pv: PropertyValue): Boolean = !pv.isDefined

}

object Prototype {

  def valueExtract[A] = new ValueExtract { type T = A }

  def bag(props: (Symbol, Any)*): HasProperties = new ListBackedBag {
    val properties = props.map(tupled((s, v) => (s, new PropertyValueDefined(v)))).toList
    val prototype = None
    type Prototype = Nothing
  }

}

object Test {

  def test {
    val bag: HasProperties = Prototype.bag('name -> "fred")
    val bag2: HasProperties = Prototype.bag('fred -> 666)

    bag('name) match {
      case StringProp(v) => println(v)
    }
    val r = for {
      StringProp(value) <- bag('name)
      IntProp(intValue) <- bag2(Symbol(value))
    }
    yield (value, intValue)
    println(r)

    val r2 = for {
      StringProp(value) <- bag('blah)
      IntProp(intValue) <- bag2(Symbol(value))
    }
    yield (value, intValue)
    println(r2)

    bag2('fred) match {
      case PropExists() => println("exists")
    }
    bag('fred) match {
      case NullProperty() => println("doesn't exist")
    }
  }
}
