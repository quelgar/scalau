package scalau.ptype


trait HasProperties {

  type Prototype <: HasProperties

  val prototype: Prototype

  def apply(key: Symbol): 
}
