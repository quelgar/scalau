package scalau.text


object Version {

  private val numberRE = """^([0-9]+)$""".r

  def apply(parts: Int*): String = parts.mkString(".")

  def unapplySeq(s: String): Option[Seq[Int]] = {
    val list = for (part <- s.split("\\.")) yield {
      numberRE.findFirstIn(part)
    }
    list.foldRight(Some(Nil): Option[List[Int]])((e, option) => option match {
      case None => None
      case Some(l) => e match {
        case None => None
        case Some(stringValue) => Some(stringValue.toInt :: l)
      }
    })
  }
}
