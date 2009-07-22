package scalau.text


object DomainName {

	def apply(parts: String*) = parts.reverse.mkString(".")

	def unapplySeq(domainName: String): Option[Seq[String]] = Some(domainName.split("\\.").reverse)

}
