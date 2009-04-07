package scalau.text


object EMail {

	private val EMailRE = """^(\w+)@((?:\w+\.)*(?:\w+))$""".r

	def apply(name: String, domain: String) = "%s@%s".format(name, domain)

	def unapply(s: CharSequence): Option[(String, String)] = s match {
		case EMailRE(name, domain) => Some((name, domain))
		case _ => None
	}

}
