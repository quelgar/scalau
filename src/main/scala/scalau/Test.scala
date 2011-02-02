
package scalau


import java.nio.charset.Charset
import text._

object Test {

	def main(args: Array[String]) {

		val data = ByteData.wrap("this is a test xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ ¼", Charset.defaultCharset)
		println(data);

		val email = "lodea@mac.com"
		val EMail(name, s@DomainName("com", second, _*)) = email
		printf("name = '%s', second = '%s'%n", name, second)
		printf("s = '%s', class = %s%n", s, s.getClass.getName)
		// this no longer works in Scala 2.8.1
//		for (EMail(n2, d2) <- "blahxx")
//			printf("n2 = $s, d2 = %s%n", n2, d2)

		val EMail(_, DomainName(dom@_*)) = email
		printf("dom = %s%n", dom)
	}
}
