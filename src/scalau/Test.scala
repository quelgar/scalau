
package scalau


import java.nio.charset.Charset

object Test {

	def main(args: Array[String]) {

		val data = ByteData.wrap("this is a test xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ ¼", Charset.defaultCharset)
		println(data);
	}
}
