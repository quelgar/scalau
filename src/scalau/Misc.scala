package scalau


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

}
