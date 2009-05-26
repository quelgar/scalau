package scalau.logging

import java.util.logging.LogRecord


final class TestLoggingConfig extends LoggingConfig {

	def config {
		val jndiFileHandler = fileHandler("blah")

		logger("jndilogger").addHandler(jndiFileHandler).level(FINEST)

		logger("anotherlogger").level(FINER)

	}
}

object TestLogger extends Application {

	import java.util.logging.Level._

	val logger1 = Logger.logger("one")
	logger1.level = SEVERE
	logger1.severe(msg("At severe"))
	logger1.severe("severe with arg {0}", 666)
	logger1.severe("severe with arg {0}", 668)
	logger1.warning(msg("At warning"))
	logger1.warning(msg("with arg {0}"), 666)
	logger1.logRecord(WARNING)("record warning", recordInit _)
	logger1.logRecord(SEVERE)("record severe", recordInit _)
	logger1.level = INFO
	logger1.warning(new NullPointerException, "test thrown")
	logger1.warning(new NullPointerException, "test thrown, args = {0}, {1}", "beast", 666)
	logger1.logThrown(WARNING)(new NullPointerException, "test thrown, arg = {0}", "arg with exception")

	private def msg(s: String) = {
		printf("Generating %s%n", s)
		s
	}

	private def recordInit(rec: LogRecord) {
		println("record init")
		rec.setSourceClassName("XXX Class Name XXX")
	}

}
