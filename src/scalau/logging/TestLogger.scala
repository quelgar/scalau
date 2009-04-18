package scalau.logging

import java.util.logging.Level._
import java.util.logging.LogRecord


object TestLogger extends Application {

  val logger1 = Logger.logger("one")
  logger1.level = SEVERE
  logger1.severe(msg("At severe"))
  logger1.warning(msg("At warning"))
	logger1.warning(msg("with arg {0}"), 666)
  logger1.logRecord(WARNING)("record warning", recordInit _)
  logger1.logRecord(SEVERE)("record severe", recordInit _)
	logger1.warning("test thrown", new NullPointerException)
	logger1.warning("test thrown, args = {0}, {1}", new NullPointerException, "beast", 666)
	logger1.logThrown(WARNING)("test thrown, arg = {0}", new NullPointerException, "arg with exception")

  private def msg(s: String) = {
    printf("Generating %s%n", s)
    s
  }

  private def recordInit(rec: LogRecord) {
    rec.setSourceClassName("XXX Class Name XXX")
  }

}
