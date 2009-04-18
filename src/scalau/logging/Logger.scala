package scalau.logging

import java.util.logging.{Logger => JLogger, _}
import java.util.logging.Level._
import java.util.ResourceBundle


class Logger(val javaLogger: JLogger) {

  def isLoggable(level: Level) = javaLogger.isLoggable(level)

  def logRecord(level: Level)(msg: => String, recordInit: (LogRecord) => Unit) {
    if (isLoggable(level)) {
      val record = new LogRecord(level, msg)
      recordInit(record)
      javaLogger.log(record)
    }
  }

  def log(level: Level)(msg: => String, args: Any*) {
    if (isLoggable(level)) {
      if (args.isEmpty) {
        javaLogger.log(level, msg)
      }
      else {
        javaLogger.log(level, msg, Misc.anyToJavaObjectArray(args))
      }
    }
  }

  def logThrown(level: Level)(msg: => String, thrown: Throwable, args: Any*) {
	  if (args.isEmpty && isLoggable(level))
	    javaLogger.log(level, msg, thrown)
	  else
      log(level)(msg, { (rec: LogRecord) =>
		      rec.setThrown(thrown)
          rec.setParameters(Misc.anyToJavaObjectArray(args))
      })
  }

  def severe(msg: => String, args: Any*): Unit = log(SEVERE) _

  def severe(msg: => String, thrown: Throwable, args: Any*): Unit = logThrown(SEVERE) _

  def warning(msg: => String, args: Any*): Unit = log(WARNING) _

  def warning(msg: => String, thrown: Throwable, args: Any*): Unit = logThrown(WARNING) _

	def info(msg: => String, args: Any*): Unit = log(INFO) _

	def info(msg: => String, thrown: Throwable, args: Any*): Unit = logThrown(INFO) _

	def config(msg: => String, args: Any*): Unit = log(CONFIG) _

	def config(msg: => String, thrown: Throwable, args: Any*): Unit = logThrown(CONFIG) _

	def fine(msg: => String, args: Any*): Unit = log(FINE) _

	def fine(msg: => String, thrown: Throwable, args: Any*): Unit = logThrown(FINE) _

	def finer(msg: => String, args: Any*): Unit = log(FINER) _

	def finer(msg: => String, thrown: Throwable, args: Any*): Unit = logThrown(FINER) _

	def finest(msg: => String, args: Any*): Unit = log(FINEST) _

	def finest(msg: => String, thrown: Throwable, args: Any*): Unit = logThrown(FINEST) _

  def level = javaLogger.getLevel

  def level_=(level: Level) { javaLogger.setLevel(level) }

}

object Logger {

  def logger(name: String) = new Logger(JLogger.getLogger(name))

  def logger(name: Class[_]) = new Logger(JLogger.getLogger(name.getName))

  def logger(name: String, resourceBundleName: String) =
    new Logger(JLogger.getLogger(name, resourceBundleName))

  def logger(name: Class[_], resourceBundleName: String) =
    new Logger(JLogger.getLogger(name.getName, resourceBundleName))

    implicit def logger(javaLogger: JLogger) = new Logger(javaLogger)

}
