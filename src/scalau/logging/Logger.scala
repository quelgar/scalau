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
				javaLogger.log(level, msg, Misc.anyToJavaObjectArray(args: _*))
			}
		}
	}

	def logThrown(level: Level)(thrown: Throwable, msg: => String, args: Any*) {
		if (args.isEmpty && isLoggable(level))
			javaLogger.log(level, msg, thrown)
		else
			logRecord(level)(msg, {
				(rec: LogRecord) =>
						rec.setThrown(thrown)
						rec.setParameters(Misc.anyToJavaObjectArray(args: _*))
			})
	}

	def severe(msg: => String, args: Any*): Unit = log(SEVERE)(msg, args: _*)

	def severe(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(SEVERE)(thrown, msg, args: _*)

	def warning(msg: => String, args: Any*): Unit = log(WARNING)(msg, args: _*)

	def warning(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(WARNING)(thrown, msg, args: _*)

	def info(msg: => String, args: Any*): Unit = log(INFO)(msg, args)

	def info(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(INFO)(thrown, msg, args)

	def config(msg: => String, args: Any*): Unit = log(CONFIG)(msg, args)

	def config(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(CONFIG)(thrown, msg, args)

	def fine(msg: => String, args: Any*): Unit = log(FINE)(msg, args)

	def fine(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(FINE)(thrown, msg, args)

	def finer(msg: => String, args: Any*): Unit = log(FINER)(msg, args)

	def finer(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(FINER)(thrown, msg, args)

	def finest(msg: => String, args: Any*): Unit = log(FINEST)(msg, args)

	def finest(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(FINEST)(thrown, msg, args)

	def level = javaLogger.getLevel

	def level_=(level: Level) {javaLogger.setLevel(level)}

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
