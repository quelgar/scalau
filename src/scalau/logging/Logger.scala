package scalau.logging

import java.util.logging.{Logger => JLogger, _}
import java.util.logging.Level._
import java.util.ResourceBundle
import scalau.Misc._


class Logger(val javaLogger: JLogger) {

	def isLoggable(level: Level) = javaLogger.isLoggable(level)

	private def sourceInfo = {
		val stackTrace = Thread.currentThread.getStackTrace()
		(for (frame <- stackTrace.drop(1).find(classOf[Logger].getName() != _.getClassName())) yield {
			val lineNum = frame.getLineNumber()
			val sourceFile = frame.getFileName()
			(frame.getClassName(), if (lineNum < 0 || sourceFile == null)
				frame.getMethodName() else "%s(%s:%d)".format(frame.getMethodName(), sourceFile, lineNum))
		}).getOrElse(("UNKNOWN", "UNKNOWN"))
	}

	def logRecord(level: Level)(msg: => String, recordInit: (LogRecord) => Unit) {
		if (isLoggable(level)) {
			val record = new LogRecord(level, msg)
			val (sourceClass, sourceMethod) = sourceInfo
			record.setSourceClassName(sourceClass)
			record.setSourceMethodName(sourceMethod)
			recordInit(record)
			javaLogger.log(record)
		}
	}

	def log(level: Level)(msg: => String, args: Any*) {
		if (isLoggable(level)) {
			val (sourceClass, sourceMethod) = sourceInfo
			if (args.isEmpty) {
				javaLogger.logp(level, sourceClass, sourceMethod, msg)
			}
			else {
				javaLogger.logp(level, sourceClass, sourceMethod, msg, Misc.anyToJavaObjectArray(args: _*))
			}
		}
	}

	def logThrown(level: Level)(thrown: Throwable, msg: => String, args: Any*) {
		if (args.isEmpty && isLoggable(level)) {
			val (sourceClass, sourceMethod) = sourceInfo
			javaLogger.logp(level, sourceClass, sourceMethod, msg, thrown)
		}
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

	def info(msg: => String, args: Any*): Unit = log(INFO)(msg, args: _*)

	def info(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(INFO)(thrown, msg, args: _*)

	def config(msg: => String, args: Any*): Unit = log(CONFIG)(msg, args: _*)

	def config(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(CONFIG)(thrown, msg, args: _*)

	def fine(msg: => String, args: Any*): Unit = log(FINE)(msg, args: _*)

	def fine(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(FINE)(thrown, msg, args: _*)

	def finer(msg: => String, args: Any*): Unit = log(FINER)(msg, args: _*)

	def finer(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(FINER)(thrown, msg, args: _*)

	def finest(msg: => String, args: Any*): Unit = log(FINEST)(msg, args: _*)

	def finest(thrown: Throwable, msg: => String, args: Any*): Unit = logThrown(FINEST)(thrown, msg, args: _*)

	def level = javaLogger.getLevel

	def level_=(level: Level) {javaLogger.setLevel(level)}

	def parent = Logger.logger(javaLogger.getParent)

	def parent_=(parent: Logger) { javaLogger.setParent(parent.javaLogger) }

	def addHandler(handler: Handler): this.type = {
		javaLogger.addHandler(handler)
		this
	}

	def removeHandler(handler: Handler): this.type = {
		javaLogger.removeHandler(handler)
		this
	}

	def handlers = javaLogger.getHandlers

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


abstract class LoggingConfig {

	final class LoggerConfig(val logger: Logger) {

		def addHandler(handler: Handler) = {
			logger.addHandler(handler)
			this
		}

		def level(level: Level) = {
			logger.level = level
			this
		}

	}

	val FINEST = Level.FINEST
	val FINER = Level.FINER


	config

	def config: Unit

	final def logger(name: String) = new LoggerConfig(Logger.logger(name))

	final def fileHandler(pattern: String) = new FileHandler(pattern)

}
