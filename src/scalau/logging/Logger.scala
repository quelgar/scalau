package scalau.logging

import java.util.logging.{Logger => JLogger, _}
import java.util.logging.Level._
import java.util.ResourceBundle


class Logger(val javaLogger: JLogger) {

  def isLoggable(level: Level) = javaLogger.isLoggable(level)

  def logRecord(level: Level, msg: => String, recordInit: (LogRecord) => Unit) {
    if (isLoggable(level)) {
      val record = new LogRecord(level, msg)
      recordInit(record)
      javaLogger.log(record)
    }
  }

  def log(level: Level, msg: => String, args: AnyRef*) {
    if (isLoggable(level)) {
      if (args.isEmpty) {
        javaLogger.log(level, msg)
      }
      else {
        javaLogger.log(level, msg, args)
      }
    }
  }

  def logThrown(level: Level, msg: => String, thrown: Throwable) {
    if (isLoggable(level)) {
      javaLogger.log(level, msg, thrown)
    }
  }

  def logThrown(level: Level, msg: => String, thrown: Throwable, args: AnyRef*) {
    log(level, msg, { (_: LogRecord).setThrown(thrown) } )
  }

  def severe(msg: => String) {
    log(SEVERE, msg)
  }

  def severe(msg: => String, thrown: Throwable) {
    log(SEVERE, msg, thrown)
  }

  def warning(msg: => String) {
    log(WARNING, msg)
  }

  def warning(msg: => String, thrown: Throwable) {
    log(WARNING, msg)
  }


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
