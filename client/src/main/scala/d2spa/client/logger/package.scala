package d2spa.client

package object logger {
  private val defaultLogger = LoggerFactory.getLogger("Log")

  def log = defaultLogger
}
