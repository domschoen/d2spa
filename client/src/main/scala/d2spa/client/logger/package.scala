package d2spa.client

import java.util.logging._
import java.util.logging.ConsoleHandler
import java.util.logging.Handler

package object logger {
  val log = {
    val ch = new BlackConsoleHandler()
    ch.setLevel(Level.ALL)

    val rootLog = Logger.getLogger("")
    rootLog.setLevel( Level.ALL)
    val handlers = rootLog.getHandlers
    println("handlers " + handlers.size)
    //handlers(0).setLevel(Level.ALL)
    rootLog.addHandler(ch)

    rootLog
  }


  def logSolution1 = {
    val ch = new ConsoleHandler()
    ch.setLevel(Level.ALL)
    val logger = Logger.getGlobal()
    logger.addHandler(ch)
    logger.setLevel( Level.ALL)
    //Logger.getLogger( "" ).setLevel( Level.OFF )
    logger.setUseParentHandlers(false)
    logger
  }

}
