package d2spa.client.logger
import d2spa.client.D2WContext
import d2spa.client.logger._
import java.util.logging._

object D2SpaLogger {

    val ALL = "ALL"
    val EntityFocus = List("Project","Customer")
    //val EntityFocus = List()


    def logfinest(entityName: String, text: String) = {
      if (entityName.equals(ALL) || EntityFocus.contains(entityName))
        log.finest(entityName + " -> " + text)
    }

    def logDebugWithD2WContext(d2WContext: D2WContext, text: String) = {
        val entityFilter: String = d2WContext.entityName match {
            case Some(entityName) => entityName
            case None => ALL
        }
        D2SpaLogger.logfinest(entityFilter,text)
    }

}
