package d2spa.client.logger
import d2spa.client.PageContext
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

    def logDebugWithD2WContext(pageContext: PageContext, text: String) = {
        val d2wContext = pageContext.d2wContext
        val entityFilter: String = d2wContext.entityName match {
            case Some(entityName) => entityName
            case None => ALL
        }
        D2SpaLogger.logfinest(entityFilter,text)
    }

}
