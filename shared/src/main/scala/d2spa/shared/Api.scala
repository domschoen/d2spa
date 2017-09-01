package d2spa.shared
import scala.concurrent.Future

trait Api {

  def search(entity: String, qualifier: EOKeyValueQualifier): Future[Seq[EO]]

  def getMenus(): Future[Menus]

  def getMetaData(): MetaDatas

  def updateEO(entity: String, eo: EO): Future[EO]

  def newEO(entity:String) : Future[EO]

}
