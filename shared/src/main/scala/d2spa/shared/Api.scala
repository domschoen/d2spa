package d2spa.shared
import scala.concurrent.Future

trait Api {

  def search(entity: String, qualifiers: List[EOKeyValueQualifier]): Future[Seq[EO]]

  def getMenus(): Future[Menus]

  //def getMetaData(): MetaDatas
  def getMetaData(entity: String): Future[EntityMetaData]

  def updateEO(entity: String, eo: EO): Future[EO]

  def newEO(entity:String) : Future[EO]

}
