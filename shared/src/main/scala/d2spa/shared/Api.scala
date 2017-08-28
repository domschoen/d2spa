package d2spa.shared
import scala.concurrent.Future

trait Api {

  def search(qualifier: EOKeyValueQualifier): Future[Seq[EO]]

  def getMenus(): Future[Menus]

  def getMetaData(): MetaDatas

}
