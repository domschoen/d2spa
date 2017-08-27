package d2spa.shared
import scala.concurrent.Future

trait Api {
  // message of the day
  def welcomeMsg(name: String): String
  
  def search(qualifier: EOKeyValueQualifier): Seq[EO]

  def getMenus(): Future[Menus]

}
