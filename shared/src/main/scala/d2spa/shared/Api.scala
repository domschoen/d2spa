package d2spa.shared
import scala.concurrent.Future

trait Api {
  
  def getDebugConfiguration() : Future[DebugConf]
  def searchAll(fs: EOFetchAll): Future[Seq[EO]]
  def search(fs: EOQualifiedFetch): Future[Seq[EO]]

  def getMenus(): Future[Menus]

  def getMetaData(d2wContext: D2WContextFullFledged): Future[EntityMetaData]

  def newEO(entityName: String, eo: EO): Future[EO]

  def updateEO(eo: EO): Future[EO]
  def deleteEO(eo: EO): Future[EO]
  def completeEO(eo: EOFault, missingKeys: Set[String]): Future[EO]
  def hydrateEOs(entityName: String, pks: Seq[Int], missingKeys: Set[String]): Future[Seq[EO]]
  def fireRule(rhs: D2WContextFullFledged, key: String): Future[RuleResult]
  def fetchEOModel(): Future[EOModel]

}
