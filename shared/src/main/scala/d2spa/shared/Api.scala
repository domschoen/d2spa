package d2spa.shared

trait Api {
  // message of the day
  def welcomeMsg(name: String): String
  
  def search(qualifier: EOKeyValueQualifier): Seq[EO]
}
