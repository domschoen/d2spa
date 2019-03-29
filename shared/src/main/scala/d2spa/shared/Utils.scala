package d2spa.shared

object Utils {

  def escapeHtml(html: String) = {
    html.replaceAll("<","&lt;").replaceAll(">", "&gt;")
  }
  def juiceStringOrDate(str: String) : String = {
    str/*
    try {
      val dateInstant = Instant.parse(str)
      val someDate = LocalDateTime.ofInstant(dateInstant, ZoneId.of("GMT+2"));
      //someDate.year().toString + "-" + someDate.month() + "-" + someDate.dayOfMonth()
      someDate.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")).toString
    }catch {
      case _ =>{
        return str; //Strings other than date will throw exception and will be handled here
      }
    }*/
  }

}
