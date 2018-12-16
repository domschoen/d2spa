package d2spa.shared

object Utils {

  def escapeHtml(html: String) = {
    html.replaceAll("<","&lt;").replaceAll(">", "&gt;")
  }

}
