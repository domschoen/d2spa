package d2spa.client

object Utils {

  def escapeHtml(html: String) = {
    html.replaceAll("<","&lt;").replaceAll(">", "&gt;")
  }

}
