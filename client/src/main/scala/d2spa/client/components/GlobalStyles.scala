package d2spa.client.components

import d2spa.client.CssSettings._

object GlobalStyles extends StyleSheet.Inline {
  import dsl._

  style(unsafeRoot("body")(
    paddingTop(70.px))
  )

  val bootstrapStyles = new BootstrapStyles
}
