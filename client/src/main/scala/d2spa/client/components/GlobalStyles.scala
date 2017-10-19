package d2spa.client.components

import d2spa.client.CssSettings._

object GlobalStyles extends StyleSheet.Inline {
  import dsl._

  /*style(unsafeRoot("body")(
    paddingTop(70.px))
  )*/

  val menuAddon = style(
    addClassNames("input-group-addon"),
    textAlign.center,
    borderRight(1.px,solid,c"#787c83"),
    borderTop(1.px,solid,c"#787c83"),
    borderBottom(1.px,solid,c"#787c83"),
    backgroundColor(transparent),
    padding(6.px,20.px),
    borderRadius(0.px),
    fontSize(18.px),
    lineHeight(27.px)
  )


  val bootstrapStyles = new BootstrapStyles
}
