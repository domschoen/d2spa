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
    border(0.px),
    backgroundColor(transparent),
    padding(6.px,20.px),
    borderRadius(0.px),
    fontSize(14.px),
    lineHeight(27.px),
    paddingLeft(12.px),
    paddingRight(12.px),
    &.hover(
      backgroundColor(gray)
    )
  )

  val menuItem = style(
    padding(0.px)
  )

  val menuInputGroup = style(
    addClassNames("input-group"),
    height(40.px)
  )

  val menuLabel = style(
    paddingLeft(10.px),
    paddingTop(10.px)
  )

  val bootstrapStyles = new BootstrapStyles
}
