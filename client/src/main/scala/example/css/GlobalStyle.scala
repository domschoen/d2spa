package example.css

import scalacss.Defaults._

object GlobalStyle extends StyleSheet.Inline {
  import dsl._

  val big =
  style(
    unsafeRoot("body")(
      margin.`0`,
      padding.`0`,
      fontSize(14.px),
      fontFamily := "Roboto, sans-serif"
    ),
    unsafeRoot("div#a")(
      //width(auto),
      maxWidth(1735 px),
      marginLeft(20 px),
      marginTop(15 px)
    ),
    unsafeRoot("div#b ul.action")(
      border(1 px,inset, gray),
      backgroundColor(white),
      //-webkitAppearance(listbox),
      maxHeight(250 px),
      //overflowY(scroll),
      //overflowX(hidden),
      width(150 px),
      fontSize(12 px)
    )
  )

  val pageTitle = style(
    unsafeRoot("table")(
      borderSpacing(0.px),
      width(100 %%),
      border(0 px)
    )
  )
  val buttonBar = style(
    unsafeRoot("td")(
      fontFamily := "Arial, sans-serif",
      color(c"#0c1830"),
      //backgroundColor(c"#aabbcc"),
      fontSize(10 pt),
      //align(left),
      width(100 %%)
    )
  )


}
