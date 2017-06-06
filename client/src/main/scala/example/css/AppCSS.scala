package example.css

import scalacss.Defaults._
import scalacss.internal.mutable.GlobalRegistry
//import todomvc.{TopNav}

object AppCSS {

  def load = {
    GlobalRegistry.register(GlobalStyle)
      //TopNav.Style)
    GlobalRegistry.onRegistration(_.addToDocument())
  }
}
