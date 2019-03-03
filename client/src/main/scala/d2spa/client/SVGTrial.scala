package d2spa.client

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.components.D2WComponentInstaller
import d2spa.client.components.NVListComponent.Props
import d2spa.client.logger._
import d2spa.shared._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.all.key
import japgolly.scalajs.react.vdom.SvgTags._
import japgolly.scalajs.react.vdom.SvgAttrs._


import paths.high.Bar



object SVGTrial {
  case class Stats(values: List[List[Double]], labels: List[String])
  case class Color(r: Double, g: Double, b: Double, alpha: Double = 1)

  val stats = Stats(
    values = List(
      List(1, 2.0, 3, 4),
      List(2, 3.0, 1, 4),
      List(2, 2.5, 3, 3)
    ),
    labels = List("2009", "2010", "2011", "2012")
  )

  def string(c: Color) =
    if (c.alpha == 1) s"rgb(${ c.r.floor },${ c.g.floor },${ c.b.floor })"
    else s"rgba(${ c.r.floor },${ c.g.floor },${ c.b.floor },${ c.alpha })"


  def cut(x: Double) = x.floor min 255

  def average(c1: Color, c2: Color) =
    Color(
      cut((c1.r + c2.r) / 2),
      cut((c1.g + c2.g) / 2),
      cut((c1.b + c2.b) / 2),
      (c1.alpha + c2.alpha / 2)
    )

  def multiply(factor: Double) = { c: Color =>
    Color(cut(factor * c.r), cut(factor * c.g), cut(factor * c.b), c.alpha)
  }


  val lighten = multiply(1.2)
  val darken = multiply(0.8)


  def mix(c1: Color, c2: Color) = {
    val c3 = average(c1, c2)
    val colors = List(
      lighten(c1),
      c1,
      darken(c1),
      lighten(c3),
      c3,
      darken(c3),
      lighten(c2),
      c2,
      darken(c2)
    )

    Stream.continually(colors).flatten
  }

  private val palette = mix(Color(130, 140, 210), Color(180, 205, 150))
  private def below(p: Array[Double]) = s"translate(${ p(0) }, 320)"


  case class Props(ctl: RouterCtl[TaskAppPage])


  class Backend($ : BackendScope[Props, Unit]) {
    def didMounted(p: Props) = {


      Callback.empty
    }

    def render(p: Props) = {
      //var Pie = require('paths/pie');
      val bar = Bar[Double](
        data = stats.values,
        accessor = identity,
        width = 380,
        height = 300,
        gutter = 10,
        offset = (80, 50)
      )
      val groups = stats.values.length
      val middle = groups / 2
      val count = stats.values.head.length
/*
      val rectangles = bar.curves.zipWithIndex map { case (curve, i) =>
        if (curve.index == middle) g(
          path(d := curve.line.path.print, stroke := "none", fill := string(palette(curve.index))),
          text(transform := below(curve.line.centroid), textAnchor := "middle", stats.labels(i / count))
        )
        else path(d := curve.line.path.print, stroke := "none", fill := string(palette(curve.index)))
      }

      svg(width := 460, height := 400,
        rectangles
      )*/
      <.div("")
    }
  }




  private val component = ScalaComponent.builder[Props]("D3Trial")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.didMounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage]) = {
    component(Props(ctl))
  }
}
