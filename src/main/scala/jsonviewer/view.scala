package jsonviewer

import colibri.Observer
import outwatch._
import outwatch.dsl._
import io.circe._
import jsonviewer.reducer._
import jsonviewer.utils._
import libs.humanizeString

import scala.util.{Failure, Success, Try}

object view {
  def canRenderAsTable(xs: List[Json]): Boolean = {
    val allProps: Set[String] = getAllProps(xs.flatMap(_.asObject))

    val sharedProps = allProps.filter(p => {
      xs.forall(_.asObject.exists(o => o.keys.toSet.contains(p)))
    })

    xs.forall(v =>
      v.isObject &&
        (sharedProps.size >= allProps.size - 5) &&
        xs.forall(_.asObject.exists(m => m.values.forall(v => !v.isObject && !v.isArray)))
    )
  }

  def emptyElement(label: String): HtmlVNode = {
    span(cls := "uk-label", label)
  }

  def renderJson(json: Json, level: Int): HtmlVNode =
    json.fold(
      emptyElement("Null"),

      (b: Boolean) => {
        val icon = if (b) "check" else "close"
        val colour = if (b) "limegreen" else "red"
        span(attr("uk-icon") := icon, style("color") := colour)
      },

      (n: JsonNumber) => span(n.toString()),

      {
        case "" => emptyElement("Empty String")
        case s => span(s)
      },

      (xs: Vector[Json]) =>
        xs.toList match {
          case x :: Nil => renderJson(x, level + 1)
          case List() => emptyElement("Empty List")
          case xs if xs.length < 5 && xs.forall(_.isObject) =>
            div(
              ul(attr("data-uk-tab") := "animation: uk-animation-fade").apply(
                xs.zipWithIndex.map {
                  case (value, idx) => li(a(href := "#", s"Item $idx"))
                }
              ),
              ul(cls := "uk-switcher uk-margin").apply(
                xs.map(x => li(renderJson(x, level + 1)))
              )
            )
          case xs if xs.length < 5 && xs.forall(x => x.isNumber || x.isString || x.isBoolean || x.isNull) =>
            div(cls := "horiz-list").apply(
              intersperse(xs.map(renderJson(_, level + 1)), span(", "))
            )
          case xs if canRenderAsTable(xs) =>
            val childObjects: List[JsonObject] = xs.flatMap {
              _.asObject
            }
            val props = getAllProps(childObjects)
            table(
              cls := "uk-table uk-table-divider",
              tr.apply(props.map(s => th(humanizeString(s))).toList),
              childObjects.map(o =>
                tr.apply(
                  props.map(p => td(o.toMap.get(p).map(renderJson(_, level + 1)).getOrElse(""))).toList
                )
              )
            )
          case x :: rest =>
            ul(cls := "uk-list uk-list-bullet").apply(
              xs.map(x => li(renderJson(x, level + 1))).toList
            )
        },

      (o: JsonObject) =>
        if (o.isEmpty)
          emptyElement("Empty Object")
        else
          dl(cls := s"uk-description-list json-object-display level-$level").apply(
            o.toIterable.flatMap {
              case (k, v) => List(
                dt(humanizeString(k)),
                dd(div(renderJson(v, level + 1)))
              )
            }.toList
          )
    )

  def view(state: Model, dispatch: Observer[Action]): HtmlVNode = {
    val tryView: Try[HtmlVNode] = Try {
      div(
        div(
          cls := "uk-navbar-container",
          id := "header-bar",
          div(
            cls := "uk-navbar-left",
            div(
              cls := "uk-navbar-item uk-logo logo",
              "JSON Viewer"
            )
          )
        ),
        div(
          cls := "uk-section",
          div(
            cls := "uk-container",
            textArea(
              cls := "uk-textarea",
              placeholder := "Enter JSON here",
              rows := 5,
              onInput.value.map(InputChanged) --> dispatch,
              value := state.input
            ),
            state.error.map(e =>
              div(
                cls := "uk-alert uk-alert-danger",
                p(e)
              )
            ),
            state.json.map(json => {
              div(
                cls := "uk-card uk-card-default uk-card-body json-display",
                renderJson(json, 0)
              )
            })
          )
        )
      )
    }

    tryView match {
      case Success(v) => v
      case Failure(f) => div("something broke: ", f.toString)
    }
  }
}
