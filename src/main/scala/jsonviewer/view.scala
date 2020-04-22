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
    ) && xs.length > 1
  }

  def emptyElement(label: String): HtmlVNode = {
    span(cls := "uk-label", label)
  }

  def pathToString(path: List[String]): String = {
    path.map(humanizeString(_)).mkString(" -> ")
  }

  def tooltipAttr(path: List[String], position: String = "left"): BasicAttr = {
    attr("uk-tooltip") := s"title: ${pathToString(path)}; pos: $position; delay: 1000"
  }

  def extractName(obj: JsonObject): Option[HtmlVNode] = {
    val nameProps = List("name", "title", "id")
    obj.keys
      .find(k => nameProps.exists(p => k.toLowerCase() == p))
      .flatMap(k => obj(k))
      .filter(v => v.isNull || v.isBoolean || v.isString || v.isNumber)
      .map(renderJson(_, List.empty))
  }

  def renderJson(json: Json, path: List[String] = List.empty): HtmlVNode =
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
          case List() => emptyElement("Empty List")
          case xs if xs.length > 1 && xs.length < 5 && xs.forall(_.isObject) =>
            div(
              ul(attr("data-uk-tab") := "animation: uk-animation-fade").apply(
                xs.zipWithIndex.map {
                  case (value, idx) =>
                    val title: HtmlVNode = value.asObject.flatMap(extractName).getOrElse(span(s"Item $idx"))
                    li(a(href := "#", title))
                }
              ),
              ul(cls := "uk-switcher uk-margin")(
                xs.zipWithIndex.map {
                  case (x, i) => li(renderJson(x, path :+ i.toString)(
                    tooltipAttr(path :+ i.toString, "top")
                  ))
                }
              )
            )
          case xs if xs.length > 1 && xs.length < 5 && xs.forall(x => x.isNumber || x.isString || x.isBoolean || x.isNull) =>
            div(cls := "horiz-list").apply(
              intersperse(
                xs.zipWithIndex.map {
                  case (x, i) => renderJson(x, path :+ i.toString)(tooltipAttr(path :+ i.toString, "top"))
                },
                span(", ")
              )
            )
          case xs if canRenderAsTable(xs) =>
            val childObjects: List[JsonObject] = xs.flatMap {
              _.asObject
            }
            val props = getAllProps(childObjects)
            table(
              cls := "uk-table uk-table-divider",
              tr.apply(props.map(s => th(humanizeString(s))).toList),
              childObjects.zipWithIndex.map {
                case (o, i) =>
                  tr.apply(
                  props.map(p => {
                    val newPath = path :+ i.toString :+ p
                    td(
                      o.toMap.get(p)
                        .map(renderJson(_, newPath)(tooltipAttr(newPath, "top")))
                        .getOrElse("")
                    )
                  }
                  ).toList
                )
              }
            )
          case x :: rest =>
            ul(cls := "uk-list uk-list-bullet")(
              xs.zipWithIndex.map {
                case (x, i) => li(renderJson(x, path :+ i.toString)(tooltipAttr(path :+ i.toString)))
              }.toList
            )
        },

      (o: JsonObject) =>
        if (o.isEmpty)
          emptyElement("Empty Object")
        else
          dl(cls := s"uk-description-list json-object-display level-${path.length}")(
            o.toIterable.flatMap {
              case (k, v) => List(
                dt(humanizeString(k)),
                dd(renderJson(v, path :+ k)(tooltipAttr(path :+ k)))
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
              cls := "uk-navbar-item uk-logo logo header",
              img(src := "/images/logo.png"),
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
                renderJson(json, List.empty)
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
