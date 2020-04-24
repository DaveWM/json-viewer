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

  def extractName(viewType: ViewType, obj: JsonObject): Option[HtmlVNode] = {
    val nameProps = List("name", "title", "id")
    obj.keys
      .find(k => nameProps.contains(k.toLowerCase()))
      .flatMap(k => obj(k))
      .filter(v => v.isNull || v.isBoolean || v.isString || v.isNumber)
      .map(renderJson(_, viewType, List.empty))
  }

  def renderJson(json: Json, viewType: ViewType, path: List[String] = List.empty): HtmlVNode =
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
        case s => tryParseDate(s)
          .map(d => span(formatDate(d)))
          .getOrElse(span(
            linkifyText(s).flatMap[VDomModifier] {
              case StringVNode(s) => emailifyText(s)
              case other => List(other)
            }
          ))
      },

      (xs: Vector[Json]) =>
        xs.toList match {
          case List() => emptyElement("Empty List")
          case x :: Nil =>
            renderJson(x, viewType, path :+ "0")
          case xs if xs.length > 1 && xs.length < 5 && xs.forall(_.isObject) =>
            div(
              ul(attr("data-uk-tab") := "animation: uk-animation-fade").apply(
                xs.zipWithIndex.map {
                  case (value, idx) =>
                    val title: HtmlVNode = value.asObject.flatMap(extractName(viewType, _)).getOrElse(span(s"Item $idx"))
                    li(a(href := "#", title))
                }
              ),
              ul(cls := "uk-switcher uk-margin")(
                xs.zipWithIndex.map {
                  case (x, i) => li(renderJson(x, viewType, path :+ i.toString)(
                    tooltipAttr(path :+ i.toString, "top")
                  ))
                }
              )
            )
          case xs if xs.length > 1 && xs.length < 5 && xs.forall(x => x.isNumber || x.isString || x.isBoolean || x.isNull) =>
            div(cls := "horiz-list").apply(
              intersperse(
                xs.zipWithIndex.map {
                  case (x, i) => renderJson(x, viewType, path :+ i.toString)(tooltipAttr(path :+ i.toString, "top"))
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
                        .map(renderJson(_, viewType, newPath)(tooltipAttr(newPath, "top")))
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
                case (x, i) => li(renderJson(x, viewType, path :+ i.toString)(tooltipAttr(path :+ i.toString)))
              }.toList
            )
        },

      (o: JsonObject) =>
        if (o.isEmpty)
          emptyElement("Empty Object")
        else
          div(cls := s"json-object-display level-${path.length}")(
            viewType match {
              case Compact =>
                o.toIterable.map {
                  case (k, v) => div(
                    cls := "kvp",
                    b(humanizeString(k)),
                    renderJson(v, viewType, path :+ k)(tooltipAttr(path :+ k))
                  )
                }.toList
              case Normal =>
                dl(
                  cls := "uk-description-list",
                  o.toIterable.flatMap {
                    case (k, v) => List(
                      dt(humanizeString(k)),
                      dd(renderJson(v, viewType, path :+ k)(tooltipAttr(path :+ k)))
                    )
                  }.toList
                )
            }

          )
    )

  def savedHtmlView(json: Json, viewType: ViewType): HtmlVNode = {
    htmlTag("html")(
      htmlTag("head")(
        htmlTag("title")("JSON Viewer"),
        htmlTag("link")(attr("rel") := "stylesheet", href := "https://json-viewer.io/main.css"),
        htmlTag("link")(attr("rel") := "stylesheet", href := "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;500&display=swap"),
        htmlTag("link")(attr("rel") := "stylesheet", href := "https://cdn.jsdelivr.net/npm/uikit@3.4.1/dist/css/uikit.min.css"),
        htmlTag("script")(src := "https://cdn.jsdelivr.net/npm/uikit@3.4.1/dist/js/uikit.min.js"),
        htmlTag("script")(src := "https://cdn.jsdelivr.net/npm/uikit@3.4.1/dist/js/uikit-icons.min.js"),
        htmlTag("link")(attr("rel") := "icon", href := "https://json-viewer.io/favicon.ico")
      ),
      htmlTag("body")(
        div(
          cls := "uk-container uk-section",
          style("width") := "100%",
          renderJson(json, viewType)
        )
      )
    )
  }

  def bannerAd(): HtmlVNode =
    div(
      cls := "banner-ad",
      htmlTag("script")(
        attr("data-cfasync") := "false",
        `type` := "text/javascript",
        src := "//p393590.clksite.com/adServe/banners?tid=393590_773019_0"
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
              span(cls := "header-text", "JSON Viewer")
            )
          )
        ),
        div(
          cls := "main",
          div(
            cls := "uk-container container",
            textArea(
              cls := "uk-textarea",
              placeholder := "Enter JSON here",
              rows := 5,
              onInput.value.map(InputChanged) --> dispatch,
              value := state.input
            ),
            div(
              cls := "uk-button-group view-controls",
              button(
                cls := "uk-button", cls := (if (state.viewType == Normal) "uk-button-primary" else "uk-button-default"),
                onClick.use(SetViewType(Normal)) --> dispatch, "Normal"
              ),
              button(
                cls := "uk-button", cls := (if (state.viewType == Compact) "uk-button-primary" else "uk-button-default"),
                onClick.use(SetViewType(Compact)) --> dispatch, "Compact")
            ),
            bannerAd(),
            state.error.map(e =>
              div(
                cls := "uk-alert uk-alert-danger",
                p(e)
              )
            ),
            state.json.map(json => {
              div(
                cls := "uk-card uk-card-default uk-card-body json-display",
                button(
                  cls := "uk-button uk-button-default uk-button-small control-button",
                  onClick.use(SaveHTML) --> dispatch,
                  "Save HTML"
                ),
                renderJson(json, state.viewType, List.empty)
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
