
package controllers

import scala.io.Source
import scala.concurrent._

import javax.inject._

import play.api._
import play.api.mvc._
import play.api.libs.json._

import algorithms._

@Singleton
class ApiEndpoint @Inject() ()(implicit exec: ExecutionContext) extends Controller {

  def upload = Action(parse.multipartFormData) { request =>
    request.body.file("file").map { graph =>
      val csv: List[String] =
        Source.fromFile(graph.ref.file).getLines().filter(!_.isEmpty()).toList
      Ok(computePagerank(csv))
    }.getOrElse {
      BadRequest
    }
  }

  private def computePagerank (csv: List[String]): JsValue = {
    val graph = Graph.parse(csv)
    val nodes = graph.vertices.toList
    val links = graph.edges.toList
    val n = nodes.tail.size
    val idsSum = nodes.reduce(_ + _)
    val validSum = (n + 1) * n / 2
    if (idsSum == validSum) {
      val pagerank = new Pagerank(graph, 0.85f, 50)
      Json.obj(
        "max" -> pagerank.thresholdMax,
        "nodes" -> JsArray(nodes.map { case node: Int =>
          Json.obj("name" -> JsString("ID: "+node.toString), "metric" -> JsString(pagerank.values.getOrElse(node, 0f).toString))
        }),
        "links" -> JsArray(links.map { case (from, to) =>
          Json.obj("source" -> from, "target" -> to)
        })
      )
    } else {
      Json.obj("error" -> "bad ids")
    }
  }
}
