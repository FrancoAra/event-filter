
package controllers

import scala.io.Source
import scala.concurrent._

import javax.inject._

import play.api._
import play.api.mvc._
import play.api.libs.json._

@Singleton
class ApiEndpoint @Inject() ()(implicit exec: ExecutionContext) extends Controller {

  def upload = Action(parse.multipartFormData) { request =>
    request.body.file("file").map { graph =>
      val csv: List[String] =
        Source.fromFile(graph.ref.file).getLines().filter(!_.isEmpty()).toList
    }.getOrElse {
      BadRequest
    }
  }
}
