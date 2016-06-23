
package algorithms

import algorithms.types._

abstract class FilterAlgorithm (graph: Graph) {

  val values: Map[Vertex, Float] = init

  def init: Map[Vertex, Float]

  def filter (vertex: Vertex, threshold: Float): Option[Vertex] =
    values.get(vertex) match {
      case None => None
      case Some(value) =>
        if (value >= threshold) Some(vertex)
        else None
    }
}
