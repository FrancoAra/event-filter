
package algorithms

import algorithms.types._

abstract class FilterAlgorithm (graph: Graph) {

  val values: Map[Vertex, Float] = init

  val thresholdMax: Float

  def init: Map[Vertex, Float]

  def filter (vertex: Vertex, threshold: Float): Option[Vertex] =
    values.get(vertex) match {
      case Some(value) if value >= threshold => Some(vertex)
      case _ => None
    }
}
