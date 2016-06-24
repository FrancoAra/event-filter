/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package algorithms

import algorithms.types._

/** Abstracts the concept of an algorithm which filters vertices from a graph by
  * computing a relative importance measure for the nodes, creating this way a
  * potential subset of the graph if is applied iteratively.
  *
  * @param graph from which the relative importance measure will be computed.
  */
abstract class FilterAlgorithm (graph: Graph) {

  /** A map from the graph vertices to the computed importance values. */
  val values: Map[Vertex, Float] = init

  /** A measure max threhold computed by the class which extends this abstract class. */
  val thresholdMax: Float

  /** Computes the relative importance values. */
  def init: Map[Vertex, Float]

  /** Returns the provided vertex only if its relative importance value passes
    * the threshold.
    *
    * @param vertex to be filtered.
    * @param threshold to be tested against.
    * @return Some(vertex) if passes, else None.
    */
  def filter (vertex: Vertex, threshold: Float): Option[Vertex] =
    values.get(vertex) match {
      case Some(value) if value >= threshold => Some(vertex)
      case _ => None
    }
}
