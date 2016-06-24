/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package algorithms

import algorithms.types._

/** The computed pagerank values of a [[algorithms.Graph]], extends [[algorithms.FilterAlgorithm]]
  * so it can filter vertices based on their pagerank value.
  *
  * @constructor create a new set of pagerank values for a graph.
  * @param graph to be used for the computation.
  * @param λ of the pagerank algorithm. (recomended value: 0.85)
  * @param max number of iterations to be done by the pagerank algorithm.
  */
class Pagerank (graph: Graph, λ: Float, max: Int) extends FilterAlgorithm (graph) {

  /** [[algorithms.FilterAlgorithm]] override, max threhold is max pagerank. */
  val thresholdMax: Float = values.map(_._2).max

  /** [[algorithms.FilterAlgorithm]] override, computes the pagerank values. */
  def init = computePagerank(graph.vertices.map((_ -> 1f / graph.n)).toMap, 1)

  /** Computes the pagerank algorithm recursively.
    *
    * @param pagerank values result of the last recursion.
    * @param iteration currently on.
    * @return a map of vertices to pagerank values.
    */
  private def computePagerank (
    pagerank: Map[Vertex, Float],
    iteration: Int
  ): Map[Vertex, Float] = (max, iteration) match {
    case (max, iteration) if iteration > max => pagerank
    case _ =>
      val sumSink = sumSinkNodesPagerank(pagerank)
      val newPagerank = graph.vertices.foldLeft(Map.empty[Vertex, Float]) {
        case (map, vertex) => map + (vertex -> pr(vertex, sumSink, pagerank))
      }
      if (newPagerank == pagerank) pagerank
      else computePagerank(newPagerank, iteration + 1)
  }

  /** Computes the pagerank value of one vertex.
    *
    * @param vertex to be computed its pagerank value.
    * @param s sumatory of pagerank from sink nodes.
    * @param lastPagerank values.
    * @return the vertex pagerank value for this recursion.
    */
  private def pr (vertex: Vertex, s: Float, lastPagerank: Map[Vertex, Float]): Float =
    ((1 - λ + λ * s) / graph.n) + (λ * sumIn(vertex, lastPagerank))

  /** Sumatory part of the pagerank algorithm for the current recursion.
    *
    * @param vertex to be computed its pagerank value.
    * @param lastPagerank values.
    * @return the sumatory result for the pagerank algorithm.
    */
  private def sumIn (vertex: Vertex, lastPagerank: Map[Vertex, Float]): Float =
    graph.inEdges.getOrElse(vertex, List.empty).foldLeft(0f) {
      case (sum, y) => sum + (lastPagerank.getOrElse(y, 0f) / (graph.outDegree(y) match {
        case None => 1f
        case Some(degree) => degree
      }))
    }

  /** Sums the pagerank from sink nodes, used for the s parameter in the pr function.
    *
    * @param lastPagerank values.
    * @return the sum of pagerank from sink nodes.
    */
  private def sumSinkNodesPagerank (lastPagerank: Map[Vertex, Float]): Float =
    graph.sinkNodes.foldLeft(0f)(_ + lastPagerank.getOrElse(_, 0f))
}
