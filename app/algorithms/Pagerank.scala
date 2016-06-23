
package algorithms

import algorithms.types._

class Pagerank (graph: Graph, λ: Float, max: Int) extends FilterAlgorithm (graph) {

  def init = computePagerank(
    graph.vertices.map((_ -> 1f / graph.n)).toMap, 1
  )

  private def computePagerank (
    pagerank: Map[Vertex, Float],
    iter: Int
  ): Map[Vertex, Float] = (max, iter) match {
    case (max, iter) if iter > max => pagerank
    case _ =>
      val sumSink = sumSinkNodesPagerank(pagerank)
      val newPagerank = graph.vertices.foldLeft(Map.empty[Vertex, Float]) {
        case (map, vertex) => map + (vertex -> pr(vertex, sumSink, pagerank))
      }
      if (newPagerank == pagerank) pagerank
      else computePagerank(newPagerank, iter + 1)
  }

  private def pr (vertex: Vertex, s: Float, lastPagerank: Map[Vertex, Float]): Float =
    ((1 - λ + λ * s) / graph.n) + (λ * sumIn(vertex, lastPagerank))

  private def sumIn (vertex: Vertex, lastPagerank: Map[Vertex, Float]): Float =
    graph.inEdges.getOrElse(vertex, List.empty).foldLeft(0f) {
      case (sum, y) => sum + (lastPagerank.getOrElse(y, 0f) / (graph.outDegree(y) match {
        case None => 1f
        case Some(degree) => degree
      }))
    }

  private def sumSinkNodesPagerank (lastPagerank: Map[Vertex, Float]): Float =
    graph.sinkNodes.foldLeft(0f)(_ + lastPagerank.getOrElse(_, 0f))
}
