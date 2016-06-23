
package algorithms

import algorithms.types._

object Graph {

  private val CSVReg = """\s*(\d+)\s*,\s*(\d+)\s*""".r

  def parse (csv: List[String]): Graph = {
    val (vertices, edges) = csv.foldLeft (Set.empty[Vertex], Set.empty[Edge]){
      case ((vertices, edges), CSVReg(a, b)) =>
        val from = a.toInt
        val to = b.toInt
        (vertices + from + to, edges + (from.toInt -> to.toInt))
    }
    new Graph(vertices, edges)
  }
}

class Graph (val vertices: Set[Vertex], val edges: Set[Edge]) {

  lazy val (outEdges, inEdges): (AdjacencyMap, AdjacencyMap) = computeAdjacencyMaps

  lazy val sinkNodes: Set[Vertex] = vertices diff outEdges.map(_._1).toSet

  val n = vertices.size

  def outDegree (vertex: Vertex): Option[Int] = outEdges.get(vertex) match {
    case None => None
    case Some(adjacencyList) => Some(adjacencyList.size)
  }

  def inDegree (vertex: Vertex): Option[Int] = inEdges.get(vertex) match {
    case None => None
    case Some(adjacencyList) => Some(adjacencyList.size)
  }

  private def computeAdjacencyMaps: (AdjacencyMap, AdjacencyMap) = edges.foldLeft((emptyAdj, emptyAdj)) {
    case ((out, in), (from, to)) =>
      val newOut = out + (from -> (out.getOrElse(from, Set.empty) + to))
      val newIn = in + (to -> (in.getOrElse(to, Set.empty) + from))
      (newOut, newIn)
  }

  private def emptyAdj: AdjacencyMap = Map.empty
}
