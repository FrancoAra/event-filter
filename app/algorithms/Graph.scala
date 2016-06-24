/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package algorithms

import algorithms.types._

/** Singleton for [[algorithms.Graph]], contains util functions. */
object Graph {

  /** Regular expression used to parse csv lines to extract vertices and edges . */
  private val CSVReg = """\s*(\d+)\s*,\s*(\d+)\s*""".r

  /** Transforms a csv already splited into lines, into a [[algorithms.Graph]]
    * object.
    *
    * @param csv list of strings to be parsed.
    * @return a new instance of [[algorithms.Graph]] containing all vertices and
    *         edges in the input csv.
    */
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

/** Abstraction of a graph.
  *
  * @constructor create a new graph with provided vertices and edges.
  * @param vertices set of the graph.
  * @param edges set of the graph in tuple form (v1, v2).
  */
class Graph (val vertices: Set[Vertex], val edges: Set[Edge]) {

  /** Maps of vertices to a list of related vertices, in and out relationships. */
  lazy val (outEdges, inEdges): (AdjacencyMap, AdjacencyMap) = computeAdjacencyMaps

  /** Set of vertices which have no out edges. */
  lazy val sinkNodes: Set[Vertex] = vertices diff outEdges.map(_._1).toSet

  /** Amount of vertices. */
  val n = vertices.size

  /** Computes the out degree of a vertex.
    *
    * @param vertex to compute the out degree.
    * @return some(int) representing the out degree if the vertex was found in
    *         the graph, none otherwise.
    */
  def outDegree (vertex: Vertex): Option[Int] = outEdges.get(vertex) match {
    case None => None
    case Some(adjacencyList) => Some(adjacencyList.size)
  }

  /** Computes the in degree of a vertex.
    *
    * @param vertex to compute the in degree.
    * @return some(int) representing the in degree if the vertex was found in
    *         the graph, none otherwise.
    */
  def inDegree (vertex: Vertex): Option[Int] = inEdges.get(vertex) match {
    case None => None
    case Some(adjacencyList) => Some(adjacencyList.size)
  }

  /** Computes the out and in edges maps from the set of edges.
    *
    * @return a tuple of maps, the out edges and in edges values.
    */
  private def computeAdjacencyMaps: (AdjacencyMap, AdjacencyMap) = edges.foldLeft((emptyAdj, emptyAdj)) {
    case ((out, in), (from, to)) =>
      val newOut = out + (from -> (out.getOrElse(from, Set.empty) + to))
      val newIn = in + (to -> (in.getOrElse(to, Set.empty) + from))
      (newOut, newIn)
  }

  /** Convenience function to make a shorter version of the explicit type of the
    * empty maps used in the computeAdjacencyMaps functions. */
  private def emptyAdj: AdjacencyMap = Map.empty
}
