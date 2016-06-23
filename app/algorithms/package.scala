
package algorithms

package object types {

  type Vertex = Int
  type Edge = (Vertex, Vertex)
  type AdjacencyMap = Map[Vertex, Set[Vertex]]
}
