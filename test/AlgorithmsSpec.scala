import org.specs2.mutable._

import algorithms._
import algorithms.types._

class AlgorithmsSpec extends Specification {

  val graphCSV = List(
    "4,1", "4,2", "2,3", "3,2", "7,2", "7,5",
    "8,2", "8,5", "9,2", "9,5", "10,5", "11,5",
    "5,6", "6,5", "6,2", "5,4"
  )

  val graph = Graph.parse(graphCSV)

  "Graph" should {
    "parse csv" in {
      graph.n must beEqualTo(11)
      graph.edges.size must beEqualTo(16)
      graph.outEdges.getOrElse(1, Set.empty[Vertex]) must beEqualTo(Set.empty[Vertex])
      graph.outEdges.getOrElse(2, Set.empty[Vertex]) must beEqualTo(Set(3))
      graph.outEdges.getOrElse(3, Set.empty[Vertex]) must beEqualTo(Set(2))
      graph.outEdges.getOrElse(4, Set.empty[Vertex]) must beEqualTo(Set(1,2))
      graph.outEdges.getOrElse(5, Set.empty[Vertex]) must beEqualTo(Set(4,6))
      graph.outEdges.getOrElse(6, Set.empty[Vertex]) must beEqualTo(Set(5,2))
      graph.outEdges.getOrElse(7, Set.empty[Vertex]) must beEqualTo(Set(2,5))
      graph.outEdges.getOrElse(8, Set.empty[Vertex]) must beEqualTo(Set(2,5))
      graph.outEdges.getOrElse(9, Set.empty[Vertex]) must beEqualTo(Set(2,5))
      graph.outEdges.getOrElse(10, Set.empty[Vertex]) must beEqualTo(Set(5))
      graph.outEdges.getOrElse(11, Set.empty[Vertex]) must beEqualTo(Set(5))
    }
  }

  "Pagerank" should {
    "compute pagerank (1)" in {
      val pagerank = new Pagerank(graph, 0.85f, 50).values
      pagerank.foreach(println)
      val pagerankSum = pagerank.foldLeft(0f)(_ + _._2).toInt
      val pr1 = pagerank.getOrElse(1, 0f)
      val pr2 = pagerank.getOrElse(2, 0f)
      val pr3 = pagerank.getOrElse(3, 0f)
      val pr4 = pagerank.getOrElse(4, 0f)
      val pr5 = pagerank.getOrElse(5, 0f)
      pagerankSum must beEqualTo(1f)
      pr2 must be_>(pr1)
      pr2 must be_>(pr3)
      pr2 must be_>(pr4)
      pr2 must be_>(pr5)
      pr3 must be_>(pr1)
      pr3 must be_>(pr4)
      pr3 must be_>(pr5)
      pr5 must be_>(pr4)
      pr5 must be_>(pr1)
    }
  }
}
