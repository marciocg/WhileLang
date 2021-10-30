package br.unb.cic.wlang.cfg
import scalax.collection.Graph
import scalax.collection.Graph.{newBuilder} // or scalax.collection.mutable.Graph
//import scalax.collection.mutable.Graph.newBuilder
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.GraphTraversal._
// import scalax.collection.io.dot_

import br.unb.cic.wlang.cfg.CFGBuilder._

class CFGGraph {}

object CFGGraph {
  def main(args: Array[String]) = {
    val flow3: CFG = Set(
      (1, 2),
      (2, 3),
      (3, 8),
      (2, 4),
      (4, 1),
      (8, 5),
      (5, 6),
      (6, 1),
      (8, 7),
      (7, 8),
      (9, 1),
      (8, 10)
    )
    println(flow3.toList)
    //println(flow3.toList.map[List[Graph]]((a,b)=>DiEdge(a,b)))
    //val grafo: DiHyperEdge = 1~>2~>2~>3
    val grafoHyper = DiHyperEdge(1, 2, 2, 3)
    val grafo = Graph(flow3.map(par => DiEdge(par._1,par._2)).mkString(","))
    //val nodes = Graph(grafoset.mkString(","))
    //val builder = nodes.newBuilder()
    //println("node" + nodes.toString)
    println("string : " + grafo.toString())
    println("nodes  : " + grafo.nodes.toString())
    println("edges  : " + grafo.nodes.toString())

  }
}
