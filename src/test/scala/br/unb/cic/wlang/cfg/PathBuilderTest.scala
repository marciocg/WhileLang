package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.cfg.PathBuilder._
import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.wlang.cfg.CFGBuilder._
import br.unb.cic.wlang.WhileProgram._

class PathBuilderTest extends AnyFunSuite {

  val flow1: CFG = Set((1,2), (2,3), (3,4), (4,5), (5,3), (3,6))

  test("Test for path 1 flow1") {
    val expected : Set[Path] = Set(List(1))
    assert(expected == path(1, 1, flow1))
  }

  test("Test for path 1 2 flow1") {
    val expected : Set[Path] = Set(List(1,2))
    assert(path(1, 2, flow1) == expected)
  }

  test("Test for path 1 3 flow1") {
    val expected : Set[Path] = Set(List(1,2,3), List(1,2,3,4,5,3))
    assert(expected == path(1, 3, flow1))
  }

  test("Test for path 2 3 flow1") {
    val expected : Set[Path] = Set(List(2,3),  List(2, 3, 4, 5, 3))
    assert(expected == path(2, 3, flow1))
  }

  test("Test for path 1 4 flow1") {
    val expected : Set[Path] = Set(List(1,2,3,4), List(1,2,3,4,5,3,4))
    assert(expected == path(1, 4, flow1))
  }

  test("Test for path 1 5 flow1") {
    val expected : Set[Path] = Set(List(1,2,3,4,5), List(1,2,3,4,5,3,4,5))
    assert(expected == path(1, 5, flow1))
  }

  test("Test for path 1 6 flow1") {
    val expected : Set[Path] = Set(List(1,2,3,4,5,3,6), List(1,2,3,6))
    assert(expected == path(1, 6, flow1))
  }


  // val flow2: CFG = Set((2,0), (2,1), (0,3), (0,1), (1,3))
  val flow2: CFG = Set((0,1), (0,3), (1,3), (2,0), (2,1))

  test("Test new") {
    val expected : Set[Path] = Set(List(2, 1, 3), List(2, 0, 1, 3), List(2, 0, 3))
    assert(expected == path(2, 3, flow2))
  }


  val flow3: CFG = Set(
      (1,2), (2,3), (3,8),
      (2,4), (4,1), (8,5), (5,6), (6,1), (8,7), (7,8),
      (9, 1), (8,10)
    )

  val interFlow3: InterCFG =  Set(
      (9, 1, 8,10), (4,1, 8,5), (6,1, 8,7)
    )

  test("test complete path") {
    val expected : List[Label] = List(9,1,2,4,1,2,3,8,5,6,1,2,3,8,7,8,10)
    // val expected : List[Label] = List(9,1,2,3,8,10)
    assert(completePath(9, 10, flow3, interFlow3, List(), List()).contains(expected))
  }

}
