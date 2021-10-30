package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.cfg.PathBuilder._
import org.scalatest.funsuite.AnyFunSuite
import br.unb.cic.wlang.cfg.CFGBuilder._
import br.unb.cic.wlang.WhileProgram
import br.unb.cic.wlang.parser.{ResourceHandle, WhileProgramParser}
import br.unb.cic.wlang.semantics.{StructuralSemantics, TC}

class PathBuilderFibonacciTest extends AnyFunSuite {
  test("Test for the fibonacci program with flow comparison") {
    val content = ResourceHandle.getContent("Fibonacci.wp")

    assert(content != null)

    val p: WhileProgramParser = new WhileProgramParser()

    val wp: WhileProgram = p.parse(p.whileProgram, content) match {
      case p.Success(program: WhileProgram, _) => program
      case p.Failure(msg, _)                   => println(s"FAILURE: $msg"); fail
      case p.Error(msg, _)                     => println(s"ERROR: $msg"); fail
    }

    assert(wp != null)

    val interpreter = new StructuralSemantics()

    interpreter.run(wp) match {
      case TC(e, s) => assert(s(e("y")) == 34)
      case _        => fail()
    }

    val interflow3: InterCFG = Set((9, 1, 8, 10), (4, 1, 8, 5), (6, 1, 8, 7))
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
    val cfgflow = CFGBuilder.flow(wp)
    assert(cfgflow.toSet.contains(flow3))

    val expected1 = List(9, 1, 2, 4, 1, 2, 3, 8, 5, 6, 1, 2, 3, 8, 7, 8, 10)
    val expected2 = List(9, 1, 2, 4, 1, 2, 3, 8, 10)
    assert(path(9, 10, flow3).contains(expected1))
    assert(path(9, 10, flow3).contains(expected2))

    // val paths = path(9, 10, flow3)
    // for (p <- paths) {
    // completePaths(9, 10, p, interflow3)
    // }
  }

}
