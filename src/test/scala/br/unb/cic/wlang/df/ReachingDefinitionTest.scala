package br.unb.cic.wlang.df

import br.unb.cic.wlang.df.ReachingDefinition.undef
import br.unb.cic.wlang._
import org.scalatest.funsuite.AnyFunSuite

class ReachingDefinitionTest extends  AnyFunSuite {

  val d1 = Assignment("y", Variable("x"), 1)
  val d2 = Assignment("z", Const(1), 2)
  val d3 = Assignment("z", Mult(Variable("z"), Variable("y")), 4)
  val d4 = Assignment("y", Sub(Variable("y"), Const(1)), 5)
  val w1 = While(Condition(GT(Variable("y"), Const(1)), 3), Sequence(d3, d4))
  val d5 = Assignment("y", Const(0), 6)

  val p = WhileProgram(List(), Sequence(d1, Sequence(d2, Sequence(w1, d5))))

  test("Test case for Reaching Definition") {
    val (in, out) = ReachingDefinition.execute(p)

    assert(in(1) == Set(("x", undef), ("y", undef), ("z", undef)))
    assert(out(1) == Set(("x", undef), ("y", 1), ("z", undef)))

    assert(in(2) == Set(("x", undef), ("y", 1), ("z", undef)))
    assert(out(2) == Set(("x", undef), ("y", 1), ("z", 2)))

    assert(in(3) == Set(("x", undef), ("y", 1), ("z", 2), ("y", 5), ("z", 4)))
    assert(out(3) == Set(("x", undef), ("y", 1), ("z", 2), ("y", 5), ("z", 4)))

    assert(in(4) == Set(("x", undef), ("y", 1), ("z", 2), ("y", 5), ("z", 4)))
    assert(out(4) == Set(("x", undef), ("y", 1), ("y", 5), ("z", 4)))

    assert(in(5) == Set(("x", undef), ("y", 1), ("y", 5), ("z", 4)))
    assert(out(5) == Set(("x", undef), ("y", 5), ("z", 4)))

    assert(in(6) == Set(("x", undef), ("y", 1), ("z", 2), ("y", 5), ("z", 4)))
    assert(out(6) == Set(("x", undef), ("y", 6), ("z", 2), ("z", 4)))
  }

}
