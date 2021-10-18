package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.{Assignment, Condition, Skip, WhileProgram, Exp}
import br.unb.cic.wlang.WhileProgram.{Label, assignments, block, fv, nonTrivialExpression, expHasVariable}
//import br.unb.cic.wlang.df.AvailableExpression.Abstraction  //-> Não consegui utilizar nosso Abstraction

class AvailableExpressionRep(wp: WhileProgram) extends MFP[Exp](wp) {

  val nonTrivialExpressionSet: Set[Exp] = nonTrivialExpression(wp)

  override def lattice(): Lattice[Exp] = Lattice(Intersection, nonTrivialExpressionSet)
  override def direction(): AnalysisDirection = ForwardAnalysis
  override def extremeValues(): Set[Exp] = Set.empty

  override def kill(label: Label): Set[Exp] = block(label, wp).get match {
      case Assignment(x, exp, label) => nonTrivialExpression(wp).filter(exp => expHasVariable(x, exp))
      case Skip(_) => Set.empty
      case Condition(_, _) => Set.empty
    }

  override def gen(label: Label): Set[Exp] =
    block(label, wp).get match {
      case Assignment(x, exp, label) => nonTrivialExpression(exp).filterNot(exp => expHasVariable(x, exp))
      case Skip(_) => Set.empty
      case Condition(b, _) => nonTrivialExpression(b)
    }
}
