package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.WhileProgram.Label
import br.unb.cic.wlang.cfg.CFGBuilder._

object PathBuilder {

  type Path = List[Label]

  def paths(target: Label, extremeLabels: Set[Label], flow: CFG): Set[Path] =
    extremeLabels.flatMap(from => path(from, target, flow))

  def path(from: Label, target: Label, flow: CFG): Set[Path] = path(from, target, flow, List(), List())

  def path(from: Label, target: Label, flow: CFG, visiting: List[Label], finished: List[Label]): Set[Path] = {
    var res: Set[Path] = if(from == target) Set(List(from)) else Set()

    val newVisiting = if(!visiting.contains(from)) from :: visiting else visiting
    val newFinished = if(visiting.contains(from)) from:: finished else finished

    for((n, t) <- flow if (n == from) && !newFinished.contains(t)) {
      res = res ++ path(t, target, flow, newVisiting, newFinished).map(path => from :: path)
    }
    res
  }

  def completePath(l1: Label, l2: Label, flow: CFG, interFlow: InterCFG) : Set[Path] = {

    //case: CP(l1,l2) => l1
    if (l1 == l2) Set(List(l1))


    // //case: CP(l1,l3) => l1, CP(l2,l3)  
    // (lc, ln, lx, lr) = interFlow.find({ case (lc, ln, lx, lr) => lc == l1}).get

    // if(ief != null){
    //   l1 ++ completePath(ln , lx , flow , interFlow) ++ completePath(lr , l2 , flow , interFlow)
    // }


    //case: CP(lc,l) => lc, CP(ln,lx), CP(lr,l)   
    var res: Set[Path] = Set(List(l1))
    for((n, t) <- flow if (n == l1)) {
      res = res ++: completePath(t , l2 , flow , interFlow)
    }
    res
  }
}
