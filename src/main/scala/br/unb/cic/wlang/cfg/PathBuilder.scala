package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.WhileProgram.Label
import br.unb.cic.wlang.cfg.CFGBuilder.{CFG, InterCFG, flowR}

import scala.collection.mutable

object PathBuilder {
  type Path = List[Label]

  def paths(target: Label, extremeLabels: Set[Label], flow: CFG): Set[Path] =
    extremeLabels.flatMap(from => path(from, target, flow))

  def path(from: Label, target: Label, flow: CFG): Set[Path] = path(from, target, flow, List())

  def path(from: Label, target: Label, flow: CFG, visited: List[Label], limit: Int = 3): Set[Path] = {
    var res: Set[Path] = if(from == target) Set(List(from)) else Set()

    val newVisited = from :: visited

    for((n, t) <- flow if (n == from) && (newVisited.filter(p => p == t).size < limit)) {
      res = res ++ path(t, target, flow, newVisited).map(path => from :: path)
    }
    res
  }

  def completePath(path: Path, flow: CFG, interFlow: InterCFG): Boolean =
    completePath(path, flow, interFlow, new mutable.Stack[Label]())

  def completePath(path: Path, flow: CFG, interFlow: InterCFG, stack: mutable.Stack[Label]): Boolean =
    path match {
      case List() if stack.isEmpty => true // base case + succeeded to recognize the path
      case List() if !stack.isEmpty => false // base case + failed to recognize the path
      case _ => { // the recursive case
        val callEdge = interFlow.find({ case (lc, _, _, _) => lc == path.head })
        val returnEdge = interFlow.find({ case (_, _, _, lr) => lr == path.head })
        if (callEdge.isDefined) { // if this is a call edge, we must "push" into the stack.
          val (_, _, _, lr) = callEdge.get
          return completePath(path.tail, flow, interFlow, stack.push(lr))
        }
        else if (returnEdge.isDefined) { // if this is a return edge, we must "pop" from the stack (if lr == stack.top)
          val (_, _, _, lr) = returnEdge.get
          if (lr == stack.top) {
            stack.pop() // removes the top element
            return completePath(path.tail, flow, interFlow, stack)
          } //if lr != top, we found an incomplete path.
          else return false
        } // otherwise, we just continue checking if a path is valid or not.
        else return completePath(path.tail, flow, interFlow, stack)
      }
    }

/*   def completePathsErrado(l1: Label, l2: Label, path: Path, interflow: InterCFG): Path = {
    var res: Path = if (l1 == l2) List(l1) else List()
    var tem_lcln : Boolean = false
    var tem_lxlr : Boolean = false


      for ((lc, ln, lx, lr) <- interflow) {
        //println(s"$lc : $path.contains(lc) $ln : $path.contains(ln), $lx : $path.contains(lx), $lr : $path.contains(lc), path")
       // printf(s"$lc  $ln  $lx  $lr")
        // if (path.contains(lc)) printf("- tem lc ")
        // if (path.contains(ln)) printf("- tem ln ")
        // if (path.contains(lx)) printf("- tem lx ")
        // if (path.contains(lr)) printf("- tem lr \n")
        if (path.contains(lc)) {
          if (path.indexOf(ln) == (path.indexOf(lc)+1)) tem_lcln = true
        }

        if (path.contains(lx)) {
          if (path.indexOf(lr) == (path.indexOf(lx)+1)) tem_lxlr = true
        }

        if (tem_lxlr && tem_lcln) {
           //res = res ++ Set(path)
           res = res ::: List(lc) ::: completePaths(ln, lx, path, interflow) ::: completePaths(lr, l2, path, interflow)
           //println(res)        
        } else { res = List(l1) ::: completePaths(path.indexOf(l1)+1, l2, path, interflow)}
      }
    
    res
  }
 */
}
