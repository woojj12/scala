/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable.ListBuffer
import symtab.Flags._

/** This trait ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait EtaExpansion { self: Analyzer =>
  import global._

  /** Expand partial method application `p.f(es_1)...(es_n)`.
    *
    * We expand this to the following block, which evaluates
    * the target of the application and its supplied arguments if needed (they are not stable),
    * and then wraps a Function that abstracts over the missing arguments.
    *
    * ```
    * {
    *   private synthetic val eta\$f   = p.f   // if p is not stable
    *   ...
    *   private synthetic val eta\$e_i = e_i   // if e_i is not stable
    *   ...
    *   (ps_1 => ... => ps_m => eta\$f([es_1])...([es_m])(ps_1)...(ps_m))
    * }
    * ```
    *
    * This is called from instantiateToMethodType after type checking `tree`,
    * and we realize we have a method type, where a function type (builtin or SAM) is expected.
    *
    **/
  def etaExpand(unit: CompilationUnit, tree: Tree, typer: Typer): Tree = {
    val tpe = tree.tpe
    var cnt = 0 // for NoPosition
    def freshName() = {
      cnt += 1
      freshTermName("eta$" + (cnt - 1) + "$")(typer.fresh)
    }
    val defs = new ListBuffer[Tree]

    /* Append to `defs` value definitions for all non-stable
     * subexpressions of the function application `tree`.
     */
    def liftoutPrefix(tree: Tree): Tree = {
      def liftout(tree: Tree, byName: Boolean): Tree =
        if (treeInfo.isExprSafeToInline(tree)) tree
        else {
          val vname: Name = freshName()
          // Problem with ticket #2351 here
          defs += atPos(tree.pos) {
            val rhs = if (byName) {
              val res = typer.typed(Function(List(), tree))
              new ChangeOwnerTraverser(typer.context.owner, res.symbol) traverse tree // scala/bug#6274
              res
            } else tree
            ValDef(Modifiers(SYNTHETIC), vname.toTermName, TypeTree(), rhs)
          }
          atPos(tree.pos.focus) {
            if (byName) Apply(Ident(vname), List()) else Ident(vname)
          }
        }
      val tree1 = tree match {
        // a partial application using named arguments has the following form:
        // { val qual$1 = qual
        //   val x$1 = arg1
        //   [...]
        //   val x$n = argn
        //   qual$1.fun(x$1, ..)..(.., x$n) }
        // Eta-expansion has to be performed on `fun`
        case Block(stats, fun) =>
          defs ++= stats
          liftoutPrefix(fun)
        case Apply(fn, args) =>
          val byName: Int => Option[Boolean] = fn.tpe.params.map(p => definitions.isByNameParamType(p.tpe)).lift
          val newArgs = mapWithIndex(args) { (arg, i) =>
            // with repeated params, there might be more or fewer args than params
            liftout(arg, byName(i).getOrElse(false))
          }
          treeCopy.Apply(tree, liftoutPrefix(fn), newArgs).clearType()
        case TypeApply(fn, args) =>
          treeCopy.TypeApply(tree, liftoutPrefix(fn), args).clearType()
        case Select(qual, name) =>
          val name = tree.symbol.name // account for renamed imports, scala/bug#7233
          treeCopy.Select(tree, liftout(qual, byName = false), name).clearType() setSymbol NoSymbol
        case Ident(name) =>
          tree
      }
      if (tree1 ne tree) tree1 setPos tree1.pos.makeTransparent
      tree1
    }

    /* Eta-expand lifted tree. */
    def expand(tree: Tree, tpe: Type): Tree = tpe match {
      case mt @ MethodType(paramSyms, restpe) if !mt.isImplicit =>
        val params: List[(ValDef, Boolean)] = paramSyms.map {
          sym =>
            val origTpe = sym.tpe
            val isRepeated = definitions.isRepeatedParamType(origTpe)
            // scala/bug#4176 Don't leak A* in eta-expanded function types. See t4176b.scala
            var droppedStarTpe = dropIllegalStarTypes(origTpe)
            droppedStarTpe = droppedStarTpe.setAnnotations(sym.annotations)
            //var mods = Modifiers(SYNTHETIC | PARAM, typeNames.EMPTY, Tree(sym.annotations))
            var mods = Modifiers(SYNTHETIC | PARAM, typeNames.EMPTY)
            /*
            if (sym.annotations.isEmpty != true)
            {
              mods = Modifiers(SYNTHETIC | PARAM,typeNames.EMPTY, List(sym.annotations(0).original))
            }
             */
            var valDef = ValDef(mods, sym.name.toTermName, TypeTree(droppedStarTpe), EmptyTree)
            valDef.symbol = NoSymbol.newTypeParameter(TypeName("T"))
            valDef.symbol = valDef.symbol.setAnnotations(sym.annotations)
            //valDef.mods.annotations = sym.annotations(0).args
            (valDef, isRepeated)
        }
        atPos(tree.pos.makeTransparent) {
          val args = params.map {
            case (valDef, isRepeated) => {
              var ident = Ident(valDef.name)
              ident.symbol = valDef.symbol
              gen.paramToArg(ident, isRepeated)
            }
          }
          Function(params.map(_._1), expand(Apply(tree, args), restpe))
        }
      case _ =>
        tree
    }

    val tree1 = liftoutPrefix(tree)
    atPos(tree.pos)(Block(defs.toList, expand(tree1, tpe)))
  }
}
