package constraints

import interpreter._
import enumerator._
import symbolicinterpreter._

import z3.scala._

import scala.collection.mutable.{Set => MutSet}

type Examples = Map[Map[String, ConcreteBitVector], ConcreteBitVector]

object Constraints {
  def to_constraint(expr: ConcreteB, variables: Map[String, Z3AST], context: Z3Context): Z3AST = expr match {
    case ConcreteBitVector(value) => {
      val intSort = context.mkIntSort()
      val intVal = context.mkInt(Integer.parseInt(value, 2), intSort)
      context.mkInt2BV(8, intVal)
   }
    case ConcreteZero => {
      val intSort = context.mkIntSort()
      val intZero = context.mkInt(0, intSort)
      context.mkInt2BV(8, intZero)
   }
    case ConcreteOne => {
      val intSort = context.mkIntSort()
      val intOne = context.mkInt(1, intSort)
      context.mkInt2BV(8, intOne)
   }
    case ConcreteVar(name) => variables.get(name) match {
      case Some(bvconst) => bvconst
      case None => throw new RuntimeException(s"Variable $name not defined.")
   }
    case ConcreteBVAdd(b1, b2) => context.mkBVAdd(to_constraint(b1, variables, context), to_constraint(b2, variables, context))
    case ConcreteBVSub(b1, b2) => context.mkBVSub(to_constraint(b1, variables, context), to_constraint(b2, variables, context))
    case ConcreteBVNot(b) => context.mkBVNot(to_constraint(b, variables, context))
    case ConcreteBVOr(b1, b2) => context.mkBVOr(to_constraint(b1, variables, context), to_constraint(b2, variables, context))
    case ConcreteBVAnd(b1, b2) => context.mkBVAnd(to_constraint(b1, variables, context), to_constraint(b2, variables, context))
    case ConcreteBVXor(b1, b2) => context.mkBVXor(to_constraint(b1, variables, context), to_constraint(b2, variables, context))
  }

  def findBestInput(programs: Set[ConcreteB], variables: Set[String]): (Map[String, ConcreteBitVector], Int) = {
    //println(programs.size)

    if (programs.size <= 1) {
      (Map.empty, 0)
    } else {
      val ctx = new Z3Context(Map("MODEL" -> "true"))

      val env = variables.map(v => {
          val bvSort = ctx.mkBVSort(8)
          (v, ctx.mkConst(v, bvSort))
        }).toMap

      val indexed_programs = programs.zipWithIndex;
      val program_pairs = indexed_programs.map((p, i) => indexed_programs.filter((o, j) => j > i).map((o, _) => (p, o))).flatten

      val constraints = programs.toList.sliding(2, 1).map(l => ctx.mkNot(ctx.mkEq(to_constraint(l(0), env, ctx), to_constraint(l(1), env, ctx)))).toList

      val hard_constraint = constraints.tail.foldLeft(constraints.head) { (hs, curr) => { ctx.mkOr(hs, curr) }}

      val soft_constraints = program_pairs.map((p1, p2) => {
          ctx.mkNot(ctx.mkEq(to_constraint(p1, env, ctx), to_constraint(p2, env, ctx)))
        }
      )
      //println(soft_constraints.size)
      val opt_solver = ctx.mkOptimizer()
      opt_solver.assertCnstr(hard_constraint)
      soft_constraints.foreach(cons => opt_solver.assertCnstr(cons, 1))
      val result = opt_solver.check()

      result match {
        case Some(false) => {
          ctx.delete()
          (Map.empty, 0)
        }
        case Some(true) => {
          // Get the model
          val model = opt_solver.getModel()
          val assignments = variables.map(v => (v, ConcreteBitVector(model.evalAs[Int](env(v)).get.toBinaryString))).toMap
          //println(assignments)

          val diff_num = program_pairs.foldLeft(0) { (sum, p) => {
            val diff = ConcreteBitVectorInterpreter.eval(p._1, assignments) != ConcreteBitVectorInterpreter.eval(p._2, assignments)
            if (diff) sum + 1 else sum
          }}

          //println(diff_num)

          ctx.delete()
          (assignments, diff_num)
        }
        case None => {
          ctx.delete()
          throw new RuntimeException(s"Solver return unkown result.")
        }
      }
    }
  }
}
