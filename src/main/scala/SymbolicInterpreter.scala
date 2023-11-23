package symbolicinterpreter

import interpreter._

import z3.scala._ 

object SymbolicBitVectorInterpreter {

 def eval(expr: ConcreteB, env: Map[String, ConcreteBitVector], context: Z3Context): Z3AST = expr match {
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
    case ConcreteVar(name) => env.get(name) match {
      case Some(concreteBV) => eval(concreteBV, env, context) 
      case None => throw new RuntimeException(s"Variable $name not defined.")
   }
    case ConcreteBVAdd(b1, b2) => context.mkBVAdd(eval(b1, env, context), eval(b2, env, context))
    case ConcreteBVSub(b1, b2) => context.mkBVSub(eval(b1, env, context), eval(b2, env, context))
    case ConcreteBVNot(b) => context.mkBVNot(eval(b, env, context))
    case ConcreteBVOr(b1, b2) => context.mkBVOr(eval(b1, env, context), eval(b2, env, context))
    case ConcreteBVAnd(b1, b2) => context.mkBVAnd(eval(b1, env, context), eval(b2, env, context))
    case ConcreteBVXor(b1, b2) => context.mkBVXor(eval(b1, env, context), eval(b2, env, context))
 }
}
