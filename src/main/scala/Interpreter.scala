package interpreter

abstract class ConcreteB
case class ConcreteBitVector(value: String) extends ConcreteB
case object ConcreteZero extends ConcreteB
case object ConcreteOne extends ConcreteB
case class ConcreteVar(name: String) extends ConcreteB
case class ConcreteBVAdd(b1: ConcreteB, b2: ConcreteB) extends ConcreteB
case class ConcreteBVSub(b1: ConcreteB, b2: ConcreteB) extends ConcreteB
case class ConcreteBVNot(b: ConcreteB) extends ConcreteB
case class ConcreteBVOr(b1: ConcreteB, b2: ConcreteB) extends ConcreteB
case class ConcreteBVAnd(b1: ConcreteB, b2: ConcreteB) extends ConcreteB
case class ConcreteBVXor(b1: ConcreteB, b2: ConcreteB) extends ConcreteB

object ConcreteBitVectorInterpreter {
  def size(expr: ConcreteB): Int = expr match {
    case ConcreteBitVector(value) => 1
    case ConcreteZero => 1
    case ConcreteOne => 1
    case ConcreteVar(name) => 1
    case ConcreteBVAdd(b1, b2) => size(b1) + size(b2)
    case ConcreteBVSub(b1, b2) => size(b1) + size(b2)
    case ConcreteBVNot(b) => size(b)
    case ConcreteBVOr(b1, b2) => size(b1) + size(b2)
    case ConcreteBVAnd(b1, b2) => size(b1) + size(b2)
    case ConcreteBVXor(b1, b2) => size(b1) + size(b2)
  }

  def eval(expr: ConcreteB, env: Map[String, ConcreteBitVector] = Map.empty): String = expr match {
    case ConcreteBitVector(value) => value
    case ConcreteZero => "00000000"
    case ConcreteOne => "00000001"
    case ConcreteVar(name) => eval(env.getOrElse(name, throw new RuntimeException(s"Variable $name not defined.")), env)
    case ConcreteBVAdd(b1, b2) => concreteBitVectorAdd(eval(b1, env), eval(b2, env))
    case ConcreteBVSub(b1, b2) => concreteBitVectorSub(eval(b1, env), eval(b2, env))
    case ConcreteBVNot(b) => eval(b, env).map(bit => if (bit == '0') '1' else '0')
    case ConcreteBVOr(b1, b2) => concreteBitVectorOperation(_ | _, eval(b1, env), eval(b2, env))
    case ConcreteBVAnd(b1, b2) => concreteBitVectorOperation(_ & _, eval(b1, env), eval(b2, env))
    case ConcreteBVXor(b1, b2) => concreteBitVectorOperation(_ ^ _, eval(b1, env), eval(b2, env))
  }

  private def concreteBitVectorAdd(b1: String, b2: String): String = {
    val sum = Integer.parseInt(b1.takeRight(8), 2) + Integer.parseInt(b2.takeRight(8), 2)
    val binarySum = Integer.toBinaryString(sum)
    String.format("%8s", binarySum).replace(' ', '0').takeRight(8)
  }

  private def concreteBitVectorSub(b1: String, b2: String): String = {
    val difference = Integer.parseInt(b1.takeRight(8), 2) - Integer.parseInt(b2.takeRight(8), 2)
    String.format("%8s", Integer.toBinaryString(difference)).replace(' ', '0')
  }

  private def concreteBitVectorOperation(op: (Int, Int) => Int, b1: String, b2: String): String = {
    b1.zip(b2).map { case (bit1, bit2) => if (op(bit1 - '0', bit2 - '0') == 1) '1' else '0' }.mkString
  }
}
