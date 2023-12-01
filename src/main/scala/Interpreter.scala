package interpreter

abstract class ConcreteB
case class ConcreteBitVector(value: String) extends ConcreteB {
  override def toString = value
}
case object ConcreteZero extends ConcreteB {
  override def toString = "0"
}
case object ConcreteOne extends ConcreteB {
  override def toString = "1"
}
case class ConcreteVar(name: String) extends ConcreteB {
  override def toString = name
}
case class ConcreteBVAdd(b1: ConcreteB, b2: ConcreteB) extends ConcreteB {
  override def toString = "bvAdd("+b1.toString+","+b2.toString+")"
}
case class ConcreteBVSub(b1: ConcreteB, b2: ConcreteB) extends ConcreteB {
  override def toString = "bvSub("+b1.toString+","+b2.toString+")"
}
case class ConcreteBVNot(b: ConcreteB) extends ConcreteB {
  override def toString = "bvNot("+b.toString+")"
}
case class ConcreteBVOr(b1: ConcreteB, b2: ConcreteB) extends ConcreteB {
  override def toString = "bvOr("+b1.toString+","+b2.toString+")"
}
case class ConcreteBVAnd(b1: ConcreteB, b2: ConcreteB) extends ConcreteB {
  override def toString = "bvAnd("+b1.toString+","+b2.toString+")"
}
case class ConcreteBVXor(b1: ConcreteB, b2: ConcreteB) extends ConcreteB {
  override def toString = "bvXor("+b1.toString+","+b2.toString+")"
}

object ConcreteBitVectorInterpreter {
  def size(expr: ConcreteB): Int = expr match {
    case ConcreteBitVector(value) => 1
    case ConcreteZero => 1
    case ConcreteOne => 1
    case ConcreteVar(name) => 1
    case ConcreteBVAdd(b1, b2) => size(b1) + size(b2) + 1
    case ConcreteBVSub(b1, b2) => size(b1) + size(b2) + 1
    case ConcreteBVNot(b) => size(b) + 1
    case ConcreteBVOr(b1, b2) => size(b1) + size(b2) + 1
    case ConcreteBVAnd(b1, b2) => size(b1) + size(b2) + 1
    case ConcreteBVXor(b1, b2) => size(b1) + size(b2) + 1
  }

  def eval(expr: ConcreteB, env: Map[String, ConcreteBitVector] = Map.empty): String = expr match {
    case ConcreteBitVector(value) => value.reverse.padTo(8, '0').reverse.takeRight(8)
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
    String.format("%8s", Integer.toBinaryString(difference)).replace(' ', '0').takeRight(8)
  }

  private def concreteBitVectorOperation(op: (Int, Int) => Int, b1: String, b2: String): String = {
    b1.zip(b2).map { case (bit1, bit2) => if (op(bit1 - '0', bit2 - '0') == 1) '1' else '0' }.mkString
  }
}
