abstract class B
case class BitVector(value: String) extends B
case object Zero extends B
case object One extends B
case class BVAdd(b1: B, b2: B) extends B
case class BVSub(b1: B, b2: B) extends B
case class BVNot(b: B) extends B
case class BVOr(b1: B, b2: B) extends B
case class BVAnd(b1: B, b2: B) extends B
case class BVXor(b1: B, b2: B) extends B

object BitVectorInterpreter {

  def eval(expr: B): String = expr match {
    case BitVector(value) => value
    case Zero => "00000000"
    case One => "00000001"
    case BVAdd(b1, b2) => bitVectorAdd(eval(b1), eval(b2))
    case BVSub(b1, b2) => bitVectorSub(eval(b1), eval(b2))
    case BVNot(b) => eval(b).map(bit => if (bit == '0') '1' else '0')
    case BVOr(b1, b2) => bitVectorOperation(_ | _, eval(b1), eval(b2))
    case BVAnd(b1, b2) => bitVectorOperation(_ & _, eval(b1), eval(b2))
    case BVXor(b1, b2) => bitVectorOperation(_ ^ _, eval(b1), eval(b2))
  }

  private def bitVectorAdd(b1: String, b2: String): String = {
    val sum = Integer.parseInt(b1, 2) + Integer.parseInt(b2, 2)
    String.format("%8s", Integer.toBinaryString(sum)).replace(' ', '0')
  }

  private def bitVectorSub(b1: String, b2: String): String = {
    val difference = Integer.parseInt(b1, 2) - Integer.parseInt(b2, 2)
    String.format("%8s", Integer.toBinaryString(difference)).replace(' ', '0')
  }

  private def bitVectorOperation(op: (Int, Int) => Int, b1: String, b2: String): String = {
    b1.zip(b2).map { case (bit1, bit2) => if (op(bit1 - '0', bit2 - '0') == 1) '1' else '0' }.mkString
  }
}