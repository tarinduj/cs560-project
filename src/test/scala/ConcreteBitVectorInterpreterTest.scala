import org.scalatest.funsuite.AnyFunSuite

class ConcreteBitVectorInterpreterTest extends AnyFunSuite {

  // Function Helper for BVAdd
  def testBVAdd(op1: String, op2: String, res: String) = {
    val expr = ConcreteBVAdd(ConcreteBitVector(op1), ConcreteBitVector(op2))
    assert(ConcreteBitVectorInterpreter.eval(expr) == res, "Invalid result for BVAdd")
  }

  test("BVAdd") {
    testBVAdd("10000000", "01111111", "11111111")
  }

  // Function Helper for BVSub
  def testBVSub(op1: String, op2: String, res: String) = {
    val expr = ConcreteBVSub(ConcreteBitVector(op1), ConcreteBitVector(op2))
    assert(ConcreteBitVectorInterpreter.eval(expr) == res, "Invalid result for BVSub")
  }

  test("BVSub") {
    testBVSub("10000000", "01111111", "00000001")
  }

  // Function Helper for BVAnd
  def testBVAnd(op1: String, op2: String, res: String) = {
    val expr = ConcreteBVAnd(ConcreteBitVector(op1), ConcreteBitVector(op2))
    assert(ConcreteBitVectorInterpreter.eval(expr) == res, "Invalid result for BVAnd")
  }

  test("BVAnd") {
    testBVAnd("10000000", "01111111", "00000000")
  }

  // Function Helper for BVOr
  def testBVOr(op1: String, op2: String, res: String) = {
    val expr = ConcreteBVOr(ConcreteBitVector(op1), ConcreteBitVector(op2))
    assert(ConcreteBitVectorInterpreter.eval(expr) == res, "Invalid result for BVOr")
  }

  test("BVOr") {
    testBVOr("10000000", "01111111", "11111111")
  }

  // Function Helper for BVXor
  def testBVXor(op1: String, op2: String, res: String) = {
    val expr = ConcreteBVXor(ConcreteBitVector(op1), ConcreteBitVector(op2))
    assert(ConcreteBitVectorInterpreter.eval(expr) == res, "Invalid result for BVXor")
  }

  test("BVXor") {
    testBVXor("10000000", "01111111", "11111111")
  }

  // Function Helper for BVNot
  def testBVNot(op: String, res: String) = {
    val expr = ConcreteBVNot(ConcreteBitVector(op))
    assert(ConcreteBitVectorInterpreter.eval(expr) == res, "Invalid result for BVNot")
  }

  test("BVNot") {
    testBVNot("10000000", "01111111")
  }
}
