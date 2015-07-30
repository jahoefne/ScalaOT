import org.scalatest._
import scalot.Scalot
import scalot.Scalot.TransformedPair

import scala.util.Random


class ScalotTests extends FunSuite {

  def randomString(length: Int) : String = Random.alphanumeric.take(length).mkString
  def randomOpFor(str: String) : Scalot.Operation = {
    for()
    Scalot.Operation()
  }

  test("Inverting an Operation"){
    val str = "Hello World"
    info(s"Original String is '$str'")

    val op1 = Scalot.Operation().insert("Hallo Welt!").delete(11)
    info(s"Original Op is ${op1.toString}")

    val res = op1(str).get
    info(s"\tResult String is '$res'")

    val invertedOp = op1.invert(str)
    info(s"Inverted Op is ${invertedOp.toString}")

    val original = invertedOp(res).get
    info(s"\tRestored Original String is '$original'")

    assert(original == str)
  }

  test("Compose Test"){
    info("Starting Compose Test…")
    val str = "Hello World!"

    val op1 = Scalot.Operation().skip(6).insert("There").delete(6)
    val op2 = Scalot.Operation().insert("BEGINNING").skip(6).insert("MIDDLE").skip(5).insert(" END")

    val res1 = op1(str).get
    info(s"\tstr = '$str'")
    info(s"\tOp1 is ${op1.toString}")
    info(s"\t\t res1 = Op1(str) = $res1")

    val res2 = op2(res1).get
    info(s"\tOp2 is ${op2.toString}")
    info(s"\t\t Op1(res) = $res2")

    val composed = op1.compose(op2).get
    val composedResult = composed(str)
    info(s"\t\tComposed Operation is ${composed.toString}")
    info(s"\t\t\t composed(str) = $composedResult")
  }


  test("Chaining Components of the same type does not increase the total number of components "){
    val op1 = Scalot.Operation()
      .insert("abc")
      .skip(2)
      .delete(2)

    val op2 = Scalot.Operation()
      .insert("a")
      .insert("b")
      .insert("c")
      .skip(1)
      .skip(1)
      .delete(1)
      .delete(1)

    assert(op2.ops.length == op1.ops.length, "Number of components in chained operations must not differ")
    assert(op2.baseLength == op1.baseLength, "BaseLength should be equal")
    assert(op2.targetLength == op1.targetLength, "TargetLength should be equal")
  }

  test("An OT with two clients creating a collision should resolvable by a simple transformation"){
    val start = "Hello World"
    val op1 = Scalot.Operation().insert("Start ").delete(11).insert(" End")
    val op2 = Scalot.Operation().skip(6).insert("Middle ").skip(5)

    val res1 = op1.apply(start)
    val res2 = op2.apply(start)

    info(s"Start String '$start")
    info(s"Operation1 -> ${op1.toString}")
    info(s"\tgives -> $res1")
    info(s"Operation2 -> ${op1.toString}")
    info(s"\tgives -> $res2")

    val TransformedPair(prime1, prime2) = Scalot.Operation.transform(op1,op2).get

    val resolved1 = prime1(res2.get)
    assert(resolved1.isDefined, "resolved1 must not be None")
    info(s"Reolver 1 -> ${prime1.toString}")
    info(s"\tResolves -> $resolved1")

    val resolved2 = prime2(res1.get)
    assert(resolved2.isDefined,"resolved2 must not be None" )
    info(s"Reolver 2 -> ${prime2.toString}")
    info(s"\tResolves -> $resolved2")

    assert(resolved1 == resolved2)
  }
}
