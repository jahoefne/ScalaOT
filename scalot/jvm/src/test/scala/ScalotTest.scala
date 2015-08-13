package scalot

import utest._
import utest.ExecutionContext.RunNow
import upickle.default._
import scala.util.Random

object ScalotTest{
  def repeatRandomCount = 10
  def randomString(length: Int): String = Random.alphanumeric.take(length).mkString
  def randomSeqWithSum(max: Int, curr: Seq[Int] = Seq[Int]()): Seq[Int] = curr.sum match {
    case x if x > max => randomSeqWithSum(max, curr.drop(1))
    case x if x < max => randomSeqWithSum(max, curr :+ Random.nextInt(10) + 1)
    case x if x == max => curr
  }

  def randomOpFor(str: String): Operation = {
    val numberz = randomSeqWithSum(str.length)
    var op = Operation()
    for (x <- numberz) {
      Random.nextBoolean() match {
        case true => op = op.delete(x)
        case false => op = op.skip(x)
      }
      if (Random.nextInt(2) == 0)
        op = op.insert(randomString(x))
    }
    op
  }

  val test = TestSuite {

    'RandomTransform {
      print("\nRandom transformation test")
      (1 to repeatRandomCount).foreach(x => {
        val str = randomString(Random.nextInt(500) + 5)
        val opA = randomOpFor(str)
        val opB = randomOpFor(str)

        val resA = opA.applyTo(str).get
        val resB = opB.applyTo(str).get

        val trans = Operation.transform(opA, opB).get
        // println(s"Input String Length ${str.length}")
        // println(s"Prime1: base(${trans.prime1.baseLength} should be ${resB.length}}) target(${trans.prime1.targetLength})  \n\t${trans.prime1}")
        // println(s"Prime2: base(${trans.prime2.baseLength} should be ${resA.length}) target(${trans.prime2.targetLength})  \n\t${trans.prime2}")

        val resAB = trans.prime2.applyTo(resA)
        val resBA = trans.prime1.applyTo(resB)
        //info(resAB)
        assert(resAB.isDefined && resBA.isDefined)
        assert(resAB == resBA)
      })
    }


    'RandomInverse {
      print(s"\nRandom Inverse Test")
      (1 to repeatRandomCount).foreach(x => {
        val str = randomString(Random.nextInt(500) + 5)
        val op1 = randomOpFor(str)

        val res1 = op1.applyTo(str).get

        val inverse = op1.invert(str)
        assert(str == inverse.applyTo(res1).get)
      })
    }

    'RandomApply {
      print(s"\nRandom Apply Test")
      (1 to repeatRandomCount).foreach(x => {
        val str = randomString(Random.nextInt(500) + 5)
        // Given(s"a Random String - $str")

        // And("a random operation for that string")
        val op1 = randomOpFor(str)
        assert(str.length == op1.baseLength)

        // Then("the Operation should be applicable to the string")
        val res = op1.applyTo(str).get
        assert(res.length == op1.targetLength)
      })
    }

    'InvertingAnOperation {
      val str = "Hello World"
      // println(s"\nOriginal String is '$str'")

      val op1 = Operation().insert("Hallo Welt!").delete(11)
      // println(s"Original Op is ${op1.toString}")

      val res = op1.applyTo(str).get
      // println(s"\tResult String is '$res'")

      val invertedOp = op1.invert(str)
      //println(s"Inverted Op is ${invertedOp.toString}")

      val original = invertedOp.applyTo(res).get
      // println(s"\tRestored Original String is '$original'")

      assert(original == str)
    }

    'RandomComposeTest {
      print(s"\nStarting Compose Test")
      (1 to repeatRandomCount).foreach(retry => {
        val str = randomString(200)

        val op1 = randomOpFor(str)
        val op2 = randomOpFor(op1.applyTo(str).get)

        val res1 = op1.applyTo(str).get
        //info(s"\tstr = '$str'")
        // info(s"\tOp1 is ${op1.toString}")
        // info(s"\t\t res1 = Op1(str) = $res1")

        val res2 = op2.applyTo(res1).get
        // info(s"\tOp2 is ${op2.toString}")
        // info(s"\t\t Op1(res) = $res2")

        val composed = op1.compose(op2).get
        val composedResult = composed.applyTo(str)
        // info(s"\t\tComposed Operation is ${composed.toString}")
        // info(s"\t\t\t composed(str) = $composedResult")
      })
    }

    /** Some successive components are combinable */
    'ChainingComponents {
      val op1 = Operation()
        .insert("abc")
        .skip(2)
        .delete(2)

      val op2 = Operation()
        .insert("a")
        .insert("b")
        .insert("c")
        .skip(1)
        .skip(1)
        .delete(1)
        .delete(1)

      assert(op2.ops.length == op1.ops.length)
      assert(op2.baseLength == op1.baseLength)
      assert(op2.targetLength == op1.targetLength)
    }

    'ResolveCollisionByTransformation {
      val start = "Hello World"
      val op1 = Operation().insert("Start ").delete(11).insert(" End")
      val op2 = Operation().skip(6).insert("Middle ").skip(5)

      val res1 = op1.applyTo(start)
      val res2 = op2.applyTo(start)
      val TransformedPair(prime1, prime2) = Operation.transform(op1, op2).get

      val resolved1 = prime1.applyTo(res2.get)
      assert(resolved1.isDefined)

      val resolved2 = prime2.applyTo(res1.get)
      assert(resolved2.isDefined)
      assert(resolved1 == resolved2)
    }

    'TestingUpickle{
      println("\nTesting upickle serializeation of operations!")
      val op = new Operation().insert("Bla").skip(10).delete(2).insert("Bla")
     try {
       val res = write(op)
       println(write(op))
     }catch {
       case e: Exception =>
         e.printStackTrace()
     }
    }
  }

  def main(args: Array[String]) {
    val results = test.run()
  }
}
