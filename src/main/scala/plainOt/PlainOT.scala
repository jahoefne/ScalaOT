package plainOt

/**
 * A simple Implementation of Operational Transformation for Plain Text
 *
 * The main problem OT sovels is if n users modify a their copy of a document in an collaborative
 * editor the state of all copies must converge to finally be equal again. That problem is not trivial because
 * of network latency and conflicts.
 */
object PlainOT {

  /** Represents a sequence of operations valid for any string with length -> baseLength */
  case class Operation(ops: Seq[Component] = Seq[Component]()) {

    lazy val baseLength = ops.filterNot(_.isInstanceOf[InsComp]).map(_.length).sum

    lazy val targetLength: Int = ops.map(_.length).sum - ops.filter(_.isInstanceOf[DelComp]).map(_.length).sum

    def skip(pos: Int) = copy(ops = ops :+ SkipComp(pos))

    def insert(str: String) = copy(ops = ops :+ InsComp(str))

    def delete(count: Int) = copy(ops = ops :+ DelComp(count))

    /**
     * Helper for apply, does the actual applying in a recursive manner in O(n)
     */
    @scala.annotation.tailrec
    private def applyRec(c: Int = 0, t: Seq[Component] = ops, str: String): Option[String] = t.headOption match {

      case Some(ins: InsComp) =>
        val (start, end) = str.splitAt(c)
        applyRec(c + ins.length, t.drop(1), start + ins.str + end)

      case Some(skip: SkipComp) => applyRec(c + skip.length, t.drop(1), str)

      case Some(del: DelComp) =>
        applyRec(c, t.drop(1), str.patch(c, "", del.length))

      case None => Some(str)

      case _ => None
    }

    /**
     * Applies the operation to an input String
     * It is important to notice that not every input String is valid for a given
     * operation. There are two requirements for the input string
     * 1. The baseLength of the operation must match the length of the input string
     * 2. The characters of a delete component must match the characters in the input
     * string at the given point
     * @param str the string on which the input should be applied
     * @return the result of the input transformation
     */
    def apply(str: String): Option[String] = str.length == baseLength match {
      case true => applyRec(str = str)
      case _ => None
    }

    /**
     * Combine the current Operation with op
     * Note: op must be the operation that directly follows this operation
     */
    def compose(op: Operation): Operation = ???

    /** Computes the invert operation fro input string str
      * such that:
      * str == this.invert(str).apply(this.apply(str))
      * For implementing undo
      */
    def invert(str: String) : Operation = ???
  }

  /** Companion of the Operation case class */
  object Operation {
    /** The transform function is the heart of every OT implementation
      * It's use is explained below.
      * Let client A and client B start with the same string S
      * and A produced operation opA and B produces opB at the same time.
      *
      * The transform function yields an operation pair that makes the both strings
      * identical if A applies primeB and B applies primeA
      */
    def transform(a: Operation, b: Operation): Option[TransformedPair] = {

      def getElOpt(ops: Seq[Component], idx: Int): Option[Component] = try{
          Some(ops(idx))
        }catch{
          case e: Exception => None
        }


      if (a.baseLength != b.baseLength) return None

    }

  }


  /** The Result of the almighty 'transform operation' */
  case class TransformedPair(primeA: Operation, primeB: Operation)


  /**
   * Component Interface, every component has an explicit or implicit length
   */
  sealed trait Component {
    val length: Int
    def transform(other: Component) : Component
  }

  /**
   * Component for inserting plain text at the current position
   * @param str the string to insert
   */
  private case class InsComp(str: String) extends Component {
    override lazy val length: Int = str.length
  }

  /**
   * Component for deleting plain text at the current position
   * @param str the string to delete
   */
  private case class DelComp(count: Int) extends Component {
    override lazy val length: Int = count
  }

  /** Component for moving the cursor to the right
    * @param ret the number of chars the cursor should move
    */
  private case class SkipComp(ret: Int) extends Component {
    override lazy val length: Int = ret
  }

}
