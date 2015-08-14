package scalot

import java.util.UUID

import scala.util.Random


case class ApplyResult(send:Option[Operation], apply: Option[Operation])

case class Client(var str: String = "",
                  var revision: Int = 0,
                  var title: String = "",
                  id: String = Random.alphanumeric.take(20).mkString) {

  import ClientFSM._

  var state: State = Synchronized(revision)

  /** If this returns an operation send it to the server */
  def applyLocal(op: Operation): ApplyResult = {
    revision = revision + 1
    val opRev = op.copy(revision = revision)
    require(opRev.applyTo(str).isDefined, s"The given operation can't be applied! $op String Length ${str.length}")
    str = opRev.applyTo(str).get
    handleFMS(state.applyLocal(opRev),opRev)
  }

  def getState(): State = state

  private def handleFMS(res: Action, op: Operation): ApplyResult = res match {
    case NoOp(newState) =>
      state = newState
      revision = op.revision
      ApplyResult(None, None)
      
    case Send(sendOp, newState) =>
      state = newState
      revision = sendOp.revision
      ApplyResult(Some(sendOp), None)
      
    case Apply(applyOp: Operation, newState) =>
      state = newState
      val applied = applyOp.applyTo(str)
      require(applied.isDefined,
        s"Not defined! Tried to ${applyOp} apply on ${this.str.length} new state should be: ${newState}, incomming op version: ${op}, client had version: $revision")
      str = applied.get
      revision = op.revision
      ApplyResult(None, Some(applyOp))
  }

  def applyRemote(op: Operation): ApplyResult = {
    handleFMS(state.applyRemote(op),op)
  }
}


/**
 * Local Changes must be applied to the document instantly, and then given into the FSM
 * in order to determine possible further actions
 * Remote Changes must be given in the FSM to determine the action
 */
object ClientFSM {

  sealed trait Action
  case class NoOp(state: State) extends Action
  case class Send(op: Operation, state: State) extends Action
  case class Apply(op: Operation, state: State) extends Action

  sealed trait State {
    def applyLocal(op: Operation): Action
    def applyRemote(op: Operation): Action
  }

  /**
   * Synchronized state means the instance was not changed since it was last synchronized with the
   * server.
   */
  case class Synchronized(revision: Int) extends State {
    /**
     * If the users triggers an operation -> Send it to the server and wait for the confirmation
     */
    def applyLocal(op: Operation): Action = {
      println(s"Synchronized: applyLocal $op")

      Send(op, AwaitConfirm(op))
    }

    /**
     * If we get an operation from the server in synced state we can directly apply it without a transformation
     */
    def applyRemote(op: Operation): Action = {
      println(s"Synchronized: applyRemote $op")
      Apply(op, Synchronized(op.revision))
    }
  }

  /**
   * We've sent an operation to the server and didn't get a confirmation yet. The user did not yet trigger any other
   * operations.
   */
  case class AwaitConfirm(outstanding: Operation) extends State {

    /**
     * The user triggered a new operation while we still wait for a confirmation from the server!
     * Buffer the operation the user triggers until we get the confirmation from the server
     */
    def applyLocal(op: Operation): Action = {
      println(s"AwaitConfirm: applyLocal $op")
      NoOp(AwaitWithBuffer(outstanding, op))
    }

    /**
     * We got an operation from the server!
     * Two cases might happen here.
     * Either the *incoming operation is the operation we've sent to the server* - the confirmation!
     * Or the incomming operation is a operation from another client!
     */
    def applyRemote(op: Operation): Action = {
      println(s"AwaitConfirm: applyRemote $op")

      if (op.id == outstanding.id) {
        /** It's our confirmation!
          * We can change back to the synchronized state
          */
        println("\t It's the confirmation!"+ op)
        NoOp(Synchronized(op.revision))
      } else {
        /** It's an operation from another client we have to transform it
          * and still wait for our confirmation
          */
        println("\t AwaitConfirm Transformation"+op)
        val pair = Operation.transform(outstanding, op)

        require(pair.isDefined,
          s"The transformation result is None, tried to transform $outstanding" +
            s"with $op (rev:${op.revision} id:${op.id}")
        val (client, server) = (pair.get.prime1, pair.get.prime2)
        val outstanding2 = outstanding.copy(ops = client.ops)
        Apply(op.copy(ops = server.ops), AwaitConfirm(outstanding2.copy(revision = op.revision + 1)))
      }
    }
  }

  /**
   * We've sent an operation to the server and didn't get a confirmation yet. But the user already triggered other
   * operations, we must buffer those operations until we get our confirmation.
   */
  case class AwaitWithBuffer(outstanding: Operation, buffer: Operation) extends State {
    def applyLocal(op: Operation): Action = {
      /**
       * The User triggered an operation again and we still didn't get a confirmation from the server.
       * => Combine Operation into buffer and wait for outstanding confirmation
       */
      println(s"AwaitWithBuffer: applyLocal $op")
      val composition = buffer.compose(op)
      require(composition.isDefined, "The two operations must follow each other directly but are not composeable! This is not possible!")
      NoOp(AwaitWithBuffer(outstanding, composition.get))
    }

    def applyRemote(op: Operation): Action = {
      println(s"AwaitWithBuffer: applyRemote $op")
      if (op.id == outstanding.id) {
        println(s"\tIt's the confirmation ouststanding is $outstanding")
        val newBuf = buffer.copy(revision = outstanding.revision+1)
        Send(newBuf, AwaitConfirm(newBuf))
      } else {
        println(s"\tIt's NOT the confirmation")
        val pair = Operation.transform(outstanding, op)
        require(pair.isDefined, "The first transformation result is None!")
        val (client, server) = (pair.get.prime1, pair.get.prime2)
        val outstanding2 = outstanding.copy(ops = client.ops, revision = op.revision + 1)

        val pair2 = Operation.transform(buffer, server)
        require(pair2.isDefined, "The second transformation result is None!")
        val (client2, server2) = (pair2.get.prime1, pair2.get.prime2)
        val buffer2 = buffer.copy(ops = client2.ops, revision = outstanding2.revision)

        Apply(op.copy(ops = server2.ops), AwaitWithBuffer(outstanding2, buffer2))
      }
    }
  }

}
