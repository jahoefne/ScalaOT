package scalot

import java.util.UUID

import scala.util.Random


case class Client(var str: String = "",
                  var revision: Int = 0,
                  var title: String = "",
                  id: String = Random.alphanumeric.take(20).mkString) {

  import ClientFSM._

  private var state: State = Synchronized(revision)

  /** If this returns an operation send it to the server */
  def applyLocal(op: Operation): Option[Operation] = {
    println("ApplyLocal")
    revision = revision + 1
    val opRev = op.copy(revision = revision)
    require(opRev(str).isDefined, s"The given operation can't be applied! OpLength ${opRev.baseLength} String Length ${str.length}")
    str = opRev(str).get
    handleFMS(state.applyLocal(opRev),opRev)
  }

  private def handleFMS(res: Action, op: Operation): Option[Operation] = res match {
    case NoOp(newState) =>
      state = newState
      None
    case Send(sendOp, newState) =>
      state = newState
      Some(sendOp)
    case Apply(applyOp: Operation, newState) =>
      state = newState
      val applied = applyOp(str)
      require(applied.isDefined, s"Not defined! Tried to ${applyOp.baseLength} on ${this.str.length} new state should be: ${newState}")
      str = applied.get
      revision = op.revision
      None
  }

  def applyRemote(op: Operation): Option[Operation] = {
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
      Send(op, AwaitConfirm(op))
    }

    /**
     * If we get an operation from the server in synced state we can directly apply it without a transformation
     */
    def applyRemote(op: Operation): Action = {
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
      NoOp(AwaitWithBuffer(outstanding, op))
    }

    /**
     * We got an operation from the server!
     * Two cases might happen here.
     * Either the *incoming operation is the operation we've sent to the server* - the confirmation!
     * Or the incomming operation is a operation from another client!
     */
    def applyRemote(op: Operation): Action = {
      if (op.id == outstanding.id) {
        /** It's out confirmation!
          * We can change back to the synchronized state
          */
        NoOp(Synchronized(op.revision))
      } else {
      //  println("Id does not equals!")
        /** It's an operation from another client we have to transform it
          * and still wait for our confirmation
          */
        val pair = Operation.transform(outstanding, op)
        require(pair.isDefined, "The transformation result is None, like wtf!!!1!1111!")
        val (client, server) = (pair.get.prime1, pair.get.prime2)
        val outstanding2 = outstanding.copy(ops = client.ops)
        Apply(op.copy(ops = server.ops), AwaitConfirm(outstanding2.copy(revision = op.revision + 1)))
      }
    }
  }

  /**
   * We've sent an operation to the server and didn't get a confirmation yet. The user did not yet trigger any other
   * operations.
   */
  case class AwaitWithBuffer(outstanding: Operation, buffer: Operation) extends State {
    def applyLocal(op: Operation): Action = {
      /**
       * The User triggered an operation again and we still didn't get a confirmation from the server.
       * => Combine Operation into buffer and wait for outstanding confirmation
       */
      val composition = buffer.compose(op)
      require(composition.isDefined, "The two operations must follow each other directly but are not composeable! This is not possible!")
      NoOp(AwaitWithBuffer(outstanding, composition.get))
    }

    def applyRemote(op: Operation): Action = {
      if (op.id == outstanding.id) {
        Send(buffer, AwaitConfirm(buffer))
      } else {
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
