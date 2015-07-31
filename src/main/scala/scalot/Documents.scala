package scalot

import java.util.UUID

import scalot.Scalot.Operation
import scalot.ClientFSM._

object Documents {

  /**
   * Represents a basic document
   */
  class BaseDoc(var str: String, val uuid: String) {

    /**
     * Apply a operation to the text
     * @param op the operation to apply
     */
    def ingest(op: Operation): Boolean = {
      op(str) match {
        case Some(result) => str = result
          true
        case _ => false
      }
    }
  }

  /**
   * Represents a serverside document.
   * A server document has to keep a history of previously occured operations.
   * TODO: Optimize History (Clear Methods, etc)
   **/
  case class ServerDocument(stri: String,
                            override val uuid: String = UUID.randomUUID().toString,
                            var operations: Seq[Operation] = Seq[Operation]()) extends BaseDoc(stri, uuid) {

    def receiveOperation(rev: Int, op: Operation): Option[Operation] = rev > 0 && operations.length < rev match {
      case true =>
        val droped = operations.drop(rev - 1) // take all operations that happened concurrently
        if (!droped.isEmpty) {
          val resolved = droped.reduceLeft((curr, res) => // and resolve all conflicts recursively
            Operation.transform(curr, res).get.prime1)
          ingest(resolved) // apply operation to out text
          operations = operations :+ resolved // store operation in history
          Some(resolved)
        } else {
          None
        }
      case _ => None
    }

  }

  /**
   *
   * @param revision
   * @param str
   * @param uuid
   */
  abstract class ClientDocument(var revision: Int,
                                var stri: String,
                                override val uuid: String = UUID.randomUUID().toString) extends BaseDoc(stri, uuid) {

    var state: ClientState = Synchronized

    def applyClient(op: Operation) = {
      incRev()
      str = op(str).get
      state = state.applyClient(op, this)
    }

    def applyServer(op: Operation) = {
      incRev()
      state = state.applyServer(op, this)
    }

    private def incRev() = {
      revision = revision + 1
    }

    def serverAck() = {
      incRev()
      state = state.serverAck(this)
    }

    def serverReconnect() = state.resend(this)

    def applyOp(op: Operation) = {
      op(str) match {
        case Some(s) => str = s
        case _ => throw new Exception("Could not apply operation!")
      }
    }

    def sendOp(op: Operation, rev: Int)
  }

}
