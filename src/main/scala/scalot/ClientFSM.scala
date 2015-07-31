package scalot

import scalot.Documents.ClientDocument
import scalot.Scalot.{TransformedPair, Operation}


object ClientFSM {

  sealed trait ClientState {
    def applyClient(op: Operation, sender: ClientDocument): ClientState

    def applyServer(op: Operation, sender: ClientDocument): ClientState

    def transformSelection(x: Int)

    def serverAck(sender: ClientDocument) : ClientState

    def resend(sender: ClientDocument)
  }

  /**
   * Client haz the same state as the server
   */
  case object Synchronized extends ClientState {
    def applyClient(op: Operation, sender: ClientDocument) = {
      sender.sendOp(op, sender.revision)
      new AwaitingConfirm(op)
    }

    def applyServer(op: Operation, sender: ClientDocument) = {
      sender.applyOp(op)
      this
    }

    override def transformSelection(x: Int): Unit ={ x}

    override def resend(sender: ClientDocument): Unit = throw new Exception("Trying to Resend in Synchronized State!")

    override def serverAck(sender: ClientDocument): ClientState = throw new Exception("Got Ack but - There is no pending operation!")
  }


  /**
   * Client is waiting for an confirmation from the server for an outstanding operation
   * @param outstanding the operation that's not yet been acknowledged
   */
  case class AwaitingConfirm(outstanding: Operation) extends ClientState {
    def applyClient(op: Operation, sender: ClientDocument) = {
      new AwaitingWithBuffer(outstanding, op)
    }

    def applyServer(op: Operation, sender: ClientDocument) = {
      Operation.transform(outstanding, op) match {
        case Some(x: TransformedPair) =>
          sender.applyOp(x.prime2)
          new AwaitingConfirm(x.prime1)
        case _ =>
          throw new Exception("Couldn't transform 2 transformable operations!")
      }
    }

    override def transformSelection(x: Int): Unit = ???

    override def resend(sender: ClientDocument): Unit = sender.sendOp(outstanding, sender.revision)

    override def serverAck(sender: ClientDocument): ClientState =  Synchronized
  }

  /**
   * Client Waits for Confirmation on the server but received other operations already
   * @param outstanding
   * @param buffer
   */
  case class AwaitingWithBuffer(outstanding: Operation, buffer: Operation) extends ClientState {
    def applyClient(op: Operation, sender: ClientDocument) = buffer.compose(op) match {
      case Some(newBuf: Operation) => new AwaitingWithBuffer(outstanding, newBuf)
      case _ => throw new Exception("Could not compose operations to new buffer!")
    }

    def applyServer(op: Operation, sender: ClientDocument) = {
      val pair1 = Operation.transform(outstanding, op).get
      val pair2 = Operation.transform(buffer, pair1.prime2).get
      sender.applyOp(pair2.prime2)
      new AwaitingWithBuffer(pair1.prime1, pair2.prime1)
    }

    override def transformSelection(x: Int): Unit = {
      throw new Exception("Wtf!")
    }

    override def resend(sender: ClientDocument): Unit = {
      sender.sendOp(outstanding, sender.revision)
    }

    override def serverAck(sender: ClientDocument): ClientState = {
      sender.sendOp(buffer, sender.revision)
      new AwaitingConfirm(buffer)
    }
  }

}
