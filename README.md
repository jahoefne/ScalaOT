# Scalot

**Scalot** provides easy Operational Transformations to enable collaborative text editing.
It works on both ScalaJS and the JVM and it is very easy to use

####Client side
```scala
/* Create a new Client Side Document, wich is empty and at revision 0 */
val client = Client(str = "", revision = 0)

/* on user changes do */
client.applyLocal(change) match {
   case Some(operation) => // send operation to server
   case _ =>
}

/** on message from server */
client.applyRemote(incommingMessage) match {
   case Some(operation) => // send operation to server
   case _ =>
}
```

####Server side
```scala
val server = new Server(str = "", revision = 0)

/* on incomming message */
server.receiveOperation(message) match {
   case Some(resp) => /* broadcast resp to all clients */
   case _ =>
}
```

Inspired by https://github.com/Operational-Transformation/ot.js
