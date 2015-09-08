# Scalot

Add to your SBT Project with
`libraryDependencies += "com.github.jahoefne" % "scalot_2.11" % "1.0"`


**Scalot** provides easy Operational Transformations to enable collaborative text editing.
It works on both ScalaJS and the JVM and it is very easy to use.

The goal of OT is to transfrom editing operations of users working on a shared document in a way that preserves editing intentions and automatically resolves conflicts.

##Operational Transformation
Assume two clients *client1* and *client2* edit the same document with the initial string `'Hello World!'` and the following operations happen concurrently.

* *client1* inserts `'Cruel '` after character 6 yielding `'Hello Cruel World!'` and producing operation `Insert(6,'Cruel ')`

* *client2* replaces `'Hello'` with `'Goodbye'` yielding `'Goodbye World!'` and producing two operations `Delete(1-5)` and `Insert(0, 'Goodbye')`

The combined edits of *client1* and *client2* should of course yield `'Goodbye Cruel World!'` however when simply passing the operations around the results might differ.
Assume the server receives the operations in the following order:
1. `Delete(1-5)`
2. `Insert(6,'Cruel ')`
3. `Insert(0, 'Goodbye')`

*The resulting string would be `'Goodbye WorldCruel !'`.*
Operational Transformation takes care of *transforming* the operations of any number of clients. So that the resulting string will always be the expeceted one.

##Usage Example
The usage of Scalot is very simple:

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

Edits are represented as `Operations`, a operation consists of a sequence of components.
Components are alternation instructions for a certain part of a string.
A Operation can be applied to a given string if the operation contains a component for each part of the text.

**Scalot** defines three different operations *skip(count)*, *delete(count)*, *insert(string)*. Components can be cascaded in the following manner.
```scala
val operation = new Operation().skip(5).insert("Hello").skip(5).delete(2) // …
```
the above Operation has a baseLength of 12 and is therefore applicaple to any string with exactly 12 character.

Inspired by https://github.com/Operational-Transformation/ot.js
