---
title: TCP/IP Echo Server
pagegroup: Tutorials
pageorder: 3100
---

This tutorial implements a `ServerSocketActor`-based TCP/IP "echo"
service, and a matching client.
[Documentation on socket support is available.](sockets.html). A
larger TCP/IP chat server example can be found in the `Actors-Demos`
package in class `ChatRoom`.

{: .warning}
Socket support is experimental.

## The server

We will need two classes for the server: one for the
connection-accepting part, and one for each connected session.

### EchoServiceActor

The `EchoServiceActor` has a single instance variable, `sock`, which
holds an `ActorProxy` for a `ServerSocketActor`.

```smalltalk
ActorBehavior subclass: #EchoServiceActor
    instanceVariableNames: 'sock'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'ActorExamples'
```

#### Initialization

Initialization of the behavior object involves two steps. First, we
create the listening socket actor. Second, we issue it some *credit*,
allowing it to begin sending us new connections. Each connection we
accept depletes its credit by one unit; we will need to replenish it.

```smalltalk
initialize
    super initialize.
    sock := ServerSocketActor listenOnPort: 4096.
    sock issueCredit.
```

#### Readiness notification

The first event we are sent is an indication that the server socket is
listening for new connections.

```smalltalk
tcpServer: aServer ready: aBoolean
    self logAll: {
        'Echo server '.
        aBoolean ifTrue: ['is'] ifFalse: ['is not'].
        ' accepting connections'.
        String cr
    }
```

#### Accepting a connection

Second, whenever a new connection arrives, we are notified with a
`tcpServer:accepted:` event.

```smalltalk
tcpServer: aServer accepted: aConnection
    | session |
    self logAll: {'Connection accepted: '. aConnection}.
    session := EchoSessionActor spawn.
    session actor monitor: self.
    session connection: aConnection.
    aConnection controllingActor: session.
    sock issueCredit.
```

Let's take this a step at a time.

First, we log a message to Transcript about the new connection:

```smalltalk
self logAll: {'Connection accepted: '. aConnection}.
```

Then, we spawn the actor that will be responsible for this connection:

```smalltalk
session := EchoSessionActor spawn.
```

We [monitor](links-and-monitors.html#monitors) the new actor, so that
we are notified when it terminates:

```smalltalk
session actor monitor: self.
```

We tell it about the socket it is to be using:

```smalltalk
session connection: aConnection.
```

We tell the *socket* that the new `EchoSessionActor` is to be its
[controlling actor](sockets.html):

```smalltalk
aConnection controllingActor: session.
```

Finally, we tell the server socket that we are ready to accept another
connection when one arrives:

```smalltalk
sock issueCredit.
```

#### Handling session termination

Finally, because we called `monitor:` on the `EchoSessionActor`, we
will be sent an event when it terminates:

```smalltalk
peer: aSession monitor: aReference terminatedWith: aReason
    self logAll: {'Session ended: '. aSession}.
```

### EchoSessionActor

The `EchoSessionActor` has a single instance variable, `conn`, which
holds an `ActorProxy` for a `SocketActor`.

```smalltalk
ActorBehavior subclass: #EchoSessionActor
    instanceVariableNames: 'conn'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'ActorExamples'
```

#### Initialization

The actor's connection is passed to it by its `EchoServiceActor` with
a call to `connection:`.

```smalltalk
connection: aConnection
    conn := aConnection.
    conn delimiterMode: String lf.
    conn issueCredit.
```

The first thing it does with its new connection is configures it to
use
[linefeed-terminated UTF-8 work units](sockets.html#delimiter-separated-work-units):

```smalltalk
conn delimiterMode: String lf.
```

Then, having declared what kind of work unit it wishes to receive, it
sends some receive credit to the connection:

```smalltalk
conn issueCredit.
```

The next line of input to arrive is sent to the actor, consuming one
credit unit in the process. Once credit reaches zero, the
`EchoSessionActor` has to issue more credit to allow more data to flow
in from the remote peer.

#### Handling incoming data

Each time a work unit arrives, it is delivered using a
`tcpSocket:data` event.

```smalltalk
tcpSocket: aSocket data: line
    self logAll: {'Received line: '. line}.
    conn sendLine: 'You said: ', line.
    conn issueCredit.
```

All the `EchoSessionActor` does is repeat the line back to the remote
peer, and issue credit to its connection allowing the next line of
input to arrive.

#### Handling disconnection

When the remote peer disconnects, a `tcpSocketClosed:reason:` event is
delivered. Here, we simply terminate the `EchoSessionActor`.

```smalltalk
tcpSocketClosed: aSocket reason: aReason
    self logAll: {'Disconnected: '. aReason}.
    Actor terminateNormally.
```

### Running the service

```smalltalk
s := EchoServiceActor spawn.
```

Now connect to the service on port 4096, perhaps using `nc` on the
command line, or `telnet`, or the `EchoClientActor` developed below.
You could also use the [`DemoSocketTerminal`](morphic.html#example)
from the `Actors-Demos` package.

When you've finished creating and destroying connections, terminate
the listening actor:

```smalltalk
s actor kill.
```

This stops the `EchoServiceActor`, thereby closing the listening
socket, but leaves the `EchoSessionActor`s running, allowing them to
continue conversing with their remote peers until they disconnect.

## The client

For the client, we only need one class, representing a connected session.

{: .tip}
As well as using this client, you can also interact with the server
using [`DemoSocketTerminal`](morphic.html#example) in the
`Actors-Demos` package.

### EchoClientActor

The `EchoClientActor` has two instance variables: `conn`, which holds
an `ActorProxy` for a `SocketActor`; and `readers`, which holds an
`OrderedCollection` of [`ActorRequest`](requests.html)s.

When a line is sent to the server with `sendLine:`, the call to
`sendLine:` doesn't return until the reply is received from the
server. The request object representing the call is placed in the
`readers` queue. Lines of input are sent to the elements of the queue
in order.

```smalltalk
ActorBehavior subclass: #EchoClientActor
    instanceVariableNames: 'conn readers'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'ActorExamples'
```

#### Initialization

We create the new actor, which will have the current actor as its
controlling actor, and will automatically be linked to us.

```smalltalk
initialize
    super initialize.
    conn := SocketActor connectToHost: 'localhost' port: 4096.
    readers := OrderedCollection new.
```

#### Connection establishment

The first event we receive is notification of successful connection.

```smalltalk
tcpSocketConnected: aSocket
    conn delimiterMode: String lf.
```

We configure the new connection by setting it to linefeed-terminated
UTF-8 work units, just like we did in `EchoSessionActor`.

We don't issue any credit at this point, however: this is done in
`sendLine:`.

#### Transmitting data

The `sendLine:` method does three things. It first
[suspends and stores its caller](behaviors.html#suspending-the-caller)
in the `readers` queue. Then, sends a line of text to the server.
Finally, it issues a unit of credit, allowing the socket to send us
the next line of input from the server—the reply.

```smalltalk
sendLine: aString
    readers add: Actor caller.
    conn sendLine: aString.
    conn issueCredit.
```

#### Receiving data

When a line arrives from the server, we get a `tcpSocket:data:` event.
We remove the first waiting reader (we know there will be one), and
finally reply to it with the received line.

```smalltalk
tcpSocket: aSocket data: line
    readers removeFirst resolveWith: line.
```

#### Closing the connection

The `stop` method terminates the actor. Because the `EchoClientActor`
is [linked](links-and-monitors.html#links) to the `SocketActor`, this
automatically causes the latter to disconnect and terminate.

```smalltalk
stop
    Actor terminateNormally.
```

### Example session

Start an `EchoServiceActor`, as
[described above](#running-the-service).

Then, create a new client:

```smalltalk
c := EchoClientActor spawn.
```

Sending a line results, ultimately, in a reply:

```smalltalk
(c sendLine: 'hello') wait. "'You said: hello'"
```

Finally, stopping the `EchoClientActor` disconnects the connection:

```smalltalk
c stop.
```
