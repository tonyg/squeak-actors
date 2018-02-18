---
title: Sockets
pagegroup: User Manual
pageorder: 2000
---

Support for client (connecting) and server (listening) TCP/IP sockets
is modeled on
[Erlang's `gen_tcp`](http://erlang.org/doc/man/gen_tcp.html) library.

{: .warning}
Socket support is experimental.

Like Erlang, each connected socket is managed by a separate actor, and
every `SocketActor` is [linked](links-and-monitors.html#links) to a
*controlling actor*, which receives socket-related events.

Unlike Erlang, *credit-based flow control*[^credit-based-flow-control]
is used to manage input from a connected socket, as well as to manage
the rate at which new sockets are accepted from a server socket.

Reading from a socket is asynchronous and event-based. Writing to a
socket produces a [promise](promises.html) which is resolved when the
write has been delivered to the socket.

  [^credit-based-flow-control]: Surprisingly, "credit-based flow
    control" doesn't seem to be a term in common usage outside very
    narrow circles such as ATM-based networking. However, it is widely
    known in distributed systems folklore. One example of its use is
    [in the RabbitMQ server internals](https://www.rabbitmq.com/blog/2015/10/06/new-credit-flow-settings-on-rabbitmq-3-5-5/)
    for managing flows of messages. A more in-depth look at the topic
    in the setting of ATM networking can be found in
    [chapter 4](https://www.nap.edu/read/5769/chapter/4) of

    > "Traffic Management for High-Speed Networks: Fourth Lecture
    International Science Lecture Series", ed. H. T. Kung. The
    National Academies Press, Washington, DC. 1997.
    [DOI.](https://doi.org/10.17226/5769)

### Example

{: .note}
See the [TCP/IP Hello World tutorial](tutorial-hello-server.html) for
a full client-server example.

{: .note}
In addition, class `DemoSocketTerminal` demonstrates combining Socket
actors with actor-based [Morphic](morphic.html) programming.

Here is a very simple example, a rough HTTP client that retrieves
`http://localhost/`. After the headers have been read, the client
switches from double-linefeed-separated [work units](#receiving-data)
to raw chunks of data. At the moment of the change, the read credit
amount is zero, ensuring that there is no risk of accidentally reading
the wrong data in the wrong mode.

```smalltalk
ActorProcess boot: [ | s |
    s := SocketActor connectToHost: 'localhost' port: 80.
    "The new SocketActor takes the current actor as controlling actor."

    s lineTerminator: String crlf. "HTTP protocol requires this."
    s sendLine: 'GET / HTTP/1.0'.
    s sendLine: ''.

    s delimiterMode: String crlfcrlf.
    "This makes the first work item a string up to the header/body break."
    s issueCredit. "Issue one work-unit's credit: this reads the headers."

    s rawMode. "Switch to raw mode for the remainder of the connection."
    s infiniteCredit. "Retrieve it all."
    "NB be more careful about credit, in production use!"

    Actor receiveUntil: [:v |
        Transcript crlf; nextPutAll: (v printStringLimitedTo: 400); flush.
        v message selector = #tcpSocketClosed:reason: ]].
```

### ServerSocketActor

#### Creating a server socket

```smalltalk
ServerSocketActor listenOnHost: interfaceDnsNameString Port: portNumber.
ServerSocketActor listenOnPort: portNumber.
```

These two methods spawn a fresh `ServerSocketActor` that starts
listening on the given port. If an `interfaceDnsNameString` is not
given, `'0.0.0.0'` is used, accepting connections on any interface.

The new socket takes the calling actor as its controlling actor, and
starts off with zero credit for accepting connections.

#### Readiness events

```smalltalk
controllingActorProxy tcpServer: serverSocketActorProxy ready: aBoolean.
```

The server socket actor persists in the image until explicitly
[stopped](#shutting-down-a-server-socket). As circumstances change,
such as the image being stopped and restarted, the underlying
listening socket may come and go. It will not always be possible to
listen on the configured interface and port number.

Therefore, the socket actor sends its controlling actor a *readiness
event* each time the socket is able to start listening or is forced to
stop listening. The `aBoolean` field in the event will be `true` when
listening is in progress, and `false` when listening is on hold.

If listening could not be established, and credit for accepting new
connections is non-zero, then the server socket actor will keep trying
to re-establish its listening socket every few seconds.

#### Accepting connections

To start receiving connections, call `ServerSocketActor >>
#issueCredit` each time you want to allow
an additional connection to be
accepted.

For example, you might wish to call `issueCredit` once at startup
time, and then once at the top of the code handling each
`tcpServer:accepted:` event.

Use `#issueCredit: aNumber` to increment the available credit by
`aNumber` steps in one go.

When the server has credit and a connection arrives, the controlling
actor will be sent a `#tcpServer:accepted:` [request](requests.html):

```smalltalk
controllingActorProxy tcpServer: serverSocketActorProxy accepted: socketActorProxy.
```

The `socketActorProxy` refers to a [`SocketActor`](#socketactor).

#### Reconfiguring the listener

To alter the interface, port number, or underlying kernel-level listen
backlog size of an already-existing `ServerSocketActor`, use the
following methods:

```smalltalk
serverSocketActorProxy listenOnHost: aString port: aPortNumber backlogSize: aBacklog.
serverSocketActorProxy listenOnHost: aString port: aPortNumber.
```

If not supplied, `aBacklog` is set to 128.

#### Shutting down a server socket

```smalltalk
serverSocketActorProxy stop.
```

The `ServerSocketActor` will terminate, activating its links as usual.

Remember that the actor links to its controlling actor! You will often
want to unlink before calling `stop`:

```smalltalk
serverSocketActorProxy actor unlink.
serverSocketActorProxy stop.
```

### SocketActor

#### Creating a connected socket

There are two ways to create a connected socket: either
[wait for one to be accepted](#accepting-connections), or create a new
outbound client socket:

```smalltalk
SocketActor connectToHost: hostname port: portNumber.
```

The new socket takes the calling actor as its controlling actor, and
starts off with zero credit for receiving data.

#### Connection events

```smalltalk
controllingActorProxy tcpSocketConnected: socketActorProxy.
```

Issued when a client socket has connected.

Only issued for new, outbound client connections: when a connection is
accepted from a server socket, the `tcpServer:accepted:` event takes
the place of this event.

```smalltalk
controllingActorProxy tcpSocketClosed: socketActorProxy reason: anExceptionOrNil.
```

Issued when the socket closes.

#### Sending data

```smalltalk
socketActorProxy send: anObject.
```

If `anObject` is a `ByteArray`, sends the raw bytes; otherwise, sends
`anObject asString`, encoded into bytes as UTF-8.

```smalltalk
socketActorProxy sendAll: aCollection.
```

Just like `aCollection do: [:x | socketActorProxy send: x]`, but more
efficient.

```smalltalk
socketActorProxy sendLine: anObject.
```

Sends `anObject` followed by the current *line terminator*, which
defaults to

```smalltalk
String lf
```

The line terminator can be retrieved and set via

```smalltalk
socketActorProxy lineTerminator.
socketActorProxy lineTerminator: aString.
```

#### Receiving data

Data is read from the socket in *work units*, which can be either

 - delimiter-separated spans of bytes or characters, or
 - arbitrarily-sized chunks of data read in a single call to the
   underlying socket.

Credit for reading from the socket is issued in terms of these work
units.

For example, if the work unit selected is spans separated by `String
crlf`, then the credit value in the actor will be interpreted as the
number of CRLF-terminated lines of input to read from the socket and
send to the controlling actor. If the work unit separated is raw
binary data, the credit value will just denote the number of chunks to
be sent.

{: .note}
The default work unit type is
[`rawMode`](#arbitrary-chunk-work-units).

##### Delimiter-separated work units

```smalltalk
socketActorProxy delimiterMode: aString.
socketActorProxy delimiterMode: aStringOrByteArray binary: aBoolean.
```

Selects delimiter-separated work units.

If `aBoolean` is not supplied or is `false`, then UTF-8 decoding will
be automatically performed on the input. In this case, `aString` or
`aStringOrByteArray` must be a `String`, the delimiter to watch for.
Delivered work unit items will be `String`s.

If `aBoolean` is supplied and is `true`, then `aStringOrByteArray`
must be a `ByteArray`. No decoding of input will be performed.
Delivered work unit items will be `ByteArray`s.

##### Arbitrary chunk work units

```smalltalk
socketActorProxy rawMode. "Default at actor startup time."
socketActorProxy rawModeAscii.
```

These select either raw *binary* mode or raw *ASCII* mode,
respectively. Each time any data is available at all, the entirety of
the available data will be delivered as a single work unit to the
controlling actor.

For `rawMode`, delivered work unit items will be `ByteArray`s; for
`rawModeAscii`, they will be `String`s.

{: .warning}
Using ASCII mode is almost always wrong.  
&nbsp;  
Strongly prefer to either use a delimiter-separated mode, which does
UTF-8 conversion for you, or `rawMode`, with UTF-8 decoding performed
in the controlling actor.

##### Issuing credit

```smalltalk
socketActorProxy issueCredit.
```

Allows the receiver to relay one more work unit from the underlying socket.

```smalltalk
socketActorProxy issueCredit: amount.
```

Allows the receiver to accept `amount` more units of input from the
underlying socket. The `amount` may be negative, in which case the
final credit is clamped to be non-negative.

```smalltalk
socketActorProxy infiniteCredit.
```

Sets credit to infinity, allowing data to be read and delivered as
fast as it arrives. *This is usually not a good idea.* Useful for
testing, and in tightly controlled situations, but in the wild this
can overwhelm your image with input.

```smalltalk
socketActorProxy zeroCredit.
```

Sets credit to zero, preventing future read work (other than anything
that has already completed or is currently running, but hasn't been
fully delivered to the controlling actor yet) until credit is
subsequently increased.

#### Data delivery events

```smalltalk
controllingActorProxy tcpSocket: socketActorProxy data: workUnit
```

This event is delivered to the controlling actor whenever a unit of
credit has been used up in receiving a work unit from the socket.

#### Other events

```smalltalk
controllingActorProxy tcpSocketTimeout: socketActorProxy.
```

Issued when a connection attempt, an attempt to send data, or an
attempt to read data times out.

{: .implementation-note}
There is currently no way to set a read timeout implemented. Also,
Squeak's send timeout appears to be hard-coded.

#### Shutting down a connected socket

```smalltalk
socketActorProxy stop.
```

The `SocketActor` will disconnect (if it was connected) and close and
then terminate, activating its links as usual.

Remember that the actor links to its controlling actor! You will often
want to unlink before calling `stop`:

```smalltalk
socketActorProxy actor unlink.
socketActorProxy stop.
```
