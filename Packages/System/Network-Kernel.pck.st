'From Cuis 5.0 of 7 November 2016 [latest update: #3120] on 26 June 2017 at 6:41:20 pm'!
'Description '!
!provides: 'Network-Kernel' 1 5!
!classDefinition: #InvalidSocketStatusException category: #'Network-Kernel'!
Error subclass: #InvalidSocketStatusException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'InvalidSocketStatusException class' category: #'Network-Kernel'!
InvalidSocketStatusException class
	instanceVariableNames: ''!

!classDefinition: #NetworkError category: #'Network-Kernel'!
Error subclass: #NetworkError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'NetworkError class' category: #'Network-Kernel'!
NetworkError class
	instanceVariableNames: ''!

!classDefinition: #ConnectionClosed category: #'Network-Kernel'!
NetworkError subclass: #ConnectionClosed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'ConnectionClosed class' category: #'Network-Kernel'!
ConnectionClosed class
	instanceVariableNames: ''!

!classDefinition: #ConnectionRefused category: #'Network-Kernel'!
NetworkError subclass: #ConnectionRefused
	instanceVariableNames: 'host port'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'ConnectionRefused class' category: #'Network-Kernel'!
ConnectionRefused class
	instanceVariableNames: ''!

!classDefinition: #ConnectionTimedOut category: #'Network-Kernel'!
NetworkError subclass: #ConnectionTimedOut
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'ConnectionTimedOut class' category: #'Network-Kernel'!
ConnectionTimedOut class
	instanceVariableNames: ''!

!classDefinition: #NameLookupFailure category: #'Network-Kernel'!
NetworkError subclass: #NameLookupFailure
	instanceVariableNames: 'hostName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'NameLookupFailure class' category: #'Network-Kernel'!
NameLookupFailure class
	instanceVariableNames: ''!

!classDefinition: #NoNetworkError category: #'Network-Kernel'!
NetworkError subclass: #NoNetworkError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'NoNetworkError class' category: #'Network-Kernel'!
NoNetworkError class
	instanceVariableNames: ''!

!classDefinition: #SocketPrimitiveFailed category: #'Network-Kernel'!
NetworkError subclass: #SocketPrimitiveFailed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'SocketPrimitiveFailed class' category: #'Network-Kernel'!
SocketPrimitiveFailed class
	instanceVariableNames: ''!

!classDefinition: #ConnectionQueue category: #'Network-Kernel'!
Object subclass: #ConnectionQueue
	instanceVariableNames: 'portNumber maxQueueLength connections accessSema socket process'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'ConnectionQueue class' category: #'Network-Kernel'!
ConnectionQueue class
	instanceVariableNames: ''!

!classDefinition: #NetNameResolver category: #'Network-Kernel'!
Object subclass: #NetNameResolver
	instanceVariableNames: ''
	classVariableNames: 'DefaultHostName HaveNetwork ResolverBusy ResolverError ResolverMutex ResolverReady ResolverSemaphore ResolverUninitialized'
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'NetNameResolver class' category: #'Network-Kernel'!
NetNameResolver class
	instanceVariableNames: ''!

!classDefinition: #Socket category: #'Network-Kernel'!
Object subclass: #Socket
	instanceVariableNames: 'semaphore socketHandle readSemaphore writeSemaphore'
	classVariableNames: 'Connected DeadServer InvalidSocket OtherEndClosed Registry RegistryThreshold TCPSocketType ThisEndClosed UDPSocketType Unconnected WaitingForConnection'
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'Socket class' category: #'Network-Kernel'!
Socket class
	instanceVariableNames: ''!

!classDefinition: #SocketStream category: #'Network-Kernel'!
Object subclass: #SocketStream
	instanceVariableNames: 'recentlyRead socket inBuffer outBuffer inNextToWrite outNextToWrite lastRead timeout autoFlush bufferSize binary shouldSignal'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Network-Kernel'!
!classDefinition: 'SocketStream class' category: #'Network-Kernel'!
SocketStream class
	instanceVariableNames: ''!


!InvalidSocketStatusException commentStamp: '<historical>' prior: 0!
Signals if an operation on a Socket found it in a state invalid for that operation.!

!NetworkError commentStamp: 'mir 5/12/2003 18:12' prior: 0!
Abstract super class for all network related exceptions.!

!ConnectionClosed commentStamp: '<historical>' prior: 0!
Signals a prematurely closed connection.!

!ConnectionRefused commentStamp: 'mir 5/12/2003 18:14' prior: 0!
Signals that a connection to the specified host and port was refused.

	host		host which refused the connection
	port		prot to which the connection was refused
!

!ConnectionTimedOut commentStamp: '<historical>' prior: 0!
Signals that a connection attempt timed out.!

!NameLookupFailure commentStamp: 'mir 5/12/2003 18:16' prior: 0!
Signals that a name lookup operation failed.

	hostName	hostName for which the name loopup failed
!

!NoNetworkError commentStamp: '<historical>' prior: 0!
Signals that no network was found. This could happen, e.g., on dial-up connection when no connection was established when Squeak tried to access it.!

!ConnectionQueue commentStamp: '<historical>' prior: 0!
A ConnectionQueue listens on a given port number and collects a queue of client connections. In order to handle state changes quickly, a ConnectionQueue has its own process that: (a) tries to keep a socket listening on the port whenever the queue isn't already full of connections and (b) prunes stale connections out of the queue to make room for fresh ones.
!

!NetNameResolver commentStamp: '<historical>' prior: 0!
This class implements TCP/IP style network name lookup and translation facilities.

Attempt to keep track of whether there is a network available.
HaveNetwork	true if last attempt to contact the network was successful.
LastContact		Time of that contact (totalSeconds).
haveNetwork	returns true, false, or #expired.  True means there was contact in the last 30 minutes.  False means contact failed or was false last time we asked.  Get out of false state by making contact with a server in some way (FileList or updates).!

!Socket commentStamp: 'gk 12/13/2005 00:43' prior: 0!
A Socket represents a network connection point. Current sockets are designed to support the TCP/IP and UDP protocols. Sockets are the lowest level of networking object in Squeak and are not normally used directly. SocketStream is a higher level object wrapping a Socket in a stream like protocol.

ProtocolClient and subclasses are in turn wrappers around a SocketStream to provide support for specific network protocols such as POP, NNTP, HTTP, and FTP.!

!SocketStream commentStamp: '<historical>' prior: 0!
SocketStream is a wrapper for class Socket making it easy to write networking code by giving the programmer a stream-like protocol. A Socket is a two way communication link with two logically separate channels - input and output. The Socket class is the lowest level in Squeak for network communication and using it directly can be difficult and bug prone.

A SocketStream can be in binary or ascii mode, ascii is the default which means you are transmitting and receiving Strings. Most Internet protocols are in clear text ascii, like for example HTTP. Another setting is what timeout you want to use - default is the standardTimeout from Socket. More settings can be found in the method category 'configuration'.

Simplest example of connecting, sending/receiving and closing:

| stream result |
stream := SocketStream openConnectionToHostNamed: 'www.squeak.org' port: 80.
[[stream nextPutAll: 'GET / HTTP/1.0'; newLine; newLine; flush.
result := stream upToEnd. "Give us all data until the socket is closed."
Transcript show: result withCuisLineEndings; newLine.]
	ensure: [stream close]]
		on: ConnectionTimedOut
		do: [:ex | Transcript show: ex asString; newLine. ex resume]

There are two important things to note above:
	- The methods in category "stream in" can signal two exceptions (unless turned off with #shouldSignal:):
		ConnectionClosed and ConnectionTimedOut
	- We close the stream using #ensure:, that is to make sure it isn't left opened.
	- We use #on:do: to catch any signal. In this case we do not need to catch ConnectionClosed since #upToEnd does that for us intrinsically.

----------------
SocketStream (below called SS) is a reimplementation of 'Old'-SocketStream (below called OSS) - the class that originates from the original Comanche implementation but now is included in standard Squeak. SS has the same protocol as OSS and is meant to replace it. SS is faster, more flexible, is better documented and adds a few features:

1. #shouldSignal:, which decides if SS should signal low level Socket exceptions (true) or if it should swallow them like original OSS did. Default is true. The only reason I added this is for backwards compatibility - not signalling causes problems - see bug 4 below.

2. #nextAllInBuffer, #nextInBuffer:, #skip:, #receiveData:, #nextPutAllFlush: and #recentlyRead are new additions to the public protocol.


It also fixes various bugs:

1. #isDataAvailable could theoretically answer false, when there actually is some in the buffer in OSS. If #receiveDataIfAvailable reads the last byte then the following "socket dataAvailable" would answer false. So the last byte would be sitting in the inStream missed.

2. #upToAll: in OSS has several problems, for example - #positionOfSubCollection:ifAbsent: which was introduced answers one position too low. This was compensated in upToAll:, but only in the pushBack: call, not the actual result being returned which was cut short 1 byte. Amusingly this makes KomHttpServer not use "Keep-Alive" since the last $e in 'Alive' was cut short. :)

3. SS doesn't inherit from PositionableStream since that just breaks various inherited messages, like for example #skip:. OSS should IMHO be changed to inherit from Object - or of course, replaced in full with SS. :)

4. Since SocketStream by default signals closes and timeouts the SocketStreamTest now passes. The reason for SocketStream to fail is that while it does timeout on a low level (#SocketStream>>receiveData doesn't hang forever) - the callers of #receiveData sometimes loop - like in #next:, and thus eliminates the timeout. SS warns about some methods (in their method comments) not honouring timeouts if shouldSignal is false, I really don't know what they should do in that case:
	#next:, #upTo:, #upToAll: and #upToEnd (and #receiveData:)


The primary reason for the SS implementation is optimal performance. The main differences in implementation with the old OSS are:

1. SS uses two buffers directly (inBuffer and outBuffer) with pointers marking start and stop within the buffer. OSS instead uses two regular streams, a ReadStream and a WriteStream. Using internal buffers makes it possible to avoid copying and reallocation in various ways, it also makes SS be able to have specialized growing/buffer moving behaviour.

2. #upTo:, #upToAll: and #peekForAll: uses selectged String messages that in turn uses fast primitives for searching. OSS used other messages that fell back on byte per byte reading.

3. #receiveData in OSS creates a temporary buffer stream for each call!! During a long read operation, like say #upToAll: (which for example is used when uploading files using HTTP POST forms), this is devastating - especially since the default size is only 2000 bytes - and leads to a very high number of low level read operations on the Socket, typically 100 times more calls than with OSS. The buffer in OSS is held in an instvar (not recreated for each call), is larger from the start and above all - grows dynamically by doubling. OSS can also avoid a grow/reallocation by doing a "move down" if data has been read from the SS as it comes in and through that making room in the lower part of the inBuffer. The net result is that upToAll: for large files is about 10 times faster.

4. The implementation of upTo: and upToAll: tries to avoid doing unnecessary find operations in the buffer and is greedy by default, which means it favors reading more data - if available - before searching for the stop sequence. If we had #findString:startingAt:stoppingAt: this wouldn't have to be greedy and we wouldn't be needlessly scanning dead buffer area. VM hackers? Also, while you are at it - make it work for ByteArrays too. :)


SS can not be run unbuffered, since that seems unneeded. The option to autoFlush is still available, with it set to true SocketStream (just like OSS) will flush on its own on each nextPut:/nextPutAll:, otherwise flushing it will have to be done manually but is done on close.

The first performance tests shows that, as noted above, receiving large amounts of data using #upToAll: is greatly improved - factor of 10. Serving HTTP with small payloads seemed at first not be faster at all - but this is due to the high overhead of Socket connect/close and other things. Increasing payloads show a difference and especially with keep alive on - where the new SS roughly doubles the throughput!!!

!ConnectionRefused methodsFor: 'accessing' stamp: 'len 12/14/2002 11:58'!
host
	^ host! !

!ConnectionRefused methodsFor: 'accessing' stamp: 'len 12/14/2002 11:39'!
host: addressOrHostName port: portNumber
	host _ addressOrHostName.
	port _ portNumber! !

!ConnectionRefused methodsFor: 'accessing' stamp: 'len 12/14/2002 11:58'!
port
	^ port! !

!ConnectionRefused class methodsFor: 'instance creation' stamp: 'len 12/14/2002 11:39'!
host: addressOrHostName port: portNumber
	^ self new host: addressOrHostName port: portNumber! !

!NameLookupFailure methodsFor: 'accessing' stamp: 'jmv 3/13/2012 23:13'!
defaultAction
	"Backward compatibility"
	| response |
	response _ (PopUpMenu labels: 'Retry\Give Up' withNewLines)
			startUpWithCaption: self messageText.
	^ response = 2
		ifFalse: [self retry]! !

!NameLookupFailure methodsFor: 'accessing' stamp: 'len 12/14/2002 11:57'!
hostName
	^ hostName! !

!NameLookupFailure methodsFor: 'accessing' stamp: 'len 12/14/2002 11:57'!
hostName: aString
	hostName _ aString! !

!NameLookupFailure class methodsFor: 'instance creation' stamp: 'len 12/14/2002 11:57'!
hostName: aString
	^ self new hostName: aString! !

!ConnectionQueue methodsFor: 'public' stamp: 'nice 12/26/2009 01:23'!
connectionCount
	"Return an estimate of the number of currently queued connections. This is only an estimate since a new connection could be made, or an existing one aborted, at any moment."

	self pruneStaleConnections.
	^accessSema critical: [connections size]! !

!ConnectionQueue methodsFor: 'public' stamp: 'jm 3/9/98 14:34'!
destroy
	"Terminate the listener process and destroy all sockets in my possesion."

	process ifNotNil: [
		process terminate.
		process _ nil].
	socket ifNotNil: [
		socket destroy.
		socket _ nil].
	connections do: [:s | s destroy].
	connections _ OrderedCollection new.
! !

!ConnectionQueue methodsFor: 'public' stamp: 'nice 12/26/2009 01:24'!
getConnectionOrNil
	"Return a connected socket, or nil if no connection has been established."

	^accessSema critical: [
		| result |
		connections isEmpty
			ifTrue: [result := nil]
			ifFalse: [
				result := connections removeFirst.
				((result isValid) and: [result isConnected]) ifFalse: [  "stale connection"
					result destroy.
					result := nil]].
		result]! !

!ConnectionQueue methodsFor: 'public' stamp: 'nice 12/26/2009 01:25'!
getConnectionOrNilLenient
	"Return a connected socket, or nil if no connection has been established."

	^accessSema critical: [
		| result |
		connections isEmpty ifTrue: [
			result := nil
		] ifFalse: [
			result := connections removeFirst.
			(result isValid and: [result isConnected or: [result isOtherEndClosed]]) ifFalse: [
				"stale connection"
				result destroy.
				result := nil
			]
		].
		result
	].
! !

!ConnectionQueue methodsFor: 'private' stamp: 'jmv 10/8/2012 22:17'!
initPortNumber: anInteger queueLength: queueLength
	"Private!! Initialize the receiver to listen on the given port number. Up to queueLength connections will be queued."

	portNumber _ anInteger.
	maxQueueLength _ queueLength.
	connections _ OrderedCollection new.
	accessSema _ Semaphore forMutualExclusion.
	socket _ nil.
	process _ [self listenLoop] newProcess.
	process priority: Processor highIOPriority.
	process name: 'ConnectionQueue'.
	process resume! !

!ConnectionQueue methodsFor: 'public' stamp: 'ls 9/26/1999 15:34'!
isValid
	^process notNil! !

!ConnectionQueue methodsFor: 'private' stamp: 'jmv 6/1/2011 23:00'!
listenLoop
	"Private!! This loop is run in a separate process. It will establish up to maxQueueLength connections on the given port."
	"Details: When out of sockets or queue is full, retry more frequently, since a socket may become available, space may open in the queue, or a previously queued connection may be aborted by the client, making it available for a fresh connection."
	"Note: If the machine is disconnected from the network while the server is running, the currently waiting socket will go from 'isWaitingForConnection' to 'unconnected', and attempts to create new sockets will fail. When this happens, delete the broken socket and keep trying to create a socket in case the network connection is re-established. Connecting and disconnecting was tested under PPP on Mac system 8.1. It is not if this will work on other platforms."


	| newConnection |

	socket := Socket newTCP.
	"We'll accept four simultanous connections at the same time"
	socket listenOn: portNumber backlogSize: 4.
	"If the listener is not valid then the we cannot use the
	BSD style accept() mechanism."
	socket isValid ifFalse: [^self oldStyleListenLoop].
	[true] whileTrue: [
		socket isValid ifFalse: [
			"socket has stopped listening for some reason"
			socket destroy.
			(Delay forMilliseconds: 10) wait.
			^self listenLoop ].
		newConnection := socket 
			waitForAcceptFor: 10
			ifTimedOut: [ nil ].
		(newConnection notNil and: [newConnection isConnected]) ifTrue: [
			accessSema critical: [connections addLast: newConnection.].
			newConnection := nil.
			self changed: self].
		self pruneStaleConnections]. ! !

!ConnectionQueue methodsFor: 'private' stamp: 'ul 11/25/2010 21:20'!
oldStyleListenLoop
	"Private!! This loop is run in a separate process. It will establish up to maxQueueLength connections on the given port."
	"Details: When out of sockets or queue is full, retry more frequently, since a socket may become available, space may open in the queue, or a previously queued connection may be aborted by the client, making it available for a fresh connection."
	"Note: If the machine is disconnected from the network while the server is running, the currently waiting socket will go from 'isWaitingForConnection' to 'unconnected', and attempts to create new sockets will fail. When this happens, delete the broken socket and keep trying to create a socket in case the network connection is re-established. Connecting and disconnecting was tested under PPP on Mac system 8.1. It is not if this will work on other platforms."

	[true] whileTrue: [
		((socket == nil) and: [connections size < maxQueueLength]) ifTrue: [
			"try to create a new socket for listening"
			socket := Socket createIfFail: [nil]].

		socket == nil
			ifTrue: [(Delay forMilliseconds: 100) wait]
			ifFalse: [
				socket isUnconnected ifTrue: [socket listenOn: portNumber].
				socket 
					waitForConnectionFor: 10
					ifTimedOut: [
						socket isConnected
							ifTrue: [  "connection established"
								accessSema critical: [connections addLast: socket].
								socket := nil]
							ifFalse: [
								socket isWaitingForConnection
									ifFalse: [socket destroy. socket := nil]]]].  "broken socket; start over"
		self pruneStaleConnections].
! !

!ConnectionQueue methodsFor: 'private' stamp: 'nice 12/26/2009 01:25'!
pruneStaleConnections
	"Private!! The client may establish a connection and then disconnect while it is still in the connection queue. This method is called periodically to prune such sockets out of the connection queue and make room for fresh connections."

	accessSema critical: [
		| foundStaleConnection |
		foundStaleConnection := false.
		connections do: [:s |
			s isUnconnected ifTrue: [
				s destroy.
				foundStaleConnection := true]].
		foundStaleConnection ifTrue: [
			connections := connections select: [:s | s isValid]]].
! !

!ConnectionQueue class methodsFor: 'instance creation' stamp: 'jm 3/9/98 14:09'!
portNumber: anInteger queueLength: queueLength

	^ self new initPortNumber: anInteger queueLength: queueLength
! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'ls 9/5/1998 01:14'!
addressForName: aString
	^self addressForName: aString timeout: 60! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'jmv 4/17/2013 12:07'!
addressForName: hostName timeout: secs
	"Look up the given host name and return its address. Return nil if the address is not found in the given number of seconds."
	"NetNameResolver addressForName: 'create.ucsb.edu' timeout: 30"
	"NetNameResolver addressForName: '100000jobs.de' timeout: 30"
	"NetNameResolver addressForName: '1.7.6.4' timeout: 30"
	"NetNameResolver addressForName: '' timeout: 30 (This seems to return nil?)"

	| deadline result |
	self initializeNetwork.
	"check if this is a valid numeric host address (e.g. 1.2.3.4)"
	result _ self addressFromString: hostName.
	result ifNotNil: [^result].

	"Look up a host name, including ones that start with a digit (e.g. 100000jobs.de or squeak.org)"
	deadline _ Time localMillisecondClock + (secs * 1000).
	"Protect the execution of this block, as the ResolverSemaphore is used for both parts of the transaction."
	self resolverMutex
		critical: [
			(self waitForResolverReadyUntil: deadline)
				ifTrue: [
					self primStartLookupOfName: hostName.
					(self waitForCompletionUntil: deadline)
						ifTrue: [result _ self primNameLookupResult]
						ifFalse: [(NameLookupFailure hostName: hostName) signal: 'Could not resolve the server named: ', hostName]]
				ifFalse: [(NameLookupFailure hostName: hostName) signal: 'Could not resolve the server named: ', hostName]].
	^result! !

!NetNameResolver class methodsFor: 'address string utils' stamp: 'jmv 3/2/2010 11:22'!
addressFromString: addressString
	"Return the internet address represented by the given string. The string should contain four positive decimal integers delimited by periods, commas, or spaces, where each integer represents one address byte. Return nil if the string is not a host address in an acceptable format."
	"NetNameResolver addressFromString: '1.2.3.4'"
	"NetNameResolver addressFromString: '1,2,3,4'"
	"NetNameResolver addressFromString: '1 2 3 4'"

	| newAddr s byte delimiter |
	newAddr _ ByteArray new: 4.
	s _ ReadStream on: addressString.
	s skipSeparators.
	1 to: 4 do: [:i |
		byte _ self readDecimalByteFrom: s.
		byte ifNil: [^ nil].
		newAddr at: i put: byte.
		i < 4 ifTrue: [
			delimiter _ s next.
			((delimiter = $.) or: [(delimiter = $,) or: [delimiter = $ ]])
				ifFalse: [^ nil]]].
	^ newAddr
! !

!NetNameResolver class methodsFor: 'class initialization' stamp: 'jm 9/17/97 16:18'!
initialize
	"NetNameResolver initialize"
	"Note: On the Mac, the name resolver is asynchronous (i.e., Squeak can do other things while it is working), but can only handle one request at a time. On other platforms, such as Unix, the resolver is synchronous; a call to, say, the name lookup primitive will block all Squeak processes until it returns."

	"Resolver Status Values"
	ResolverUninitialized _ 0.	"network is not initialized"
	ResolverReady _ 1.			"resolver idle, last request succeeded"
	ResolverBusy _ 2.			"lookup in progress"
	ResolverError _ 3.			"resolver idle, last request failed"

	DefaultHostName _ ''.
! !

!NetNameResolver class methodsFor: 'network initialization' stamp: 'mir 2/22/2002 15:03'!
initializeNetwork
	"Initialize the network drivers and record the semaphore to be used by the resolver. Do nothing if the network is already initialized. Evaluate the given block if network initialization fails."
	"NetNameResolver initializeNetwork"

	| semaIndex |
	self resolverStatus = ResolverUninitialized
		ifFalse: [^HaveNetwork _ true].  "network is already initialized"

	HaveNetwork _ false.	"in case abort"
	ResolverSemaphore _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: ResolverSemaphore.

	"result is nil if network initialization failed, self if it succeeds"
	(self primInitializeNetwork: semaIndex)
		ifNil: [NoNetworkError signal: 'failed network initialization']
		ifNotNil: [HaveNetwork _ true].
! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'jm 9/15/97 16:52'!
localAddressString
	"Return a string representing the local host address as four decimal bytes delimited with decimal points."
	"NetNameResolver localAddressString"

	^ NetNameResolver stringFromAddress: NetNameResolver localHostAddress
! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'mir 2/22/2002 15:50'!
localHostAddress
	"Return the local address of this host."
	"NetNameResolver localHostAddress"

	self initializeNetwork.
	^ self primLocalAddress
! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'mir 2/22/2002 15:12'!
localHostName
	"Return the local name of this host."
	"NetNameResolver localHostName"

	| hostName |
	hostName _ NetNameResolver
		nameForAddress: self localHostAddress
		timeout: 5.
	^hostName
		ifNil: [self localAddressString]
		ifNotNil: [hostName]! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'jmv 4/17/2013 12:07'!
nameForAddress: hostAddress timeout: secs
	"Look up the given host address and return its name. Return nil if the lookup fails or is not completed in the given number of seconds. Depends on the given host address being known to the gateway, which may not be the case for dynamically allocated addresses."
	"NetNameResolver
		nameForAddress: (NetNameResolver addressFromString: '128.111.92.2')
		timeout: 30"

	| deadline |
	self initializeNetwork.
	deadline := Time localMillisecondClock + (secs * 1000).
	"Protect the execution of this block, as the ResolverSemaphore is used for both parts of the transaction."
	^self resolverMutex
		critical: [
			(self waitForResolverReadyUntil: deadline)
				ifTrue: [
					self primStartLookupOfAddress: hostAddress.
					(self waitForCompletionUntil: deadline)
						ifTrue: [self primAddressLookupResult]
						ifFalse: [nil]]
				ifFalse: [nil]].! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primAbortLookup
	"Abort the current lookup operation, freeing the name resolver for the next query."

	<primitive: 'primitiveResolverAbortLookup' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primAddressLookupResult
	"Return the host name found by the last host address lookup. Returns nil if the last lookup was unsuccessful."

	<primitive: 'primitiveResolverAddressLookupResult' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'network initialization' stamp: 'ar 2/2/2001 15:09'!
primInitializeNetwork: resolverSemaIndex
	"Initialize the network drivers on platforms that need it, such as the Macintosh, and return nil if network initialization failed or the receiver if it succeeds. Since mobile computers may not always be connected to a network, this method should NOT be called automatically at startup time; rather, it should be called when first starting a networking application. It is a noop if the network driver has already been initialized. If non-zero, resolverSemaIndex is the index of a VM semaphore to be associated with the network name resolver. This semaphore will be signalled when the resolver status changes, such as when a name lookup query is completed."
	"Note: some platforms (e.g., Mac) only allow only one name lookup query at a time, so a manager process should be used to serialize resolver lookup requests."

	<primitive: 'primitiveInitializeNetwork' module: 'SocketPlugin'>
	^ nil  "return nil if primitive fails"
! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primLocalAddress
	"Return the local address of this host."

	<primitive: 'primitiveResolverLocalAddress' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primNameLookupResult
	"Return the host address found by the last host name lookup. Returns nil if the last lookup was unsuccessful."

	<primitive: 'primitiveResolverNameLookupResult' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primNameResolverError
	"Return an integer reflecting the error status of the last network name resolver request. Zero means no error."

	<primitive: 'primitiveResolverError' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primNameResolverStatus
	"Return an integer reflecting the status of the network name resolver. For a list of possible values, see the comment in the 'initialize' method of this class."

	<primitive: 'primitiveResolverStatus' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primStartLookupOfAddress: hostAddr
	"Look up the given host address in the Domain Name Server to find its name. This call is asynchronous. To get the results, wait for it to complete or time out and then use primAddressLookupResult."

	<primitive: 'primitiveResolverStartAddressLookup' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primStartLookupOfName: hostName
	"Look up the given host name in the Domain Name Server to find its address. This call is asynchronous. To get the results, wait for it to complete or time out and then use primNameLookupResult."

	<primitive: 'primitiveResolverStartNameLookup' module: 'SocketPlugin'>
	self primitiveFailed
! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'jm 9/17/97 16:26'!
promptUserForHostAddress
	"Ask the user for a host name and return its address."
	"NetNameResolver promptUserForHostAddress"

	^ NetNameResolver promptUserForHostAddressDefault: ''
! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'jmv 9/24/2012 19:47'!
promptUserForHostAddressDefault: defaultName
	"Ask the user for a host name and return its address. If the default name is the empty string, use the last host name as the default."
	"NetNameResolver promptUserForHostAddressDefault: ''"

	| default hostName serverAddr |
	defaultName isEmpty
		ifTrue: [default _ DefaultHostName]
		ifFalse: [default _ defaultName].
	hostName _ FillInTheBlankMorph
		request: 'Host name or address?'
		initialAnswer: default.
	hostName isEmpty ifTrue: [^ 0].
	serverAddr _ NetNameResolver addressForName: hostName timeout: 15.
	hostName size > 0 ifTrue: [DefaultHostName _ hostName].
	^ serverAddr! !

!NetNameResolver class methodsFor: 'private' stamp: 'JMM 5/3/2000 13:57'!
readDecimalByteFrom: aStream
	"Read a positive, decimal integer from the given stream. Stop when a non-digit or end-of-stream is encountered. Return nil if stream is not positioned at a decimal digit or if the integer value read exceeds 255.
JMM - 000503 fixed didn't work correctly"

	| digitSeen value digit |
	digitSeen _ false.
	value _ 0.
	[aStream atEnd] whileFalse: 
		[digit _ aStream next digitValue.
		(digit < 0 or: [digit > 9]) ifTrue: [
			aStream skip: -1.
			(digitSeen not or: [value > 255]) ifTrue: [^ nil].
			^ value].
		digitSeen _ true.
		value _ (value * 10) + digit].
	(digitSeen not or: [value > 255]) ifTrue: [^ nil].
	^ value
! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'JMM 5/3/2000 11:25'!
resolverError
	^self primNameResolverError
! !

!NetNameResolver class methodsFor: 'private' stamp: 'mir 6/18/2001 21:05'!
resolverMutex
	ResolverMutex ifNil: [ResolverMutex _ Semaphore forMutualExclusion].
	^ResolverMutex! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'JMM 5/3/2000 11:25'!
resolverStatus
	^self primNameResolverStatus
! !

!NetNameResolver class methodsFor: 'address string utils' stamp: 'jmv 2/23/2013 22:08'!
stringFromAddress: addr
	"Return a string representing the given host address as four decimal bytes delimited with decimal points."
	"NetNameResolver stringFromAddress: NetNameResolver localHostAddress"

	| s |
	s _ WriteStream on: String new.
	1 to: 3 do: [ :i | (addr at: i) printOn: s. s nextPut: $. ].
	(addr at: 4) printOn: s.
	^ s contents! !

!NetNameResolver class methodsFor: 'private' stamp: 'jmv 4/17/2013 12:07'!
waitForCompletionUntil: deadline
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is ready, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status _ self resolverStatus.
	[(status = ResolverBusy) and:
	 [Time localMillisecondClock < deadline]]
		whileTrue: [
			"wait for resolver to be available"
			ResolverSemaphore waitTimeoutMSecs: (deadline - Time localMillisecondClock).
			status _ self resolverStatus].

	status = ResolverReady
		ifTrue: [^ true]
		ifFalse: [
			status = ResolverBusy ifTrue: [self primAbortLookup].
			^ false].
! !

!NetNameResolver class methodsFor: 'private' stamp: 'jmv 4/17/2013 12:07'!
waitForResolverReadyUntil: deadline
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is not busy, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status _ self resolverStatus.
	status = ResolverUninitialized ifTrue: [^ false].

	[(status = ResolverBusy) and:
	 [Time localMillisecondClock < deadline]]
		whileTrue: [
			"wait for resolver to be available"
			ResolverSemaphore waitTimeoutMSecs: (deadline - Time localMillisecondClock).
			status _ self resolverStatus].

	^ status ~= ResolverBusy
! !

!Socket methodsFor: 'connection open/close' stamp: 'jmv 6/25/2017 21:35:04'!
accept
	"Accept a connection from the receiver socket.
	Return a new socket that is connected to the client"
	^self class acceptFrom: self! !

!Socket methodsFor: 'initialize-destroy' stamp: 'ul 4/20/2011 01:56'!
acceptFrom: aSocket
	"Initialize a new socket handle from an accept call"
	| semaIndex readSemaIndex writeSemaIndex |

	semaphore := Semaphore new.
	readSemaphore := Semaphore new.
	writeSemaphore := Semaphore new.
	semaIndex := Smalltalk registerExternalObject: semaphore.
	readSemaIndex := Smalltalk registerExternalObject: readSemaphore.
	writeSemaIndex := Smalltalk registerExternalObject: writeSemaphore.
	socketHandle := self primAcceptFrom: aSocket socketHandle
						receiveBufferSize: 8000
						sendBufSize: 8000
						semaIndex: semaIndex
						readSemaIndex: readSemaIndex
						writeSemaIndex: writeSemaIndex.
	socketHandle
		ifNotNil: [ self register ]
		ifNil: [  "socket creation failed"
			Smalltalk unregisterExternalObject: semaphore.
			Smalltalk unregisterExternalObject: readSemaphore.
			Smalltalk unregisterExternalObject: writeSemaphore.
			readSemaphore := writeSemaphore := semaphore := nil ]
! !

!Socket methodsFor: 'accessing' stamp: 'ar 4/30/1999 04:25'!
address
	"Shortcut"
	^self localAddress! !

!Socket methodsFor: 'connection open/close' stamp: 'jm 9/11/97 20:29'!
close
	"Close this connection gracefully. For TCP, this sends a close request, but the stream remains open until the other side also closes it."

	self primSocketCloseConnection: socketHandle.  "close this end"
! !

!Socket methodsFor: 'connection open/close' stamp: 'jm 11/4/97 07:15'!
closeAndDestroy
	"First, try to close this connection gracefully. If the close attempt fails or times out, abort the connection. In either case, destroy the socket. Do nothing if the socket has already been destroyed (i.e., if its socketHandle is nil)."

	self closeAndDestroy: 20.

! !

!Socket methodsFor: 'connection open/close' stamp: 'marcus.denker 9/14/2008 21:20'!
closeAndDestroy: timeoutSeconds
	"First, try to close this connection gracefully. If the close attempt fails or times out, abort the connection. In either case, destroy the socket. Do nothing if the socket has already been destroyed (i.e., if its socketHandle is nil)."

	socketHandle ifNotNil: [
			self isConnected ifTrue: [
				self close.  "close this end"
				(self waitForDisconnectionFor: timeoutSeconds) ifFalse: [
						"The other end didn't close so we just abort the connection"
						self primSocketAbortConnection: socketHandle]].
			self destroy].
! !

!Socket methodsFor: 'connection open/close' stamp: 'mir 5/9/2003 18:13'!
connectNonBlockingTo: hostAddress port: port
	"Initiate a connection to the given port at the given host address. This operation will return immediately; follow it with waitForConnectionUntil: to wait until the connection is established."

	| status |
	self initializeNetwork.
	status := self primSocketConnectionStatus: socketHandle.
	(status == Unconnected)
		ifFalse: [InvalidSocketStatusException signal: 'Socket status must Unconnected before opening a new connection'].

	self primSocket: socketHandle connectTo: hostAddress port: port.
! !

!Socket methodsFor: 'connection open/close' stamp: 'mir 5/15/2003 18:29'!
connectTo: hostAddress port: port
	"Initiate a connection to the given port at the given host address.
	Waits until the connection is established or time outs."

	self connectTo: hostAddress port: port waitForConnectionFor: Socket standardTimeout! !

!Socket methodsFor: 'connection open/close' stamp: 'mu 8/14/2003 15:15'!
connectTo: hostAddress port: port waitForConnectionFor: timeout 
	"Initiate a connection to the given port at the given host 
	address. Waits until the connection is established or time outs."
	self connectNonBlockingTo: hostAddress port: port.
	self
		waitForConnectionFor: timeout
		ifTimedOut: [ConnectionTimedOut signal: 'Cannot connect to '
					, (NetNameResolver stringFromAddress: hostAddress) , ':' , port asString]! !

!Socket methodsFor: 'connection open/close' stamp: 'mir 5/8/2003 16:03'!
connectToHostNamed: hostName port: portNumber
	| serverIP |
	serverIP _ NetNameResolver addressForName: hostName timeout: 20.
	^self connectTo: serverIP port: portNumber
! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:32'!
dataAvailable
	"Return true if this socket has unread received data."

	socketHandle ifNil: [^ false].
	^ self primSocketReceiveDataAvailable: socketHandle
! !

!Socket methodsFor: 'initialize-destroy' stamp: 'jmv 3/2/2010 11:28'!
destroy
	"Destroy this socket. Its connection, if any, is aborted and its resources are freed. Do nothing if the socket has already been destroyed (i.e., if its socketHandle is nil)."

	socketHandle ifNotNil: [
		self isValid ifTrue: [self primSocketDestroy: socketHandle].
		Smalltalk unregisterExternalObject: semaphore.
		Smalltalk unregisterExternalObject: readSemaphore.
		Smalltalk unregisterExternalObject: writeSemaphore.
		socketHandle _ nil.
		readSemaphore _ writeSemaphore _ semaphore _ nil.
		self unregister]! !

!Socket methodsFor: 'receiving' stamp: 'gk 12/14/2005 10:02'!
discardReceivedData
	"Discard any data received up until now, and return the number of bytes discarded."

	| buf totalBytesDiscarded |
	buf := String new: 10000.
	totalBytesDiscarded := 0.
	[self isConnected] whileTrue: [
		totalBytesDiscarded :=
			totalBytesDiscarded + (self receiveDataInto: buf)].
	^ totalBytesDiscarded
! !

!Socket methodsFor: 'connection open/close' stamp: 'jm 3/10/98 11:56'!
disconnect
	"Break this connection, no matter what state it is in. Data that has been sent but not received will be lost."

	self primSocketAbortConnection: socketHandle.
! !

!Socket methodsFor: 'finalization' stamp: 'JMM 5/22/2000 22:52'!
finalize
	self primSocketDestroyGently: socketHandle.
	Smalltalk unregisterExternalObject: semaphore.
	Smalltalk unregisterExternalObject: readSemaphore.
	Smalltalk unregisterExternalObject: writeSemaphore.
! !

!Socket methodsFor: 'other' stamp: 'mir 2/22/2002 16:25'!
getOption: aName 
	"Get options on this socket, see Unix man pages for values for 
	sockets, IP, TCP, UDP. IE SO_KEEPALIVE
	returns an array, element one is an status number (0 ok, -1 read only option)
	element two is the resulting of the requested option"

	(socketHandle == nil or: [self isValid not])
		ifTrue: [InvalidSocketStatusException signal: 'Socket status must valid before getting an option'].
	^self primSocket: socketHandle getOption: aName

"| foo options |
Socket initializeNetwork.
foo _ Socket newTCP.
foo connectTo: (NetNameResolver addressFromString: '192.168.1.1') port: 80.
foo waitForConnectionUntil: (Socket standardDeadline).

options _ {
'SO_DEBUG'. 'SO_REUSEADDR'. 'SO_REUSEPORT'. 'SO_DONTROUTE'.
'SO_BROADCAST'. 'SO_SNDBUF'. 'SO_RCVBUF'. 'SO_KEEPALIVE'.
'SO_OOBINLINE'. 'SO_PRIORITY'. 'SO_LINGER'. 'SO_RCVLOWAT'.
'SO_SNDLOWAT'. 'IP_TTL'. 'IP_HDRINCL'. 'IP_RCVOPTS'.
'IP_RCVDSTADDR'. 'IP_MULTICAST_IF'. 'IP_MULTICAST_TTL'.
'IP_MULTICAST_LOOP'. 'UDP_CHECKSUM'. 'TCP_MAXSEG'.
'TCP_NODELAY'. 'TCP_ABORT_THRESHOLD'. 'TCP_CONN_NOTIFY_THRESHOLD'. 
'TCP_CONN_ABORT_THRESHOLD'. 'TCP_NOTIFY_THRESHOLD'.
'TCP_URGENT_PTR_TYPE'}.

1 to: options size do: [:i | | fum |
	fum _foo getOption: (options at: i).
	Transcript show: (options at: i),fum printString;cr].

foo _ Socket newUDP.
foo setPeer: (NetNameResolver addressFromString: '192.168.1.9') port: 7.
foo waitForConnectionUntil: (Socket standardDeadline).

1 to: options size do: [:i | | fum |
	fum _foo getOption: (options at: i).
	Transcript show: (options at: i),fum printString;cr].
"! !

!Socket methodsFor: 'initialize-destroy' stamp: 'ul 4/20/2011 01:56'!
initialize: socketType
	"Initialize a new socket handle. If socket creation fails, socketHandle will be set to nil."
	| semaIndex readSemaIndex writeSemaIndex |

	semaphore := Semaphore new.
	readSemaphore := Semaphore new.
	writeSemaphore := Semaphore new.
	semaIndex := Smalltalk registerExternalObject: semaphore.
	readSemaIndex := Smalltalk registerExternalObject: readSemaphore.
	writeSemaIndex := Smalltalk registerExternalObject: writeSemaphore.
	socketHandle :=
		self primSocketCreateNetwork: 0
			type: socketType
			receiveBufferSize: 8000
			sendBufSize: 8000
			semaIndex: semaIndex
			readSemaIndex: readSemaIndex
			writeSemaIndex: writeSemaIndex.

	socketHandle 
		ifNotNil: [ self register ]
		ifNil: [  "socket creation failed"
			Smalltalk unregisterExternalObject: semaphore.
			Smalltalk unregisterExternalObject: readSemaphore.
			Smalltalk unregisterExternalObject: writeSemaphore.
			readSemaphore := writeSemaphore := semaphore := nil ]
! !

!Socket methodsFor: 'initialize-destroy' stamp: 'mir 2/22/2002 15:48'!
initializeNetwork
	self class initializeNetwork! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:32'!
isConnected
	"Return true if this socket is connected."

	socketHandle ifNil: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == Connected
! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:32'!
isOtherEndClosed
	"Return true if this socket had the other end closed."

	socketHandle ifNil: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == OtherEndClosed
! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:32'!
isThisEndClosed
	"Return true if this socket had the this end closed."

	socketHandle ifNil: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == ThisEndClosed
! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:32'!
isUnconnected
	"Return true if this socket's state is Unconnected."

	socketHandle ifNil: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == Unconnected
! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:32'!
isUnconnectedOrInvalid
	"Return true if this socket is completely disconnected or is invalid."

	| status |
	socketHandle ifNil: [^ true].
	status _ self primSocketConnectionStatus: socketHandle.
	^ (status = Unconnected) | (status = InvalidSocket)
! !

!Socket methodsFor: 'queries' stamp: 'jm 11/4/97 07:15'!
isValid
	"Return true if this socket contains a valid, non-nil socket handle."

	| status |
	socketHandle ifNil: [^ false].
	status _ self primSocketConnectionStatus: socketHandle.
	^ status ~= InvalidSocket
! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:33'!
isWaitingForConnection
	"Return true if this socket is waiting for a connection."

	socketHandle ifNil: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == WaitingForConnection
! !

!Socket methodsFor: 'connection open/close' stamp: 'mir 2/22/2002 16:25'!
listenOn: port
	"Listen for a connection on the given port. This operation will return immediately; follow it with waitForConnectionUntil: to wait until a connection is established."

	| status |
	status := self primSocketConnectionStatus: socketHandle.
	(status == Unconnected)
		ifFalse: [InvalidSocketStatusException signal: 'Socket status must Unconnected before listening for a new connection'].

	self primSocket: socketHandle listenOn: port.
! !

!Socket methodsFor: 'connection open/close' stamp: 'mir 2/22/2002 16:25'!
listenOn: portNumber backlogSize: backlog
	"Listen for a connection on the given port.
	If this method succeeds, #accept may be used to establish a new connection"
	| status |
	status := self primSocketConnectionStatus: socketHandle.
	(status == Unconnected)
		ifFalse: [InvalidSocketStatusException signal: 'Socket status must Unconnected before listening for a new connection'].
	self primSocket: socketHandle listenOn: portNumber backlogSize: backlog.
! !

!Socket methodsFor: 'connection open/close' stamp: 'ikp 9/1/2003 20:32'!
listenOn: portNumber backlogSize: backlog interface: ifAddr
	"Listen for a connection on the given port.
	If this method succeeds, #accept may be used to establish a new connection"
	| status |
	status := self primSocketConnectionStatus: socketHandle.
	(status == Unconnected)
		ifFalse: [InvalidSocketStatusException signal: 'Socket status must Unconnected before listening for a new connection'].
	self primSocket: socketHandle listenOn: portNumber backlogSize: backlog interface: ifAddr.
! !

!Socket methodsFor: 'accessing' stamp: 'ul 11/25/2010 21:21'!
localAddress

	self isWaitingForConnection ifFalse: [
		self
			waitForConnectionFor: Socket standardTimeout
			ifTimedOut: [ ^ByteArray new: 4 ] ].
	^self primSocketLocalAddress: socketHandle! !

!Socket methodsFor: 'accessing' stamp: 'ul 11/25/2010 21:21'!
localPort

	self isWaitingForConnection ifFalse: [
		self
			waitForConnectionFor: Socket standardTimeout
			ifTimedOut: [ ^0] ].
	^ self primSocketLocalPort: socketHandle! !

!Socket methodsFor: 'accessing' stamp: 'jm 3/13/98 12:11'!
peerName
	"Return the name of the host I'm connected to, or nil if its name isn't known to the domain name server or the request times out."
	"Note: Slow. Calls the domain name server, taking up to 20 seconds to time out. Even when sucessful, delays of up to 13 seconds have been observed during periods of high network load." 

	^ NetNameResolver
		nameForAddress: self remoteAddress
		timeout: 20
! !

!Socket methodsFor: 'accessing' stamp: 'ar 4/30/1999 04:25'!
port
	"Shortcut"
	^self localPort! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primAcceptFrom: aHandle receiveBufferSize: rcvBufSize sendBufSize: sndBufSize semaIndex: semaIndex
	"Create and return a new socket handle based on accepting the connection from the given listening socket"
	<primitive: 'primitiveSocketAccept' module: 'SocketPlugin'>
	^self primitiveFailed! !

!Socket methodsFor: 'primitives' stamp: 'ul 4/20/2011 01:55'!
primAcceptFrom: aHandle receiveBufferSize: rcvBufSize sendBufSize: sndBufSize semaIndex: semaIndex readSemaIndex: aReadSema writeSemaIndex: aWriteSema
	"Create and return a new socket handle based on accepting the connection from the given listening socket"
	
	<primitive: 'primitiveSocketAccept3Semaphores' module: 'SocketPlugin'>
	self primitiveFailed! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocket: socketID connectTo: hostAddress port: port
	"Attempt to establish a connection to the given port of the given host. This is an asynchronous call; query the socket status to discover if and when the connection is actually completed."

	<primitive: 'primitiveSocketConnectToPort' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'JMM 5/25/2000 21:48'!
primSocket: socketID getOption: aString 
	"Get some option information on this socket. Refer to the UNIX 
	man pages for valid SO, TCP, IP, UDP options. In case of doubt
	refer to the source code.
	TCP_NODELAY, SO_KEEPALIVE are valid options for example
	returns an array containing the error code and the option value"

	<primitive: 'primitiveSocketGetOptions' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocket: socketID listenOn: port
	"Listen for a connection on the given port. This is an asynchronous call; query the socket status to discover if and when the connection is actually completed."

	<primitive: 'primitiveSocketListenWithOrWithoutBacklog' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocket: aHandle listenOn: portNumber backlogSize: backlog
	"Primitive. Set up the socket to listen on the given port.
	Will be used in conjunction with #accept only."
	<primitive: 'primitiveSocketListenWithOrWithoutBacklog' module: 'SocketPlugin'>
	self destroy. "Accept not supported so clean up"! !

!Socket methodsFor: 'primitives' stamp: 'ikp 9/1/2003 20:33'!
primSocket: aHandle listenOn: portNumber backlogSize: backlog interface: ifAddr
	"Primitive. Set up the socket to listen on the given port.
	Will be used in conjunction with #accept only."
	<primitive: 'primitiveSocketListenOnPortBacklogInterface' module: 'SocketPlugin'>
	self destroy. "Accept not supported so clean up"! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocket: socketID receiveDataInto: aStringOrByteArray startingAt: startIndex count: count
	"Receive data from the given socket into the given array starting at the given index. Return the number of bytes read or zero if no data is available."

	<primitive: 'primitiveSocketReceiveDataBufCount' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'JMM 5/24/2000 17:19'!
primSocket: socketID receiveUDPDataInto: aStringOrByteArray startingAt: startIndex count: count
	"Receive data from the given socket into the given array starting at the given index. 
	Return an Array containing the amount read, the host address byte array, the host port, and the more flag"

	<primitive: 'primitiveSocketReceiveUDPDataBufCount' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocket: socketID sendData: aStringOrByteArray startIndex: startIndex count: count
	"Send data to the remote host through the given socket starting with the given byte index of the given byte array. The data sent is 'pushed' immediately. Return the number of bytes of data actually sent; any remaining data should be re-submitted for sending after the current send operation has completed."
	"Note: In general, it many take several sendData calls to transmit a large data array since the data is sent in send-buffer-sized chunks. The size of the send buffer is determined when the socket is created."

	<primitive: 'primitiveSocketSendDataBufCount' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'JMM 5/25/2000 00:08'!
primSocket: socketID sendUDPData: aStringOrByteArray toHost: hostAddress  port: portNumber startIndex: startIndex count: count
	"Send data to the remote host through the given socket starting with the given byte index of the given byte array. The data sent is 'pushed' immediately. Return the number of bytes of data actually sent; any remaining data should be re-submitted for sending after the current send operation has completed."
	"Note: In general, it many take several sendData calls to transmit a large data array since the data is sent in send-buffer-sized chunks. The size of the send buffer is determined when the socket is created."

	<primitive:  'primitiveSocketSendUDPDataBufCount' module: 'SocketPlugin'>
	self primitiveFailed

! !

!Socket methodsFor: 'primitives' stamp: 'ar 7/18/2000 11:42'!
primSocket: socketID setOption: aString value: aStringValue
	"Set some option information on this socket. Refer to the UNIX 
	man pages for valid SO, TCP, IP, UDP options. In case of doubt
	refer to the source code.
	TCP_NODELAY, SO_KEEPALIVE are valid options for example
	returns an array containing the error code and the negotiated value"

	<primitive: 'primitiveSocketSetOptions' module: 'SocketPlugin'>
	^nil! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocket: socketID setPort: port
	"Set the local port associated with a UDP socket.
	Note: this primitive is overloaded.  The primitive will not fail on a TCP socket, but
	the effects will not be what was desired.  Best solution would be to split Socket into
	two subclasses, TCPSocket and UDPSocket."

	<primitive: 'primitiveSocketListenWithOrWithoutBacklog' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketAbortConnection: socketID
	"Terminate the connection on the given port immediately without going through the normal close sequence. This is an asynchronous call; query the socket status to discover if and when the connection is actually terminated."

	<primitive: 'primitiveSocketAbortConnection' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketCloseConnection: socketID
	"Close the connection on the given port. The remote end is informed that this end has closed and will do no further sends. This is an asynchronous call; query the socket status to discover if and when the connection is actually closed."

	<primitive: 'primitiveSocketCloseConnection' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketConnectionStatus: socketID
	"Return an integer reflecting the connection status of this socket. For a list of possible values, see the comment in the 'initialize' method of this class. If the primitive fails, return a status indicating that the socket handle is no longer valid, perhaps because the Squeak image was saved and restored since the socket was created. (Sockets do not survive snapshots.)"

	<primitive: 'primitiveSocketConnectionStatus' module: 'SocketPlugin'>
	^ InvalidSocket
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketCreateNetwork: netType type: socketType receiveBufferSize: rcvBufSize sendBufSize: sendBufSize semaIndex: semaIndex
	"Return a new socket handle for a socket of the given type and buffer sizes. Return nil if socket creation fails.
	The netType parameter is platform dependent and can be used to encode both the protocol type (IP, Xerox XNS, etc.) and/or the physical network interface to use if this host is connected to multiple networks. A zero netType means to use IP protocols and the primary (or only) network interface.
	The socketType parameter specifies:
		0	reliable stream socket (TCP if the protocol is IP)
		1	unreliable datagram socket (UDP if the protocol is IP)
	The buffer size parameters allow performance to be tuned to the application. For example, a larger receive buffer should be used when the application expects to be receiving large amounts of data, especially from a host that is far away. These values are considered requests only; the underlying implementation will ensure that the buffer sizes actually used are within allowable bounds. Note that memory may be limited, so an application that keeps many sockets open should use smaller buffer sizes. Note the macintosh implementation ignores this buffer size. Also see setOption to get/set socket buffer sizes which allows you to set/get the current buffer sizes for reading and writing.
 	If semaIndex is > 0, it is taken to be the index of a Semaphore in the external objects array to be associated with this socket. This semaphore will be signalled when the socket status changes, such as when data arrives or a send completes. All processes waiting on the semaphore will be awoken for each such event; each process must then query the socket state to figure out if the conditions they are waiting for have been met. For example, a process waiting to send some data can see if the last send has completed."

	<primitive: 'primitiveSocketCreate' module: 'SocketPlugin'>
	^ nil  "socket creation failed"
! !

!Socket methodsFor: 'primitives' stamp: 'ul 4/20/2011 01:56'!
primSocketCreateNetwork: netType type: socketType receiveBufferSize: rcvBufSize sendBufSize: sendBufSize semaIndex: semaIndex readSemaIndex: aReadSema writeSemaIndex: aWriteSema
	"See comment in primSocketCreateNetwork: with one semaIndex. However you should know that some implementations ignore the buffer size and this interface supports three semaphores,  one for open/close/listen and the other two for reading and writing"

	<primitive: 'primitiveSocketCreate3Semaphores' module: 'SocketPlugin'>
	self primitiveFailed! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketDestroy: socketID
	"Release the resources associated with this socket. If a connection is open, it is aborted."

	<primitive: 'primitiveSocketDestroy' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketDestroyGently: socketID
	"Release the resources associated with this socket. If a connection is open, it is aborted.
	Do not fail if the receiver is already closed."

	<primitive: 'primitiveSocketDestroy' module: 'SocketPlugin'>
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketError: socketID
	"Return an integer encoding the most recent error on this socket. Zero means no error."

	<primitive: 'primitiveSocketError' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketLocalAddress: socketID
	"Return the local host address for this socket."

	<primitive: 'primitiveSocketLocalAddress' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketLocalPort: socketID
	"Return the local port for this socket, or zero if no port has yet been assigned."

	<primitive: 'primitiveSocketLocalPort' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'mtf 3/14/2011 20:13'!
primSocketReceiveDataAvailable: socketID
	"Return true if data may be available for reading from the current socket."

	<primitive: 'primitiveSocketReceiveDataAvailable' module: 'SocketPlugin'>
	self primitiveFailed! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketRemoteAddress: socketID
	"Return the remote host address for this socket, or zero if no connection has been made."

	<primitive: 'primitiveSocketRemoteAddress' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketRemotePort: socketID
	"Return the remote port for this socket, or zero if no connection has been made."

	<primitive: 'primitiveSocketRemotePort' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSocketSendDone: socketID
	"Return true if there is no send in progress on the current socket."

	<primitive: 'primitiveSocketSendDone' module: 'SocketPlugin'>
	self primitiveFailed
! !

!Socket methodsFor: 'primitives' stamp: 'mtf 3/14/2011 19:59'!
primitiveFailed: selector
	SocketPrimitiveFailed signal: selector asString, ' failed'! !

!Socket methodsFor: 'printing' stamp: 'jm 11/23/1998 11:57'!
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: '[', self statusString, ']'.
! !

!Socket methodsFor: 'accessing' stamp: 'ul 4/20/2011 01:54'!
readSemaphore
	
	^readSemaphore! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 13:52'!
receiveAvailableData
	"Receive all available data (if any). Do not wait."
 
	| buffer bytesRead |
	buffer _ String new: 2000.
	bytesRead _ self receiveAvailableDataInto: buffer.
	^buffer copyFrom: 1 to: bytesRead! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 13:52'!
receiveAvailableDataInto: buffer
	"Receive all available data into the given buffer and return the number of bytes received.
	Note the given buffer may be only partially filled by the received data.
	Do not wait for data."

	^self receiveAvailableDataInto: buffer startingAt: 1! !

!Socket methodsFor: 'receiving' stamp: 'mu 8/9/2003 18:04'!
receiveAvailableDataInto: buffer startingAt: startIndex
	"Receive all available data into the given buffer and return the number of bytes received.
	Note the given buffer may be only partially filled by the received data.
	Do not wait for data."

	| bufferPos bytesRead |
	bufferPos := startIndex.
	[self dataAvailable
		and: [bufferPos-1 < buffer size]] 
		whileTrue: [
			bytesRead := self receiveSomeDataInto: buffer startingAt: bufferPos.
			bufferPos := bufferPos + bytesRead].
	^bufferPos - startIndex! !

!Socket methodsFor: 'receiving' stamp: 'yo 10/10/2005 18:47'!
receiveAvailableDataIntoBuffer: buffer
	"Receive all available data (if any). Do not wait."
 
	| bytesRead |
	bytesRead := self receiveAvailableDataInto: buffer.
	^buffer copyFrom: 1 to: bytesRead! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 16:05'!
receiveData
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once.
	Either returns data or signals a time out or connection close."

	| buffer bytesRead |
	buffer _ String new: 2000.
	bytesRead _ self receiveDataInto: buffer.
	^buffer copyFrom: 1 to: bytesRead! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 16:05'!
receiveDataInto: aStringOrByteArray
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once.
	Either returns data or signals a time out or connection close."

	^self receiveDataInto: aStringOrByteArray startingAt: 1! !

!Socket methodsFor: 'datagrams' stamp: 'ul 4/20/2011 01:55'!
receiveDataInto: aStringOrByteArray fromHost: hostAddress port: portNumber
	| datagram |
	"Receive a UDP packet from the given hostAddress/portNumber, storing the data in the given buffer, and return the number of bytes received. Note the given buffer may be only partially filled by the received data."

	[
		datagram := self receiveUDPDataInto: aStringOrByteArray.
		((datagram at: 2) = hostAddress and: [(datagram at: 3) = portNumber]) 
			ifTrue: [^datagram at: 1]
			ifFalse: [^0]] repeat! !

!Socket methodsFor: 'receiving' stamp: 'jmv 2/28/2010 22:35'!
receiveDataInto: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once.  The answer may be zero (indicating that no data was 
	available before the socket closed)."

	| bytesRead closed |
	bytesRead := 0.
	closed := false.
	[closed not and: [bytesRead = 0]]
		whileTrue: [
			self waitForDataIfClosed: [closed := true].
			bytesRead := self primSocket: socketHandle
				receiveDataInto: aStringOrByteArray
				startingAt: aNumber
				count: aStringOrByteArray size-aNumber+1].
	^bytesRead
! !

!Socket methodsFor: 'receiving' stamp: 'jmv 2/28/2010 22:35'!
receiveDataSignallingClosedInto: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data until something is read or the socket is closed, upon which
	we signal."

	| bytesRead |
	bytesRead := 0.
	[bytesRead = 0]
		whileTrue: [
			self waitForData.
			bytesRead := self primSocket: socketHandle
				receiveDataInto: aStringOrByteArray
				startingAt: aNumber
				count: aStringOrByteArray size-aNumber+1].
	^bytesRead
! !

!Socket methodsFor: 'receiving' stamp: 'gk 2/9/2005 12:24'!
receiveDataSignallingTimeout: timeout into: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Wait for data once for the specified nr of seconds.  This method will
	throw exceptions on timeout or the socket closing."

	self waitForDataFor: timeout.
	^self primSocket: socketHandle
		receiveDataInto: aStringOrByteArray
		startingAt: aNumber
		count: aStringOrByteArray size-aNumber+1
! !

!Socket methodsFor: 'receiving' stamp: 'svp 9/23/2003 00:03'!
receiveDataTimeout: timeout
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once."

	| buffer bytesRead |
	buffer _ String new: 2000.
	bytesRead _ self receiveDataTimeout: timeout into: buffer.
	^buffer copyFrom: 1 to: bytesRead! !

!Socket methodsFor: 'receiving' stamp: 'svp 9/23/2003 00:01'!
receiveDataTimeout: timeout into: aStringOrByteArray 
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once."

	^self receiveDataTimeout: timeout into: aStringOrByteArray startingAt: 1! !

!Socket methodsFor: 'receiving' stamp: 'jmv 3/2/2010 10:09'!
receiveDataTimeout: timeout into: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Wait for data once for the specified nr of seconds.  The answer may be 
	zero (indicating that there was no data available within the given timeout)."

	self waitForDataFor: timeout ifClosed: nil ifTimedOut: nil.
	^self primSocket: socketHandle
		receiveDataInto: aStringOrByteArray
		startingAt: aNumber
		count: aStringOrByteArray size-aNumber+1! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 16:18'!
receiveDataWithTimeout
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once.
	Either returns data or signals a time out or connection close."

	| buffer bytesRead |
	buffer _ String new: 2000.
	bytesRead _ self receiveDataWithTimeoutInto: buffer.
	^buffer copyFrom: 1 to: bytesRead! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 16:18'!
receiveDataWithTimeoutInto: aStringOrByteArray
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once.
	Either returns data or signals a time out or connection close."

	^self receiveDataWithTimeoutInto: aStringOrByteArray startingAt: 1! !

!Socket methodsFor: 'receiving' stamp: 'svp 9/23/2003 00:01'!
receiveDataWithTimeoutInto: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once."

	^self receiveDataTimeout: Socket standardTimeout into: aStringOrByteArray startingAt: aNumber 
! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 13:46'!
receiveSomeData
	"Receive currently available data (if any). Do not wait."
 
	| buffer bytesRead |
	buffer _ String new: 2000.
	bytesRead _ self receiveSomeDataInto: buffer.
	^buffer copyFrom: 1 to: bytesRead! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 13:46'!
receiveSomeDataInto: aStringOrByteArray
	"Receive data into the given buffer and return the number of bytes received. Note the given buffer may be only partially filled by the received data."

	^self receiveSomeDataInto: aStringOrByteArray startingAt: 1! !

!Socket methodsFor: 'receiving' stamp: 'mir 5/15/2003 13:46'!
receiveSomeDataInto: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. Note the given buffer may be only partially filled by the received data."

	^ self primSocket: socketHandle
		receiveDataInto: aStringOrByteArray
		startingAt: aNumber
		count: aStringOrByteArray size-aNumber+1
! !

!Socket methodsFor: 'datagrams' stamp: 'JMM 6/3/2000 21:54'!
receiveUDPDataInto: aStringOrByteArray
	"Receive UDP data into the given buffer and return the number of bytes received. Note the given buffer may be only partially filled by the received data. What is returned is an array, the first element is the bytes read, the second the sending bytearray address, the third the senders port, the fourth, true if more of the datagram awaits reading"

	^ self primSocket: socketHandle
		receiveUDPDataInto: aStringOrByteArray
		startingAt: 1
		count: aStringOrByteArray size
! !

!Socket methodsFor: 'registry' stamp: 'ar 3/21/98 17:40'!
register
	^self class register: self! !

!Socket methodsFor: 'accessing' stamp: 'jm 9/17/97 14:34'!
remoteAddress

	^ self primSocketRemoteAddress: socketHandle
! !

!Socket methodsFor: 'accessing' stamp: 'jm 9/17/97 14:34'!
remotePort

	^ self primSocketRemotePort: socketHandle
! !

!Socket methodsFor: 'accessing' stamp: 'JMM 5/9/2000 15:32'!
semaphore
	^semaphore! !

!Socket methodsFor: 'sending' stamp: 'mir 5/15/2003 18:33'!
sendData: aStringOrByteArray
	"Send all of the data in the given array, even if it requires multiple calls to send it all. Return the number of bytes sent."

	"An experimental version use on slow lines: Longer timeout and smaller writes to try to avoid spurious timeouts."

	| bytesSent bytesToSend count |
	bytesToSend _ aStringOrByteArray size.
	bytesSent _ 0.
	[bytesSent < bytesToSend] whileTrue: [
		(self waitForSendDoneFor: 60)
			ifFalse: [ConnectionTimedOut signal: 'send data timeout; data not sent'].
		count _ self primSocket: socketHandle
			sendData: aStringOrByteArray
			startIndex: bytesSent + 1
			count: (bytesToSend - bytesSent min: 5000).
		bytesSent _ bytesSent + count].

	^ bytesSent
! !

!Socket methodsFor: 'sending' stamp: 'ar 7/20/1999 17:23'!
sendData: buffer count: n
	"Send the amount of data from the given buffer"
	| sent |
	sent _ 0.
	[sent < n] whileTrue:[
		sent _ sent + (self sendSomeData: buffer startIndex: sent+1 count: (n-sent))].! !

!Socket methodsFor: 'datagrams' stamp: 'ul 4/20/2011 01:53'!
sendData: aStringOrByteArray toHost: hostAddress port: portNumber
	"Send a UDP packet containing the given data to the specified host/port."

	^self sendUDPData: aStringOrByteArray toHost: hostAddress port: portNumber! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:33'!
sendDone
	"Return true if the most recent send operation on this socket has completed."

	socketHandle ifNil: [^ false].
	^ self primSocketSendDone: socketHandle
! !

!Socket methodsFor: 'sending' stamp: 'ls 1/5/1999 15:05'!
sendSomeData: aStringOrByteArray
	"Send as much of the given data as possible and answer the number of bytes actually sent."
	"Note: This operation may have to be repeated multiple times to send a large amount of data."

	^ self
		sendSomeData: aStringOrByteArray
		startIndex: 1
		count: aStringOrByteArray size! !

!Socket methodsFor: 'sending' stamp: 'ls 3/3/1999 18:59'!
sendSomeData: aStringOrByteArray startIndex: startIndex
	"Send as much of the given data as possible starting at the given index. Answer the number of bytes actually sent."
	"Note: This operation may have to be repeated multiple times to send a large amount of data."

	^ self
		sendSomeData: aStringOrByteArray
		startIndex: startIndex
		count: (aStringOrByteArray size - startIndex + 1)! !

!Socket methodsFor: 'sending' stamp: 'jmv 8/5/2011 17:38'!
sendSomeData: aStringOrByteArray startIndex: startIndex count: count
	^ self sendSomeData: aStringOrByteArray startIndex: startIndex count: count for: Socket standardTimeout! !

!Socket methodsFor: 'sending' stamp: 'fbs 2/18/2011 08:58'!
sendSomeData: aStringOrByteArray startIndex: startIndex count: count for: aTimeoutInSeconds
	"Send up to count bytes of the given data starting at the given index. Answer the number of bytes actually sent."
	"Note: This operation may have to be repeated multiple times to send a large amount of data."

	| bytesSent |
	(self waitForSendDoneFor: aTimeoutInSeconds)
		ifTrue: [
			bytesSent := self primSocket: socketHandle
				sendData: aStringOrByteArray
				startIndex: startIndex
				count: count]
		ifFalse: [ConnectionTimedOut signal: 'send data timeout; data not sent'].
	^ bytesSent
! !

!Socket methodsFor: 'sending' stamp: 'mir 2/19/2002 18:33'!
sendStreamContents: stream
	"Send the data in the stream. Close the stream.
	Usefull for directly sending contents of a file without reading into memory first."

	self sendStreamContents: stream checkBlock: [true]! !

!Socket methodsFor: 'sending' stamp: 'nice 12/26/2009 21:50'!
sendStreamContents: stream checkBlock: checkBlock
	"Send the data in the stream. Close the stream after you are done. After each block of data evaluate checkBlock and abort if it returns false.
	Usefull for directly sending contents of a file without reading into memory first."
	[
	| chunkSize buffer |
	chunkSize := 5000.
	buffer := ByteArray new: chunkSize.
	stream binary.
	[stream atEnd and: [checkBlock value]]
		whileFalse: [
			buffer := stream next: chunkSize into: buffer.
			self sendData: buffer]]
		ensure: [stream close]! !

!Socket methodsFor: 'datagrams' stamp: 'mir 5/15/2003 18:34'!
sendUDPData: aStringOrByteArray toHost: hostAddress port: portNumber
	"Send a UDP packet containing the given data to the specified host/port."
	| bytesToSend bytesSent count |

	bytesToSend _ aStringOrByteArray size.
	bytesSent _ 0.
	[bytesSent < bytesToSend] whileTrue: [
		(self waitForSendDoneFor: 20)
			ifFalse: [ConnectionTimedOut signal: 'send data timeout; data not sent'].
		count _ self primSocket: socketHandle
			sendUDPData: aStringOrByteArray
			toHost: hostAddress
			port: portNumber
			startIndex: bytesSent + 1
			count: bytesToSend - bytesSent.
		bytesSent _ bytesSent + count].

	^ bytesSent
! !

!Socket methodsFor: 'other' stamp: 'mir 2/22/2002 16:30'!
setOption: aName value: aValue 
	| value |
	"setup options on this socket, see Unix man pages for values for 
	sockets, IP, TCP, UDP. IE SO_KEEPALIVE
	returns an array, element one is the error number
	element two is the resulting of the negotiated value.
	See getOption for list of keys"

	(socketHandle == nil or: [self isValid not])
		ifTrue: [InvalidSocketStatusException signal: 'Socket status must valid before setting an option'].
	value _ aValue asString.
	aValue == true ifTrue: [value _ '1'].
	aValue == false ifTrue: [value _ '0'].
	^ self primSocket: socketHandle setOption: aName value: value! !

!Socket methodsFor: 'datagrams' stamp: 'ar 4/30/1999 04:29'!
setPeer: hostAddress port: port
	"Set the default send/recv address."

	self primSocket: socketHandle connectTo: hostAddress port: port.
! !

!Socket methodsFor: 'datagrams' stamp: 'ar 4/30/1999 04:29'!
setPort: port
	"Associate a local port number with a UDP socket.  Not applicable to TCP sockets."

	self primSocket: socketHandle setPort: port.
! !

!Socket methodsFor: 'queries' stamp: 'JMM 5/8/2000 23:24'!
socketError
	^self primSocketError: socketHandle! !

!Socket methodsFor: 'accessing' stamp: 'ar 7/16/1999 17:22'!
socketHandle
	^socketHandle! !

!Socket methodsFor: 'queries' stamp: 'jmv 3/1/2010 13:33'!
statusString
	"Return a string describing the status of this socket."

	| status |
	socketHandle ifNil: [^ 'destroyed'].
	status _ self primSocketConnectionStatus: socketHandle.
	status = InvalidSocket ifTrue: [^ 'invalidSocketHandle'].
	status = Unconnected ifTrue: [^ 'unconnected'].
	status = WaitingForConnection ifTrue: [^ 'waitingForConnection'].
	status = Connected ifTrue: [^ 'connected'].
	status = OtherEndClosed ifTrue: [^ 'otherEndClosedButNotThisEnd'].
	status = ThisEndClosed ifTrue: [^ 'thisEndClosedButNotOtherEnd'].
	^ 'unknown socket status'
! !

!Socket methodsFor: 'registry' stamp: 'ar 3/21/98 17:41'!
unregister
	^self class unregister: self! !

!Socket methodsFor: 'waiting' stamp: 'dc 10/21/2008 08:19'!
waitForAcceptFor: timeout
	"Wait and accept an incoming connection. Return nil if it falis"
	self waitForConnectionFor: timeout ifTimedOut: [^ nil].
	^ self isConnected
		ifTrue:[self accept]
		! !

!Socket methodsFor: 'waiting' stamp: 'svp 7/27/2003 00:23'!
waitForAcceptFor: timeout ifTimedOut: timeoutBlock
	"Wait and accept an incoming connection"
	self waitForConnectionFor: timeout ifTimedOut: [^timeoutBlock value].
	^self accept! !

!Socket methodsFor: 'waiting' stamp: 'mu 8/19/2003 02:57'!
waitForConnectionFor: timeout
	"Wait up until the given deadline for a connection to be established. Return true if it is established by the deadline, false if not."

	^self 
		waitForConnectionFor: timeout 
		ifTimedOut: [ConnectionTimedOut signal: 'Failed to connect in ', timeout asString, ' seconds']
! !

!Socket methodsFor: 'waiting' stamp: 'jmv 6/11/2014 19:35'!
waitForConnectionFor: timeout ifTimedOut: timeoutBlock
	"Wait up until the given deadline for a connection to be established. Return true if it is established by the deadline, false if not."

	| startTime msecsDelta msecsEllapsed status |
	startTime := Time localMillisecondClock.
	msecsDelta := (timeout * 1000) truncated.
	status := self primSocketConnectionStatus: socketHandle.
	[(status = WaitingForConnection) and: [(msecsEllapsed := Time localMillisecondClock - startTime) < msecsDelta]]
		whileTrue: [
			semaphore waitTimeoutMSecs: msecsDelta - msecsEllapsed.
			status := self primSocketConnectionStatus: socketHandle].

	status = Connected ifFalse: [^timeoutBlock value].
	^ true! !

!Socket methodsFor: 'waiting' stamp: 'svp 9/23/2003 00:09'!
waitForData
	"Wait for data to arrive.  This method will block until
	data is available or the socket is closed.  If the socket is closed
	a ConnectionClosed exception will be signaled."

	^self waitForDataIfClosed:
		[ConnectionClosed signal: 'Connection close while waiting for data.']! !

!Socket methodsFor: 'waiting' stamp: 'svp 7/27/2003 00:18'!
waitForDataFor: timeout
	"Wait for the given nr of seconds for data to arrive.
	Signal a time out or connection close exception if either happens before data becomes available."

	^self
		waitForDataFor: timeout
		ifClosed: [ConnectionClosed signal: 'Connection closed while waiting for data.']
		ifTimedOut: [ConnectionTimedOut signal: 'Data receive timed out.']
! !

!Socket methodsFor: 'waiting' stamp: 'jmv 6/11/2014 19:35'!
waitForDataFor: timeout ifClosed: closedBlock ifTimedOut: timedOutBlock
	"Wait for the given nr of seconds for data to arrive."
	
	| startTime msecsDelta |
	startTime := Time localMillisecondClock.
	msecsDelta := (timeout * 1000) truncated.
	[(Time localMillisecondClock - startTime) < msecsDelta] whileTrue: [
		(self primSocketReceiveDataAvailable: socketHandle)
			ifTrue: [^self].
		self isConnected
			ifFalse: [^closedBlock value].
		self readSemaphore waitTimeoutMSecs: 
			(msecsDelta - (Time localMillisecondClock - startTime) max: 0).
	].

	(self primSocketReceiveDataAvailable: socketHandle)
		ifFalse: [
			self isConnected
				ifTrue: [^timedOutBlock value]
				ifFalse: [^closedBlock value]].! !

!Socket methodsFor: 'waiting' stamp: 'ul 6/17/2011 12:43'!
waitForDataIfClosed: closedBlock
	"Wait indefinitely for data to arrive.  This method will block until
	data is available or the socket is closed."

	[
		(self primSocketReceiveDataAvailable: socketHandle)
			ifTrue: [^self].
		self isConnected
			ifFalse: [^closedBlock value].
		self readSemaphore wait ] repeat
! !

!Socket methodsFor: 'waiting' stamp: 'jmv 6/11/2014 19:36'!
waitForDisconnectionFor: timeout
	"Wait for the given nr of seconds for the connection to be broken.
	Return true if it is broken by the deadline, false if not.
	The client should know the connection is really going to be closed
	(e.g., because he has called 'close' to send a close request to the other end)
	before calling this method."

	| startTime msecsDelta status |
	startTime := Time localMillisecondClock.
	msecsDelta := (timeout * 1000) truncated.
	status := self primSocketConnectionStatus: socketHandle.
	[((status == Connected) or: [(status == ThisEndClosed)]) and:
	 [(Time localMillisecondClock - startTime) < msecsDelta]] whileTrue: [
		self discardReceivedData.
		self readSemaphore waitTimeoutMSecs: 
			(msecsDelta - (Time localMillisecondClock - startTime) max: 0).
		status := self primSocketConnectionStatus: socketHandle].
	^ status ~= Connected! !

!Socket methodsFor: 'waiting' stamp: 'jmv 6/11/2014 19:36'!
waitForSendDoneFor: timeout
	"Wait up until the given deadline for the current send operation to complete. Return true if it completes by the deadline, false if not."

	| startTime msecsDelta msecsEllapsed sendDone |
	startTime := Time localMillisecondClock.
	msecsDelta := (timeout * 1000) truncated.
	[(sendDone := self primSocketSendDone: socketHandle) not and: [ self isConnected
			"Connection end and final data can happen fast, so test in this order"
		and: [(msecsEllapsed := Time localMillisecondClock - startTime) < msecsDelta]]] whileTrue: [
			self writeSemaphore waitTimeoutMSecs: msecsDelta - msecsEllapsed].

	^ sendDone! !

!Socket methodsFor: 'accessing' stamp: 'ul 4/20/2011 01:56'!
writeSemaphore
	
	^writeSemaphore! !

!Socket class methodsFor: 'instance creation' stamp: 'ls 9/24/1999 09:45'!
acceptFrom: aSocket
	^[ super new acceptFrom: aSocket ]
		repeatWithGCIf: [ :sock | sock isValid not ]! !

!Socket class methodsFor: 'instance creation' stamp: 'ar 4/30/1999 04:15'!
createIfFail: failBlock
	"Attempt to create a new socket. If successful, return the new socket. Otherwise, return the result of evaluating the given block. Socket creation can fail if the network isn't available or if there are not sufficient resources available to create another socket."
	"Note: The default creates a TCP socket"
	^self tcpCreateIfFail: failBlock! !

!Socket class methodsFor: 'utilities' stamp: 'tk 4/9/98 15:54'!
deadServer

	^ DeadServer! !

!Socket class methodsFor: 'utilities' stamp: 'tk 4/9/98 15:56'!
deadServer: aStringOrNil
	"Keep the machine name of the most recently encoutered non-responding machine.  Next time the user can move it to the last in a list of servers to try."

	DeadServer _ aStringOrNil! !

!Socket class methodsFor: 'class initialization' stamp: 'ar 12/12/2001 19:12'!
initialize
	"Socket initialize"

	"Socket Types"
	TCPSocketType _ 0.
	UDPSocketType _ 1.

	"Socket Status Values"
	InvalidSocket _ -1.
	Unconnected _ 0.
	WaitingForConnection _ 1.
	Connected _ 2.
	OtherEndClosed _ 3.
	ThisEndClosed _ 4.

	RegistryThreshold _ 100. "# of sockets"! !

!Socket class methodsFor: 'network initialization' stamp: 'jm 9/15/97 09:30'!
initializeNetwork
	"Initialize the network drivers and the NetNameResolver. Do nothing if the network is already initialized."
	"Note: The network must be re-initialized every time Squeak starts up, so applications that persist across snapshots should be prepared to re-initialize the network as needed. Such applications should call 'Socket initializeNetwork' before every network transaction. "

	NetNameResolver initializeNetwork.
! !

!Socket class methodsFor: 'tests' stamp: 'jmv 3/13/2012 12:51'!
loopbackTest
	"Send data from one socket to another on the local machine.
	Tests most of the socket primitives."

	"100 timesRepeat: [Socket loopbackTest]"

	| sock1 sock2 bytesToSend sendBuf receiveBuf done bytesSent bytesReceived t extraBytes packetsSent packetsRead |
	Transcript
		newLine;
		show: 'starting loopback test';
		newLine.
	Transcript
		show: '---------- Connecting ----------';
		newLine.
	self initializeNetwork.
	sock1 := self new.
	sock2 := self new.
	sock1 listenOn: 54321.
	sock2 connectTo: NetNameResolver localHostAddress port: 54321.
	sock1 waitForConnectionFor: self standardTimeout.
	sock2 waitForConnectionFor: self standardTimeout.
	sock1 isConnected ifFalse: [self error: 'sock1 not connected'].
	sock2 isConnected ifFalse: [self error: 'sock2 not connected'].
	Transcript
		show: 'connection established';
		newLine.
	bytesToSend := 5000000.
	sendBuf := String new: 5000 withAll: $x.
	receiveBuf := String new: 50000.
	done := false.
	packetsSent := packetsRead := bytesSent := bytesReceived := 0.
	t := Time millisecondsToRun: 
					[[done] whileFalse: 
							[(sock1 sendDone and: [bytesSent < bytesToSend]) 
								ifTrue: 
									[packetsSent := packetsSent + 1.
									bytesSent := bytesSent + (sock1 sendSomeData: sendBuf)].
							sock2 dataAvailable 
								ifTrue: 
									[packetsRead := packetsRead + 1.
									bytesReceived := bytesReceived + (sock2 receiveDataInto: receiveBuf)].
							done := bytesSent >= bytesToSend and: [bytesReceived = bytesSent]]].
	Transcript
		show: 'closing connection';
		newLine.
	sock1 waitForSendDoneFor: self standardTimeout.
	sock1 close.
	sock2 waitForDisconnectionFor: self standardTimeout.
	extraBytes := sock2 discardReceivedData.
	extraBytes > 0 
		ifTrue: [
			Transcript
				show: ' *** received ' , extraBytes size printString , ' extra bytes ***';
				newLine].
	sock2 close.
	sock1 waitForDisconnectionFor: self standardTimeout.
	sock1 isUnconnectedOrInvalid ifFalse: [self error: 'sock1 not closed'].
	sock2 isUnconnectedOrInvalid ifFalse: [self error: 'sock2 not closed'].
	Transcript
		show: '---------- Connection Closed ----------';
		newLine.
	sock1 destroy.
	sock2 destroy.
	Transcript
		show: 'loopback test done; time = ' , t printString;
		newLine.
	Transcript
		show: (bytesToSend asFloat / t roundTo: 0.01) printString 
					, '* 1000 bytes/sec';
		newLine.
	Transcript endEntry! !

!Socket class methodsFor: 'utilities' stamp: 'jm 1/14/1999 12:13'!
nameForWellKnownTCPPort: portNum
	"Answer the name for the given well-known TCP port number. Answer a string containing the port number if it isn't well-known."

	| portList entry |
	portList _ #(
		(7 'echo') (9 'discard') (13 'time') (19 'characterGenerator')
		(21 'ftp') (23 'telnet') (25 'smtp')
		(80 'http') (110 'pop3') (119 'nntp')).
	entry _ portList detect: [:pair | pair first = portNum] ifNone: [^ 'port-', portNum printString].
	^ entry last
! !

!Socket class methodsFor: 'instance creation' stamp: 'ar 4/30/1999 04:13'!
new
	"Return a new, unconnected Socket. Note that since socket creation may fail, it is safer to use the method createIfFail: to handle such failures gracefully; this method is primarily for backward compatibility and may be disallowed in a future release."
	"Note: The default creates a TCP socket - this is also backward compatibility."
	^self newTCP! !

!Socket class methodsFor: 'tests' stamp: 'gk 12/15/2005 01:03'!
newAcceptCheck
	"Check if the platform has support for the BSD style accept()."

	"Socket newAcceptCheck"
	
	| socket |
	self initializeNetwork.
	socket := self newTCP.
	socket listenOn: 44444 backlogSize: 4.
	socket isValid ifTrue: [
		self inform: 'Everything looks OK for the BSD style accept()'
	] ifFalse: [
		self inform: 'It appears that you DO NOT have support for the BSD style accept()'].
	socket destroy! !

!Socket class methodsFor: 'instance creation' stamp: 'mir 2/22/2002 15:48'!
newTCP
	"Create a socket and initialise it for TCP"
	self initializeNetwork.
	^[ super new initialize: TCPSocketType ]
		repeatWithGCIf: [ :socket | socket isValid not ]! !

!Socket class methodsFor: 'instance creation' stamp: 'mir 2/22/2002 15:49'!
newUDP
	"Create a socket and initialise it for UDP"
	self initializeNetwork.
	^[ super new initialize: UDPSocketType ]
		repeatWithGCIf: [ :socket | socket isValid not ]! !

!Socket class methodsFor: 'utilities' stamp: 'jmv 6/11/2014 19:36'!
ping: hostName
	"Ping the given host. Useful for checking network connectivity. The host must be running a TCP echo server."
	"Socket ping: 'squeak.cs.uiuc.edu'"

	| tcpPort sock serverAddr startTime echoTime |
	tcpPort _ 7.  "7 = echo port, 13 = time port, 19 = character generator port"

	serverAddr _ NetNameResolver addressForName: hostName timeout: 10.
	serverAddr ifNil: [
		^ self inform: 'Could not find an address for ', hostName].

	sock _ Socket new.
	sock connectNonBlockingTo: serverAddr port: tcpPort.
	[sock waitForConnectionFor: 10]
		on: ConnectionTimedOut
		do: [:ex |
			(self confirm: 'Continue to wait for connection to ', hostName, '?')
				ifTrue: [ex retry]
				ifFalse: [
					sock destroy.
					^ self]].

	sock sendData: 'echo!!'.
	startTime _ Time localMillisecondClock.
	[sock waitForDataFor: 15]
		on: ConnectionTimedOut
		do: [:ex | (self confirm: 'Packet sent but no echo yet; keep waiting?')
			ifTrue: [ex retry]].
	echoTime _ Time localMillisecondClock - startTime.

	sock destroy.
	self inform: hostName, ' responded in ', echoTime printString, ' milliseconds'.
! !

!Socket class methodsFor: 'utilities' stamp: 'jmv 6/11/2014 19:37'!
pingPorts: portList on: hostName timeOutSecs: timeOutSecs
	"Attempt to connect to each of the given sockets on the given host. Wait at most timeOutSecs for the connections to be established. Answer an array of strings indicating the available ports."

	"Socket pingPorts: #(7 13 19 21 23 25 80 110 119) on: 'squeak.org' timeOutSecs: 15"

	| serverAddr sockets startTime timeoutMsecs done result unconnectedCount connectedCount waitingCount |
	serverAddr := NetNameResolver addressForName: hostName timeout: 10.
	serverAddr ifNil: [ 
			self inform: 'Could not find an address for ' , hostName.
			^ #() ].
	sockets := portList
		collect: [ :portNum | 
			| sock |
			sock := Socket new.
			[ sock connectTo: serverAddr port: portNum ] 
				on: ConnectionTimedOut
				do: [ ].
			sock ].
	startTime := Time localMillisecondClock.
	timeoutMsecs := (1000 * timeOutSecs) truncated.
	done := false.
	[ done ]
		whileFalse: [ 
			unconnectedCount := 0.
			connectedCount := 0.
			waitingCount := 0.
			sockets
				do: [ :s | 
					s isUnconnectedOrInvalid
						ifTrue: [ unconnectedCount := unconnectedCount + 1 ]
						ifFalse: [ 
							s isConnected
								ifTrue: [ connectedCount := connectedCount + 1 ].
							s isWaitingForConnection
								ifTrue: [ waitingCount := waitingCount + 1 ] ] ].
			waitingCount = 0
				ifTrue: [ done := true ].
			connectedCount = sockets size
				ifTrue: [ done := true ].
			(Time localMillisecondClock - startTime) >= timeoutMsecs
				ifTrue: [ done := true ] ].
	result := (sockets select: [ :s | s isConnected ]) collect: [ :s | self nameForWellKnownTCPPort: s remotePort ].
	sockets do: [ :s | s destroy ].
	^ result! !

!Socket class methodsFor: 'utilities' stamp: 'jm 1/14/1999 17:25'!
pingPortsOn: hostName
	"Attempt to connect to a set of well-known sockets on the given host, and answer the names of the available ports."
	"Socket pingPortsOn: 'www.disney.com'"

	^ Socket
		pingPorts: #(7 13 19 21 23 25 80 110 119)
		on: hostName
		timeOutSecs: 20
! !

!Socket class methodsFor: 'network initialization' stamp: 'mir 2/22/2002 14:59'!
primInitializeNetwork: resolverSemaIndex
	"Initialize the network drivers on platforms that need it, such as the Macintosh, and return nil if network initialization failed or the receiver if it succeeds. Since mobile computers may not always be connected to a network, this method should NOT be called automatically at startup time; rather, it should be called when first starting a networking application. It is a noop if the network driver has already been initialized. If non-zero, resolverSemaIndex is the index of a VM semaphore to be associated with the network name resolver. This semaphore will be signalled when the resolver status changes, such as when a name lookup query is completed."
	"Note: some platforms (e.g., Mac) only allow only one name lookup query at a time, so a manager process should be used to serialize resolver lookup requests."

	<primitive: 'primitiveInitializeNetwork' module: 'SocketPlugin'>
	^ nil  "return nil if primitive fails"
! !

!Socket class methodsFor: 'registry' stamp: 'ul 8/27/2010 23:24'!
register: anObject
	
	^self registry add: anObject! !

!Socket class methodsFor: 'registry' stamp: 'ul 8/27/2010 23:24'!
registry

	^Registry ifNil: [ Registry := WeakRegistry new ]! !

!Socket class methodsFor: 'registry' stamp: 'ar 12/12/2001 19:12'!
registryThreshold
	"Return the registry threshold above which socket creation may fail due to too many already open sockets. If the threshold is reached, a full GC will be issued if the creation of a socket fails."
	^RegistryThreshold! !

!Socket class methodsFor: 'registry' stamp: 'ar 12/12/2001 19:12'!
registryThreshold: aNumber
	"Return the registry threshold above which socket creation may fail due to too many already open sockets. If the threshold is reached, a full GC will be issued if the creation of a socket fails."
	RegistryThreshold _ aNumber! !

!Socket class methodsFor: 'tests' stamp: 'jmv 9/24/2012 19:47'!
sendTest
	"Send data to the 'discard' socket of the given host.
	Tests the speed of one-way data transfers across the
	network to the given host. Note that most hosts
	do not run a discard server."

	"Socket sendTest"

	| sock bytesToSend sendBuf bytesSent t serverName serverAddr |
	Transcript newLine; show: 'starting send test'; newLine.
	self initializeNetwork.
	serverName := FillInTheBlankMorph request: 'What is the destination server?' initialAnswer: 'create.ucsb.edu'.
	serverAddr := NetNameResolver addressForName: serverName timeout: 10.
	serverAddr ifNil: [
		^self inform: 'Could not find an address for ' , serverName].
	sock := self new.
	Transcript show: '---------- Connecting ----------';newLine.
	sock connectTo: serverAddr port: 9.
	sock isConnected ifFalse: [
		sock destroy.
		^self inform: 'could not connect'].
	Transcript show: 'connection established; sending data'; newLine.
	bytesToSend := 1000000.
	sendBuf := String new: 64 * 1024 withAll: $x.
	bytesSent := 0.
	t := Time millisecondsToRun: 
					[[bytesSent < bytesToSend] whileTrue: 
							[sock sendDone 
								ifTrue: [bytesSent := bytesSent + (sock sendSomeData: sendBuf)]]].
	sock waitForSendDoneFor: self standardTimeout.
	sock destroy.
	Transcript show: '---------- Connection Closed ----------'; newLine;
		show: 'send test done; time = ' , t printString; newLine;
		show: (bytesToSend asFloat / t roundTo: 0.01) printString, ' * 1000 bytes/sec'; newLine; endEntry! !

!Socket class methodsFor: 'utilities' stamp: 'mir 5/15/2003 16:17'!
standardDeadline
	"Return a default deadline time some seconds into the future."

	^ self deadlineSecs: self standardTimeout
! !

!Socket class methodsFor: 'utilities' stamp: 'mir 5/15/2003 16:16'!
standardTimeout

	^45
! !

!Socket class methodsFor: 'instance creation' stamp: 'mir 2/22/2002 15:49'!
tcpCreateIfFail: failBlock
	"Attempt to create a new socket. If successful, return the new socket. Otherwise, return the result of evaluating the given block. Socket creation can fail if the network isn't available or if there are not sufficient resources available to create another socket."

	| sock |
	self initializeNetwork.
	sock _ super new initialize: TCPSocketType.
	sock isValid ifFalse: [^ failBlock value].
	^ sock
! !

!Socket class methodsFor: 'instance creation' stamp: 'mir 2/22/2002 15:49'!
udpCreateIfFail: failBlock
	"Attempt to create a new socket. If successful, return the new socket. Otherwise, return the result of evaluating the given block. Socket creation can fail if the network isn't available or if there are not sufficient resources available to create another socket."

	| sock |
	self initializeNetwork.
	sock _ super new initialize: UDPSocketType.
	sock isValid ifFalse: [^ failBlock value].
	^ sock
! !

!Socket class methodsFor: 'registry' stamp: 'ul 8/27/2010 23:24'!
unregister: anObject
	
	^self registry remove: anObject ifAbsent: nil! !

!Socket class methodsFor: 'utilities' stamp: 'ar 4/30/1999 04:21'!
wildcardAddress
	"Answer a don't-care address for use with UDP sockets."

	^ByteArray new: 4		"0.0.0.0"! !

!Socket class methodsFor: 'utilities' stamp: 'ar 4/30/1999 04:21'!
wildcardPort
	"Answer a don't-care port for use with UDP sockets.  (The system will allocate an
	unused port number to the socket.)"

	^0! !

!SocketStream methodsFor: 'private' stamp: 'gk 2/9/2005 22:44'!
adjustInBuffer: bytesRead
	"Adjust markers and possibly grow inBuffer or move data down.
	Currently grows through doubling when less than 1024 bytes are left.
	Never shrinks. Returns the position in the buffer where any new
	data can be found."

	| old |
	bytesRead = 0 ifTrue: [^inNextToWrite].
	old := inNextToWrite.
	inNextToWrite := inNextToWrite + bytesRead.
	(inBuffer size - inNextToWrite) < 1024
		ifTrue: [
			"Hit the roof, move data down (if enough has been read) or do we grow?"
			(lastRead > 512)
				ifTrue: [^old - self moveInBufferDown]
				ifFalse: [self growInBuffer]].
	^old! !

!SocketStream methodsFor: 'private' stamp: 'gk 2/9/2005 22:42'!
adjustOutBuffer: bytesToWrite
	"Possibly grow outBuffer to accommodate the new data.
	Currently grows through doubling when less
	than 1024 bytes are left. If bytesToWrite is even
	larger we double that instead. Never shrinks."

	(outBuffer size - outNextToWrite - bytesToWrite) < 1024 ifTrue: [
		outBuffer := (self streamBuffer: ((outBuffer size max: bytesToWrite) * 2))
						replaceFrom: 1 to: outBuffer size with: outBuffer startingAt: 1]! !

!SocketStream methodsFor: 'configuration' stamp: 'nice 3/16/2010 22:34'!
ascii
	"Tell the SocketStream to send data
	as Strings instead of ByteArrays.
	This is default."

	binary := false.
	inBuffer
		ifNil: [self resetBuffers]
		ifNotNil:
			[inBuffer := inBuffer asString.
			outBuffer := outBuffer asString]! !

!SocketStream methodsFor: 'testing' stamp: 'gk 2/25/2005 14:23'!
atEnd
	"There is nothing more to read when
	there is no more data in our inBuffer, the socket
	is disconnected and there is none available on the socket.
	Note that we need to check isConnected before isDataAvailable,
	otherwise data may sneak in in the meantime. But we check the
	buffer first, because it is faster."

	self isInBufferEmpty ifFalse: [^false].
	^self isConnected not
		and: [self isDataAvailable not]! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/9/2005 22:26'!
autoFlush
	"If autoFlush is enabled data will be sent through
	the socket (flushed) when the bufferSize is reached
	or the SocketStream is closed. Otherwise the user
	will have to send #flush manually.
	Close will always flush. Default is false."

	^autoFlush! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/9/2005 22:27'!
autoFlush: aBoolean
	"If autoFlush is enabled data will be sent through
	the socket (flushed) when the bufferSize is reached
	or the SocketStream is closed. Otherwise the user
	will have to send #flush manually.
	Close will always flush. Default is false."

	autoFlush := aBoolean! !

!SocketStream methodsFor: 'private' stamp: 'ar 11/22/2010 23:48'!
beSignalingWhile: aBlock
	"Temporarily turn a non-signaling SocketStream into a signaling one.
	Required for some of operations that will catch ConnectionClosed in 
	order to find out that an operation completed"

	| signaling |
	signaling := shouldSignal.
	shouldSignal := true.
	^aBlock ensure:[shouldSignal := signaling]
! !

!SocketStream methodsFor: 'configuration' stamp: 'nice 3/16/2010 22:35'!
binary
	"Tell the SocketStream to send data
	as ByteArrays instead of Strings.
	Default is ascii."

	binary := true.
	inBuffer
		ifNil: [self resetBuffers]
		ifNotNil:
			[inBuffer := inBuffer asByteArray.
			outBuffer := outBuffer asByteArray]! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/9/2005 22:28'!
bufferSize
	"Default buffer size is 4kb.
	increased from earlier 2000 bytes."
	
	^bufferSize! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/9/2005 22:28'!
bufferSize: anInt
	"Default buffer size is 4kb.
	increased from earlier 2000 bytes."

	bufferSize := anInt! !

!SocketStream methodsFor: 'private' stamp: 'gk 2/7/2005 13:09'!
checkFlush
	"If autoFlush is true we flush if
	we have reached the bufferSize
	of data in the outBuffer."

	(autoFlush and: [outNextToWrite > bufferSize])
		ifTrue: [self flush]! !

!SocketStream methodsFor: 'control' stamp: 'gk 2/24/2005 11:55'!
close
	"Flush any data still not sent
	and take care of the socket."

	self flush.
	socket closeAndDestroy: 30! !

!SocketStream methodsFor: 'stream out' stamp: 'jmv 5/27/2013 11:02'!
crlf
	self nextPutAll: String crlfString! !

!SocketStream methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:47'!
debug
	"Display debug info."

	| data |
	data := self inBufferSize.
	^String streamContents: [:s |
		s
			nextPutAll: 'Buffer size: ', inBuffer size asString; newLine;
			nextPutAll: 'InBuffer data size: ', data asString; newLine;
			nextPutAll: 'In data (20):', (inBuffer copyFrom: lastRead + 1 to: lastRead + (data min: 20)); newLine;
			nextPutAll: 'OutBuffer data size: ', (outNextToWrite - 1) asString; newLine;
			nextPutAll: 'Out data (20):', (outBuffer copyFrom: 1 to: ((outNextToWrite - 1) min: 20)); newLine]! !

!SocketStream methodsFor: 'initialization' stamp: 'ar 7/24/2010 15:13'!
destroy
	"Destroy the receiver and its underlying socket. Does not attempt to flush the output buffers. For a graceful close use SocketStream>>close instead."

	socket ifNotNil:[socket destroy]! !

!SocketStream methodsFor: 'control' stamp: 'cmm 1/28/2011 15:15'!
flush
	"If the other end is connected and we have something
	to send, then we send it and reset the outBuffer."
	(outNextToWrite > 1 and: [ socket isOtherEndClosed not ]) ifTrue:
		[ [ self
			sendData: outBuffer
			count: outNextToWrite - 1 ]
			on: ConnectionTimedOut
			do: [ : ex | shouldSignal ifTrue: [ ex pass ] ].
		outNextToWrite := 1 ]! !

!SocketStream methodsFor: 'private' stamp: 'gk 2/7/2005 23:05'!
growInBuffer
	"Grows through doubling."

	self resizeInBuffer: inBuffer size * 2! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/10/2005 17:58'!
inBufferSize
	"Answers the current size of data in the inBuffer."

	^inNextToWrite - lastRead - 1! !

!SocketStream methodsFor: 'initialization' stamp: 'gk 2/25/2005 14:20'!
initialize
	autoFlush := true.
	shouldSignal := true.
	recentlyRead := 0.
	bufferSize := 4096.
	self ascii! !

!SocketStream methodsFor: 'testing' stamp: 'gk 2/3/2005 20:35'!
isBinary
	^binary! !

!SocketStream methodsFor: 'testing' stamp: 'gk 2/7/2005 12:24'!
isConnected
	"The stream is connected if the socket is."

	^socket isConnected! !

!SocketStream methodsFor: 'testing' stamp: 'ar 11/23/2010 00:04'!
isDataAvailable
	"Answer if more data can be read. It the inbuffer is empty, we read more data.

	Note: It is important not to rely on 'socket dataAvailable' here since this will
	not work for subclasses such as SecureSocketStream (which can contain
	undecrypted contents that has been read from the socket)."
 
	self isInBufferEmpty ifFalse: [^true].
	^self receiveAvailableData < inNextToWrite
! !

!SocketStream methodsFor: 'testing' stamp: 'gk 2/7/2005 13:02'!
isEmpty
	"Test if there are more data to read."

	^self isInBufferEmpty and: [self isDataAvailable not]! !

!SocketStream methodsFor: 'testing' stamp: 'gk 2/7/2005 13:02'!
isInBufferEmpty
	"Any data in the buffer?"
 
	^lastRead + 1 = inNextToWrite! !

!SocketStream methodsFor: 'testing' stamp: 'gk 2/7/2005 08:59'!
isOtherEndConnected
	^socket isOtherEndClosed not! !

!SocketStream methodsFor: 'private' stamp: 'jmv 8/18/2009 10:30'!
moveInBufferDown
	"Move down contents of inBuffer to the start.
	Return distance moved."
	"
	jmv - Horrible hack.
	To support the nasty #pushBack: as needed by the silly XMLTokenizer, do not go to the start, but leave
	slack bytes of already read stuff.
	"

	| sz distanceMoved slack |
	
	slack _ 16.
	
	sz := inNextToWrite - lastRead - 1.
	inBuffer replaceFrom: 1+slack to: sz+slack with: inBuffer startingAt: lastRead + 1.
	distanceMoved := lastRead.
	lastRead := 0+slack.
	inNextToWrite := sz + 1+slack.
	^distanceMoved
! !

!SocketStream methodsFor: 'stream out' stamp: 'jmv 3/14/2012 09:13'!
newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPutAll: String newLineString! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/7/2005 13:33'!
next
	"Return next byte, if inBuffer is empty
	we recieve some more data and try again."

	self atEnd ifTrue: [^nil].
	self isInBufferEmpty ifTrue:
		[self receiveData.
		self atEnd ifTrue: [^nil]].
	lastRead := lastRead + 1.
	^inBuffer at: lastRead! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/15/2005 14:17'!
next: anInteger
	"Answer anInteger bytes of data.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!!"

	| start |
	self receiveData: anInteger.
	start := lastRead + 1.
	lastRead := (lastRead + anInteger) min: inNextToWrite - 1.
	^inBuffer copyFrom: start to: lastRead! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 2/23/2010 13:06'!
next: n into: aCollection
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 11/22/2010 23:50'!
next: anInteger into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."

	"Implementation note: This method DOES signal timeout if not 
	enough elements are received. It does NOT signal
	ConnectionClosed as closing the connection is the only way by
	which partial data can be read."

	| start amount |

	[self beSignalingWhile:[self receiveData: anInteger]] 
		on: ConnectionClosed do:[:ex| ex return].

	"Inlined version of nextInBuffer: to avoid copying the contents"
	amount := anInteger min: (inNextToWrite - lastRead - 1).
	start := lastRead + 1.
	lastRead := lastRead + amount.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + amount-1 
		with: inBuffer 
		startingAt: start.
	^amount < anInteger 
		ifTrue:[aCollection copyFrom: 1 to:  startIndex + amount-1]
		ifFalse:[aCollection]! !

!SocketStream methodsFor: 'stream out' stamp: 'nice 3/17/2010 20:27'!
next: n putAll: aCollection startingAt: startIndex
	"Put a String or a ByteArray onto the stream.
	Currently a large collection will allocate a large buffer.
	Warning: this does not work with WideString: they have to be converted first."

	self adjustOutBuffer: n.
	outBuffer replaceFrom: outNextToWrite to: outNextToWrite + n - 1 with: aCollection startingAt: startIndex.
	outNextToWrite := outNextToWrite + n.
	self checkFlush.
	^aCollection! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/7/2005 21:26'!
nextAllInBuffer
	"Return all data currently in the inBuffer,"

	^self nextInBuffer: inNextToWrite - lastRead - 1! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/7/2005 13:47'!
nextAvailable
	"Answer all the data currently available,
	in buffer or in socket."

	self isInBufferEmpty ifFalse: [^self nextAllInBuffer].
	self isDataAvailable ifTrue: [self receiveData].
	^self nextAllInBuffer! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/7/2005 13:02'!
nextAvailable: howMany
	"Answer all the data currently available,
	in buffer or in socket - but limited to <howMany>."

	self isInBufferEmpty ifFalse: [^self nextInBuffer: howMany].
	self isDataAvailable ifTrue: [self receiveData].
	^self nextInBuffer: howMany! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/7/2005 12:51'!
nextInBuffer: anInteger
	"Answer anInteger bytes of data at most,
	but only from the inBuffer."

	| start amount |
	amount := anInteger min: (inNextToWrite - lastRead - 1).
	start := lastRead + 1.
	lastRead := lastRead + amount.
	^inBuffer copyFrom: start to: lastRead! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 2/23/2010 13:06'!
nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 2/23/2010 13:06'!
nextInto: aCollection startingAt: startIndex
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex.! !

!SocketStream methodsFor: 'stream in' stamp: 'mir 2/21/2002 18:46'!
nextLine
	^self nextLineCrLf! !

!SocketStream methodsFor: 'stream in' stamp: 'jmv 3/13/2012 11:46'!
nextLineCrLf
	^self upToAll: String crlfString! !

!SocketStream methodsFor: 'stream in' stamp: 'jmv 3/13/2012 11:55'!
nextLineLf

	^self upToAll: String lfString! !

!SocketStream methodsFor: 'stream out' stamp: 'md 2/24/2006 19:51'!
nextPut: char
	"Put a single Character or byte onto the stream."

	| toPut |
	toPut := binary ifTrue: [char asInteger] ifFalse: [char asCharacter].
	self adjustOutBuffer: 1.
	outBuffer at: outNextToWrite put: toPut.
	outNextToWrite := outNextToWrite + 1.
	self checkFlush.
	"return the argument - added by kwl"
	^ char! !

!SocketStream methodsFor: 'stream out' stamp: 'nice 3/19/2010 19:14'!
nextPutAll: aCollection
	"Put a String or a ByteArray onto the stream.
	Currently a large collection will allocate a large buffer."

	| toPut |
	toPut := binary ifTrue: [aCollection asByteArray] ifFalse: [aCollection asString].
	self adjustOutBuffer: toPut size.
	outBuffer replaceFrom: outNextToWrite to: outNextToWrite + toPut size - 1 with: toPut startingAt: 1.
	outNextToWrite := outNextToWrite + toPut size.
	self checkFlush.
	^aCollection! !

!SocketStream methodsFor: 'stream out' stamp: 'cmm 1/28/2011 15:15'!
nextPutAllFlush: aCollection 
	"Put a String or a ByteArray onto the stream.
	You can use this if you have very large data - it avoids
	copying into the buffer (and avoids buffer growing)
	and also flushes any other pending data first."
	| toPut |
	toPut := binary
		ifTrue: [ aCollection asByteArray ]
		ifFalse: [ aCollection asString ].
	self flush.
	"first flush pending stuff, then directly send"
	socket isOtherEndClosed ifFalse:
		[ [ self
			sendData: toPut
			count: toPut size ]
			on: ConnectionTimedOut
			do: [ : ex | shouldSignal ifTrue: [ ex pass ] ] ]! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/10/2005 17:59'!
noTimeout
	"Do not use timeout."

	timeout := 0! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/10/2005 17:59'!
outBufferSize
	"Answers the current size of data in the outBuffer."

	^outNextToWrite - 1! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 1/13/2010 22:30'!
peek
	"Return next byte, if inBuffer is empty
	we recieve some more data and try again.
	Do not consume the byte."

	self atEnd ifTrue: [^nil].
	self isInBufferEmpty ifTrue:
		[self receiveData.
		self atEnd ifTrue: [^nil]].
	^inBuffer at: lastRead+1! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/15/2005 14:16'!
peek: anInteger
	"Answer anInteger bytes of data.
	Do not consume data.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!!"

	| start |
	self receiveData: anInteger.
	start := lastRead + 1.
	^inBuffer copyFrom: start to: ((lastRead + anInteger) min: inNextToWrite - 1).! !

!SocketStream methodsFor: 'stream in' stamp: 'jmv 2/9/2010 10:42'!
peekFor: aCharacterOrByte
	"Read and return next character or byte
	if it is equal to the argument.
	Otherwise return false."

	| nextObject |
	self atEnd ifTrue: [^false].
	self isInBufferEmpty ifTrue: 
		[self receiveData.
		self atEnd ifTrue: [^false]].
	nextObject := inBuffer at: lastRead + 1.
	nextObject = aCharacterOrByte ifTrue: [
		lastRead := lastRead + 1.
		^true].
	^false
! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/15/2005 14:16'!
peekForAll: aString
	"Answer whether or not the next string of characters in the receiver
	matches aString. If a match is made, advance over that string in the receiver and
	answer true. If no match, then leave the receiver alone and answer false.
	We use findString:startingAt: to avoid copying.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!!"

	| sz start |
	sz := aString size.
	self receiveData: sz.
	(inNextToWrite - lastRead - 1) < sz ifTrue: [^false].
	start := lastRead + 1.
	(inBuffer findString: aString startingAt: start) = start
		ifFalse: [^false].
	lastRead := lastRead + sz.
	^true! !

!SocketStream methodsFor: 'printing' stamp: 'md 7/14/2006 12:28'!
print: anObject
	anObject printOn: self! !

!SocketStream methodsFor: 'printing' stamp: 'gk 2/10/2005 11:44'!
printOn: aStream
	"Display buffer sizes."

	aStream nextPutAll: self class name.
	inBuffer ifNotNil: [
		aStream nextPutAll: '[inbuf:',
		(inBuffer size / 1024) rounded asString, 'kb/outbuf:',
		(outBuffer size / 1024) rounded asString, 'kb]']! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 11/22/2010 23:52'!
readInto: aCollection startingAt: startIndex count: anInteger
	"Read n objects into the given collection starting at startIndex. 
	Return number of elements that have been read."

	"Implementation note: This method DOES signal timeout if not 
	enough elements are received. It does NOT signal
	ConnectionClosed as closing the connection is the only way by
	which partial data can be read."

	| start amount |

	[self beSignalingWhile:[self receiveData: anInteger]]
		on: ConnectionClosed do:[:ex| ex return].

	"Inlined version of nextInBuffer: to avoid copying the contents"
	amount := anInteger min: (inNextToWrite - lastRead - 1).
	start := lastRead + 1.
	lastRead := lastRead + amount.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + amount-1 
		with: inBuffer 
		startingAt: start.
	^amount! !

!SocketStream methodsFor: 'private-socket' stamp: 'ar 7/24/2010 15:07'!
receiveAvailableData
	"Receive available data (as much as fits in the inBuffer) but not waiting for more to arrive. Return the position in the buffer where the new data starts, regardless if anything was read, see #adjustInBuffer."
	
	recentlyRead := self receiveDataInto: inBuffer startingAt: inNextToWrite.
	^self adjustInBuffer: recentlyRead! !

!SocketStream methodsFor: 'private-socket' stamp: 'mtf 1/15/2011 20:15'!
receiveData
	self waitForData.
	^self receiveAvailableData! !

!SocketStream methodsFor: 'control' stamp: 'gk 4/14/2005 09:49'!
receiveData: nBytes
	"Keep reading the socket until we have nBytes
	in the inBuffer or we reach the end. This method
	does not return data, but can be used to make sure
	data has been read into the buffer from the Socket
	before actually reading it from the FastSocketStream.
	Mainly used internally. We could also adjust the buffer
	to the expected amount of data and avoiding several
	incremental grow operations.

	NOTE: This method doesn't honor timeouts if shouldSignal
	is false!! And frankly, I am not sure how to handle that
	case or if I care - I think we should always signal."

	[self atEnd not and: [nBytes > self inBufferSize]]
		whileTrue: [self receiveData]! !

!SocketStream methodsFor: 'private-socket' stamp: 'ar 7/24/2010 14:50'!
receiveDataIfAvailable
	"Deprecated. Use #receiveAvailableData instead"

	^self receiveAvailableData! !

!SocketStream methodsFor: 'private-socket' stamp: 'ar 7/24/2010 15:07'!
receiveDataInto: buffer startingAt: index.
	"Read data from the underlying socket. This method may be overridden by subclasses wanting to control incoming traffic for other purposes like encryption or statistics."

	^socket  receiveAvailableDataInto: buffer startingAt: index.! !

!SocketStream methodsFor: 'control' stamp: 'gk 2/9/2005 23:08'!
recentlyRead
	"Return the number of bytes read
	during the last socket operation."
	
	^recentlyRead! !

!SocketStream methodsFor: 'private' stamp: 'gk 2/9/2005 22:36'!
resetBuffers
	"Recreate the buffers with default start sizes."

	inBuffer := self streamBuffer: bufferSize.
	lastRead := 0.
	inNextToWrite := 1.
	outBuffer := self streamBuffer: bufferSize.
	outNextToWrite := 1! !

!SocketStream methodsFor: 'private' stamp: 'gk 9/9/2005 02:29'!
resizeInBuffer: newSize
	"Resize the inBuffer by recreating it.
	This also has the effect of getting rid of
	dead data above inNextToWrite.
	<newSize> must >= inBuffer size!!"

	inBuffer := (self streamBuffer: newSize)
					replaceFrom: 1 to: inNextToWrite - 1 with: inBuffer startingAt: 1! !

!SocketStream methodsFor: 'stream out' stamp: 'jmv 3/13/2012 11:46'!
sendCommand: aString
	"Sends a String ending it with CR LF and then flush
	causing it to block until sent."

	self nextPutAll: aString, String crlfString; flush! !

!SocketStream methodsFor: 'private-socket' stamp: 'ar 7/24/2010 14:50'!
sendData: buffer count: n
	"Sends outgoing data directly on the underlying socket."

	^socket sendData: buffer count: n! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/10/2005 18:00'!
shouldSignal
	"If shouldSignal is enabled the Socket Exceptions
	ConnectionClosed and ConnectionTimedOut
	will not be swallowed. Default is true.
	For more info, see #shouldSignal:"

	^shouldSignal! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/10/2005 18:03'!
shouldSignal: aBoolean
	"If shouldSignal is enabled the Socket Exceptions
	ConnectionClosed and ConnectionTimedOut will not be swallowed.
	Default is true. And please - don't set it to false - it is better to
	use an exception handler (see below)  and several methods
	in this class will not honour timeouts (says so in their method comments).
	Also, it is quite hard to understand what for example #upToEnd
	should return to indicate a timeout.
	
	Wrap your use of SocketStream with a handler like:
	
	[stuff := mySocketStream next: 10]
		on: ConnectionClosed, ConnectionTimedOut
		do: [:ex |
			Transcript show: 'Oops!! Did not get my ten bytes!!;cr]
	"

	shouldSignal := aBoolean! !

!SocketStream methodsFor: 'testing' stamp: 'dvf 6/11/2003 18:21'!
shouldTimeout
	^self timeout > 0! !

!SocketStream methodsFor: 'private-socket' stamp: 'mtf 1/15/2011 20:09'!
signalClosed
	self shouldSignal ifFalse: [^ self]. 
	ConnectionClosed signal: 'Connection closed while waiting for data.'! !

!SocketStream methodsFor: 'private-socket' stamp: 'mtf 1/15/2011 20:10'!
signalTimeout
	self shouldSignal ifFalse: [^ self]. 
	ConnectionTimedOut signal: 'Data receive timed out.'! !

!SocketStream methodsFor: 'stream in' stamp: 'gk 2/15/2005 14:15'!
skip: anInteger
	"Skip a number of bytes.
	This is faster than #next: since it does not
	have to copy and return a new String or ByteArray.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!!"

	self receiveData: anInteger.
	lastRead := (lastRead + anInteger) min: inNextToWrite - 1! !

!SocketStream methodsFor: 'accessing' stamp: 'mir 10/31/2000 12:50'!
socket
	^socket! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/3/2005 20:35'!
socket: aSocket
	socket := aSocket! !

!SocketStream methodsFor: 'stream out' stamp: 'mir 5/8/2003 18:23'!
space
	self nextPut: Character space! !

!SocketStream methodsFor: 'private' stamp: 'gk 2/9/2005 22:35'!
streamBuffer: size
	"Create a buffer of the correct class and given size."

	^(self isBinary
		ifTrue: [ByteArray]
		ifFalse: [String]) new: size! !

!SocketStream methodsFor: 'configuration' stamp: 'gk 2/7/2005 08:41'!
timeout
	"Lazily initialized unless it has been set explicitly."

	timeout ifNil: [timeout := Socket standardTimeout].
	^timeout! !

!SocketStream methodsFor: 'accessing' stamp: 'mir 5/15/2003 20:50'!
timeout: seconds
	timeout := seconds! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 8/2/2010 18:50'!
upTo: aCharacterOrByte
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of anObject in the receiver. If  anObject is not in the collection, answer the entire rest of the receiver."

	"Note: The 100k limit below is compatible with the previous version though arguably incorrect. If you need unbounded behavior either up the argument or provide nil in which case we'll read until we get it or run out of memory"

	^self upTo: aCharacterOrByte limit: 100000! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 8/22/2010 13:27'!
upTo: aCharacterOrByte limit: nBytes
	"Return data up to, but not including given character or byte. If the character is not in the stream, or not found within nBytes answer the available contents of the stream"

	| target index result searchedSoFar |
	"Deal with ascii vs. binary"
	self isBinary 
		ifTrue:[target := aCharacterOrByte asInteger] 
		ifFalse:[target := aCharacterOrByte asCharacter].

	"Look in the current inBuffer first"
	index := inBuffer indexOf: target startingAt: lastRead + 1 ifAbsent:[0].

	(index > 0 and: [(index + 1) <= inNextToWrite]) ifTrue: ["found it"
		result := self nextInBuffer: index - lastRead - 1.
		self skip: 1.
		^ result
	].

	[searchedSoFar :=  self inBufferSize.
	"Receive more data"
	self receiveData.
	"We only get recentlyRead = 0 in the case of a non-signaling socket close."
	recentlyRead > 0] whileTrue:[
		"Data begins at lastRead + 1, we add searchedSoFar as offset."

		index := inBuffer indexOf: target
						startingAt: (lastRead + searchedSoFar + 1)
						ifAbsent:[0].
		(index > 0 and: [(index + 1) <= inNextToWrite]) ifTrue: ["found it"
			result := self nextInBuffer: index - lastRead - 1.
			self skip: 1.
			^ result
		].

		"Check if we've exceeded the max. amount"
		(nBytes notNil and:[inNextToWrite - lastRead > nBytes]) 
			ifTrue:[^self nextAllInBuffer].
	].

	"not found and (non-signaling) connection was closed"
	^self nextAllInBuffer! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 8/2/2010 18:48'!
upToAll: aStringOrByteArray
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	"Note: The 100k limit below is compatible with the previous version though arguably incorrect. If you need unbounded behavior either up the argument or provide nil in which case we'll read until we get it or run out of memory"

	^self upToAll: aStringOrByteArray limit: 100000! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 8/22/2010 13:32'!
upToAll: aStringOrByteArray limit: nBytes
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aStringOrByteArray. If aCollection is not in the stream, or not found within nBytes answer the available contents of the stream"

	| index sz result searchedSoFar target |
	"Deal with ascii vs. binary"
	self isBinary
		ifTrue:[target := aStringOrByteArray asByteArray]
		ifFalse:[target := aStringOrByteArray asString].

	sz := target size.
	"Look in the current inBuffer first"
	index := inBuffer indexOfSubCollection: target
						startingAt: lastRead - sz + 2.
	(index > 0 and: [(index + sz) <= inNextToWrite]) ifTrue: ["found it"
		result := self nextInBuffer: index - lastRead - 1.
		self skip: sz.
		^ result
	].

	[searchedSoFar :=  self inBufferSize.
	"Receive more data"
	self receiveData.
	recentlyRead > 0] whileTrue:[

		"Data begins at lastRead + 1, we add searchedSoFar as offset and 
		backs up sz - 1 so that we can catch any borderline hits."

		index := inBuffer indexOfSubCollection: target
						startingAt: (lastRead + searchedSoFar - sz + 2 max: 1).
		(index > 0 and: [(index + sz) <= inNextToWrite]) ifTrue: ["found it"
			result := self nextInBuffer: index - lastRead - 1.
			self skip: sz.
			^ result
		].
		"Check if we've exceeded the max. amount"
		(nBytes notNil and:[inNextToWrite - lastRead > nBytes]) 
			ifTrue:[^self nextAllInBuffer].
	].

	"not found and (non-signaling) connection was closed"
	^self nextAllInBuffer! !

!SocketStream methodsFor: 'stream in' stamp: 'ar 11/22/2010 23:49'!
upToEnd
	"Answer all data coming in on the socket until the socket
	is closed by the other end, or we get a timeout.
	This means this method catches ConnectionClosed by itself."

	[[self atEnd] whileFalse: [self beSignalingWhile:[self receiveData]]]
		on: ConnectionClosed
		do: [:ex | "swallow it"]. 
	^self nextAllInBuffer! !

!SocketStream methodsFor: 'private-socket' stamp: 'mtf 1/15/2011 20:16'!
waitForData
	"Wait for data. If shouldTimeout, we will time out if nothing arrives, otherwise we wait indefinitely"

	self shouldTimeout
		ifTrue: [socket waitForDataFor: self timeout
			ifClosed: [self signalClosed]
			ifTimedOut: [self signalTimeout]]
		ifFalse: [socket waitForDataIfClosed: [self signalClosed]]! !

!SocketStream class methodsFor: 'example' stamp: 'jmv 3/13/2012 12:47'!
finger: userName
	"SocketStream finger: 'stp'"

	| addr s |
	addr := NetNameResolver promptUserForHostAddress.
	s := SocketStream openConnectionToHost: addr port: 79.  "finger port number"
	Transcript show: '---------- Connecting ----------'; newLine.
	s sendCommand: userName.
	Transcript show: s nextLine.
	s close.
	Transcript show: '---------- Connection Closed ----------'; newLine; endEntry.
! !

!SocketStream class methodsFor: 'instance creation' stamp: 'gk 2/3/2005 22:19'!
on: socket
	"Create a socket stream on a connected server socket."

	^self basicNew initialize socket: socket! !

!SocketStream class methodsFor: 'instance creation' stamp: 'jmv 8/5/2011 17:26'!
openConnectionToHost: hostIP port: portNumber
	^ self openConnectionToHost: hostIP port: portNumber timeout: Socket standardTimeout! !

!SocketStream class methodsFor: 'instance creation' stamp: 'jmv 8/5/2011 17:26'!
openConnectionToHost: hostIP port: portNumber timeout: timeout
	| socket |
	socket := Socket new.
	socket connectTo: hostIP port: portNumber waitForConnectionFor: timeout.
	^self on: socket! !

!SocketStream class methodsFor: 'instance creation' stamp: 'gk 2/3/2005 20:35'!
openConnectionToHostNamed: hostName port: portNumber
	| hostIP |
	hostIP := NetNameResolver addressForName: hostName timeout: 20.
	^self openConnectionToHost: hostIP port: portNumber! !
NetNameResolver initialize!
Socket initialize!
