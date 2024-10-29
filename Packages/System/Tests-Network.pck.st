'From Cuis7.1 [latest update: #6742] on 25 October 2024 at 12:32:50 PM'!
'Description '!
!provides: 'Tests-Network' 1 1!
!requires: 'Network-Kernel' 1 10 nil!
!requires: 'Identities-UUID' 1 10 nil!
SystemOrganization addCategory: 'Tests-Network'!
SystemOrganization addCategory: 'Tests-Network-Kernel'!


!classDefinition: #NetNameResolverTest category: 'Tests-Network'!
TestCase subclass: #NetNameResolverTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Network'!
!classDefinition: 'NetNameResolverTest class' category: 'Tests-Network'!
NetNameResolverTest class
	instanceVariableNames: ''!

!classDefinition: #SocketStreamTest category: 'Tests-Network-Kernel'!
TestCase subclass: #SocketStreamTest
	instanceVariableNames: 'clientStream serverStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Network-Kernel'!
!classDefinition: 'SocketStreamTest class' category: 'Tests-Network-Kernel'!
SocketStreamTest class
	instanceVariableNames: ''!

!classDefinition: #SocketTest category: 'Tests-Network-Kernel'!
TestCase subclass: #SocketTest
	instanceVariableNames: 'listenerSocket clientSocket serverSocket'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Network-Kernel'!
!classDefinition: 'SocketTest class' category: 'Tests-Network-Kernel'!
SocketTest class
	instanceVariableNames: ''!


!NetNameResolverTest methodsFor: 'accessing' stamp: '9/24/2024 06:46'!
localHostAddressIsLoopBackAddress
		^NetNameResolver localHostAddress = NetNameResolver loopBackAddress! !

!NetNameResolverTest methodsFor: 'accessing' stamp: '9/24/2024 06:46'!
localHostNameIsLoopBackName
		^NetNameResolver localHostName = NetNameResolver loopBackName! !

!NetNameResolverTest methodsFor: 'testing' stamp: '9/24/2024 06:46'!
testLocalHostAddress
false ifTrue: [
	self skip. "This test is currently broken"
	NetNameResolver isConnected
		ifTrue: [self deny: self localHostAddressIsLoopBackAddress]
		ifFalse: [self assert: self localHostAddressIsLoopBackAddress]
]
		! !

!NetNameResolverTest methodsFor: 'testing' stamp: '9/24/2024 06:46'!
testLocalHostName
	self deny: NetNameResolver localHostName isNil.
	NetNameResolver isConnected
		ifTrue: [self deny: self localHostNameIsLoopBackName]
		ifFalse: [self assert: self localHostNameIsLoopBackName]! !

!SocketStreamTest methodsFor: 'running' stamp: 'ar 8/2/2010 18:10'!
setUp
	| listener clientSocket serverSocket |
	listener := Socket newTCP.
	[listener listenOn: 0 backlogSize: 4.

	clientSocket := Socket newTCP.
	clientSocket connectTo: #[127 0 0 1] port: listener localPort.
	clientSocket waitForConnectionFor: 1.
	self assert: clientSocket isConnected.

	serverSocket := listener waitForAcceptFor: 1.
	self assert: serverSocket isConnected.
	] ensure:[listener destroy].

	clientStream := SocketStream on: clientSocket.
	serverStream := SocketStream on: serverSocket.
! !

!SocketStreamTest methodsFor: 'running' stamp: 'ar 8/2/2010 17:59'!
tearDown
	clientStream ifNotNil:[clientStream destroy].
	serverStream ifNotNil:[serverStream destroy].! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/5/2010 12:20'!
testNextIntoClose
	"Ensure that #next:into: will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream next: 100 into: (String new: 100) startingAt: 1) 
		equals: 'A line of text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/5/2010 12:20'!
testNextIntoCloseNonSignaling
	"Ensure that #next:into: will function properly when the connection is closed"

	serverStream shouldSignal: false.
	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream next: 100 into: (String new: 100) startingAt: 1)
		equals: 'A line of text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:32'!
testUpTo
	"Tests correct behavior of #upTo:"

	clientStream nextPutAll:'A line of text', String cr, 'with more text'; flush.
	self assert: (serverStream upTo: Character cr) = 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String cr; flush] fork.
	self assert: (serverStream upTo: Character cr) = 'with more text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:32'!
testUpToAfterCloseNonSignaling
	"Tests correct behavior of #upToAll"

	| resp |
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	self shouldnt: [resp := serverStream upTo: Character cr] raise: ConnectionClosed.
	self assert: resp = 'A line of text'.! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:32'!
testUpToAfterCloseSignaling
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text'.
	clientStream close.
	self should: [serverStream upTo: Character cr] raise: ConnectionClosed.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:29'!
testUpToAll
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text', String crlf, 'with more text'; flush.
	self assert: (serverStream upToAll: String crlf) = 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String crlf; flush] fork.
	self assert: (serverStream upToAll: String crlf) = 'with more text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:02'!
testUpToAllAfterCloseNonSignaling
	"Tests correct behavior of #upToAll"

	| resp |
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	self shouldnt: [resp := serverStream upToAll: String crlf] raise: ConnectionClosed.
	self assert: resp = 'A line of text'.! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:26'!
testUpToAllAfterCloseSignaling
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text'.
	clientStream close.
	self should: [serverStream upToAll: String crlf] raise: ConnectionClosed.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/22/2010 13:28'!
testUpToAllAsciiVsBinary
	"Tests correct behavior of #upToAll"

	serverStream ascii.
	clientStream nextPutAll:'A line of text', String crlf, 'with more text'; flush.
	self assert: (serverStream upToAll: #[13 10]) = 'A line of text'.

	serverStream binary.
	clientStream nextPutAll: String crlf; flush.
	self assert: (serverStream upToAll: String crlf) asString = 'with more text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:47'!
testUpToAllCrlfAscii
	"Tests correct behavior of #upToAll with a two-byte delimiter in ascii mode"
	self testUpToAllDelimiter: String crlf
		input: 'A header', String crlf, 'and a body'
		expected: {'A header'. 'and a body'}
		binary: false.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:48'!
testUpToAllCrlfBinary
	"Tests correct behavior of #upToAll with a two-byte delimiter in binary mode"
	self testUpToAllDelimiter: String crlf
		input: 'A header', String crlf, 'and a body'
		expected: {'A header'. 'and a body'}
		binary: true.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:47'!
testUpToAllCrlfCrlfAscii
	"Tests correct behavior of #upToAll with a four-byte delimiter in ascii mode"
	self testUpToAllDelimiter: String crlfcrlf
		input: 'A header', String crlfcrlf, 'and a body'
		expected: {'A header'. 'and a body'}
		binary: false.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:47'!
testUpToAllCrlfCrlfBinary
	"Tests correct behavior of #upToAll with a four-byte delimiter in binary mode"
	self testUpToAllDelimiter: String crlfcrlf
		input: 'A header', String crlfcrlf, 'and a body'
		expected: {'A header'. 'and a body'}
		binary: true.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:39'!
testUpToAllDelimiter: delimiter input: input expected: aCollection binary: useBinary
	"General test of #upToAll."
	clientStream nextPutAll: input; close.
	serverStream shouldSignal: false.
	useBinary ifTrue: [serverStream binary] ifFalse: [serverStream ascii].
	aCollection do: [:expected | | actual |
		actual := (serverStream upToAll: delimiter) asString.
		self assert: actual = expected].
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:41'!
testUpToAllEmptyPatternAscii
	"Tests correct behavior of #upToAll with an empty delimiter string, in ascii mode"
	self testUpToAllDelimiter: ''
		input: 'xaxbxc'
		expected: {'xaxbxc'}
		binary: false.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:41'!
testUpToAllEmptyPatternBinary
	"Tests correct behavior of #upToAll with an empty delimiter string, in binary mode"
	self testUpToAllDelimiter: ''
		input: 'xaxbxc'
		expected: {'xaxbxc'}
		binary: true.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:48'!
testUpToAllLimit
	"Tests correct behavior of #upToAll:limit:"

	clientStream nextPutAll:'A line of text'; flush.
	self assert: (serverStream upToAll: String crlf limit: 5) = 'A line of text'.! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:43'!
testUpToAllLongPatternAscii
	"Tests correct behavior of #upToAll with a long delimiter string, in ascii mode"
	self testUpToAllDelimiter: 'xxxxx'
		input: 'xxxxxaxxxxbxxxxxc'
		expected: {''. 'axxxxb'. 'c'}
		binary: false.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:43'!
testUpToAllLongPatternBinary
	"Tests correct behavior of #upToAll with a long delimiter string, in binary mode"
	self testUpToAllDelimiter: 'xxxxx'
		input: 'xxxxxaxxxxbxxxxxc'
		expected: {''. 'axxxxb'. 'c'}
		binary: true.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:45'!
testUpToAllMediumPatternAscii
	"Tests correct behavior of #upToAll with a two-character delimiter string, in ascii mode"
	self testUpToAllDelimiter: 'xx'
		input: 'xxaxbxxc'
		expected: {''. 'axb'. 'c'}
		binary: false.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:45'!
testUpToAllMediumPatternBinary
	"Tests correct behavior of #upToAll with a two-character delimiter string, in binary mode"
	self testUpToAllDelimiter: 'xx'
		input: 'xxaxbxxc'
		expected: {''. 'axb'. 'c'}
		binary: true.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:40'!
testUpToAllShortPatternAscii
	"Tests correct behavior of #upToAll with a short delimiter string, in ascii mode"
	self testUpToAllDelimiter: 'x'
		input: 'xaxbxc'
		expected: {''. 'a'. 'b'. 'c'}
		binary: false.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:42'!
testUpToAllShortPatternAscii2
	"Tests correct behavior of #upToAll with a short delimiter string, in ascii mode"
	self testUpToAllDelimiter: 'x'
		input: 'axbxcx'
		expected: {'a'. 'b'. 'c'. ''}
		binary: false.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:38'!
testUpToAllShortPatternBinary
	"Tests correct behavior of #upToAll with a short delimiter string, in binary mode"
	self testUpToAllDelimiter: 'x'
		input: 'xaxbxc'
		expected: {''. 'a'. 'b'. 'c'}
		binary: true.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'tonyg 2/19/2017 11:42'!
testUpToAllShortPatternBinary2
	"Tests correct behavior of #upToAll with a short delimiter string, in binary mode"
	self testUpToAllDelimiter: 'x'
		input: 'axbxcx'
		expected: {'a'. 'b'. 'c'. ''}
		binary: true.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:11'!
testUpToAllTimeout
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text'.
	serverStream timeout: 1.
	self should: [serverStream upToAll: String crlf] raise: ConnectionTimedOut.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/22/2010 13:28'!
testUpToAsciiVsBinary
	"Tests correct behavior of #upTo:"

	serverStream ascii.
	clientStream nextPutAll:'A line of text', String cr, 'with more text'; flush.
	self assert: (serverStream upTo: 13) = 'A line of text'.

	serverStream binary.
	clientStream nextPutAll: String cr; flush.
	self assert: (serverStream upTo: Character cr) asString = 'with more text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/5/2010 12:20'!
testUpToEndClose
	"Ensure that #upToEnd will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd) 
		equals: 'A line of text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/5/2010 12:20'!
testUpToEndCloseNonSignaling
	"Ensure that #upToEnd will function properly when the connection is closed"

	serverStream shouldSignal: false.
	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd)
		equals: 'A line of text'.
! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:49'!
testUpToMax
	"Tests correct behavior of #upToAll:max:"

	clientStream nextPutAll:'A line of text'; flush.
	self assert: (serverStream upTo: Character cr limit: 5) = 'A line of text'.! !

!SocketStreamTest methodsFor: 'tests - stream protocol' stamp: 'ar 8/2/2010 18:32'!
testUpToTimeout
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text'.
	serverStream timeout: 1.
	self should: [serverStream upTo: Character cr] raise: ConnectionTimedOut.
! !

!SocketTest methodsFor: 'tests' stamp: 'eem 9/25/2020 13:07'!
testClientConnect
	"Tests a client socket connection"

	clientSocket := Socket newTCP.
	clientSocket connectTo: (self listenerAddressForFamily: clientSocket addressFamily) port: self listenerPort.
	clientSocket waitForConnectionFor: 2.
	self assert: clientSocket isConnected! !

!SocketTest methodsFor: 'tests' stamp: 'ar 11/26/2010 11:08'!
testDataReceive
	"Test data transfer and related methods"

	self testDataSending.
	"It can take a tad for the status change to be visible"
	(Delay forMilliseconds: 200) wait.
	self assert: serverSocket dataAvailable.
	self assert: (serverSocket receiveData = 'Hello World').
	self deny: (serverSocket dataAvailable).
! !

!SocketTest methodsFor: 'tests' stamp: 'ar 11/26/2010 11:06'!
testDataSending
	"Test data transfer and related methods"

	self testServerAccept.
	clientSocket sendData: 'Hello World'.
	clientSocket waitForSendDoneFor: 2.
	self assert: clientSocket sendDone.

! !

!SocketTest methodsFor: 'tests' stamp: 'eem 9/25/2020 13:09'!
testLocalAddress
	"Tests the various localAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket localAddress equals: (self listenerAddressForFamily: listenerSocket addressFamily).
	self assert: clientSocket localAddress equals: (self listenerAddressForFamily: clientSocket addressFamily).
	self assert: serverSocket localAddress equals: (self listenerAddressForFamily: serverSocket addressFamily)! !

!SocketTest methodsFor: 'tests' stamp: 'ar 11/26/2010 10:25'!
testLocalPort
	"Tests the various localPort values for sockets"

	self testServerAccept.
	self assert: listenerSocket localPort = self listenerPort.
	self assert: clientSocket localPort > 0.
	self assert: serverSocket localPort > 0.
! !

!SocketTest methodsFor: 'tests' stamp: 'topa 9/7/2015 23:39'!
testPeerName
	"None of these should throw an exception."
	"This can actually take a while, depending on networks availability"
	<timeout: 30>

	Socket new peerName.
	self testServerAccept.
	listenerSocket peerName.
	clientSocket peerName.
	serverSocket peerName.! !

!SocketTest methodsFor: 'tests' stamp: 'ar 9/4/2011 15:45'!
testReceiveTimeout
	"Test data transfer and related methods"

	self testServerAccept.
	self assert: (serverSocket receiveDataTimeout: 1) isEmpty.! !

!SocketTest methodsFor: 'tests' stamp: 'dtl 6/3/2012 19:23'!
testRemoteAddress
	"Tests the various remoteAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket remoteAddress asByteArray = #[0 0 0 0].
	self assert: clientSocket remoteAddress = self listenerAddress.
	self assert: serverSocket remoteAddress = self listenerAddress.
! !

!SocketTest methodsFor: 'tests' stamp: 'ar 11/26/2010 10:25'!
testRemotePort
	"Tests the various remoteAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket remotePort = 0.
	self assert: clientSocket remotePort = self listenerPort.
	self assert: serverSocket remotePort > 0.
! !

!SocketTest methodsFor: 'tests' stamp: 'dtl 11/3/2012 14:11'!
testSendTimeout
	"Test data transfer and related methods"

	| buffer ex |
	self testServerAccept.
	buffer := ByteArray new: 1000.

	"Write to the socket until the platform reports that sending is not complete."
	[serverSocket sendDone] whileTrue:[
		serverSocket sendSomeData: buffer.
	].

	"The network layer is now either blocked or in the process of sending data in its buffers.
	It may or may not be able buffer additional write requests, depending on the platform
	implemention. Keep sending data until the network reports that it is unable to process
	the request, at which time a exception will be raised. On Windows, the exception will
	be raised on the next write request, while unix platforms may provide additional buffering
	that permit write requests to continue being accepted."
	ex := nil.
	[[serverSocket sendSomeData: buffer startIndex: 1 count: buffer size for: 1]
		on: ConnectionTimedOut
		do: [ :e | ex := e ].
	ex isNil] whileTrue: [].
	self assert: ex notNil.
! !

!SocketTest methodsFor: 'tests' stamp: 'ar 11/26/2010 10:25'!
testServerAccept
	"Tests a server-side accept"

	self testClientConnect.
	serverSocket := listenerSocket waitForAcceptFor: 2.
	self assert: (serverSocket notNil).
	self assert: (serverSocket isConnected).
! !

!SocketTest methodsFor: 'tests' stamp: 'eem 10/9/2020 01:19'!
testSocketReuse
	"Test for SO_REUSEADDR/SO_REUSEPORT. Should probably be called testUDPSocketReuse.
	 c.f. testTCPSocketReuse"

	| udp1 udp2 sendProc recvProc  |
	[
		| address port opt send1 recv2 received sent |
		address := #[255 255 255 255]. "broadcast"
		port := 31259.
		udp1 := Socket newUDP.
		udp1 setOption: 'SO_REUSEADDR' value: 1.
		self assert: 0 equals: udp1 socketError description: 'Error occured while setting SO_REUSEADDR'.
		opt := udp1 getOption: 'SO_REUSEADDR'.
		self assert: opt first isZero & opt last isZero not description: 'SO_REUSEADDR couldn''t be set'.
		udp1 setOption: 'SO_REUSEPORT' value: 1.
		self assert: 0 equals: udp1 socketError description: 'Error occured while setting SO_REUSEPORT'.
		opt := udp1 getOption: 'SO_REUSEPORT'.
		self assert: opt first isZero & opt last isZero not description: 'SO_REUSEPORT couldn''t be set'.
		udp1 setPort: port.
		self assert: port equals: udp1 localPort.
		udp1 setOption: 'SO_BROADCAST' value: 1.
		send1 := UUID new.

		udp2 := Socket newUDP.
		udp2 setOption: 'SO_REUSEADDR' value: 1.
		self assert: 0 equals: udp2 socketError.
		udp2 setOption: 'SO_REUSEPORT' value: 1.
		self assert: 0 equals: udp2 socketError.
		udp2 setPort: port.
		self assert: port equals: udp2 localPort.
		udp2 setOption: 'SO_BROADCAST' value: 1.
		recv2 := UUID new.

		received := 0.
		recvProc := [
			[received < 16] whileTrue:[
				received := received + (udp2 receiveDataInto: recv2 startingAt: received + 1)
				"No need to yield here, because #receiveDataInto:startingAt: will either wait on the readSemaphore of the socket or signal an error." ]
			] newProcess.
		sendProc := [
			udp1 setPeer: address port: port.
			sent := (udp1 sendSomeData: send1 startIndex: 1 count: 16 for: 1).
		] newProcess.
		recvProc resume.
		sendProc resume.
		(Delay forMilliseconds: 200) wait.
		self 
			assert: sendProc isTerminated description: 'sendProc hasn''t terminated till the deadline';
			assert: recvProc isTerminated description: 'recvProc hasn''t terminated till the deadline';
			assert: 16 equals: sent description: ('{1} bytes were sent instead of 16' format: { sent });
			assert: send1 equals: recv2  description: 'sent and received bytes differ'
	] ensure:[ 
		udp1 ifNotNil: [ udp1 destroy ].
		udp2 ifNotNil: [ udp2 destroy ].
		sendProc ifNotNil: [ sendProc terminate ].
		recvProc ifNotNil: [ recvProc terminate ]
	].
! !

!SocketTest methodsFor: 'tests' stamp: 'dtl 7/17/2012 08:08'!
testStringFromAddress
	"Addresses are represented by a ByteArray if NetNameResolver useOldNetwork
	is true, or by by SocketAddress otherwise. Ensure the #stringFromAddress: works
	in either case. Older versions of SocketPlugin in the VM do not provide support
	for SocketAddress, and ByteArray addresses are used in that case."

	| localAddress localAddressBytes localName1 localName2 |
	localAddress := NetNameResolver localHostAddress. "ByteArray or SocketAddress"
	localAddressBytes := localAddress asByteArray.
	localName1 := NetNameResolver stringFromAddress: localAddress.
	localName2 := NetNameResolver stringFromAddress: localAddressBytes.
	self assert: localName1 = localName2
! !

!SocketTest methodsFor: 'tests' stamp: 'eem 10/9/2020 01:17'!
testTCPSocketReuse
	"Test for SO_REUSEADDR/SO_REUSEPORT using TCP sockets.  c.f. testSocketReuse"

	| tcpSend tcpRecv sendProcess recvProcess  |
	[
		|  address port opt send1 recv2 sent |
		address := NetNameResolver addressForName: '127.0.0.1' timeout: 20.
		port := 31259.
		tcpSend := Socket newTCP.
		tcpSend setOption: 'SO_REUSEADDR' value: 1.
		self assert: 0 equals: tcpSend socketError description: 'Error occured while setting SO_REUSEADDR'.
		opt := tcpSend getOption: 'SO_REUSEADDR'.
		self assert: opt first isZero & opt last isZero not description: 'SO_REUSEADDR couldn''t be set'.
		tcpSend setOption: 'SO_REUSEPORT' value: 1.
		self assert: 0 equals: tcpSend socketError description: 'Error occured while setting SO_REUSEPORT'.
		opt := tcpSend getOption: 'SO_REUSEPORT'.
		self assert: opt first isZero & opt last isZero not description: 'SO_REUSEPORT couldn''t be set'.
		"tcpSend setOption: 'TCP_NODELAY' value: 1."
		send1 := UUID new.

		tcpRecv := Socket newTCP.
		tcpRecv setOption: 'SO_REUSEADDR' value: 1.
		self assert: 0 equals: tcpRecv socketError.
		tcpRecv setOption: 'SO_REUSEPORT' value: 1.
		self assert: 0 equals: tcpRecv socketError.
		tcpRecv setPort: port.
		self assert: port equals: tcpRecv localPort.
		recv2 := UUID new.

		[| received |
		 recvProcess := Processor activeProcess.
		 received := 0.
		 tcpRecv waitForConnectionFor: 200.
		 [received < 16] whileTrue:
			["No need to yield here, because #receiveDataInto:startingAt: will either wait on the readSemaphore of the socket or signal an error." 
			 received := received + (tcpRecv receiveDataInto: recv2 startingAt: received + 1)]] fork.
		[sendProcess := Processor activeProcess.
		 tcpSend connectTo: address port: port.
		 sent := tcpSend sendData: send1] fork.
		(Delay forMilliseconds: 200) wait.
		self 
			assert: sendProcess isTerminated description: 'sendProc hasn''t terminated till the deadline';
			assert: recvProcess isTerminated description: 'recvProc hasn''t terminated till the deadline';
			assert: 16 equals: sent description: ('{1} bytes were sent instead of 16' format: { sent });
			assert: send1 equals: recv2  description: 'sent and received bytes differ']
	ensure:
		[tcpSend ifNotNil: [ tcpSend destroy ].
		tcpRecv ifNotNil: [ tcpRecv destroy ].
		sendProcess ifNotNil: [ sendProcess terminate ].
		recvProcess ifNotNil: [ recvProcess terminate ]]! !

!SocketTest methodsFor: 'tests' stamp: 'ar 9/4/2011 15:50'!
testUDP
	"Test udp recv() and send() functionality"

	serverSocket := Socket newUDP.
	serverSocket setPort: 54321.

	clientSocket := Socket newUDP.
	clientSocket setPeer: NetNameResolver localHostAddress port: serverSocket port.
	clientSocket sendData: 'Hello World'.

	(Delay forMilliseconds: 200) wait.

	self assert: (serverSocket dataAvailable).
	self assert: (serverSocket receiveData = 'Hello World').
! !

!SocketTest methodsFor: 'failures' stamp: 'mt 6/16/2022 16:15'!
expectedFailures

	^ Smalltalk platformName = 'Win32'
		ifTrue: [#(testSocketReuse testTCPSocketReuse)]
		ifFalse: [#()]! !

!SocketTest methodsFor: 'fixtures' stamp: 'topa 9/8/2015 00:02'!
listenerAddress
	^NetNameResolver addressForName: 'localhost'
! !

!SocketTest methodsFor: 'fixtures' stamp: 'eem 9/25/2020 13:10'!
listenerAddressForFamily: addressFamily
	^NetNameResolver addressForName: 'localhost' family: addressFamily! !

!SocketTest methodsFor: 'fixtures' stamp: 'ar 11/26/2010 10:13'!
listenerPort
	^42324
! !

!SocketTest methodsFor: 'running' stamp: 'ar 11/26/2010 10:24'!
setUp

	listenerSocket := Socket newTCP listenOn: self listenerPort backlogSize: 4 interface: self listenerAddress.
! !

!SocketTest methodsFor: 'running' stamp: 'ar 11/26/2010 10:24'!
tearDown

	listenerSocket ifNotNil:[listenerSocket destroy].
	clientSocket ifNotNil:[clientSocket destroy].
	serverSocket ifNotNil:[serverSocket destroy].
! !

!String class methodsFor: '*Tests-Network' stamp: 'JMM 10/25/2024 11:33:29'!
cr
	^ self crString! !

!String class methodsFor: '*Tests-Network' stamp: 'JMM 10/25/2024 11:35:25'!
crlf
	"Answer a string containing a carriage return and a linefeed."

	^ self crlfString! !

!String class methodsFor: '*Tests-Network' stamp: 'JMM 10/25/2024 11:36:49'!
crlfcrlf
	^self crlf , self crlf.
! !
