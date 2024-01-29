'From Cuis6.3 [latest update: #6189] on 9 January 2024 at 6:04:00 pm'!
'Description Description - This package is a port and refactoring of WebClient from http://ss3.gemstone.com/ss/WebClient.html with moderate changes by David Graham, Nicola Mingotti, Juan Vuletich and Hernan Wilkinson.
	- WebClient-Core-ul.100
	- WebClient-Help-ar.10
	- WebClient-Tests-topa.48'!
!provides: 'WebClient' 1 33!
!requires: 'Network-Kernel' 1 2 nil!
!requires: 'Identities-UUID' 1 7 nil!
!requires: 'Cryptography-DigitalSignatures' 1 7 nil!
SystemOrganization addCategory: #'WebClient-Core'!
SystemOrganization addCategory: #'WebClient-SqueakSSL-Core'!


!classDefinition: #WebChunkedStream category: #'WebClient-Core'!
ReadStream subclass: #WebChunkedStream
	instanceVariableNames: 'sourceStream chunkSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebChunkedStream class' category: #'WebClient-Core'!
WebChunkedStream class
	instanceVariableNames: ''!

!classDefinition: #SqueakSSLCertificateError category: #'WebClient-SqueakSSL-Core'!
Error subclass: #SqueakSSLCertificateError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-SqueakSSL-Core'!
!classDefinition: 'SqueakSSLCertificateError class' category: #'WebClient-SqueakSSL-Core'!
SqueakSSLCertificateError class
	instanceVariableNames: ''!

!classDefinition: #WebAuthRequired category: #'WebClient-Core'!
Exception subclass: #WebAuthRequired
	instanceVariableNames: 'client request response authParams message'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebAuthRequired class' category: #'WebClient-Core'!
WebAuthRequired class
	instanceVariableNames: ''!

!classDefinition: #SecureSocket category: #'WebClient-SqueakSSL-Core'!
Socket subclass: #SecureSocket
	instanceVariableNames: 'ssl decoded readBuf sendBuf'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-SqueakSSL-Core'!
!classDefinition: 'SecureSocket class' category: #'WebClient-SqueakSSL-Core'!
SecureSocket class
	instanceVariableNames: ''!

!classDefinition: #SecureSocketStream category: #'WebClient-SqueakSSL-Core'!
SocketStream subclass: #SecureSocketStream
	instanceVariableNames: 'ssl sendBuf readBuf decoded certIssues'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-SqueakSSL-Core'!
!classDefinition: 'SecureSocketStream class' category: #'WebClient-SqueakSSL-Core'!
SecureSocketStream class
	instanceVariableNames: ''!

!classDefinition: #WebClient category: #'WebClient-Core'!
Object subclass: #WebClient
	instanceVariableNames: 'flags server scheme timeout stream cookies proxyServer lastScheme lastServer lastPort maxRedirect redirections userAgent authParams proxyParams accessLog debugLog'
	classVariableNames: 'DebugLog FlagAcceptCookies FlagAllowAuth FlagAllowRedirect ProxyHandler'
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebClient class' category: #'WebClient-Core'!
WebClient class
	instanceVariableNames: ''!

!classDefinition: #WebCookie category: #'WebClient-Core'!
Object subclass: #WebCookie
	instanceVariableNames: 'name value path domain expiry version secure httpOnly comment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebCookie class' category: #'WebClient-Core'!
WebCookie class
	instanceVariableNames: ''!

!classDefinition: #WebMessage category: #'WebClient-Core'!
Object subclass: #WebMessage
	instanceVariableNames: 'stream protocol headers content'
	classVariableNames: 'StatusCodes'
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebMessage class' category: #'WebClient-Core'!
WebMessage class
	instanceVariableNames: ''!

!classDefinition: #WebRequest category: #'WebClient-Core'!
WebMessage subclass: #WebRequest
	instanceVariableNames: 'method request rawUrl server'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebRequest class' category: #'WebClient-Core'!
WebRequest class
	instanceVariableNames: ''!

!classDefinition: #WebResponse category: #'WebClient-Core'!
WebMessage subclass: #WebResponse
	instanceVariableNames: 'request status code url'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebResponse class' category: #'WebClient-Core'!
WebResponse class
	instanceVariableNames: ''!

!classDefinition: #WebServer category: #'WebClient-Core'!
Object subclass: #WebServer
	instanceVariableNames: 'listenerSocket listenerProcess interface port mutex connections entryPoints vault logAction nonceCache sessions serverString errorHandler accessLog certName siteUrl'
	classVariableNames: 'Default Registry'
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebServer class' category: #'WebClient-Core'!
WebServer class
	instanceVariableNames: ''!

!classDefinition: #WebSocket category: #'WebClient-Core'!
Object subclass: #WebSocket
	instanceVariableNames: 'name process stream handlers'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebSocket class' category: #'WebClient-Core'!
WebSocket class
	instanceVariableNames: ''!

!classDefinition: #WebSocket00 category: #'WebClient-Core'!
WebSocket subclass: #WebSocket00
	instanceVariableNames: 'frameType frameData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebSocket00 class' category: #'WebClient-Core'!
WebSocket00 class
	instanceVariableNames: ''!

!classDefinition: #WebSocket07 category: #'WebClient-Core'!
WebSocket subclass: #WebSocket07
	instanceVariableNames: 'frameType frameData closing masking'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebSocket07 class' category: #'WebClient-Core'!
WebSocket07 class
	instanceVariableNames: ''!

!classDefinition: #WebUtils category: #'WebClient-Core'!
Object subclass: #WebUtils
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-Core'!
!classDefinition: 'WebUtils class' category: #'WebClient-Core'!
WebUtils class
	instanceVariableNames: 'currentProxyServer'!

!classDefinition: #SqueakSSL category: #'WebClient-SqueakSSL-Core'!
Object subclass: #SqueakSSL
	instanceVariableNames: 'handle readBlock writeBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClient-SqueakSSL-Core'!
!classDefinition: 'SqueakSSL class' category: #'WebClient-SqueakSSL-Core'!
SqueakSSL class
	instanceVariableNames: ''!


!WebChunkedStream commentStamp: 'ar 1/31/2012 14:00' prior: 0!
Can process chunked data.!

!WebAuthRequired commentStamp: 'ar 2/10/2012 12:39' prior: 0!
Exception signaled when authentication is required.!

!SecureSocketStream commentStamp: 'ar 7/25/2010 14:19' prior: 0!
A variant on SocketStream supporting SSL/TLS encryption via SqueakSSL.
!

!WebClient commentStamp: 'jmv 6/25/2017 22:23:28' prior: 0!
WebClient provides a simple yet complete HTTP client implementation.

To retrieve the contents of a web page:
(WebClient httpGet: 'http://www.cuis-smalltalk.org/') content

INTRO
-------

The simplest form to use WebClient is by one of its convenience APIs:

	WebClient httpGet: 'http://www.squeak.org/'.
	WebClient httpPost: 'http://www.squeak.org/' content:'Hello Squeak' type: 'text/plain'.

For more elaborate use of headers and some other options in the request, a client can utilize modified variants:

	WebClient new httpGet: 'http://www.squeak.org/' do: [ :req|
		"Set an if-modified-since header"
		req headerAt: 'If-Modified-Since' put: 'Sat, 29 Oct 1994 19:43:31 GMT'.
		"Add several accept headers"
		req addHeader: 'Accept' value: 'text/plain'.
		req addHeader: 'Accept' value: 'application/x-foo-bar'.
		req addHeader: 'Accept' value: 'image/jpg'.
	].

The set of utility methods is limited to a few useful ones but it is easy to do the setup on your own:

	| url client request data |
	data := 'Hello Squeak'.								"POST data"
	url := 'http://www.squeak.org/'.						"POST url"
	client := WebClient new initializeFromUrl: url. 		"sets host etc"
	request := client requestWithUrl: url. 				"sets path etc"
	request method: 'POST'.							"sets method"
	request headerAt: 'Content-Length' put: data size.
	request headerAt: 'Content-Type' put: 'text/plain'.
	"... any other headers required ..."
	^client sendRequest: request 
		content: data readStream
		size: data size.

The utility methods like httpGet: etc. are similarly simple requests.

RESPONSES
--------------

The WebClient request methods return a WebResponse that the client can process:

	| resp |
	resp := WebClient httpGet: 'http://www.squeak.org/'.
	resp isSuccess ifFalse: [^self error: resp status].
	"Process the content from the response"
	^resp content

In addition, content can be streamed from the response so that it does not need to be downloaded all at once:

	| client resp |
	client := WebClient new.
	[
		resp := client httpGet: 'http://www.squeak.org/'.
		resp isSuccess ifFalse:[^self error: resp status].
		"Stream the content from the response"
		'page.html' asFileEntry forceWriteStreamDo: [ :file |
			resp streamTo: file 
				size: resp contentLength
				progress: [ :total :amount ]].
	] ensure: [ client close].

The progress block in the above can be omitted but has been included in this example to illustrate its usage. The block takes a total length (which can be nil if the length is not known) and the amount that has been loaded.

MULTIPLE REQUESTS
-------------------------

WebClient can and should be used for multiple requests to the same host. This will ensure persistent connections as well as having cookies processed properly within one session:

	| client resp |
	client := WebClient new.
	resp := client httpGet: 'http://www.squeak.org/'.
	resp := client httpGet: 'http://www.squeak.org/Download'.
	resp := client httpGet: 'http://www.squeak.org/Features'.
	client close.

One important issue to keep in mind is that because WebClient is optimized for persistent connections, you need to close it when you are done. That is not true for WebClient's class-side convenience APIs, which prefetch the response and close the socket. Generally speaking, whenever you say 'WebClient new' you need to close the client when you're done (however, you can do so by sending #close to a response you've received). For example:

	"Convenience API. Don't need to close, but prefetches result."
	WebClient httpGet: 'http://www.squeak.org'.

	| client resp |
	"Regular use. Create WebClient, return after header is read ..."
	client := WebClient new.
	[response := client httpGet: 'http://www.squeak.org/'.
	"... then fetch (or stream) the content ..."
	response content.
	] ensure:[
		"... and close the client when done."
		client close.
	].

AUTHENTICATION
---------------------

WebClient supports basic and digest authentication by default. WebClient delegates the retrieval of username/password to WebUtils which prompts the user for credentials. 

WebClient can either be supplied with specific credentials to be used or custom credentials handlers, for example:

	| client |
	client := WebClient new.
	client username: 'squeak'.
	client password: 'squeak'.
	client httpGet: 'http://www.squeak.org/protected'.

Proxy authentication works the same way as authentication but operates on a different authentication context to allow different sets of credentials to work.
!

!WebCookie commentStamp: 'jmv 6/25/2017 19:47:45' prior: 0!
WebCookie represents an http cookie for use by WebClient.!

!WebMessage commentStamp: 'ar 2/23/2010 10:37' prior: 0!
A common base class for WebRequest and WebResponse.!

!WebRequest commentStamp: 'ar 2/23/2010 10:38' prior: 0!
Represents an HTTP request for WebClient/WebServer.!

!WebResponse commentStamp: 'ar 2/23/2010 10:39' prior: 0!
Represents an HTTP response for WebClient/WebServer.!

!WebServer commentStamp: '<historical>' prior: 0!
WebServer provides a simple yet complete HTTP server implementation.

Example:
(WebServer new listenOn: 8080) addService: '/hello' action: [ :req | req send200Response: 'Hello World!!']!

!WebSocket commentStamp: '<historical>' prior: 0!
A wrapper object for the WebSocket API.!

!WebSocket00 commentStamp: 'ar 7/8/2010 18:21' prior: 0!
A wrapper object for the WebSocket API.!

!WebSocket07 commentStamp: '<historical>' prior: 0!
A wrapper object for the WebSocket API.!

!WebUtils commentStamp: 'ar 2/23/2010 10:39' prior: 0!
WebUtils contains a number of utility methods used by WebClient/WebServer.!

!SqueakSSL commentStamp: 'ar 7/16/2010 23:14' prior: 0!
SqueakSSL provides an interface to the platforms SSL/TLS facilities.
!

!WebChunkedStream methodsFor: 'testing' stamp: 'ar 1/31/2012 14:26'!
atEnd
	^chunkSize = 0 and:[super atEnd]! !

!WebChunkedStream methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:09'!
next
	"Answer the next decompressed object in the Stream represented by the
	receiver."

	<primitive: 65>
	position >= readLimit
		ifTrue: [^self pastEndRead]
		ifFalse: [^collection at: (position := position + 1)]! !

!WebChunkedStream methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:25'!
next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for simplicity"

	[(position + anInteger >= readLimit) and:[chunkSize ~= 0]] 
		whileTrue:[self nextChunk].

	^super next: anInteger
! !

!WebChunkedStream methodsFor: 'accessing' stamp: 'DSG 5/29/2012 13:40'!
nextChunk
	"Answer the next chunk from a message using chunked transfer encoding."

	| chunk |
	chunkSize = 0 ifTrue:[^'']. "read last chunk"
	chunkSize := Integer readFrom: (sourceStream upToAll: String crlfString) asString base: 16.
	chunkSize = 0 ifFalse:[chunk := sourceStream next: chunkSize].
	sourceStream skip: 2. "CrLf"
	(chunkSize + readLimit - position) <= collection size ifTrue:[
		collection replaceFrom: 1 to: (readLimit-position) with: collection startingAt: position+1.
		readLimit := readLimit - position.
		position := 0.
		collection replaceFrom: readLimit+1 to: readLimit + chunkSize with: chunk startingAt: 1.
		readLimit := readLimit + chunkSize.
	] ifFalse:[
		collection := collection, chunk.
		readLimit := readLimit + chunkSize.
	].
	^chunk
! !

!WebChunkedStream methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:11'!
pastEndRead
	"Need more data"

	self nextChunk.
	^self next! !

!WebChunkedStream methodsFor: 'initialize' stamp: 'ar 1/31/2012 14:24'!
on: aStream

	sourceStream := aStream.
	collection := (aStream isBinary ifTrue:[ByteArray] ifFalse:[String]) new.
	position := readLimit := 0.! !

!SqueakSSLCertificateError methodsFor: 'testing' stamp: 'ar 8/16/2011 19:18'!
isResumable
	"Determine whether an exception is resumable."

	^true! !

!WebAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:14'!
client
	"The client causing the exception"

	^client! !

!WebAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:49'!
message
	"Answer a default message for the user to ask for input"

	self isProxyAuth ifTrue:[
		^'The proxy server at "', client proxyServerName, '" requires authentication.'.
	].

	^'The server at "', client serverName, '" requires authentication.'.! !

!WebAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:04'!
request
	"The request object causing the exception"

	^request! !

!WebAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:05'!
response
	"The response object causing the exception"

	^response! !

!WebAuthRequired methodsFor: 'initialize' stamp: 'ar 2/10/2012 13:06'!
client: aWebClient request: aWebRequest response: aWebResponse
	"Initializes the exception"

	client := aWebClient.
	request := aWebRequest.
	response := aWebResponse.
! !

!WebAuthRequired methodsFor: 'initialize' stamp: 'ar 2/10/2012 13:13'!
username: username password: password
	"Use the given username/password as credentials"

	| params retry |
	params := self isProxyAuth ifTrue:[client proxyParams] ifFalse:[client authParams].
	retry := false.
	(params at: #username ifAbsent:[nil]) = username ifFalse:[
		params at: #username put: username.
		retry := true.
	].
	(params at: #password ifAbsent:[nil]) = password ifFalse:[
		params at: #password put: password.
		retry := true.
	].
	retry ifTrue:[self resume: true].
! !

!WebAuthRequired methodsFor: 'defaults' stamp: 'ar 2/10/2012 13:14'!
defaultAction

	self resume: false.! !

!WebAuthRequired methodsFor: 'defaults' stamp: 'ar 2/10/2012 13:07'!
defaultResumeValue
	"Resume with false by default"

	^false! !

!WebAuthRequired methodsFor: 'testing' stamp: 'ar 2/10/2012 13:09'!
isProxyAuth
	"Return true if proxy authorization is required"

	^response code = 407! !

!WebAuthRequired methodsFor: 'testing' stamp: 'ar 2/10/2012 13:09'!
isUnauthorized
	"Return true if authorization is required"

	^response code = 401! !

!WebAuthRequired class methodsFor: 'instance creation' stamp: 'ar 2/10/2012 13:16'!
client: aWebClient request: aWebRequest response: aWebResponse
	"Creates a new exception"

	^(self new)
		client: aWebClient 
		request: aWebRequest 
		response: aWebResponse! !

!SecureSocket methodsFor: 'accessing' stamp: 'ar 6/17/2012 11:07'!
certState
	^ssl ifNotNil:[ssl certState]! !

!SecureSocket methodsFor: 'accessing' stamp: 'ar 6/17/2012 11:07'!
peerName
	^ssl ifNotNil:[ssl peerName]! !

!SecureSocket methodsFor: 'accessing' stamp: 'ar 6/17/2012 11:05'!
ssl
	"Answer the SqueakSSL instance"
	^ssl! !

!SecureSocket methodsFor: 'primitives' stamp: 'ar 6/17/2012 12:00'!
decodeData
	"Receive data from the given socket into the given array starting at the given index. Return the number of bytes read or zero if no data is available."
	| total bytesRead |
	decoded atEnd ifFalse:[^self].

	"Decrypt more data if available"
	bytesRead := 0.
	[total := ssl decrypt: readBuf from: 1 to: bytesRead into: decoded originalContents.
	total < 0 ifTrue:[^self error: 'SSL error, code: ', total].
	bytesRead := 0.
	total = 0 ifTrue:[
		bytesRead := super primSocket: socketHandle receiveDataInto: readBuf startingAt: 1 count: readBuf size.
	].
	bytesRead = 0] whileFalse.

	"Update for number of bytes decoded"
	decoded setFrom: 1 to: total.
! !

!SecureSocket methodsFor: 'primitives' stamp: 'ar 6/19/2012 21:12'!
isConnected
	"Return true if this socket is connected."
	"We mustn't return false if there is data available"

	^super isConnected or:[self dataAvailable]! !

!SecureSocket methodsFor: 'primitives' stamp: 'ar 6/17/2012 12:00'!
primSocket: socketID receiveDataInto: buffer startingAt: index count: count
	"Receive data from the given socket into the given array starting at the given index. Return the number of bytes read or zero if no data is available."
	| total |

	ssl ifNil:[^super primSocket: socketID receiveDataInto: buffer startingAt: index count: count].

	self decodeData.

	"Push data from decoded into the result buffer"
	total := (decoded size - decoded position) min: (buffer size - index + 1).
	(decoded readInto: buffer startingAt: index count: total) = total 
		ifFalse:[self error: 'Unexpected read failure'].
	^total
! !

!SecureSocket methodsFor: 'primitives' stamp: 'ar 6/17/2012 11:00'!
primSocket: socketID sendData: buffer startIndex: start count: amount
	"Send data to the remote host through the given socket starting with the given byte index of the given byte array. The data sent is 'pushed' immediately. Return the number of bytes of data actually sent; any remaining data should be re-submitted for sending after the current send operation has completed."
	"Note: In general, it many take several sendData calls to transmit a large data array since the data is sent in send-buffer-sized chunks. The size of the send buffer is determined when the socket is created."

	| count |
	ssl ifNil:[^super primSocket: socketID sendData: buffer startIndex: start count: amount].

	count := ssl encrypt: buffer from: start to: start+amount-1 into: sendBuf.
	count < 0 ifTrue:[self error: 'SSL Error: ', count].
	^super primSocket: socketID sendData: sendBuf startIndex: 1 count: count! !

!SecureSocket methodsFor: 'primitives' stamp: 'ar 6/17/2012 12:01'!
primSocketReceiveDataAvailable: socketID
	"Return true if data may be available for reading from the current socket."

	ssl ifNil:[^super primSocketReceiveDataAvailable: socketID].
	self decodeData.
	^decoded atEnd not! !

!SecureSocket methodsFor: 'initialize' stamp: 'ar 6/17/2012 11:08'!
destroy
	ssl ifNotNil:[
		ssl destroy.
		ssl := nil
	].
	super destroy.! !

!SecureSocket methodsFor: 'initialize' stamp: 'ar 6/17/2012 11:11'!
initialize

	super initialize.
	decoded := ReadStream on: (ByteArray new: 20000) from: 1 to: 0.
	sendBuf := ByteArray new: 4096.
	readBuf := ByteArray new: 4096.
! !

!SecureSocket methodsFor: 'connect' stamp: 'jmv 4/13/2023 15:49:41'!
sslAccept: certName
	"Perform the SSL server handshake. This method uses all the common SocketStream methods to adhere to the various timeout/signalling settings of SocketStream. It only installs the SSL instance after the handshake is complete."

	| squeakSSL result inbuf |
	inbuf := ''.
	squeakSSL := SqueakSSL new.
	squeakSSL certName: certName.

	"Perform the server handshake"
	[[squeakSSL isConnected] whileFalse:[
		"Read input"
		inbuf := self receiveData.
		result := squeakSSL accept: inbuf from: 1 to: inbuf size into: sendBuf.

		"Check for errors first"
		result < -1 ifTrue:[^self error: 'SSL accept failed with code: ', result printString].

		"If a token has been produced in the handshake, send it to the remote"
		result > 0 ifTrue:[self sendData: (sendBuf copyFrom: 1 to: result)].
	].
	"We are connected. From here on, encryption will take place."
	ssl := squeakSSL.
	] ifCurtailed:[
		"Make sure we destroy the platform handle if the handshake gets interrupted"
		squeakSSL destroy.
	].
! !

!SecureSocket methodsFor: 'connect' stamp: 'ul 10/15/2014 19:27'!
sslConnect
	"Perform the SSL client handshake. This method uses all the common SocketStream methods to adhere to the various timeout/signalling settings of SocketStream. It only installs the SSL instance after the handshake is complete."

	self sslConnectTo: nil! !

!SecureSocket methodsFor: 'connect' stamp: 'jmv 4/13/2023 15:49:46'!
sslConnectTo: serverName
	"Perform the SSL client handshake. This method uses all the common SocketStream methods to adhere to the various timeout/signalling settings of SocketStream. It only installs the SSL instance after the handshake is complete. If serverName is not nil, then try to use it for SNI."

	| inbuf squeakSSL result |
	inbuf := ''.
	squeakSSL := SqueakSSL new.
	serverName ifNotNil: [ squeakSSL serverName: serverName ].
	"Perform the SSL handshake"
	[[result := squeakSSL connect: inbuf from: 1 to: inbuf size into: sendBuf.
	result = 0] whileFalse:[
		"Check for errors first"
		result < -1 ifTrue:[^self error: 'SSL connect failed with code: ', result printString].

		"If a token has been produced in the handshake, send it to the remote"
		result > 0 ifTrue:[self sendData: (sendBuf copyFrom: 1 to: result)].

		"Read more input and repeat"
		inbuf := self receiveData.
	].
	"We are connected. From here on, encryption will take place."
	ssl := squeakSSL.
	] ifCurtailed:[
		"Make sure we destroy the platform handle if the handshake gets interrupted"
		squeakSSL destroy.
	].
! !

!SecureSocket class methodsFor: 'examples' stamp: 'jmv 6/26/2017 18:57:14'!
google: query
	"An example HTTPS query to encrypted.google.com.
	Example:
		SecureSocket google: 'squeak'.
		SecureSocket google: 'SqueakSSL'.
	"

	| hostName address socket |

	"Change the host name to try an https request to some other host"
	hostName := 'encrypted.google.com'.

	address := NetNameResolver addressForName: hostName.
	socket := SecureSocket newTCP.

	"Connect the TCP socket"
	socket connectTo: address port: 443.
	socket waitForConnectionFor: 10.

	["Handle the client handshake"
	socket sslConnectTo: hostName.

	"Verify that the cert is valid"
	socket certState = 0 ifFalse:[
		self error: 'The certificate is invalid (code: ', socket certState,')'.
	].

	"If the certificate is valid, make sure we're were we wanted to go"
	(socket peerName match: hostName) ifFalse:[
		self error: 'Host name mismatch: ', socket peerName.
	].

	"Send encrypted data"
	socket sendData:
		'GET /search?q=', query,' HTTP/1.0', String crlfString,
		'Host: ', hostName, String crlfString,
		'Connection: close', String crlfString,
		String crlfString.

	"Wait for the response"
	^String streamContents:[:s|
		[[true] whileTrue:[s nextPutAll: socket receiveData]]
			on: ConnectionClosed, ConnectionTimedOut do:[:ex| ex return].
	]] ensure:[socket destroy].
! !

!SecureSocketStream methodsFor: 'accessing' stamp: 'ar 7/25/2010 16:31'!
ascii
	"Switch to ASCII"

	super ascii.
	decoded := (ReadStream 
		on: decoded originalContents asString 
		from: 1 to: decoded size)
			position: decoded position;
			yourself.
! !

!SecureSocketStream methodsFor: 'accessing' stamp: 'ar 11/9/2010 11:09'!
binary
	"Switch to binary"

	super binary.
	decoded := (ReadStream 
		on: decoded originalContents asByteArray 
		from: 1 to: decoded size)
			position: decoded position;
			yourself.
! !

!SecureSocketStream methodsFor: 'accessing' stamp: 'ar 7/25/2010 16:31'!
certState
	"Returns the certificate verification bits. The returned value indicates
	whether the certificate is valid. The two standard values are:

		0 - The certificate is valid.
		-1 - No certificate has been provided by the peer.

	Otherwise, the result is a bit mask of the following values:

		1 - If set, there is an unspecified issue with the cert (generic error)
		2 - If set, the root CA is untrusted (usually a self-signed cert)
		4 - If set, the certificate is expired.
		8 - If set, the certificate is used for the wrong purpose
		16 - If set, the CN of the certificate is invalid.
		32 - If set, the certificate was revoked.

	"

	^ssl ifNotNil:[ssl certState]! !

!SecureSocketStream methodsFor: 'accessing' stamp: 'ar 7/25/2010 16:31'!
ignoredCertIssues
	"Answer the mask of 'acceptable issues' with certs. To completely ignore all cert issues use -1 which still ensures privacy (encryption) to the remote host, but does not guard against a man-in-the-middle attack (i.e., you cannot be sure that the remote host is what he says he is). The reasons are a bit mask consisting of the following values:
		1 - If set, there is an unspecified issue with the cert (generic error)
		2 - If set, the root CA is untrusted (usually a self-signed cert)
		4 - If set, the certificate is expired.
		8 - If set, the certificate is used for the wrong purpose
		16 - If set, the CN of the certificate is invalid.
		32 - If set, the certificate was revoked.
	"

	^certIssues! !

!SecureSocketStream methodsFor: 'accessing' stamp: 'ar 7/25/2010 16:31'!
ignoredCertIssues: reasonsMask
	"Set the mask of 'acceptable issues' with certs. To completely ignore all cert issues use -1 which still ensures privacy (encryption) to the remote host, but does not guard against a man-in-the-middle attack (i.e., you cannot be sure that the remote host is what he says he is). The reasons are a bit mask consisting of the following values:
		1 - If set, there is an unspecified issue with the cert (generic error)
		2 - If set, the root CA is untrusted (usually a self-signed cert)
		4 - If set, the certificate is expired.
		8 - If set, the certificate is used for the wrong purpose
		16 - If set, the CN of the certificate is invalid.
		32 - If set, the certificate was revoked.
	"

	certIssues := reasonsMask! !

!SecureSocketStream methodsFor: 'accessing' stamp: 'ar 7/25/2010 14:45'!
peerName
	"Returns the certificate name of the remote peer.
	The method only returns a name if the certificate has been verified."

	^ssl ifNotNil:[ssl peerName]! !

!SecureSocketStream methodsFor: 'accessing' stamp: 'ar 7/25/2010 16:32'!
ssl
	"The SqueakSSL instance"

	^ssl! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 11/23/2010 15:03'!
atEnd
	"Pre Squeak 4.2 compatibility"

	self receiveAvailableData.
	^super atEnd! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 7/25/2010 17:33'!
flush
	"Pre-Squeak 4.2 compatibility"

	((outNextToWrite > 1) and: [socket isOtherEndClosed not])
		ifTrue: [
			[self sendData: outBuffer count: outNextToWrite - 1]
				on: ConnectionTimedOut
				do: [:ex | shouldSignal ifFalse: ["swallow"]].
			outNextToWrite := 1]
! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 11/23/2010 15:03'!
isDataAvailable
	"Pre Squeak 4.2 compatibility"
 
	self isInBufferEmpty ifFalse: [^true].
	^self receiveAvailableData < inNextToWrite
! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 7/25/2010 17:35'!
nextPutAllFlush: aCollection
	"Pre Squeak 4.2 compatibility"

	| toPut |
	toPut := binary ifTrue: [aCollection asByteArray] ifFalse: [aCollection asString].
	self flush. "first flush pending stuff, then directly send"
	socket isOtherEndClosed ifFalse: [
		[self sendData: toPut count: toPut size]
			on: ConnectionTimedOut
			do: [:ex | shouldSignal ifFalse: ["swallow"]]]
! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 7/25/2010 17:32'!
receiveAvailableData
	"Pre Squeak 4.2 compatibility"
	
	recentlyRead := self receiveDataInto: inBuffer startingAt: inNextToWrite.
	^self adjustInBuffer: recentlyRead
! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 11/23/2010 15:03'!
receiveData: nBytes
	"Pre Squeak 4.2 compatibility"

	self receiveAvailableData.
	^super receiveData: nBytes.! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 7/25/2010 17:32'!
receiveDataIfAvailable
	"Pre Squeak 4.2 compatibility"

	^self receiveAvailableData
! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 11/23/2010 15:03'!
upToAll: aStringOrByteArray
	"Pre Squeak 4.2 compatibility"

	^self upToAll: aStringOrByteArray limit: 100000! !

!SecureSocketStream methodsFor: 'private-compat' stamp: 'ar 11/23/2010 15:03'!
upToAll: aStringOrByteArray limit: nBytes
	"Pre Squeak 4.2 compatibility"

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

!SecureSocketStream methodsFor: 'errors' stamp: 'ar 8/16/2011 10:21'!
certError: errorString code: reason
	"Signal an issue with a certificate. If the reason code matches the acceptable cert issues, continue, otherwise signal an error."

	(certIssues allMask: reason) ifTrue:[^self].
	^SqueakSSLCertificateError signal: errorString, '(code: ', reason, ')'.
! !

!SecureSocketStream methodsFor: 'initialize' stamp: 'ar 7/25/2010 17:30'!
close
	"Flush any data still not sent and take care of the socket."

	super close.
	ssl ifNotNil:[
		ssl destroy.
		ssl := nil.
	].! !

!SecureSocketStream methodsFor: 'initialize' stamp: 'ar 7/25/2010 17:34'!
destroy
	"Destroy the receiver and its underlying socket. Does not attempt to flush the output buffers. For a graceful close use SocketStream>>close instead."

	"Pre-4.2 compatibility. Should be 'super destroy' instead of 'socket destroy'"
	socket ifNotNil:[
		socket destroy.
		socket := nil.
	].

	ssl ifNotNil:[
		ssl destroy.
		ssl := nil.
	].! !

!SecureSocketStream methodsFor: 'initialize' stamp: 'ar 7/25/2010 15:58'!
initialize
	"Initialize the receiver"
	
	"I think 16k is the max for SSL frames so use a tad more"
	decoded := ReadStream on: (ByteArray new: 20000) from: 1 to: 0.

	super initialize.

	sendBuf := ByteArray new: 4096.
	readBuf := ByteArray new: 4096.
	certIssues := 0.
! !

!SecureSocketStream methodsFor: 'initialize' stamp: 'jmv 4/13/2023 15:49:51'!
sslAccept: certName
	"Perform the SSL server handshake. This method uses all the common SocketStream methods to adhere to the various timeout/signalling settings of SocketStream. It only installs the SSL instance after the handshake is complete."

	| squeakSSL result inbuf |
	inbuf := ''.
	squeakSSL := SqueakSSL new.
	squeakSSL certName: certName.

	"Perform the server handshake"
	[[squeakSSL isConnected] whileFalse:[
		"Read input"
		self receiveData.
		inbuf := self nextAvailable.
		result := squeakSSL accept: inbuf from: 1 to: inbuf size into: sendBuf.

		"Check for errors first"
		result < -1 ifTrue:[^self error: 'SSL accept failed with code: ', result printString].

		"If a token has been produced in the handshake, send it to the remote"
		result > 0 ifTrue:[
			self nextPutAll: (sendBuf copyFrom: 1 to: result).
			self flush.
		].
	].
	"There should be no pending data at this point, ensure it is so.
	XXXX: If you ever see this problem, please inform me."
	self isInBufferEmpty ifFalse:[self error: 'Unexpected input data'].
	"We are connected. From here on, encryption will take place."
	ssl := squeakSSL.
	] ifCurtailed:[
		"Make sure we destroy the platform handle if the handshake gets interrupted"
		squeakSSL destroy.
	].
! !

!SecureSocketStream methodsFor: 'initialize' stamp: 'ul 10/15/2014 19:20'!
sslConnect
	"Perform the SSL client handshake. This method uses all the common SocketStream methods to adhere to the various timeout/signalling settings of SocketStream. It only installs the SSL instance after the handshake is complete."

	self sslConnectTo: nil! !

!SecureSocketStream methodsFor: 'initialize' stamp: 'jmv 6/26/2017 18:55:27'!
sslConnectTo: serverName
	"Perform the SSL client handshake. This method uses all the common SocketStream methods to adhere to the various timeout/signalling settings of SocketStream. It only installs the SSL instance after the handshake is complete. If serverName is not nil, then try to use it for SNI."

	| inbuf squeakSSL result |
	inbuf := ''.
	squeakSSL := SqueakSSL new.
	serverName ifNotNil: [ squeakSSL serverName: serverName ].
	"Perform the SSL handshake"
	[[result := squeakSSL connect: inbuf from: 1 to: inbuf size into: sendBuf.
	result = 0] whileFalse:[
		"Check for errors first"
		result < -1 ifTrue:[^self error: 'SSL connect failed with code: ', result printString].

		"If a token has been produced in the handshake, send it to the remote"
		result > 0 ifTrue:[
			self nextPutAll: (sendBuf copyFrom: 1 to: result).
			self flush.
		].

		"Read more input and repeat"
		self receiveData.
		inbuf := self nextAvailable.
	].
	"There should be no pending data at this point, ensure it is so.
	XXXX: If you ever see this problem, please inform me."
	self isInBufferEmpty ifFalse:[self error: 'Unexpected input data'].
	"We are connected. From here on, encryption will take place."
	ssl := squeakSSL.
	] ifCurtailed:[
		"Make sure we destroy the platform handle if the handshake gets interrupted"
		squeakSSL destroy.
	].
! !

!SecureSocketStream methodsFor: 'initialize' stamp: 'ar 7/27/2010 21:09'!
verifyCert: hostName
	"Verifies the cert state and host name"

	| certFlags |
	certFlags := self certState.
	certFlags = -1 
		ifTrue:[^self certError: 'No certificate was provided' code: -1].
	certFlags = 0 
		ifFalse:[self certError: 'Invalid certificate' code: certFlags].
	(ssl peerName match: hostName) 
		ifFalse:[self certError: 'Host name mismatch' code: -1].! !

!SecureSocketStream methodsFor: 'private-socket' stamp: 'ar 11/15/2010 11:39'!
receiveData
	"This method drains the available decryption data before waiting for the socket"

	| pos |

	"Note: The loop here is necessary to catch cases where a TLS packet is
	split among TCP packets. In this case we would pull the first portion of
	the TLS packet here but receiveAvailableData would return nothing since
	the contents of the packet can't be decoded until the rest has come in."

	[pos := inNextToWrite.
	self receiveAvailableData.
	pos = inNextToWrite ifFalse:[^pos].

	"Pre-4.2 compatibility; should be 'super receiveData' instead."
	socket
		waitForDataFor: self timeout
		ifClosed: [self shouldSignal 
			ifTrue:[ConnectionClosed signal: 'Connection closed while waiting for data.']]
		ifTimedOut: [self shouldTimeout
			ifTrue:[ConnectionTimedOut signal: 'Data receive timed out.']].
	self isConnected] whileTrue.

	"Final attempt to read data if a non-signaling connection closes"
	^self receiveAvailableData.
! !

!SecureSocketStream methodsFor: 'private-socket' stamp: 'ar 11/17/2011 13:19'!
receiveDataInto: buffer startingAt: index
	"Read and decrypt the data from the underlying socket. "

	| count bytesRead |
	"While in handshake, use the superclass version"
	ssl ifNil:[
		"Pre-4.2 compatibility; should be 'super receiveDataInto: buffer startingAt: index'"
		^socket  receiveAvailableDataInto: buffer startingAt: index.
	].

	"Only decode more data if all the decoded contents has been drained"
	decoded atEnd ifTrue:[
		"Decrypt more data if available"
		bytesRead := 0.
		[count := ssl decrypt: readBuf from: 1 to: bytesRead into: decoded originalContents.
		count < 0 ifTrue:[^self error: 'SSL error, code: ', count].
		bytesRead := 0.
		count = 0 ifTrue:[
			bytesRead := socket receiveAvailableDataInto: readBuf startingAt: 1.
		].
		bytesRead = 0] whileFalse.
		"Update for number of bytes decoded"
		decoded setFrom: 1 to: count.
	].

	"Push data from decoded into the result buffer"
	count := (decoded size - decoded position) min: (buffer size - index + 1).
	(decoded next: count into: buffer startingAt: index) size < count
		ifTrue:[^self error: 'Unexpected read failure'].
	^count
! !

!SecureSocketStream methodsFor: 'private-socket' stamp: 'ar 7/25/2010 17:37'!
sendData: buffer count: n
	"Encrypts the data before sending it on the underlying socket.
	Breaks large chunks into 2k components to fit safely into ssl frame."

	| remain start amount count |
	"While in handshake, use the superclass version"
	ssl ifNil:[
		"Pre-4.2 compatibility; should be 'super sendData: buffer count: n' instead"
		^socket sendData: buffer count: n
	].

	"Break the input into reasonable chunks and send them"
	remain := n. start := 1.
	[remain > 0] whileTrue:[
		amount := remain min: 2048.
		count := ssl encrypt: buffer from: start to: start+amount-1 into: sendBuf.
		socket sendData: sendBuf count: count.
		remain := remain - amount.
		start := start + amount.
	].! !

!WebClient methodsFor: 'printing' stamp: 'ar 8/9/2010 08:50'!
printOn: aStream
	"Print the receiver on aStream"

	aStream nextPutAll: self class name.
	aStream nextPut: $(.
	(stream notNil and:[stream isConnected]) ifTrue:[
		aStream nextPutAll: 'Connected: '.
	].
	scheme ifNotNil:[aStream nextPutAll: scheme, '://'].
	server ifNotNil:[aStream nextPutAll: server].
	aStream nextPut:$).
! !

!WebClient methodsFor: 'cookies' stamp: 'ar 8/12/2010 21:40'!
acceptCookie: aCookie host: reqHost path: path
	"Accept a cookie from a prior response"

	self acceptCookies ifTrue:[
		"Set the defaults per RFC 2109 section 4.3.1 "
	
		"Ensure the cookie domain is set"
		aCookie domain ifNil:[aCookie domain: reqHost].

		"Ensure the cookie path is set"
		aCookie path ifNil:[aCookie path: path].

		"Reject the cookie per RFC 2109 section 4.3.2"
		(path beginsWith: aCookie path) ifFalse:[^false].

		aCookie domain = reqHost ifFalse:[ | domain |
			domain := aCookie domain.
			"For misconfigured servers, insert the leading dot into the domain"
			domain first = $. ifFalse:[aCookie domain: (domain := '.', domain)].
			((domain count:[:ch| ch = $.]) >= 2) ifFalse:[^false].
			(reqHost endsWith: domain) ifFalse:[^false].
			((reqHost allButLast: domain size) includes: $.) ifTrue:[^false].
		].
	
		"Remove any old cookies"
		cookies := cookies reject:[:any| any = aCookie].
		
		"Remember the cookie if not expired"
		(aCookie expiry == nil or:[aCookie expiry > DateAndTime now])  ifTrue:[
			cookies add: aCookie.
			^true
		].
	].
	^false! !

!WebClient methodsFor: 'cookies' stamp: 'ar 4/1/2010 18:04'!
acceptCookies
	"Whether we should accept cookies"

	^flags anyMask: FlagAcceptCookies! !

!WebClient methodsFor: 'cookies' stamp: 'ar 4/1/2010 18:05'!
acceptCookies: aBool
	"Whether we should accept cookies"

	flags := aBool ifTrue:[flags bitOr: FlagAcceptCookies] ifFalse:[flags bitClear: FlagAcceptCookies].
! !

!WebClient methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:49'!
accessLog
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	^accessLog! !

!WebClient methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:49'!
accessLog: aStreamOrFilename
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	accessLog := aStreamOrFilename! !

!WebClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:03'!
allowAuth
	"If true, WebClient will attempt to handle authorization requests"

	^flags anyMask: FlagAllowAuth! !

!WebClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:02'!
allowAuth: aBool
	"If true, WebClient will attempt to handle authorization requests"

	flags := aBool ifTrue:[flags bitOr: FlagAllowAuth] ifFalse:[flags bitClear: FlagAllowAuth].
! !

!WebClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:01'!
allowRedirect
	"If true, WebClient will attempt to handle redirect responses"

	^flags anyMask: FlagAllowRedirect! !

!WebClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:02'!
allowRedirect: aBool
	"If true, WebClient will attempt to handle redirect responses"

	flags := aBool ifTrue:[flags bitOr: FlagAllowRedirect] ifFalse:[flags bitClear: FlagAllowRedirect].
! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:13'!
authParams
	"The authentication parameters"

	^authParams! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:13'!
authParams: aDictionary
	"The authentication parameters"

	authParams := aDictionary! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/23/2010 10:31'!
cookies
	"The cookies that have been set for this session"

	^cookies! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/23/2010 10:31'!
cookies: aCollection
	"The cookies that have been set for this session"

	cookies := aCollection.! !

!WebClient methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:16'!
debugLog
	"If provided, WebClient will print all requests and responses to the debugLog,
	except from the content of the message. The debug log must be a stream of
	some sort; file names (like for the accessLog are not supported)."

	^debugLog! !

!WebClient methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:17'!
debugLog: aStream
	"If provided, WebClient will print all requests and responses to the debugLog,
	except from the content of the message. The debug log must be a stream of
	some sort; file names (like for the accessLog are not supported)."

	debugLog := aStream! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:12'!
maxRedirect
	"The max. number of redirects we allow for the SAME SITE before giving up.
	Redirects can happen for any number of reasons between any number of sites
	and we give up if we have seen the SAME SITE maxRedirect number of times."

	^maxRedirect! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:12'!
maxRedirect: aNumber
	"The max. number of redirects we allow for the SAME SITE before giving up.
	Redirects can happen for any number of reasons between any number of sites
	and we give up if we have seen the SAME SITE maxRedirect number of times."

	maxRedirect := aNumber! !

!WebClient methodsFor: 'accessing' stamp: 'ar 3/27/2010 12:01'!
password
	"The password for remote authentication.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	^authParams at: #password ifAbsent:[nil]! !

!WebClient methodsFor: 'accessing' stamp: 'ar 3/27/2010 01:32'!
password: aStringOrValuable
	"The password for remote authentication.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	^authParams at: #password put: aStringOrValuable! !

!WebClient methodsFor: 'accessing' stamp: 'ar 8/5/2010 19:50'!
scheme
	"The scheme used for the request (usually http or https)"

	^scheme! !

!WebClient methodsFor: 'accessing' stamp: 'ar 8/5/2010 19:50'!
scheme: aString
	"The scheme used for the request (usually http or https)"

	scheme := aString! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/21/2010 09:50'!
server
	"The server to use for connections.
	The server is specified as server:port if needed"

	^server! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/21/2010 09:50'!
server: aString
	"The server to use for connections.
	The server should be specified as server:port if needed"

	server := aString
! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/20/2010 19:22'!
serverName
	"Returns the name part of the server:port description"

	^server copyUpTo: $:! !

!WebClient methodsFor: 'accessing' stamp: 'NM 8/23/2021 16:36:46'!
serverPort
	"Returns the port of the server:port description"

	^(server copyAfter: $:) 
		ifEmpty:[self defaultPort]
		ifNotEmpty:[:portString| portString asNumber].
! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:12'!
timeout
	"Timeout for the http operations"

	^timeout! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:12'!
timeout: aNumber
	"Timeout for the http operations"

	timeout := aNumber! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:16'!
userAgent
	"The User-Agent string sent to the server"

	^userAgent! !

!WebClient methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:17'!
userAgent: aString
	"The User-Agent string sent to the server. 
	If no user agent should be sent, this value can be set to nil"

	userAgent := aString! !

!WebClient methodsFor: 'accessing' stamp: 'ar 3/24/2010 16:05'!
username
	"The username for remote authentication"

	^authParams at: #username ifAbsent:[nil]! !

!WebClient methodsFor: 'accessing' stamp: 'ar 3/24/2010 16:05'!
username: aString
	"The username for remote authentication"

	authParams at: #username put: aString! !

!WebClient methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:45'!
authDispatch: request from: response header: authHeader params: params
	"Dispatch on an authentication method. 
	Subclasses can extend this method to support more auth methods."

	(authHeader copyUpTo: Character space) caseOf: {
		['Basic'] -> [self basicAuth: request from: response 
							header: authHeader params: params].
		['Digest'] -> [self digestAuth: request from: response 
							header: authHeader  params: params].
	} otherwise:["ignore"].
! !

!WebClient methodsFor: 'authentication' stamp: 'jmv 5/26/2022 12:07:21'!
basicAuth: request from: response header: header params: params
	"Provide basic authentication for the request"

	| user pass args |
	authParams at: #authMethod ifPresent:[:method| ^self]. "do not retry repeatedly"
	authParams at: #authMethod put: 'Basic'.

	args := WebUtils parseAuthParams: header.
	args at: 'realm' ifPresent:[:realm| authParams at: #authRealm put: realm].
	
	user := (params at: #username ifAbsent:[nil]) ifNil:[^self].
	pass := (params at: #password ifAbsent:[nil]) ifNil:[^self].
	authParams at: #authResponse put: (user, ':', pass value) asUtf8Bytes base64Encoded! !

!WebClient methodsFor: 'authentication' stamp: 'ar 8/26/2010 07:48'!
digestAuth: request from: response header: authHeader params: params
	"Perform digest authentication"

	| realm nonce ha1 ha2 nc cnonce qop header uri md5 key args user pass |
	params at: #authMethod ifPresent:[:method| ^self]. "do not retry repeatedly"
	params at: #authMethod put: 'Digest'.

	args := WebUtils parseAuthParams: authHeader.
	realm := args at: 'realm'.
	authParams at: #authRealm put: realm.

	user := (params at: #username ifAbsent:[nil]) ifNil:[^self].
	pass := (params at: #password ifAbsent:[nil]) ifNil:[^self].

	nonce := args at: 'nonce'.
	uri := request rawUrl.

	"VERY IMPORTANT NOTE: Some servers fail horribly if the nonce count
	isn't precisely eight digits and without quotes."
	nc := args at: 'nc' put: (args at: 'nc' ifAbsent:[0]) + 1.
	nc := nc asString padded: #left to: 8 with: $0.
	
	key := user, ':', realm, ':', pass value.
	ha1 := WebUtils md5Digest: key.
	ha2 := WebUtils md5Digest: request method, ':', uri.
	cnonce := UUID new hex.

	(args includesKey: 'qop') ifTrue:["use qop"
		qop := 'auth'. "the only qop we support"
		md5 := WebUtils md5Digest: ha1, ':', nonce, ':', nc, ':', cnonce, ':', qop, ':', ha2.
		header :=
			'username="', user, '"',
			', realm="', realm, '"',
			', nonce="', nonce, '"',
			', uri="', uri, '"',
			', response="', md5, '"',
			', qop="', qop,'"',
			', nc="', nc, '"',
			', cnonce="', cnonce, '"'.
	] ifFalse:["ignore qop"
		md5 := WebUtils md5Digest: ha1, ':', nonce, ':', ha2.
		header :=
			'username="', user, '"',
			', realm="', realm, '"',
			', nonce="', nonce, '"',
			', uri="', uri, '"',
			', response="', md5, '"'.
	].
	args at: 'opaque' ifPresent: [:opaque | 
		header := header, ', opaque="', opaque, '"'
	].
	params at: #authResponse put: header.! !

!WebClient methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:46'!
flushAuthState: params
	"Fliush authentication state that should not been preserved 
	inbetween failed attempts to authenticate. 
	Subclasses can extend this method to support more auth methods."

	params removeKey: #authMethod ifAbsent:[].
! !

!WebClient methodsFor: 'sending' stamp: 'ar 2/10/2012 13:12'!
authenticate: request from: response
	"Authenticate after having received a 401/407 response.
	Returns true if we should retry, false if we fail here."

	"NOTE: The first time through we do NOT ask for credentials right away.
	Some authentication mechanisms (NTLM/Negotiate) can use the credentials
	of the currently logged on user. Consequently we only ask for credentials
	if we're unable to do so without asking. Methods that require credentials
	(basic, digest) test for their existence explicitly."

	| headers authHeader params |

	"Pick the right set of parameters"
	response code = 401 ifTrue:[
		params := authParams.
		headers := response headersAt: 'WWW-Authenticate'.
		"If the connection was closed, we need to flush the
		proxy params or we won't pick up prior credentials."
		self isConnected 
			ifFalse:[self flushAuthState: proxyParams].
	] ifFalse:[
		params := proxyParams.
		headers := response headersAt: 'Proxy-Authenticate'.
	].

	"Remove any old response"
	params removeKey: #authResponse ifAbsent:[].

	"Process the authentication header(s)"
	1 to: headers size do:[:i|
		authHeader := headers at: i.
		self authDispatch: request from: response header: authHeader params: params.
		"If we generated an authentication response for the header use it"
		params at: #authResponse ifPresent:[:resp|
			request headerAt: (response code = 401 
								ifTrue:['Authorization'] 
								ifFalse:['Proxy-Authorization'])
					put: (params at: #authMethod), ' ', resp.
			^true].
	].

	"If we fall through here this can have two reasons: One is that we don't have
	a suitable authentication method. Check for that first."
	params at: #authMethod ifAbsent:[^false].

	"The other possibility is that the credentials are wrong. 
	Clean out the previous auth state and go ask for credentials."
	self flushAuthState: params.

	"Clean out old authentication headers"
	response code = 401 
		ifTrue:[request removeHeader: 'Authorization'].
	"Always clean out the proxy auth header since we don't support pre-authentication"
	request removeHeader: 'Proxy-Authorization'.

	"Signal WebAuthRequired"
	(WebAuthRequired client: self request: request response: response)
		signal == true ifFalse:[^false].

	"And retry with the new credentials"
	^self authenticate: request from: response! !

!WebClient methodsFor: 'sending' stamp: 'ar 9/4/2010 11:10'!
redirect: request from: response
	"Handle a 3xx redirect response"

	| location max |
	
	"Check if we handle the specific type of redirect here"
	(#(	301 
		302 "Found" 
		303 "See Other" 
		307 "Temporary Redirect")
			includes: response code) ifFalse:[^false].

	"RFC 2616 states that requests other than HEAD and GET MUST NOT be auto
	redirected for 302 and 307 responses"
	(request method = 'GET' or:[request method = 'HEAD']) ifFalse:[
		(response code = 302 or:[response code = 307]) ifTrue:[^false].
	].

	"Find the new location"
	location := response headerAt: 'location'.

	"Check if we've exceeded max redirections"
	max := redirections at: location ifAbsent:[0].
	max > self maxRedirect ifTrue:[^false].
	redirections at: location put: max+1.

	"Do the actual redirect, i.e., set us up for the new url"
	(location findString: '://') > 0 "do we have a scheme:// url?"
		ifTrue:[self initializeFromUrl: location].

	request initializeFromUrl: location.

	^true! !

!WebClient methodsFor: 'sending' stamp: 'ar 2/25/2010 22:16'!
sendRequest: request
	"Send an http request"

	^self sendRequest: request content: nil size: 0! !

!WebClient methodsFor: 'sending' stamp: 'DSG 5/25/2012 23:35'!
sendRequest: request content: contentStream size: streamSize
	"Send an http request"

	^self sendRequest: request contentBlock:[:aStream|
		contentStream ifNotNil:[
			"Upload content if provided"
			contentStream position: 0.
			request streamFrom: contentStream to: aStream size: streamSize 
				progress:[:total :amount|
					(ProgressBarMorph new) 
						progressValue: amount / total.
						]]].
! !

!WebClient methodsFor: 'sending' stamp: 'CK 11/24/2017 14:32:10'!
sendRequest: request contentBlock: contentBlock
	"Send an http request"

	|  response repeatRedirect repeatAuth |

	"XXXX: Fixme. Pre-authenticate the request if we have valid auth credentials"

	redirections := Dictionary new.

	["The outer loop handles redirections"
	repeatRedirect := false.

	"Always update the host header due to redirect"
	request headerAt: 'Host' put: server.

		["The inner loop handles authentication"
		repeatAuth := false.

		"Connect can fail if SSL proxy CONNECT is involved"
		self connect ifNotNil:[:resp| ^resp].
		
		"Write the request to the debugLog if present"
		debugLog ifNotNil:[self writeRequest: request on: debugLog].

		"Send the request itself"
		self writeRequest: request on: stream.
		contentBlock value: stream.

		response := request newResponse readFrom: stream.
		response url: (scheme, '://', server, request rawUrl).

		debugLog ifNotNil:[
			response writeOn: debugLog.
			debugLog flush.
		].
		response setCookiesDo:[:cookie| 
			self acceptCookie: cookie host: self serverName path: request url.
		].
		accessLog ifNotNil:[
			WebUtils logRequest: request response: response on: accessLog
		].
		"Handle authentication if needed"
		(self allowAuth and:[response code = 401 or:[response code = 407]]) ifTrue:[
			"Eat up the content of the previous response"
			response content.
			repeatAuth := self authenticate: request from: response.
		].

		repeatAuth] whileTrue.

	"Flush previous authState.
	XXXX: Fixme. authState must be preserved for pre-authentication of requests."
	self flushAuthState.

	"Handle redirect if needed"
	(self allowRedirect and:[response isRedirect]) ifTrue:[
		"Eat up the content of the previous response"
		response content.
		repeatRedirect := self redirect: request from: response.
	].
	repeatRedirect] whileTrue:[
		"When redirecting, remove authentication headers"
		request removeHeader: 'Authorization'.
		request removeHeader: 'Proxy-Authorization'.
	].

	"If the response is not a success, eat up its content"
	(response isSuccess or:[response isInformational]) ifFalse:[response content].

	^response! !

!WebClient methodsFor: 'sending' stamp: 'ar 8/12/2010 21:37'!
writeCookiesFor: request on: aStream
	"Write the cookies for a particular request"

	(cookies select:[:ck| (request acceptsCookie: ck from: self )]) ifNotEmpty:[:ckset|
		aStream nextPutAll: 'Cookie: '.
		ckset 
			do:[:ck| aStream nextPutAll: ck name,'=', ck value]
			separatedBy:[aStream  nextPutAll:'; '].
		aStream crlf.
	].
! !

!WebClient methodsFor: 'sending' stamp: 'DSG 5/24/2012 17:24'!
writeHeadersFor: request on: aStream
	"Write all the headers for the given request on aStream"

	request headersDo:[:key :value| aStream nextPutAll: key, ': ', value asString; nextPutAll: String crlfString].
! !

!WebClient methodsFor: 'sending' stamp: 'CK 11/24/2017 14:43:55'!
writeRequest: request on: aStream
	"Write all the request headers on the given stream"

	self retryOnce: [
		self writeRequestLine: request on: aStream.
		self writeHeadersFor: request on: aStream.
		self writeCookiesFor: request on: aStream.
		aStream crlf.
		aStream flush].! !

!WebClient methodsFor: 'sending' stamp: 'CK 11/24/2017 14:37:08'!
writeRequestLine: request on: aStream
	"Write the HTTP request line on the given stream"

	aStream nextPutAll: request method; space.

	"https proxy support is transparent via CONNECT"
	(proxyServer notNil and:[scheme ~= 'https']) ifTrue:[
		"http://www.ietf.org/rfc/rfc2616.txt 
		5.1.2 Request-URI:
			...
		The absoluteURI form is REQUIRED when the request is being made to a
		proxy. The proxy is requested to forward the request or service it
		from a valid cache, and return the response. Note that the proxy MAY
		forward the request on to another proxy or directly to the server"
		aStream nextPutAll: scheme, '://', self server.

		"XXXX: This is a hack. Indicate that we want the proxy connection 
		to be persistent. This should NOT be necessary; the proxy should
		know that from the HTTP/1.1 request but apparently, squid will fail 
		NTLM and Negotiate authentication unless explicitly instructed to 
		keep the proxy connection alive."
		request headerAt: 'Proxy-Connection' put: 'keep-alive'.
	].

	aStream nextPutAll: request rawUrl; space; nextPutAll: request protocol; nextPutAll: String crlfString.! !

!WebClient methodsFor: 'initialize' stamp: 'topa 6/17/2016 20:59'!
close
	"Close the client's stream"

	stream ifNotNil:[
		stream isConnected ifTrue: [stream close].
		stream := nil].
! !

!WebClient methodsFor: 'initialize' stamp: 'CK 11/24/2017 14:24:01'!
connect
	"Connect the client to a web server. Returns nil if successful,
	a WebResponse if a proxy CONNECT request fails."

	| actualServer actualPort |

	"Determine which server to connect to (proxy or real)"
	proxyServer ifNil:[
		actualServer := self serverName.
		actualPort := self serverPort.
	] ifNotNil:[
		actualServer := self proxyServerName.
		actualPort := self proxyServerPort.
	].

	"Check if we can use the same stream or if we need to reconnect"
	(stream notNil
		and:[stream isConnected
		and:[lastServer = actualServer
		and:[lastPort = actualPort
		and:[lastScheme = scheme]]]]) ifTrue:[^nil].

	self close.
	lastServer := actualServer.
	lastPort := actualPort.
	lastScheme := scheme.
	stream := SocketStream openConnectionToHostNamed: lastServer port: lastPort.
	stream timeout: timeout.
	"Perform ssl initialization if necessary"
	scheme = 'https' ifTrue:[self retryOnce: [^self sslConnect]].
	^nil! !

!WebClient methodsFor: 'initialize' stamp: 'ar 7/21/2010 21:12'!
defaultPort
	"Return the default port to use if no port was specified.
	Since we only support https and http, default to port 80 unless https."

	^scheme = 'https' 
		ifTrue:[443]
		ifFalse:[80]! !

!WebClient methodsFor: 'initialize' stamp: 'BAP 12/13/2023 15:57:57'!
defaultUserAgent
	"Answer the default User-Agent string to use for WebClient"

	^ 'WebClient/1.5 (',
		SystemVersion current versionString,'; ', 
		WebUtils platformName,')'.! !

!WebClient methodsFor: 'initialize' stamp: 'ul 7/28/2016 18:09'!
destroy
	"Destroys the client"

	stream ifNotNil:[
		stream destroy.
		stream := nil.
	].
! !

!WebClient methodsFor: 'initialize' stamp: 'ar 3/27/2010 11:59'!
detectProxyServer
	"Automatically detect the proxy server to use"

	proxyServer := ProxyHandler proxyServerFor: self server.
! !

!WebClient methodsFor: 'initialize' stamp: 'ar 4/1/2010 18:51'!
flushAuthState
	"Flush all the auth state"

	self flushAuthState: authParams.
	self flushAuthState: proxyParams.! !

!WebClient methodsFor: 'initialize' stamp: 'ar 2/10/2012 13:33'!
initialize
	"Initializes the receiver"
	flags := 0.
	super initialize.

	self allowAuth: true.
	self allowRedirect: true.
	self acceptCookies: true.

	timeout := Socket standardTimeout.
	cookies := OrderedCollection new.
	redirections := Dictionary new.
	maxRedirect := 10.
	userAgent := self defaultUserAgent.
	authParams := IdentityDictionary new.
	proxyParams := IdentityDictionary new.

	"Use the default debug log"
	debugLog := DebugLog.
! !

!WebClient methodsFor: 'initialize' stamp: 'jmv 4/13/2023 15:19:00'!
initializeFromUrl: urlString
	"Initialize the client from a url string"

	| percentEscapedUrl urlStream serverAndPort userAndPass |
	percentEscapedUrl := urlString percentEscapeUrl.
	urlStream := percentEscapedUrl readStream.
	scheme := (urlStream upToAll: '://') asLowercase.
	(scheme = 'http' or:[scheme = 'https'])
		ifFalse:[self error: 'Unsupported scheme: ', scheme].
	serverAndPort := urlStream upTo: $/.

	userAndPass := serverAndPort copyUpTo: $@.
	userAndPass = serverAndPort ifFalse:[
		serverAndPort := serverAndPort copyAfter: $@.
		self username: (userAndPass copyUpTo: $:).
		self password: (userAndPass copyAfter: $:).
	].

	self server: serverAndPort.
	self detectProxyServer.! !

!WebClient methodsFor: 'initialize' stamp: 'ar 8/9/2010 08:30'!
newRequest
	"Create an return a new WebRequest.
	Subclasses can use this method to override the default request class."

	^WebRequest new! !

!WebClient methodsFor: 'initialize' stamp: 'CK 11/24/2017 14:59:44'!
proxyConnect
	"Send a proxy CONNECT request to connect to a remote host via the chosen proxy server"

	| uri request repeatAuth response |
	uri := self serverName,':', self serverPort asString.
	request := self newRequest.
	request method: 'CONNECT'.
	request rawUrl: uri.

	[repeatAuth := false.

	"The proxy response may use Connection: Close; reconnect when that happens"
	self isConnected ifFalse:[
		stream := SocketStream openConnectionToHostNamed: lastServer port: lastPort.
		stream timeout: timeout.
	].

	debugLog ifNotNil:[
		request writeOn: debugLog.
		debugLog flush.
	].

	"Don't use 'self writeRequestOn:' since this will insert both cookies
	as well as modify the url target when a proxy is present"
	request writeOn: stream.
	
	stream flush.

	response := request newResponse readFrom: stream.

	debugLog ifNotNil:[
		response writeOn: debugLog.
		debugLog flush.
	].

	"Handle authentication if needed"
	(self allowAuth and:[response code = 407]) ifTrue:[
		"Eat up the content of the previous response"
		response content.
		repeatAuth := self authenticate: request from: response.
	].
	repeatAuth] whileTrue.

	^response! !

!WebClient methodsFor: 'initialize' stamp: 'ar 9/4/2010 11:10'!
requestWithUrl: urlString
	"Create and return a new WebRequest initialized with the given url."

	^self  newRequest initializeFromUrl: urlString! !

!WebClient methodsFor: 'initialize' stamp: 'topa 11/4/2014 00:13'!
retryOnce: aTryBlock
	" Retry the block once if it failes,
	except for direct network errors.
	This can help in cases like temporary failing
	SqueakSSL primitives"
	| again |
	again := true.
	[^ aTryBlock value.
	] on: Error do: [:e |
		((e isKindOf: NetworkError) or: [again not])
			ifTrue: [e pass]
			ifFalse: [again := false. e retry]].! !

!WebClient methodsFor: 'initialize' stamp: 'CK 11/24/2017 14:25:04'!
sslConnect
	"Do the SSL handshake"
	"Connect the client to a web server"

	| sqSSL |
	proxyServer ifNotNil:[ | resp |
		"If we have a proxy server, do the proxy connect"
		resp := self proxyConnect.
		resp isSuccess ifFalse:[^resp].
	].

	sqSSL := Smalltalk at: #SqueakSSL ifAbsent:[self error: 'SqueakSSL is missing'].
	"Convert the stream to a secure stream"
	stream := sqSSL secureSocketStream on: stream socket.
	stream timeout: timeout.
	self sslConnect: stream to: lastServer.
	"And cert verification
	(unless on OSX, where this does not work yet)"
	WebUtils platformName = 'Mac OS'
		ifFalse: [stream verifyCert: self serverName].
	^ nil"indicating success"
! !

!WebClient methodsFor: 'initialize' stamp: 'jmv 3/18/2019 13:15:39'!
sslConnect: aStream to: aLastServer
	"Do the SSL handshake, use SNI if available"
	(aStream respondsTo: #sslConnectTo:)
		ifTrue: [aStream sslConnectTo: aLastServer]
		ifFalse: [aStream sslConnect].! !

!WebClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:04'!
httpDelete: urlString
	"Sends an DELETE request"

	^self httpDelete: urlString do:[:req]
! !

!WebClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:06'!
httpDelete: urlString do: aBlock
	"Sends a DELETE request"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'DELETE'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request.! !

!WebClient methodsFor: 'methods' stamp: 'ar 2/23/2010 08:57'!
httpGet: urlString
	"GET the response from the given url"
	"(WebClient httpGet: 'http://www.squeak.org') content"

	^self httpGet: urlString do:[:req]
! !

!WebClient methodsFor: 'methods' stamp: 'CK 11/24/2017 14:46:24'!
httpGet: urlString do: aBlock
	"GET the response from the given url
	(WebClient httpGet: 'http://www.cuis-smalltalk.org/') content
	"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'GET'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	request headerAt: 'Accept-Encoding' put: 'gzip'.
	aBlock value: request.
	^self sendRequest: request
! !

!WebClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:05'!
httpHead: urlString
	"Sends a HEAD request"

	^self httpHead: urlString do:[:req]
! !

!WebClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:06'!
httpHead: urlString do: aBlock
	"Sends a HEAD request"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'HEAD'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request.! !

!WebClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:05'!
httpOptions: urlString
	"Sends an OPTIONS request"

	^self httpOptions: urlString do:[:req]
! !

!WebClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:06'!
httpOptions: urlString do: aBlock
	"Sends an OPTIONS request"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'OPTIONS'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request
! !

!WebClient methodsFor: 'methods' stamp: 'ar 2/23/2010 08:56'!
httpPost: urlString content: postData type: contentType
	"POST the data to the given url"

	^self httpPost: urlString content: postData type: contentType do:[:req]! !

!WebClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:07'!
httpPost: urlString content: postData type: contentType do: aBlock
	"POST the data to the given url"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'POST'.
	contentType ifNotNil:[request headerAt: 'Content-Type' put: contentType].
	request headerAt: 'Content-Length' put: postData size.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request content: postData readStream size: postData size! !

!WebClient methodsFor: 'methods' stamp: 'ar 8/31/2010 22:54'!
httpPostChunked: urlString content: chunkBlock type: contentType
	"POST the data to the given url using chunked transfer-encoding.
	The chunkBlock takes a request and can be fed using #nextChunkPut:
	until all the data has been sent.

	Chunked encoding can be used for long-lasting connections to a server,
	but care must be taken to ensure that the client isn't running afoul of
	the server expecting to read the full response (i.e., you should use this
	only if you have control over both ends).

	However, it is a great way to send output from commands that take awhile
	and other time-consuming operations if authentication has been handled."

	^self httpPostChunked: urlString content: chunkBlock type: contentType do: [:req]! !

!WebClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:07'!
httpPostChunked: urlString content: chunkBlock type: contentType do: aBlock
	"POST the data to the given url using chunked transfer-encoding. 
	The chunkBlock takes a request and can be fed using #nextChunkPut:
	until all the data has been sent. 

	Chunked encoding can be used for long-lasting connections to a server,
	but care must be taken to ensure that the client isn't running afoul of
	the server expecting to read the full response (i.e., you should use this
	only if you have control over both ends).

	However, it is a great way to send output from commands that take awhile
	and other time-consuming operations if authentication has been handled."

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'POST'.
	contentType ifNotNil:[request headerAt: 'Content-Type' put: contentType].
	request headerAt: 'Transfer-Encoding' put: 'chunked'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	"Send the chunked data"
	^self sendRequest: request contentBlock:[:aStream| 
		"Set the stream in the request and pass it in the chunk block"
		request stream: aStream.
		chunkBlock value: request.
		"send termination chunk"
		aStream nextPutAll: '0'; crlf; crlf; flush.
	].
! !

!WebClient methodsFor: 'methods' stamp: 'ar 2/23/2010 08:57'!
httpPut: urlString content: postData type: contentType
	"PUT the data to the given url"

	^self httpPut: urlString content: postData type: contentType do:[:req]! !

!WebClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:07'!
httpPut: urlString content: postData type: contentType do: aBlock
	"PUT the data to the given url"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'PUT'.
	contentType ifNotNil:[request headerAt: 'Content-Type' put: contentType].
	request headerAt: 'Content-Length' put: postData size.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request content: postData readStream size: postData size! !

!WebClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:05'!
httpTrace: urlString
	"Sends a TRACE request"

	^self httpTrace: urlString do:[:req]
! !

!WebClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:08'!
httpTrace: urlString do: aBlock
	"Sends a TRACE request"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'TRACE'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request
! !

!WebClient methodsFor: 'testing' stamp: 'ar 3/30/2010 19:18'!
isConnected
	"Returns true if the client is still connected"

	^stream notNil and:[stream isConnected]! !

!WebClient methodsFor: 'proxy' stamp: 'ar 2/10/2012 13:12'!
proxyParams
	"The proxy authentication parameters"

	^proxyParams! !

!WebClient methodsFor: 'proxy' stamp: 'ar 2/10/2012 13:12'!
proxyParams: aDictionary
	"The proxy authentication parameters"

	proxyParams := aDictionary! !

!WebClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:04'!
proxyPass
	"The password for an authenticating proxy.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	^proxyParams at: #password! !

!WebClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:04'!
proxyPass: aStringOrValuable
	"The password for an authenticating proxy.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	proxyParams at: #password put: aStringOrValuable! !

!WebClient methodsFor: 'proxy' stamp: 'ar 2/20/2010 19:17'!
proxyServer
	"The proxy server to use for connections.
	The server should be specified as server:port if the proxy
	port to be used is different from the desitation port."

	^proxyServer! !

!WebClient methodsFor: 'proxy' stamp: 'CK 11/24/2017 14:01:01'!
proxyServer: aString
	"The proxy server to use for connections.
	The server should be specified as server:port if the proxy
	port to be used is different from the desitation port."

	proxyServer := aString.
	ProxyHandler defaultProxyServer: aString.! !

!WebClient methodsFor: 'proxy' stamp: 'ar 2/20/2010 19:22'!
proxyServerName
	"Returns the name part of the server:port description"

	^proxyServer copyUpTo: $:! !

!WebClient methodsFor: 'proxy' stamp: 'ar 2/20/2010 19:21'!
proxyServerPort
	"Returns the port of the proxyServer:port description"

	^(proxyServer copyAfter: $:) 
		ifEmpty:[self serverPort]
		ifNotEmpty:[:portString| portString asInteger].
! !

!WebClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:02'!
proxyUser
	"The user name for an authenticating proxy"

	^proxyParams at: #username ifAbsent:[nil]! !

!WebClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:02'!
proxyUser: aString
	"The user name for an authenticating proxy"

	proxyParams at: #username put: aString! !

!WebClient class methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:20'!
debugLog
	"The default debugLog, if any:
		WebClient debugLog: Transcript.
		WebClient debugLog: nil.
	"

	^DebugLog! !

!WebClient class methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:20'!
debugLog: aStream
	"The default debugLog, if any:
		WebClient debugLog: Transcript.
		WebClient debugLog: nil.
	"

	DebugLog := aStream.! !

!WebClient class methodsFor: 'accessing' stamp: 'ar 2/23/2010 22:26'!
proxyHandler
	"The currently registered proxy handler"

	^ProxyHandler! !

!WebClient class methodsFor: 'accessing' stamp: 'ar 2/23/2010 22:26'!
proxyHandler: anObject
	"The currently registered proxy handler"

	ProxyHandler := anObject! !

!WebClient class methodsFor: 'utilities' stamp: 'ar 6/1/2010 20:24'!
htmlSubmit: urlString fields: fieldMap
	"A utility method for html submit operations. The fieldMap can be EITHER
	an array of associations OR a Dictionary of key value pairs (the former is
	useful for providing multiple fields and/or specifying the order of fields).

		WebClient 
			htmlSubmit: 'http://www.google.com/search'
			fields: {
				'hl' -> 'en'.
				'q' -> 'Squeak'
			}
	"
	^self htmlSubmit: urlString 
			fields: fieldMap
			method: 'GET'
! !

!WebClient class methodsFor: 'utilities' stamp: 'ar 6/1/2010 20:24'!
htmlSubmit: urlString fields: fieldMap method: method
	"A utility method for html submit operations. The fieldMap can be EITHER
	an array of associations OR a Dictionary of key value pairs (the former is
	useful for providing multiple fields and/or specifying the order of fields).

		WebClient 
			htmlSubmit: 'http://www.google.com/search'
			fields: {
				'hl' -> 'en'.
				'q' -> 'Squeak'
			} method: 'GET'
	"
	^self htmlSubmit: urlString 
			fields: fieldMap
			method: method
			encoding: 'application/x-www-form-urlencoded'
! !

!WebClient class methodsFor: 'utilities' stamp: 'ar 7/20/2010 20:10'!
htmlSubmit: urlString fields: fields method: method encoding: encoding
	"A utility method for html submit operations. The fieldMap can be EITHER
	an array of associations OR a Dictionary of key value pairs (the former is
	useful for providing multiple fields and/or specifying the order of fields).

		WebClient 
			htmlSubmit: 'http://www.google.com/search'
			fields: {
				'hl' -> 'en'.
				'q' -> 'Squeak'
			} method: 'GET'
			encoding: 'application/x-www-form-urlencoded'
	"

	method = 'GET' ifTrue:[
		"GET only supports url encoded requests"
		encoding = 'application/x-www-form-urlencoded' 
			ifFalse:[^self error: 'Unsupported encoding: ', encoding].
		^self httpGet: urlString, '?', (WebUtils encodeUrlEncodedForm: fields).
	].

	method = 'POST' ifTrue:[
		"Dispatch on encoding type"
		encoding caseOf: {
			[ 'application/x-www-form-urlencoded'] -> [
				^self httpPost: urlString
					content: (WebUtils encodeUrlEncodedForm: fields)
					type: encoding.
			].
			['multipart/form-data'] -> [
				^self httpPost: urlString multipartFields: fields
			].
		} otherwise:[]
	].

	self error: 'Unsupported method: ', method.
! !

!WebClient class methodsFor: 'utilities' stamp: 'ar 7/20/2010 20:09'!
httpPost: url multipartFields: fieldMap
	"Make a form submission using multipart/form-data POST.

	The fieldMap may contain MIMEDocument instances to indicate the presence
	of a file to upload to the server. If the MIMEDocument is present, its
	content type and file name will be used for the upload.

	The fieldMap can be EITHER an array of associations OR a Dictionary of 
	key value pairs (the former is useful for providing multiple fields and/or 
	specifying the order of fields)."

	| boundary |
	boundary := WebUtils multipartBoundary.

	^self httpPost: url 
		content: (WebUtils encodeMultipartForm: fieldMap boundary: boundary) 
		type: 'multipart/form-data; boundary=', boundary! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:30'!
httpDelete: urlString
	"Sends a DELETE request"
	"WebClient httpDelete: 'http://ftp.squeak.org/trunk'"

	^self httpDo:[:client| client httpDelete: urlString].
! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:29'!
httpDo: aBlock
	"Simplified wrapper for running various methods"

	| client response |
	client := self new.
	response := WebUtils handleAuth: [aBlock value: client].
	response content; close.
	^response! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:30'!
httpGet: urlString
	"Get the response from the given url"
	"(WebClient httpGet: 'http://www.squeak.org') content"

	^self httpDo:[:client| client httpGet: urlString].! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
httpHead: urlString
	"Sends a HEAD request"
	"WebClient httpHead: 'http://ftp.squeak.org/trunk'"

	^self httpDo:[:client| client httpHead: urlString].
! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
httpOptions: urlString
	"Sends an OPTIONS request"
	"WebClient httpOptions: 'http://ftp.squeak.org/*'"
	"WebClient httpOptions: 'http://ftp.squeak.org/4.1/Squeak4.1.zip'"

	^self httpDo:[:client| client httpOptions: urlString]
! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
httpPost: urlString content: postData type: contentType
	"Fire off an HTTP post request"

	^self httpDo:[:client| client httpPost: urlString content: postData type: contentType]
! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
httpPostChunked: urlString content: chunkBlock type: contentType
	"POST the data to the given url using chunked transfer-encoding.
	The chunkBlock takes a request and can be fed using #nextChunkPut:
	until all the data has been sent.

	Chunked encoding can be used for long-lasting connections to a server,
	but care must be taken to ensure that the client isn't running afoul of
	the server expecting to read the full response (i.e., you should use this
	only if you have control over both ends).

	However, it is a great way to send output from commands that take awhile
	and other time-consuming operations if authentication has been handled."

	^self httpDo:[:client| 
		client httpPostChunked: urlString content: chunkBlock type: contentType]
! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:32'!
httpPut: urlString content: postData type: contentType
	"Fire off an HTTP PUT request"

	^self httpDo:[:client| client httpPut: urlString content: postData type: contentType]! !

!WebClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:32'!
httpTrace: urlString
	"Sends a TRACE request"
	"(WebClient httpTrace: 'http://lists.squeakfoundation.org') content"
	"(WebClient httpTrace: 'http://ftp.squeak.org/trunk') content"

	^self httpDo:[:client| client httpTrace: urlString]! !

!WebClient class methodsFor: 'class initialization' stamp: 'jmv 6/26/2017 19:16:36'!
initialize
	"WebClient initialize"

	FlagAcceptCookies := 1.
	FlagAllowAuth := 2.
	FlagAllowRedirect := 4.

	"Default proxy handler"
	ProxyHandler := WebUtils! !

!WebClient class methodsFor: 'benchmark' stamp: 'jmv 6/25/2017 20:07:19'!
requestBenchmark: url persistent: aBool
	"Run a WebClient benchmark to measure request handling speed.
	Try using a persistent connection if requested.

	1) Launch WebServer  (or Seaside, or whatever):
	
		(WebServer reset default)
			listenOn: 8888;
			addService: '/' action:[:r| r send200Response: ''].

	2) Run the benchmark:

		MessageTally spyOn:[
			WebClient requestBenchmark: 'http://localhost:8888/' persistent: true.
		]
	"

	| time resp startTime endTime count client |
	count := 0.
	client := WebClient new.
	startTime := Time localMillisecondClock.
	[endTime := Time localMillisecondClock.
	(time := endTime - startTime) < 5000] whileTrue:[
		resp := client httpGet: url.
		resp isSuccess ifFalse:[self error: resp status].
		resp content.
		aBool ifFalse:[resp close].
		count := count + 1.
	].
	^(count  * 1000 // time) printStringWithCommas, ' requests/sec'! !

!WebClient class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:39'!
webSocket00: serverUrl protocol: protocol
	"Attempt to connect to the given server url using the WebSocket api"

	| key1 key2 bytes hash client req resp |
	"Fixed keys from spec"
	key1 := 155712099.
	key2 := 173347027.
	bytes := ByteArray new: 8.
	(1 to: bytes size) do:[:i| bytes at: i put: (256 atRandom - 1)].
	hash := WebUtils webSocketHandshake: key1 with: key2 with: bytes.

	client := self new.
	client initializeFromUrl: serverUrl.
	req := client requestWithUrl: serverUrl.
	req method: 'GET'.
	req headerAt: 'Upgrade' put: 'WebSocket'.
	req headerAt: 'Connection' put: 'Upgrade'.
	req headerAt: 'Origin' put: 'null'.
	protocol ifNotNil:[req headerAt: 'Sec-WebSocket-Protocol' put: protocol].
	req headerAt: 'Sec-WebSocket-Key1' put: '18x 6]8vM;54 *(5:  {   U1]8  z [  8'.
	req headerAt: 'Sec-WebSocket-Key2' put: '1_ tx7X d  <  nw  334J702) 7]o}` 0'.
	resp := client sendRequest: req content: bytes readStream size: bytes size.
	resp code = 101 ifFalse:[client close. ^nil].
	(resp stream next: 16) asByteArray = hash ifFalse:[client close. ^nil].

	"Close of stream is left to caller"
	^WebSocket00 on: resp stream.
! !

!WebClient class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:39'!
webSocket07: serverUrl protocol: protocol
	"Attempt to connect to the given server url using the WebSocket api"
	"http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07"

	| hash client req resp nonce |
	nonce := ByteArray new: 16.
	(1 to: nonce size) collect:[:i| nonce at: i put: (256 atRandom - 1)].
	nonce := nonce base64Encoded.

	client := self new.
	client initializeFromUrl: serverUrl.
	req := client requestWithUrl: serverUrl.
	req method: 'GET'.
	req headerAt: 'Upgrade' put: 'WebSocket'.
	req headerAt: 'Connection' put: 'Upgrade'.
	req headerAt: 'Sec-WebSocket-Origin' put: 'null'.
	protocol ifNotNil:[req headerAt: 'Sec-WebSocket-Protocol' put: protocol].
	req headerAt: 'Sec-WebSocket-Key' put: nonce.
	req headerAt: 'Sec-WebSocket-Version' put: '7'.

	resp := client sendRequest: req.
	resp code = 101 ifFalse:[client close. ^nil].

	"Verify WebSocket hash"
	hash := WebUtils webSocketHash07: nonce.
	hash = (resp headerAt: 'Sec-WebSocket-Accept') ifFalse:[client close. ^nil].

	"Close of stream is left to caller"
	^WebSocket07 on: resp stream.
! !

!WebClient class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:39'!
webSocket68: serverUrl protocol: protocol
	"Attempt to connect to the given server url using the WebSocket api"

	| client req resp |
	client := self new.
	client initializeFromUrl: serverUrl.
	req := client requestWithUrl: serverUrl.
	req method: 'GET'.
	req headerAt: 'Upgrade' put: 'WebSocket'.
	req headerAt: 'Connection' put: 'Upgrade'.
	req headerAt: 'Origin' put: 'null'.
	protocol ifNotNil:[req headerAt: 'WebSocket-Protocol' put: protocol].
	resp := client sendRequest: req.
	resp code = 101 ifFalse:[client close. ^nil].

	"Close of stream is left to caller"
	^WebSocket00 on: resp stream.
! !

!WebClient class methodsFor: 'websockets' stamp: 'ar 7/9/2010 00:37'!
webSocketTo: serverUrl
	"Attempt to connect to the given server url using the WebSocket api"

	"Testing:

		1) Launch WebSocket example.
		2) Run the following code:
		
		| ws |
		ws := WebClient webSocketTo: 'http://localhost:8080/broadcast'.
		ws onMessage:[:data| Transcript cr; show: data].
		ws fork.
		ws send: 'This is the first message'.
		ws send: 'This is the last message'.
		ws close.

	"

	^self webSocketTo: serverUrl protocol: nil! !

!WebClient class methodsFor: 'websockets' stamp: 'ar 7/9/2010 00:31'!
webSocketTo: serverUrl protocol: protocol
	"Attempt to connect to the given server url using the WebSocket api"

	^self webSocket00: serverUrl protocol: protocol! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 8/5/2010 18:22'!
comment
	"Cookie comment"

	^comment! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 8/5/2010 18:22'!
comment: aString
	"Cookie comment"

	comment := aString! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
domain
	"Cookie domain"

	^domain! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
domain: aString
	"Cookie domain"

	domain := aString.! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:06'!
expiry
	"The expiry DateAndTime"

	^expiry! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:06'!
expiry: aDateAndTime
	"The expiry DateAndTime"

	expiry := aDateAndTime ifNotNil:[:dt| dt asUTC].! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 8/5/2010 18:21'!
expirySeconds: maxAge
	"Compute the cookie's expiry age from the given number of seconds"

	maxAge = 0 ifTrue:[self expiry: DateAndTime new].
	self expiry: DateAndTime now + maxAge seconds.! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 3/31/2010 08:38'!
httpOnly
	"If the cookie should be httpOnly (not scriptable).
	See http://msdn2.microsoft.com/en-us/library/ms533046.aspx"

	^httpOnly! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 3/31/2010 08:38'!
httpOnly: aBool
	"If the cookie should be httpOnly (not scriptable).
	See http://msdn2.microsoft.com/en-us/library/ms533046.aspx"

	httpOnly := aBool! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
name
	"The name of the cookie"

	^name! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
name: aString
	"The name of the cookie"

	name := aString! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
path
	"Cookie path"

	^path! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
path: aString
	"Cookie path"

	path := aString! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
secure
	"Secure cookie flag"

	^secure! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
secure: aBool
	"Secure cookie flag"

	secure := aBool! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
value
	"The value of the cookie"

	^value! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:06'!
value: aString
	"The value of the cookie"

	value := aString! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:06'!
version
	"Cookie version"

	^version! !

!WebCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:06'!
version: aString
	"Cookie version"

	version := aString! !

!WebCookie methodsFor: 'printing' stamp: 'jmv 6/25/2017 19:15:34'!
printOn: aStream
	"Creates the string representation for this cookie"

	aStream nextPutAll: name, '=', value asString.
	expiry ifNotNil:[ 
		aStream nextPutAll:'; expires='.
		aStream nextPutAll: expiry dayOfWeekName ,  ', '.
		expiry date printOn: aStream format: #(1 2 3 $- 2 2 2 ).
		aStream nextPutAll: ' ', expiry time print24,' GMT'.
	].
	path ifNotNil:[
		aStream nextPutAll:'; path=', (WebUtils quote: path).
	].
	domain ifNotNil:[
		aStream nextPutAll:'; domain=', (WebUtils quote: domain).
	].
	version ifNotNil:[
		aStream nextPutAll:'; version=', version asString.
	].
	secure ifTrue:[
		aStream nextPutAll: '; secure'.
	].
	httpOnly ifTrue:[
		aStream nextPutAll: '; HttpOnly'.
	].
! !

!WebCookie methodsFor: 'comparing' stamp: 'ar 5/11/2010 08:58'!
= aCookie
	"Compare the receiver with aCookie"

	^aCookie class == self class 
		and:[aCookie name = self name
		and:[aCookie domain = self domain
		and:[aCookie path = self path]]].! !

!WebCookie methodsFor: 'comparing' stamp: 'ar 5/11/2010 08:59'!
hash
	"Hash is re-implemented because #= is re-implemented"

	^name hash + domain hash + path hash.! !

!WebCookie methodsFor: 'converting' stamp: 'ar 2/20/2010 11:06'!
asString
	"Create the string representation for this cookie"

	^String streamContents:[:s| self printOn: s].! !

!WebCookie methodsFor: 'initialize' stamp: 'ar 3/31/2010 08:40'!
initialize
	"Initializes the receiver"
	super initialize.
	secure := false.
	httpOnly := false.
! !

!WebCookie methodsFor: 'initialize' stamp: 'jmv 1/10/2023 14:58:08'!
readExpiryFrom: aString
	"Read a cookie expiry date from the given string.
	The expiration date is specified in the 'Wdy, DD-Mon-YYYY HH:MM:SS GMT' format."
	
	| tokens |
	^[
		tokens := aString withBlanksTrimmed.
		(tokens copyAfterLast: Character space) = 'GMT'
			ifFalse:[^DateAndTime new].
		tokens := ((tokens copyAfter: Character space) copyUpToLast: Character space), '+00:00'.
		DateAndTime readFrom: tokens readStream.
	] on: Error do:[:ex| ex return: DateAndTime new].! !

!WebCookie methodsFor: 'initialize' stamp: 'jmv 6/25/2017 19:15:38'!
readFrom: aStream
	"Read a cookie from the given stream"

	| keyval key val |
	aStream skipSeparators.
	name := (aStream upTo: $=) withBlanksTrimmed.
	value := (aStream upTo: $;) withBlanksTrimmed.
	[aStream atEnd] whileFalse:[
		keyval := aStream upTo: $;.
		key := (keyval copyUpTo: $=) withBlanksTrimmed.
		val := (keyval copyAfter: $=) withBlanksTrimmed.
		key asLowercase caseOf: {
			['expires'] 	-> [self expiry: (self readExpiryFrom: val)].
			['path']		-> [self path: (WebUtils unquote: val)].
			['domain'] 	-> [self domain: (WebUtils unquote: val)].
			['secure'] 	-> [self secure: true].
			['version'] 	-> [self version: val].
			['httponly'] 	-> [self httpOnly: true].
			['comment']	 ->[self comment: val].
			['max-age']	->[self expirySeconds: val asNumber].
		} otherwise:[Transcript log: 'Unknown cookie field: ', key].
	].! !

!WebCookie methodsFor: 'testing' stamp: 'ar 5/11/2010 20:06'!
isExpired
	"Return true if this cookie is expired"
	
	expiry ifNil:[^false].
	^expiry asUTC < DateAndTime now asUTC! !

!WebMessage methodsFor: 'printing' stamp: 'ar 2/20/2010 11:05'!
printOn: aStream
	"Prints the receiver"

	aStream nextPutAll: self class name.
	aStream nextPut: $(.
	self writeOn: aStream.
	aStream nextPut: $).! !

!WebMessage methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:38'!
writeHeadersOn: aStream
	"Write the request"

	self headersDo:[:key :value| aStream nextPutAll: key, ': ', value asString; nextPutAll: String crlfString].
! !

!WebMessage methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:38'!
writeOn: aStream
	"Write the request"

	self writeHeadersOn: aStream.
	aStream nextPutAll: String crlfString.
! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 16:31'!
addHeader: key value: value
	"Adds an ADDITIONAL header for the given key"

	headers add: key -> value.! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:02'!
headerAt: key
	"Answers a single value for the given http header. 
	Do not use for headers that may appear multiple times, such at www-authenticate."

	^self headerAt: key ifAbsent:[''].! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headerAt: key ifAbsent: aBlock
	"Answers a single value for the given http header. 
	Do not use for headers that may appear multiple times, such at www-authenticate."

	self headersAt: key do:[:value| ^value].
	^aBlock value! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 12:18'!
headerAt: key put: value
	"Sets the value of a header, adding if necessary"

	headers do:[:assoc|
		(assoc key sameAs: key) ifTrue:[
			assoc value: value.
			^self
		].
	].
	self addHeader: key value: value.! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersAt: key
	"Answers a collection of values for the given http header. 
	Use for headers that may appear multiple times, such at www-authenticate."

	^self headersAt: key ifAbsent:[#()].! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersAt: aKey do: aBlock
	"Evaluate aBlock with all the headers matching aKey"

	self headersDo:[:key :value|
		(aKey sameAs: key) ifTrue:[ aBlock value: value].
	].! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersAt: key ifAbsent: aBlock
	"Answers a collection of values for the given http header. 
	Use for headers that may appear multiple times, such at www-authenticate."

	| hdrs |
	hdrs := OrderedCollection new.
	self headersAt: key do:[:value| hdrs add: value].
	^hdrs asArray ifEmpty:[aBlock value]
! !

!WebMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersDo: aBlock
	"Evaluate aBlock with all of the headers key-value pairs"

	headers do:[:assoc|
		aBlock value: assoc key value: assoc value.
	].! !

!WebMessage methodsFor: 'headers' stamp: 'ar 3/26/2010 19:02'!
removeHeader: key
	"Removes ALL headers with the given key"

	headers := headers reject:[:assoc| assoc key sameAs: key].
! !

!WebMessage methodsFor: 'converting' stamp: 'ar 2/23/2010 09:02'!
asString
	"Prints the receiver"

	^String streamContents:[:s| self writeOn: s].! !

!WebMessage methodsFor: 'streaming' stamp: 'ar 2/2/2012 15:55'!
chunkFrom: srcStream to: dstStream progress: progressBlock
	"Stream the content of srcStream to dstStream.
	Use chunked transfer-encoding."

	| chunkSize firstLine buffer totalRead |
	totalRead := 0.
	buffer := (srcStream isBinary ifTrue:[ByteArray] ifFalse:[String]) new.
	[firstLine := srcStream nextLine asString.
	chunkSize := ('16r',firstLine asUppercase) asNumber.
	chunkSize = 0] whileFalse:[
		progressBlock ifNotNil:[progressBlock value: nil value: totalRead].
		buffer size = chunkSize ifFalse:[buffer := buffer class new: chunkSize].
		buffer := srcStream next: chunkSize into: buffer startingAt: 1.
		dstStream nextPutAll: buffer.
		srcStream skip: 2. "CRLF"
		totalRead := totalRead + chunkSize.
	].
	srcStream skip: 2. "CRLF"
	dstStream flush.
	progressBlock ifNotNil:[progressBlock value: nil value: totalRead].! !

!WebMessage methodsFor: 'streaming' stamp: 'DSG 3/29/2015 09:10'!
nextChunk
	"Answer the next chunk from a message using chunked transfer encoding."

	| chunkSize chunk |
	chunkSize := Integer readFrom: (stream upToAll: String crlfString) readStream base: 16.
	chunkSize = 0 ifFalse:[chunk := stream next: chunkSize].
	stream skip: 2. "CrLf"
	^chunk! !

!WebMessage methodsFor: 'streaming' stamp: 'ar 8/31/2010 22:57'!
nextChunkPut: chunk
	"Send the next chunk of a message using chunked transfer encoding.

	Note: This method does not allow to send empty chunks to avoid accidentally
	terminating the chunked transfer. Ending the transfer is part of the chunked
	request/response protocol (see #httpPostChunked:encoded:content:type:do:
	as well as #sendResponse:chunked:do:)"

	chunk ifNotEmpty:[
		stream nextPutAll: (chunk size printStringBase: 16); crlf.
		stream nextPutAll: chunk; crlf; flush.
	].! !

!WebMessage methodsFor: 'streaming' stamp: 'ar 5/4/2010 15:44'!
streamFrom: srcStream size: size progress: aBlock
	"Stream the given source stream to the message's socket stream.
	Outbound. Can be used on both request/response depending on
	whether it is utilized by WebClient or WebServer."

	^self streamFrom: srcStream to: stream size: size progress: aBlock! !

!WebMessage methodsFor: 'streaming' stamp: 'ar 7/10/2010 13:51'!
streamFrom: srcStream to: dstStream size: sizeOrNil progress: progressBlock
	"Stream the content of srcStream to dstStream.
	If a size is given, stream that many elements, otherwise stream up to the end."

	| buffer totalRead remaining size |
	(self headerAt: 'transfer-encoding') ifNotEmpty:[:encoding|
		encoding = 'chunked' 
			ifTrue:[^self chunkFrom: srcStream to: dstStream progress: progressBlock]
			ifFalse:[self error: 'Unknown transfer-encoding: ', encoding]].

	sizeOrNil = 0 ifTrue:[^self].

	buffer := (srcStream isBinary ifTrue:[ByteArray] ifFalse:[String]) new: 4096.
	totalRead := 0.
	size := sizeOrNil ifNil:[0].
	[(sizeOrNil == nil and:[stream atEnd not]) or:[totalRead < size]] whileTrue:[
		progressBlock ifNotNil:[progressBlock value: sizeOrNil value: totalRead].
		remaining := sizeOrNil ifNil:[99999] ifNotNil:[sizeOrNil - totalRead].
		remaining > buffer size ifTrue:[remaining := buffer size].
		buffer := srcStream next: remaining into: buffer startingAt: 1.
		dstStream nextPutAll: (remaining < buffer size  
			ifTrue:[(buffer copyFrom: 1 to: remaining)]
			ifFalse:[buffer]).
		totalRead := totalRead + buffer size.
	].
	dstStream flush.
	progressBlock ifNotNil:[progressBlock value: sizeOrNil value: totalRead].! !

!WebMessage methodsFor: 'streaming' stamp: 'ar 5/4/2010 15:46'!
streamTo: dstStream size: size progress: aBlock
	"Stream from the receiver's socket stream to the given destination stream.
	Inbound. Can be used on both request/response depending on
	whether it is utilized by WebClient or WebServer."
	content ifNil:[
		self streamFrom: stream to: dstStream size: size progress: aBlock
	] ifNotNil:[
		self streamFrom: content readStream to: dstStream size: size progress: aBlock
	].! !

!WebMessage methodsFor: 'initialize' stamp: 'ar 5/4/2010 20:02'!
close
	"Always closes regardless of connection header"

	stream ifNotNil:[stream close].! !

!WebMessage methodsFor: 'initialize' stamp: 'ar 5/6/2010 20:47'!
closeIfTransient
	"Close the underlying connection if it's not persistent"

	self isPersistent ifFalse:[self close].
! !

!WebMessage methodsFor: 'initialize' stamp: 'ar 5/4/2010 20:02'!
destroy
	"Destroys the underlying stream"

	stream ifNotNil:[stream destroy].! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 6/18/2010 23:52'!
content
	"Reads and caches available content and returns it."

	^content ifNil:[content := self getContent].
! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 3/30/2010 22:05'!
content: aString
	"Sets content for a response"

	content := aString.! !

!WebMessage methodsFor: 'accessing' stamp: 'tsl 1/18/2021 11:17:47'!
contentLength
	"Answers the numeric value of the Content-Length header, or nil if no header exist"

	^(self headerAt: 'Content-Length' ifAbsent:[^nil]) asNumber! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 5/4/2010 21:04'!
contentLength: aNumber
	"Sets the numeric value of the Content-Length header"

	^self headerAt: 'Content-Length' put: aNumber! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:01'!
contentStream
	"Answers a stream for the conent."

	^stream! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:02'!
contentType
	"Answer the content type for the receiver"

	^self headerAt: 'Content-Type' ifAbsent:['application/octet-stream']
! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 3/30/2010 21:58'!
contentType: aString
	"Sets the content type for the receiver"

	^self headerAt: 'Content-Type' put: aString! !

!WebMessage methodsFor: 'accessing' stamp: 'ul 4/21/2015 06:40'!
contentWithProgress: progressBlock
	"Reads and caches available content and returns it."

	^content ifNil: [ content := self getContentWithProgress: progressBlock ]! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 7/20/2010 20:09'!
multipartBoundary
	"Answer the boundary of a multipart/form-data message"

	| header |
	header := self headerAt: 'Content-Type' ifAbsent:[^nil].
	(header beginsWith: 'multipart/form-data') ifFalse:[^nil].
	^(header copyFrom: (header findString: 'boundary=') + 9 to: header size)! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 16:31'!
protocol
	"Returns the requested protocol"

	^protocol! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 16:31'!
protocol: aString
	"Returns the requested protocol"

	protocol := aString! !

!WebMessage methodsFor: 'accessing' stamp: 'dtl 7/17/2012 18:59'!
remoteHost
	"Return the name of the remote host"

	| remote |
	(stream isKindOf: SocketStream) ifFalse:[^nil]. "don't have it"
	remote := stream socket remoteAddress.
	^ NetNameResolver stringFromAddress: remote.
! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:01'!
stream
	"Answers the underlying TCP stream"

	^stream! !

!WebMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:01'!
stream: aStream
	"Sets the underlying TCP stream"

	stream := aStream! !

!WebMessage methodsFor: 'private' stamp: 'ul 4/21/2015 06:31'!
getContent
	"Reads available content and returns it."

	^self getContentWithProgress: nil! !

!WebMessage methodsFor: 'private' stamp: 'jmv 4/13/2023 21:59:33'!
getContentWithProgress: progressBlockOrNil
	"Reads available content and returns it."

	| length result |
	length := self contentLength.
	result := (stream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ])
		streamContents: [ :outputStream | 
			self 
				streamFrom: stream
				to: outputStream
				size: length
				progress: progressBlockOrNil ]
		estimatedSize: (length ifNil: [ 1000 ]).
	(self headerAt: 'content-encoding') = 'gzip' ifFalse: [ ^result ].
	^UnicodeString fromBytesStream: (GZipReadStream on: result asByteArray)! !

!WebMessage methodsFor: 'testing' stamp: 'pmm 6/19/2010 08:05'!
isPersistent
	"Return true if this is a persistent connection"

	"Check for explicit connection headers"
	((self headerAt:'Connection') sameAs: 'close') ifTrue:[^false].
	((self headerAt:'Connection') sameAs: 'keep-alive') ifTrue:[^true].

	"Defaults for various versions"
	(self protocol = 'HTTP/1.0') ifTrue:[^false].
	(self protocol = 'HTTP/1.1') ifTrue:[^true].

	"Assume that later HTTP versions are compatible with 1.1"
	^true! !

!WebMessage class methodsFor: 'class initialization' stamp: 'ar 2/21/2010 09:55'!
initialize
	"self initialize"

	StatusCodes := Dictionary newFromPairs:{
		"1xx: Meta status codes"
		100. 	'Continue'.
		101. 	'Switching Protocols'.
		102.	'Processing'.
		"2xx: Success status codes"
		200.	'OK'.
		201.	'Created'.
		202.	'Accepted'.
		203. 	'Non-Authoritative Information'.
		204.	'No Content'.
		205. 	'Reset Content'.
		206.	'Partial Content'.
		207.	'Multi-Status'. 				"WebDav RFC: 2518"
		"3xx: Redirection status codes"
		300.	'Multiple Choices'.
		301.	'Moved Permanently'.
		302.	'Moved Temporarily'.
		303.	'See Other'.
		304.	'Not Modified'.
		305.	'Use Proxy'.
		307.	'Temporary Redirect'.
		"4xx: Client-Side Error status code"
		400.	'Bad Request'.
		401.	'Unauthorized'.
		402.	'Payment Required'.
		403.	'Forbidden'.
		404.	'Not Found'.
		405.	'Method Not Allowed'.
		406.	'Not Acceptable'.
		407.	'Proxy Authentication Required'.
		408.	'Request Timeout'.
		409.	'Conflict'.
		410.	'Gone'.
		411.	'Lenght Required'.
		412.	'Precondition Failed'.
		413.	'Request Entity Too Large'.
		414.	'Request-URI Too Long'.
		415.	'Unsupported Media Type'.
		416.	'Requested Range Not Satisfiable'.
		417.	'Expectation Failed'.
		422.	'Unprocessable Entity'. 		"WebDav RFC: 2518"
		423.	'Locked'. 					"WebDav RFC: 2518"
		424.	'Failed Dependency'. 		"WebDav RFC: 2518"
		"5xx: Server-Side Error status codes"
		500.	'Internal Server Error'.
		501.	'Not Implemented'.
		502.	'Bad Gateway'.
		503.	'Service Unavailable'.
		504.	'Gateway Timeout'.
		505.	'HTTP Version Not Supported'.
		507.	'Insufficient Storage'. 		"WebDav RFC: 2518"
	}.! !

!WebMessage class methodsFor: 'utilities' stamp: 'klub 1/14/2014 22:09'!
statusCodeAt: code
	"Return the textual description of a status code"

	^StatusCodes at: code ifAbsent: [ 'Unknown error' ]! !

!WebRequest methodsFor: 'testing' stamp: 'ar 8/5/2010 19:51'!
acceptsCookie: aCookie from: webClient
	"Returns true if this cookie is applicable to this request"

	aCookie domain ifNotNil:[:domain|
		(webClient serverName endsWith: domain) ifFalse:[^false].
	].
	aCookie path ifNotNil:[:path|
		(self url beginsWith: path) ifFalse:[^false].
	].
	aCookie secure ifNotNil:[:secure|
		secure ifTrue:[webClient scheme = 'https' ifFalse:[^false]].
	].
	aCookie expiry ifNotNil:[:expiry|
		aCookie isExpired ifTrue:[^false].
	].
	^true! !

!WebRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:18'!
isDeleteRequest
	"Returns true if this is an HTTP DELETE request"

	^self method = 'DELETE'! !

!WebRequest methodsFor: 'testing' stamp: 'ar 2/20/2010 10:40'!
isGetRequest
	"Returns true if this is an HTTP GET request"

	^self method = 'GET'! !

!WebRequest methodsFor: 'testing' stamp: 'ar 2/20/2010 10:41'!
isHeadRequest
	"Returns true if this is an HTTP HEAD request"

	^self method = 'HEAD'! !

!WebRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:19'!
isOptionsRequest
	"Returns true if this is an HTTP OPTIONS request"

	^self method = 'OPTIONS'! !

!WebRequest methodsFor: 'testing' stamp: 'ar 2/20/2010 10:41'!
isPostRequest
	"Returns true if this is an HTTP POST request"

	^self method = 'POST'! !

!WebRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:18'!
isPutRequest
	"Returns true if this is an HTTP PUT request"

	^self method = 'PUT'! !

!WebRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:18'!
isTraceRequest
	"Returns true if this is an HTTP TRACE request"

	^self method = 'TRACE'! !

!WebRequest methodsFor: 'converting' stamp: 'ar 9/26/2011 10:25'!
asWebSocket
	"Convert this request into a WebSocket"

	"Figure out the version of the protocol"
	| field version |
	version := self headerAt: 'Sec-WebSocket-Version' ifAbsent:[nil].
	version ifNil:[
		field := self headerAt: 'Sec-WebSocket-Key1' ifAbsent:[nil].
		field 
			ifNil:[^self asWebSocket68] 
			ifNotNil:[^self asWebSocket00].
	].

	"Attempt WebSocket07 conversion"
	^self asWebSocket07
! !

!WebRequest methodsFor: 'converting' stamp: 'ar 9/26/2011 10:39'!
asWebSocket00
	"Convert this request into a WebSocket"

	| field key1 key2 bytes resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 101.
	resp status: 'HTTP/1.1 101 Web Socket Protocol Handshake'.

	"Chrome 4/5 is *VERY* sensitive to the order of the fields"
	resp headerAt: 'Upgrade' put: 'WebSocket'.
	resp headerAt: 'Connection' put: 'Upgrade'.

	field := self headerAt: 'Sec-WebSocket-Key1'.
	key1 := WebUtils extractWebSocketKey: field.
	field := self headerAt: 'Sec-WebSocket-Key2'.
	key2 := WebUtils extractWebSocketKey: field.
	bytes := WebUtils webSocketHandshake: key1 with: key2 with: (stream next: 8).

	resp headerAt: 'Sec-WebSocket-Origin' put: (self headerAt: 'Origin').
	resp headerAt: 'Sec-WebSocket-Location' put: ('ws://', (self headerAt: 'host'), self rawUrl).
	(self headerAt: 'Sec-WebSocket-Protocol' ifAbsent:[nil]) ifNotNil:[:hdr|
		resp headerAt: 'Sec-WebSocket-Protocol' put: hdr.
	].

	resp writeOn: stream.
	bytes ifNotNil:[stream nextPutAll: bytes].
	stream flush.
	^WebSocket00 on: stream.
! !

!WebRequest methodsFor: 'converting' stamp: 'klub 6/13/2012 17:00'!
asWebSocket07
	"Convert this request into a WebSocket"

	| resp hash nonce |
	resp := self newResponse protocol: 'HTTP/1.1' code: 101.
	resp status: 'HTTP/1.1 101 Web Socket Protocol Handshake'.

	"Chrome 4/5 is *VERY* sensitive to the order of the fields"
	resp headerAt: 'Upgrade' put: 'WebSocket'.
	resp headerAt: 'Connection' put: 'Upgrade'.

	nonce := self headerAt: 'Sec-WebSocket-Key'.
	hash := WebUtils webSocketHash07: nonce.

	resp headerAt: 'Sec-WebSocket-Accept' put: hash.
	(self headerAt: 'Sec-WebSocket-Origin' ifAbsent: [self headerAt: 'Origin']) 
		ifNotNil: [ :origin | resp headerAt: 'Sec-WebSocket-Origin' put: origin ].
	resp headerAt: 'Sec-WebSocket-Location' put: ('ws://', (self headerAt: 'host'), self rawUrl).
	(self headerAt: 'Sec-WebSocket-Protocol' ifAbsent:[nil]) ifNotNil:[:hdr|
		resp headerAt: 'Sec-WebSocket-Protocol' put: hdr.
	].

	resp writeOn: stream.
	stream flush.
	^(WebSocket07 on: stream)
		masking: false;
		yourself.
	"http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07#section-4.1 says:
   ''Frames sent from the server to the client are not masked.''
	http://tools.ietf.org/html/rfc6455#section-5.1 says: 
   ''A client MUST close a connection if it detects a masked
   frame.''
	So not using masking is compatible with the 07 protocol and is required by RFC 6455."! !

!WebRequest methodsFor: 'converting' stamp: 'ar 9/26/2011 10:40'!
asWebSocket68
	"Convert this request into a WebSocket"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 101.
	resp status: 'HTTP/1.1 101 Web Socket Protocol Handshake'.

	"Chrome 4/5 is *VERY* sensitive to the order of the fields"
	resp headerAt: 'Upgrade' put: 'WebSocket'.
	resp headerAt: 'Connection' put: 'Upgrade'.

	resp headerAt: 'WebSocket-Origin' put: (self headerAt: 'Origin').
	resp headerAt: 'WebSocket-Location' put: ('ws://', (self headerAt: 'host'), self rawUrl).
	(self headerAt: 'WebSocket-Protocol' ifAbsent:[nil]) ifNotNil:[:hdr|
		resp headerAt: 'WebSocket-Protocol' put: hdr.
	].
	resp writeOn: stream.
	stream flush.
	^WebSocket00 on: stream.! !

!WebRequest methodsFor: 'sending' stamp: 'klub 7/24/2011 19:25'!
closeIfTransient: resp after: aBlock
	"Evaluate aBlock. Close the connection if it is transient.
	Inserts Connection: close header if needed."

	| close |
	close := self isPersistent not.
	close 
		ifTrue: [
			"Always be explicit and insert the Connection: close header"
			resp headerAt: 'Connection' put: 'close' ]
		ifFalse: [
			"We should tell the client that we keep this connection alive."
			resp headerAt: 'Connection' put: 'Keep-Alive' ].
	aBlock value.
	stream flush.
	close  ifTrue:[self close].
! !

!WebRequest methodsFor: 'sending' stamp: 'ar 2/23/2010 22:04'!
sendResponse: resp content: aString
	"Send a WebResponse with content."

	^self sendResponse: resp contentStream: aString readStream size: aString size! !

!WebRequest methodsFor: 'sending' stamp: 'ar 8/31/2010 22:11'!
sendResponse: resp contentBlock: contentBlock
	"Sends a WebResponse, streaming its contents from aStream.
	If a size is provided, insert a Content-Length header, otherwise
	ensure that the connection is transient."

	"Add Date and Server headers"
	resp headerAt: 'Date' put: (server ifNil:[WebServer]) serverDate.
	resp headerAt: 'Server' put: (server ifNil:[WebServer]) serverString.

	self closeIfTransient: resp after:[
		"Ignore all network errors while sending the response"
		[resp writeOn: stream.
		stream flush.
		self isHeadRequest ifFalse:[contentBlock value: stream].
		] on: NetworkError do:["ignore"].

		"Make sure we log before closing the connection"
		server ifNotNil:[server logRequest: self response: resp].
	].! !

!WebRequest methodsFor: 'sending' stamp: 'ar 8/31/2010 22:12'!
sendResponse: resp contentStream: aStream size: streamSize
	"Sends a WebResponse, streaming its contents from aStream.
	If a size is provided, insert a Content-Length header, otherwise
	ensure that the connection is transient."

	streamSize 
		ifNil:[self headerAt: 'Connection' put: 'close'] "mark transient"
		ifNotNil:[resp headerAt: 'Content-Length' put: streamSize].

	^self sendResponse: resp contentBlock:[:sockStream|
		resp streamFrom: aStream to: sockStream size: streamSize progress: nil
	]! !

!WebRequest methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:34'!
cookieAt: aString
	"Answer the value for a cookie with the given name"

	^self cookieAt: aString ifAbsent:['']! !

!WebRequest methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
cookieAt: aString ifAbsent: aBlock
	"Answer the value for a cookie with the given name"

	self cookiesDo:[:name :value| name = aString ifTrue:[^value]].
	^aBlock value! !

!WebRequest methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
cookies
	"Answer all the cookies defined in the request"

	| cookies |
	cookies := Dictionary new.
	self cookiesDo:[:name :value| cookies at: name put: value].
	^cookies! !

!WebRequest methodsFor: 'cookies' stamp: 'ar 8/5/2010 19:56'!
cookiesDo: aBlock
	"Parse the cookie headers and evaluate aBlock with name / value pairs"

	self headersAt: 'Cookie' do:[:hdr|
		(hdr findTokens: ';,') do:[:token|
			aBlock 
				value: (token copyUpTo: $=) withBlanksTrimmed 
				value: (token copyAfter: $=) withBlanksTrimmed]].! !

!WebRequest methodsFor: 'fields' stamp: 'ar 7/20/2010 20:08'!
fields
	"Answer the fields for the given request."

	| fields |
	fields := Dictionary new.
	self getFields associationsDo:[:a| fields add: a].
	self postFields associationsDo:[:a| fields add: a].
	self multipartFields associationsDo:[:a| fields add: a].
	^fields
! !

!WebRequest methodsFor: 'fields' stamp: 'ar 2/21/2010 09:55'!
getFields
	"Decodes the fields embedded in the url ?var1=val1&var2=val2"

	| args |
	args := (rawUrl copyAfter: $?) ifEmpty:[^Dictionary new].
	^WebUtils decodeUrlEncodedForm: args multipleValues: false
! !

!WebRequest methodsFor: 'fields' stamp: 'ar 7/28/2010 21:01'!
multipartFields
	"Answers the multipart fields of a post request"

	| fields |
	fields := Dictionary new.
	self multipartFieldsDo:[:hdrs :params :cntnt|
		params at: 'name' ifPresent:[:fieldName| fields at: fieldName put: cntnt].
	].
	^fields! !

!WebRequest methodsFor: 'fields' stamp: 'ar 7/20/2010 20:08'!
multipartFieldsDo: aBlock
	"Evaluate aBlock with the multipart fields of a post request.
	The block is expected to take three arguments:
		* The headers from the part being processed
		* The parameters from the content-disposition header
		* The content from the part being processed
	"

	^(self isPostRequest and: [
		self contentType beginsWith: 'multipart/form-data'])
			ifTrue: [WebUtils decodeMultipartForm: self content readStream boundary: self multipartBoundary do: aBlock]
			ifFalse: [Dictionary new]! !

!WebRequest methodsFor: 'fields' stamp: 'jmv 6/25/2017 20:10:00'!
postFields
	"Answers the multipart fields of a post request"

	^(self isPostRequest and: [
		self contentType beginsWith: 'application/x-www-form-urlencoded'])
			ifTrue: [
				WebUtils 
					decodeUrlEncodedForm: self content 
					multipleValues: true ]
			ifFalse: [Dictionary new]! !

!WebRequest methodsFor: 'initialize' stamp: 'ar 2/23/2010 09:02'!
initialize
	"Initialize the receiver"

	super initialize.
	method := 'GET'.
	protocol := 'HTTP/1.1'.
	headers := OrderedCollection new.! !

!WebRequest methodsFor: 'initialize' stamp: 'jmv 4/13/2023 15:19:08'!
initializeFromUrl: urlString
	"Initialize the client from a http url or string"

	| percentEscapedUrl urlStream |
	percentEscapedUrl := urlString percentEscapeUrl.
	urlStream := percentEscapedUrl readStream.
	"Skip past scheme if present"
	urlStream upToAll: '://'.
	urlStream atEnd 
		ifTrue:[rawUrl := percentEscapedUrl]
		ifFalse:[rawUrl := urlStream upTo: $/; upToEnd].
	(rawUrl beginsWith: '/') ifFalse:[rawUrl := '/', rawUrl].
! !

!WebRequest methodsFor: 'initialize' stamp: 'ar 4/1/2010 16:03'!
newResponse
	"Create a new response entitiy.
	Subclasses can override this method to provide a different response class"

	^(WebResponse new)
		request: self;
		yourself! !

!WebRequest methodsFor: 'initialize' stamp: 'ar 7/8/2010 19:11'!
newWebSocketOn: aStream
	"Create a new socket entitiy.
	Subclasses can override this method to provide a different response class"

	^WebSocket on: aStream! !

!WebRequest methodsFor: 'initialize' stamp: 'jmv 6/25/2017 19:15:52'!
readFrom: aStream
	"Initialize the receiver initialized from an existing socket stream 
	This method will block until the headers have been parsed."

	| tokens |
	stream := aStream.
	stream isBinary ifTrue:[stream ascii].
	"Parse request header"
	request := stream upToAll: String crlfString.
	tokens := request findTokens: ' '.
	tokens size < 2 ifTrue:[^self error: 'Invalid HTTP request: ', request].
	method := tokens first.
	rawUrl := tokens second.
	tokens size > 2 
		ifTrue:[protocol := tokens third]
		ifFalse:[protocol := 'HTTP/1.0'].

	"Parse HTTP header"
	headers := WebUtils readHeadersFrom: stream.
! !

!WebRequest methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:40'!
method
	"The request method (GET, POST etc)"

	^method! !

!WebRequest methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:40'!
method: aString
	"The request method (GET, POST etc)"

	method := aString! !

!WebRequest methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:40'!
rawUrl
	"The raw (unparsed) url"

	^rawUrl! !

!WebRequest methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:07'!
rawUrl: aString
	"The raw (unparsed) url"

	rawUrl := aString
! !

!WebRequest methodsFor: 'accessing' stamp: 'ar 7/28/2010 19:49'!
requestLine
	"The actual request line as sent by the client"

	^request ifNil:[self method, ' ', self rawUrl, ' ', self protocol]! !

!WebRequest methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:55'!
server
	"If non-nil, the WebServer instance associated with the request."

	^server! !

!WebRequest methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:55'!
server: aWebServer
	"If non-nil, the WebServer instance associated with the request."

	server := aWebServer! !

!WebRequest methodsFor: 'accessing' stamp: 'jmv 4/13/2023 12:21:21'!
url
	"The url without fragment or arguments"

	^((rawUrl copyUpTo: $#) copyUpTo: $?)! !

!WebRequest methodsFor: 'responses' stamp: 'jmv 5/26/2022 12:07:45'!
send200Response: aString
	"Send a 200 OK response"

	^self 
		send200Response: aString asUtf8Bytes
		contentType: 'text/plain; charset=utf-8'.! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/20/2010 15:42'!
send200Response: aString contentType: contentType
	"Send a 200 OK response"

	^self send200Response: aString contentType: contentType do:[:resp]! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 16:32'!
send200Response: aString contentType: contentType do: aBlock
	"Send a 200 OK response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 200.
	resp headerAt: 'Content-Type' put: contentType.
	aBlock value: resp.
	^self sendResponse: resp content: aString.! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:26'!
send301Response: location
	"Send a 301 permanent redirect response"

	^self send301Response: location do:[:resp].! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:27'!
send301Response: location do: aBlock
	"Send a 301 permanent redirect response"

	^self send3xxResponse: location code: 301 do: aBlock! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:26'!
send302Response: location
	"Send a 302 temporary redirect response"

	^self send302Response: location do:[:resp].! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:26'!
send302Response: location do: aBlock
	"Send a 302 temporary redirect response"

	^self send3xxResponse: location code: 302 do: aBlock! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/20/2010 15:42'!
send3xxResponse: location code: statusCode
	"Send a 3xx redirect response"

	^self send3xxResponse: location code: statusCode do:[:resp].! !

!WebRequest methodsFor: 'responses' stamp: 'ar 7/23/2010 09:33'!
send3xxResponse: location code: statusCode do: aBlock
	"Send a 3xx redirect response"

	| resp url |
	"If the redirect is not to an absolute url, prefix it with the host if we have one"
	url := location.
	(location findString: '://') > 0 ifFalse:[
		(self headerAt: 'host') ifNotEmpty:[:host| url := 'http://', host, location]].

	resp := self newResponse protocol: 'HTTP/1.1' code: statusCode.
	resp headerAt: 'Location' put: url.
	aBlock value: resp.
	^self sendResponse: resp content: ''.
! !

!WebRequest methodsFor: 'responses' stamp: 'jmv 5/26/2022 12:07:36'!
send400Response
	"Send a 400 bad request response"

	| message |
	message := '<html><head><title>400 Bad Request</title></head><body>',
		'<h1>Bad Request</h1>
		<p>The server encountered a request it did not understand.
		</body></html>'.
	
	self sendResponseCode: 400 
		content: message asUtf8Bytes
		type: 'text/plain; charset=utf-8'
		close: true! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/20/2010 15:43'!
send404Response
	"Send a 404 not found response"

	^self send404Response: '<html><head><title>404 Not Found</title></head><body><h1>404 Not Found</h1><p>The requested URL ',self rawUrl,' was not found on this server.</p></body></html>'.
! !

!WebRequest methodsFor: 'responses' stamp: 'jmv 5/26/2022 12:07:53'!
send404Response: body
	"Send a 404 not found response"

	^self 
		send404Response: body asUtf8Bytes
		do: [ :resp | resp headerAt: 'Content-Type' put: 'text/html; charset=utf-8' ]! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 16:32'!
send404Response: body do: aBlock
	"Send a 404 not found response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 404.
	resp headerAt: 'Content-Type' put: 'text/html; charset=utf-8'.
	aBlock value: resp.
	^self sendResponse: resp content: body.
! !

!WebRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 20:08'!
send405Response: allowed
	"Send a 405 method not allowed response"

	^self send405Response: allowed content: '<html><head><title>405 Method Not Allowed</title></head><body><h1>405 Method Not Allowed</h1><p>The requested method ',self method,' is not allowed for the URL', self rawUrl, '</p></body></html>'.! !

!WebRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 20:08'!
send405Response: allowed content: body
	"Send a 405 method not allowed response"
	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 405.
	resp headerAt: 'Content-Type' put: 'text/html; charset=utf-8'.
	resp headerAt: 'allow' put: (String streamContents:[:s|
		allowed do:[:m| s nextPutAll: m] separatedBy:[s nextPut: $,]
	]).
	^self sendResponse: resp content: body.! !

!WebRequest methodsFor: 'responses' stamp: 'jmv 5/26/2022 12:07:57'!
send500Response: errReport
	"Send a 500 internal server error response"

	| errLog |
	errLog := '<html><head><title>500 Internal Server Error</title></head><body>',
		'<h1>Internal Server Error</h1><pre>',
		(errReport  withLineEndings: String crlfString),
		'</pre></body></html>'.

	self sendResponseCode: 500
				content: errLog asUtf8Bytes
				type: 'text/html; charset=utf-8'
				close: true.
! !

!WebRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 20:08'!
sendOptionsResponse: allowed
	"Send a 200 OK response for an OPTIONS request"

	^self send200Response: '' contentType: 'application/octet-stream' do:[:resp|
		resp headerAt: 'allow' put: (String streamContents:[:s|
			allowed do:[:m| s nextPutAll: m] separatedBy:[s nextPut: $,]
		]).
	].! !

!WebRequest methodsFor: 'responses' stamp: 'ar 8/31/2010 22:16'!
sendResponse: code chunked: chunkBlock
	"Send a chunked response"

	^self sendResponse: code chunked: chunkBlock do:[:resp]! !

!WebRequest methodsFor: 'responses' stamp: 'ar 8/31/2010 22:16'!
sendResponse: code chunked: chunkBlock do: hdrBlock
	"Send a chunked response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: code.
	resp headerAt: 'Transfer-Encoding' put: 'chunked'.
	hdrBlock value: resp.
	^self sendResponse: resp contentBlock:[:aStream|
		"Set the stream in the request and pass it in the chunk block"
		resp stream: aStream.
		chunkBlock value: resp.
		"send termination chunk"
		aStream nextPutAll: '0'; crlf; crlf; flush.
	].
! !

!WebRequest methodsFor: 'responses' stamp: 'ar 7/28/2010 20:22'!
sendResponseCode: code content: aString type: contentType close: aBool
	"Send a 500 Internal server error response"

	^self sendResponseCode: code content: aString type: contentType do:[:hdr|
		aBool ifTrue:[hdr headerAt: 'Connection' put: 'close'].
	].! !

!WebRequest methodsFor: 'responses' stamp: 'ar 7/28/2010 20:19'!
sendResponseCode: code content: aString type: contentType do: aBlock
	"Send a 500 Internal server error response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: code.
	contentType ifNotNil:[resp headerAt: 'Content-Type' put: contentType].
	aBlock value: resp.
	^self sendResponse: resp content: aString.! !

!WebRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 23:35'!
sendXmlResponse: xmlString
	"Send an a-ok 200 xml-ish response"

	^self send200Response: xmlString contentType: 'application/xml'! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:48'!
stream200Response: aStream
	"Stream a 200 OK response"

	^self stream200Response: aStream size: aStream size! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:53'!
stream200Response: aStream size: streamSize
	"Stream a 200 OK response"

	^self stream200Response: aStream size: streamSize type: 'application/octet-stream'! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:53'!
stream200Response: aStream size: streamSize type: contentType
	"Stream a 200 OK response"

	^self stream200Response: aStream size: streamSize type: contentType do:[:resp].! !

!WebRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 18:14'!
stream200Response: aStream size: streamSize type: contentType do: aBlock
	"Stream a 200 OK response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 200.
	resp headerAt: 'Content-Type' put: contentType.
	aBlock value: resp.
	^self sendResponse: resp contentStream: aStream size: streamSize.! !

!WebRequest methodsFor: 'printing' stamp: 'CK 11/24/2017 14:53:04'!
writeOn: aStream
	"Write the request"

	self writeRequestOn: aStream.
	super writeOn: aStream! !

!WebRequest methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:37'!
writeRequestOn: aStream
	"Write the request"

	aStream nextPutAll: method; space; nextPutAll: rawUrl; space; nextPutAll: protocol; nextPutAll: String crlfString.
! !

!WebRequest class methodsFor: 'instance creation' stamp: 'ar 2/20/2010 11:23'!
readFrom: aStream
	"Create a WebRequest from a given stream"

	^self new readFrom: aStream! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:39'!
code
	"The HTTP response code"

	^code! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 6/18/2010 23:51'!
content
	"Reimplemented to close the socket if the request is transient"

	content ifNil:[
		content := self getContent.
		self closeIfTransient.
	].
	^content! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:02'!
contentStream
	"Answers a stream for the conent."

	(self headerAt: 'transfer-encoding') = 'chunked' ifTrue:[
		^WebChunkedStream on: stream.
	].
	^super contentStream! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 4/1/2010 16:02'!
request
	"The original request for this response"

	^request! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 4/1/2010 16:03'!
request: aRequest
	"The original request for this response"

	request := aRequest! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:39'!
status
	"The status line from the response"

	^status! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:39'!
status: aString
	"The status line from the response"

	status := aString! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 8/24/2010 21:37'!
url
	"The url of the resource at which a request was terminated after a series
	of redirects. Without redirects, this will be the same as the request url."

	^url! !

!WebResponse methodsFor: 'accessing' stamp: 'ar 8/24/2010 21:37'!
url: urlString
	"The url of the resource at which a request was terminated after a series
	of redirects. Without redirects, this will be the same as the request url."

	url := urlString! !

!WebResponse methodsFor: 'private' stamp: 'topa 4/3/2013 17:32'!
getContent
	"Do not read any content if this was a HEAD request or code is 204 (no content)"
	(request method = 'HEAD' or: [code = 204]) ifTrue:[^''].
	^super getContent! !

!WebResponse methodsFor: 'testing' stamp: 'ar 7/9/2010 00:27'!
isInformational
	"Is this a 1xx response?"

	^self code between: 100 and: 199
! !

!WebResponse methodsFor: 'testing' stamp: 'ar 8/10/2010 08:56'!
isRedirect
	"Is this a redirect response?"

	^self code between: 300 and: 399
! !

!WebResponse methodsFor: 'testing' stamp: 'ar 2/23/2010 10:13'!
isSuccess
	"Is this a successful response?"

	^self code between: 200 and: 299
! !

!WebResponse methodsFor: 'initialize' stamp: 'SvenVanCaekenberghe 6/14/2010 15:48'!
protocol: respProtocol code: respCode
	"Initialize a response from protocol and code"

	code := respCode.
	protocol := respProtocol.
	status := protocol, ' ', code asString, ' ', (self class statusCodeAt: code).
	headers := OrderedCollection new.! !

!WebResponse methodsFor: 'initialize' stamp: 'jmv 6/25/2017 19:15:56'!
readFrom: aStream
	"Parse an HTTP response from the given stream"

	| tokens |
	stream := aStream.
	status := stream upToAll: String crlfString.
	tokens := status findTokens: ' '.
	tokens size < 2 ifTrue:[^self error:'Invalid response: ', status].
	protocol := tokens first.
	code := tokens second asNumber.
	headers := WebUtils readHeadersFrom: stream.
! !

!WebResponse methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
setCookie: aCookie
	"Adds a Set-Cookie header"

	^self addHeader: 'Set-Cookie' value: aCookie asString! !

!WebResponse methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
setCookie: name value: value path: path 
	"Adds a Set-Cookie header"

	^self setCookie: ((WebCookie new)
			name: name;
			value: value;
			path: path;
		yourself)! !

!WebResponse methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
setCookie: name value: value path: path expires: date domain: domain secure: aBool
	"Adds a Set-Cookie header"

	^self setCookie: ((WebCookie new)
			name: name;
			value: value;
			path: path;
			expiry: date;
			domain: domain;
			secure: aBool;
		yourself)! !

!WebResponse methodsFor: 'cookies' stamp: 'ar 2/25/2010 18:16'!
setCookieAt: key
	"Evaluate aBlock with all the cookies deined in the request"

	self setCookiesDo:[:cookie|
		cookie name = key ifTrue:[^cookie].
	].
	^nil! !

!WebResponse methodsFor: 'cookies' stamp: 'ar 2/21/2010 09:51'!
setCookiesDo: aBlock
	"Evaluate aBlock with all the cookies deined in the request"

	self headersAt: 'Set-Cookie' do:[:hdr|
		aBlock value: (WebCookie new readFrom: hdr readStream)
	].
! !

!WebResponse methodsFor: 'printing' stamp: 'ar 2/20/2010 16:33'!
writeOn: aStream
	"Store this response on aStream"

	self writeStatusOn: aStream.
	super writeOn: aStream.
! !

!WebResponse methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:39'!
writeStatusOn: aStream
	"Store this response on aStream"

	aStream nextPutAll: status; nextPutAll: String crlfString.
! !

!WebResponse class methodsFor: 'instance creation' stamp: 'ar 2/20/2010 11:24'!
protocol: protocol code: code
	"Create a response using the given protocol / code"

	^self new protocol: protocol code: code! !

!WebServer methodsFor: 'logging' stamp: 'ar 7/28/2010 19:56'!
accessLog
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	^accessLog! !

!WebServer methodsFor: 'logging' stamp: 'ar 7/28/2010 19:56'!
accessLog: aStreamOrFilename
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	accessLog := aStreamOrFilename! !

!WebServer methodsFor: 'logging' stamp: 'ar 7/28/2010 20:02'!
logRequest: request response: response
	"Log a request after it has been successfully processed"

	accessLog ifNil:[^self].

	"Do not use the standard exception handler for errors logging; 
	it would possibly send a 500 response. Really this should be handled
	by a different process with its own error handler."

	[WebUtils logRequest: request response: response on: accessLog]
		on: Error do:[:ex| self log: 'Error in logging: ', ex description].
! !

!WebServer methodsFor: 'services' stamp: 'ar 5/11/2010 20:10'!
addService: entryPoint action: aBlockOrMessage
	"Add a service to the web server. The service is specified by an entryPoint, 
	denoting the url under which the service can be requested 
	(such as /my/great/service) and an action which is a message 
	send taking a single argument, the WebRequest."

	^self addService: entryPoint 
		action: aBlockOrMessage 
		methods: self defaultHttpMethods.! !

!WebServer methodsFor: 'services' stamp: 'jmv 4/13/2023 15:21:51'!
addService: entryPoint action: aBlockOrMessage methods: methods
	"Add a service to the web server. The service is specified by an entryPoint, 
	denoting the url under which the service can be requested 
	(such as /my/great/service) and an action which is a message 
	send taking a single argument, the WebRequest.

	The list of methods describes what HTTP methods the given url supports."

	mutex critical: [
		entryPoints at: entryPoint percentEscapeUrl asLowercase put: {methods. aBlockOrMessage}.
	].! !

!WebServer methodsFor: 'services' stamp: 'jmv 4/13/2023 15:22:01'!
removeService: entryPoint 
	"Remove a service from the HTTP service provider.  See #addService:action:"

	mutex critical: [
		entryPoints removeKey: entryPoint percentEscapeUrl asLowercase ifAbsent: [].
	].! !

!WebServer methodsFor: 'handling' stamp: 'ul 7/3/2014 23:56'!
asyncHandleConnectionFrom: aSocket
	"Handle an incoming connection asynchronously to avoid blocking the listener."

	| handlerProcess |
	handlerProcess := [
		[ [ self handleConnectionFrom: aSocket ] 
			on: Error do: [ :ex | aSocket destroy ] ]
			ensure: [
				aSocket destroy.
				mutex critical: [ 
					"If we get here, then handlerProcess is initialized, so it's safe to use that variable. Using Processor activeProcess is not possible, because this block may be evaluated by another process. Doing this outside the #ensure: block is not safe."
					connections remove: handlerProcess ifAbsent: [ ] ] ] ] newProcess.

	mutex critical:[connections add: handlerProcess].

	handlerProcess resume.
! !

!WebServer methodsFor: 'handling' stamp: 'ar 2/23/2010 22:34'!
dispatchRequest: request
	"Look up the handler for a given request and dispatch to it"

	^self dispatchRequest: request url: request url! !

!WebServer methodsFor: 'handling' stamp: 'jmv 4/13/2023 15:21:56'!
dispatchRequest: request url: dispatchUrl
	"Look up the handler for a given url and dispatch to it"

	| entryUrl action |

	"Handle TRACE requests right away"
	request method = 'TRACE' ifTrue: [
		^request send200Response: request asString contentType: 'message/http'.
	].

	"Look up the entry point for the request"
	action := nil.
	entryUrl := dispatchUrl percentEscapeUrl asLowercase.
	(entryUrl beginsWith: '/') ifFalse:[entryUrl := '/', entryUrl].
	mutex critical: [
		[entryUrl notEmpty and:[action == nil]] whileTrue: [
			action := entryPoints at: entryUrl ifAbsent:[nil].
			entryUrl := entryUrl copyUpToLast: $/.
		].
		action ifNil:[action := entryPoints at: '/' ifAbsent:[nil]].
	].

	"Handle OPTIONS requests"
	request method = 'OPTIONS' ifTrue: [
		"HEAD, TRACE, and OPTIONS are always supported"
		request sendOptionsResponse: self builtinHttpMethods,
			(action ifNil:[self defaultHttpMethods] ifNotNil:[action first])
	].

	"Handle 404 not found"
	action ifNil: [^request send404Response].

	"Handle 405 method not allowed"
	(request method = 'HEAD' "builtin" 
		or: [(action at: 1) includes: request method]) ifFalse:[
			^request send405Response: self builtinHttpMethods, action first.
		].

	^[self invokeAction: (action at: 2) request: request] 
		on: Error 
		do: [ :ex|
			errorHandler 
				ifNil:[self handleError: ex request: request]
				ifNotNil:[errorHandler value: ex value: request]].! !

!WebServer methodsFor: 'handling' stamp: 'ar 7/29/2010 18:46'!
handleConnectionFrom: aSocket
	"Synchronously handle an incoming socket connection."

	| stream request |

	"Give the process a pretty name"
	Processor activeProcess name: (String streamContents:[:s|
		aSocket remoteAddress do:[:b| s print: b] separatedBy:[s nextPut: $.].
		s nextPut: $:; print: aSocket remotePort.
		s nextPutAll: ' - ', self class name, ' request handler'.
	]).

	"Read the request from the socket, and dispatch it.
	Since we've been forked already it's okay to block."
	[
		certName ifNil:["Regular request"
			stream := SocketStream on: aSocket.
		] ifNotNil:["SSL/TLS"
			| ssl |
			ssl := Smalltalk at: #SqueakSSL ifAbsent:[self error: 'SqueakSSL not installed'].
			stream := ssl secureSocketStream on: aSocket.
			[stream sslAccept: certName] on: Error do:[^stream destroy].
		].

		[aSocket isConnected] whileTrue:[

			"Read the next request from the connection. If the connection is closed,
			or if a network error occurs, or the read times out, just close the connection
			without further ado (rather than logging an error). Any other error resulting
			from parsing the input is treated as a 400 bad request."

			stream peek ifNil:[^aSocket destroy].
			request := self newRequest.
			[request readFrom: stream] on: Error do:[:ex| 
				(ex isKindOf: NetworkError) ifFalse:[
					[request send400Response] on: Error do:[:ignore].
				].
				^aSocket destroy].

			self dispatchRequest: request.
		].
	] on: Error do:[:ex|
		(ex isKindOf: NetworkError) ifFalse:[
			self log: 'Error in request handling: ', ex description.
			self log: (self errorReportFor: ex).
		].
	].! !

!WebServer methodsFor: 'handling' stamp: 'ar 2/20/2010 14:30'!
invokeAction: action request: request
	"Invokes the given action. Subclasses can override this method to serialize
	actions if necessary. At this point, the request header has been read from
	the network, but not its content."

	^action valueWithArguments: {request}
! !

!WebServer methodsFor: 'handling' stamp: 'ar 7/28/2010 20:27'!
newRequest
	"Answer a new request.
	Subclasses should override this method to use a different request class."

	^(WebRequest new)
		server: self;
		yourself! !

!WebServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:43'!
authAccept: method request: request realm: realm header: authHeader
	"Performs the requested authentication method. 
	Returns true if successfully authenticated.
	Subclasses can extend this method to support more auth methods."

	^method asLowercase caseOf: {
		['basic'] 	-> [self basicAuth: request realm: realm header: authHeader].
		['digest'] 	-> [self digestAuth: request realm: realm header: authHeader].
	} otherwise:[false].
! !

!WebServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:43'!
authHeader: method request: request realm: realm
	"Answer the authentication header for the given method.
	Subclasses can extend this method to support more auth methods."

	^method asLowercase caseOf: {
		['basic'] 	-> ['Basic realm="', realm,'"'].
		['digest'] 	-> ['Digest realm="', realm,'", nonce="', self newNonce,'", qop="auth"'].
	} otherwise:[nil].
	
	! !

!WebServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:43'!
authenticate: request realm: realm methods: accepted do: aBlock
	"Authenticates an incoming request using one of the accepted methods.

	Evaluates aBlock upon successful authentication. Responds with a 401
	(Unauthorized) if the authentication fails."

	| method resp |
	request headersAt: 'Authorization' do:[:authHeader|
		method := authHeader copyUpTo: Character space.
		(accepted anySatisfy:[:auth| auth sameAs: method]) ifTrue:[
			(self authAccept: method request: request realm: realm header: authHeader)
				ifTrue:[^aBlock value].
		].
	].

	"Send a 401 (unauthorized) response"
	resp := request newResponse protocol: 'HTTP/1.1' code: 401.
	resp headerAt: 'Content-Type' put: 'text/html; charset=utf-8'.
	accepted do:[:auth| | hdr |
		hdr := self authHeader: auth request: request realm: realm.
		hdr ifNotNil:[resp addHeader: 'WWW-Authenticate' value: hdr].
	].
	request sendResponse: resp content: '<html><head><title>401 Unauthorized</title></head><body><h1>401 Unauthorized</h1><p>You are not authorized to access the requested URL</p></body></html>'.
! !

!WebServer methodsFor: 'authentication' stamp: 'jmv 5/26/2022 12:05:28'!
basicAuth: request realm: realm header: authHeader
	"Perform Basic auth for the given request"

	| data user pass hash |
	data := String fromUtf8Bytes: (authHeader copyAfter: $ ) base64Decoded.
	user := data copyUpTo: $:.
	pass := data copyAfter: $:.
	data atAllPut: $*. data := nil.

	hash := self hashUser: user password: pass realm: realm.
	pass atAllPut: $*. pass := nil.

	^(self passwordHashAt: user, ':', realm) = hash
! !

!WebServer methodsFor: 'authentication' stamp: 'jmv 6/25/2017 19:16:08'!
digestAuth: request realm: realm header: authHeader
	"Authenticates an incoming request using Digest auth."

	| user nonce uri response ha1 ha2 md5 qop nc cnonce 
	  nonceData nonceCounter nonceTimeout params |

	params := WebUtils parseAuthParams: authHeader.

	"Flush the nonce cache randomly about every 100 auth attempts"
	100 atRandom = 42 ifTrue:[self flushNonceCache].

	user := params at: 'username' ifAbsent:[''].
	nonce := params at: 'nonce' ifAbsent:[''].
	uri := params at: 'uri' ifAbsent:[request url].
	response := params at: 'response' ifAbsent:[''].
	
	qop := params at: 'qop' ifAbsent:[''].
	qop = 'auth' ifFalse:[^false]. "we require qop=auth"

	nc := params at: 'nc' ifAbsent:[''].
	cnonce := params at: 'cnonce' ifAbsent:[''].
	
	"Verify that this is a nonce that we have handed out, that it isn't expired
	and that the nc counter has increased from the last use."
	mutex critical:[
		nonceData := nonceCache at: nonce ifAbsent:[^false].
	].
	nonceCounter := nonceData first.		"last used nc"
	nonceTimeout := nonceData second.	"timeout for nonce"
	nc <= nonceCounter ifTrue:[^false]. 	"nc must increase"
	Time localSecondClock > nonceTimeout ifTrue:[^false]. "nonce expired"
	nonceData at: 1 put: nc.

	ha1 := self passwordHashAt: user, ':', realm.
	ha2 := WebUtils md5Digest: request method, ':', uri.
	md5 := WebUtils md5Digest: ha1, ':', nonce, ':', nc, ':', cnonce, ':', qop, ':', ha2.
	^md5 = response
! !

!WebServer methodsFor: 'authentication' stamp: 'DSG 12/5/2013 17:11'!
flushNonceCache
	"Flushes expired nonces from the cache"

	| deadline |
	deadline := Time localSecondClock.
	mutex critical:[
		nonceCache := nonceCache reject:[:nonceData| deadline > nonceData second]
	].
! !

!WebServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:36'!
hashUser: user password: pass realm: realm
	"Creates a hash for the given username password pair.
	This method is the ha1 part of digest auth and can be used for both digest
	as well as basic auth."

	^WebUtils md5Digest: user, ':', realm, ':', pass! !

!WebServer methodsFor: 'authentication' stamp: 'DSG 12/5/2013 17:12'!
newNonce
	"Create a new nonce to be used for a client authentication session"

	| nonce |
	nonce := UUID new hex.
	mutex critical:[
		nonceCache at: nonce put: {''. Time localSecondClock + self nonceDeadline}.
	].
	^nonce! !

!WebServer methodsFor: 'authentication' stamp: 'ar 2/23/2010 09:20'!
nonceDeadline
	"Answer the number of seconds a nonce is considered valid for digest auth
	by the server. After this period, the client will have to reauthenticate.
	The default is 30 minutes."

	^30*60 ! !

!WebServer methodsFor: 'authentication' stamp: 'ar 2/20/2010 12:54'!
passwordAt: username realm: realm put: password
	"Store the password hash for the key."

	| hash |
	hash := self hashUser: username password: password realm: realm.
	self passwordHashAt: (username,':',realm) put: hash! !

!WebServer methodsFor: 'authentication' stamp: 'ar 2/20/2010 12:51'!
passwordHashAt: key
	"Answer the password hash for the given key"

	^vault at: key ifAbsent:['']! !

!WebServer methodsFor: 'authentication' stamp: 'ar 2/20/2010 12:01'!
passwordHashAt: key put: hash
	"Store the password hash for the key."

	^vault at: key put: hash! !

!WebServer methodsFor: 'initialize' stamp: 'ar 5/11/2010 20:10'!
builtinHttpMethods
	"Returns the list of HTTP methods that the server handles built-in,
	i.e., without passing them to the registered service."

	^#('HEAD' 'TRACE' 'OPTIONS')! !

!WebServer methodsFor: 'initialize' stamp: 'ar 2/23/2010 12:10'!
critical: aBlock
	"Evaluate aBlock protected byt the server's mutex.
	Necessary if code needs to manipulate internal server state."

	^mutex critical: aBlock
! !

!WebServer methodsFor: 'initialize' stamp: 'ar 5/11/2010 20:10'!
defaultHttpMethods
	"Answer the list of HTTP methods that should be be supported by default.
	The methods must be implemented by the request handler."

	^#('GET' 'POST')! !

!WebServer methodsFor: 'initialize' stamp: 'ar 11/1/2010 21:07'!
destroy
	"Destroys the receiver"

	self stopListener.
	self destroyConnections.
	listenerSocket ifNotNil:[listenerSocket destroy].

	"De-register the WebServer"
	(self class forUrl: self siteUrl ifAbsent:[self]) == self 
		ifTrue:[self class removeUrl: siteUrl]
! !

!WebServer methodsFor: 'initialize' stamp: 'ul 10/16/2014 13:16'!
destroyConnections
	"Destroy all current connections."

	| copy |
	copy := mutex critical: [ connections copy ].
	" #terminate will trigger the #ensure: block in #asyncHandleConnectionFrom:, which will block if we don't release the mutex "
	copy do: [ :p | p terminate ].
	" make sure all of them were really removed "
	mutex critical: [ connections removeAll ]
! !

!WebServer methodsFor: 'initialize' stamp: 'DSG 3/28/2015 20:16'!
initialize
	"Initialize the receiver"

	mutex := Mutex new.
	entryPoints := Dictionary new.
	connections := IdentitySet new.
	vault := Dictionary new.
	nonceCache := Dictionary new.
	sessions := Dictionary new.
	self logAction: [ :aString |
		aString isString
			ifTrue: [ Transcript log: aString ]
			ifFalse: [ Transcript log: aString printString ]].
! !

!WebServer methodsFor: 'initialize' stamp: 'ar 2/22/2010 05:35'!
log: aString
	"Log the given information."

	logAction ifNotNil:[logAction valueWithArguments: {aString}].
! !

!WebServer methodsFor: 'accessing' stamp: 'ar 7/29/2010 18:49'!
certName
	"The cert name to use for a secure (SSL/TLS) web server."

	^certName! !

!WebServer methodsFor: 'accessing' stamp: 'ar 7/29/2010 18:49'!
certName: aString
	"The cert name to use for a secure (SSL/TLS) web server."

	certName := aString.! !

!WebServer methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:22'!
connections
	"The active connection processes. Can only be examined after aquiring
	the WebServer's mutex."

	^connections! !

!WebServer methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:24'!
errorHandler
	"The user-supplied error handler. If present the handler takes two arguments:
		* The error being caught by WebServer
		* The request the error occurred in.
	The user-supplied error handler can do whatever it wants, including calling
	WebServer's #handleError:request: method which is the default."

	^errorHandler! !

!WebServer methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:24'!
errorHandler: aBlock
	"The user-supplied error handler. If present the handler takes two arguments:
		* The error being caught by WebServer
		* The request the error occurred in.
	The user-supplied error handler can do whatever it wants, including calling
	WebServer's #handleError:request: method which is the default."

	errorHandler := aBlock.! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:01'!
listenerInterface
	"Returns the listener interface for the server"

	^interface! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:01'!
listenerPort
	"Returns the listener port for the server"

	^port! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:00'!
listenerProcess
	"Returns the socket listener process for the server."

	^listenerProcess! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:00'!
listenerSocket
	"Returns the listener socket for the server."

	^listenerSocket! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 05:35'!
logAction
	"The current log action (a block or message)"

	^logAction! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 05:36'!
logAction: aBlockOrMessage
	"The current log action (a block or message).
	Example:
		WebServer new logAction: [:aString| Transcript show: aString].
		WebServer new logAction: (MessageSend receiver: Transcript selector: #show).
	"

	logAction := aBlockOrMessage! !

!WebServer methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:53'!
serverDate
	"Returns the date string to be used in responses"

	^self class serverDate! !

!WebServer methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:54'!
serverString
	"Returns the server string to be used in responses.
	The response is cached for efficiency."

	^serverString ifNil:[serverString := self class serverString].! !

!WebServer methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:57'!
serverString: aString
	"Set the server string to be used in responses"

	^serverString := aString! !

!WebServer methodsFor: 'accessing' stamp: 'ar 11/1/2010 21:01'!
siteUrl
	"The site URL for WebServer, i.e., http://www.mysite.com or http://www.mysite.com/server (when used in a proxied environment) or https://www.mysite.com (when using SSL). Used to allow a client to derive the 'correct' full URL for a running server instance."

	^siteUrl! !

!WebServer methodsFor: 'accessing' stamp: 'ar 11/1/2010 21:13'!
siteUrl: aString
	"The site URL for WebServer, i.e., http://www.mysite.com or http://www.mysite.com/server (when used in a proxied environment) or https://www.mysite.com (when using SSL). Used to allow a client to derive the 'correct' full URL for a running server instance."

	siteUrl := aString! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/20/2010 12:03'!
vault
	"The vault used for storing password hashes."

	^vault! !

!WebServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 02:51'!
vault: anObject
	"The vault used for storing password hashes.

	Clients can provide their own storage mechanism. 
	Subclasses can also override the lookup and authentication mechanism."

	vault := anObject! !

!WebServer methodsFor: 'errors' stamp: 'HAW 5/6/2020 12:06:30'!
debugErrorHandler

	^[:anException :aRequest | 
		[nil handleSignal: anException ] 
			on: UnhandledError 
			do: [ :unhandledError | nil handleSignal: unhandledError ]]! !

!WebServer methodsFor: 'errors' stamp: 'ar 2/20/2010 10:35'!
errorReportFor: anError
	"Provide a report for an error that's been caught during processing"

	^[
		String streamContents:[:s| 
			self errorReportFor: anError 
				stack: ([anError signalerContext stackOfSize: 2000] 
							on: Error do:[:ex| ex return: #()])
				on: s
		].
	] on: Error do: [:ex| ex pass. 'error reporting failed'].! !

!WebServer methodsFor: 'errors' stamp: 'jmv 5/19/2015 21:55'!
errorReportFor: anError stack: preStack on: strm
	"Provide a report for an error that's been caught during processing"

	self errorStackFor: anError stack: preStack on: strm.
	strm newLine; nextPutAll: '-----------------------------------------------------------------'; newLine; newLine.
	self systemInfoOn: strm.
! !

!WebServer methodsFor: 'errors' stamp: 'jmv 5/19/2015 21:55'!
errorStackFor: anError stack: preStack on: strm
	"Provide a report for an error that's been caught during processing"

	| cnt |
	cnt := 0.
	(preStack copyFrom: 1 to: (5 min: preStack size)) do:[:ctx|
		ctx printDetails: strm.	"variable values"
		strm newLine.
	].

	strm newLine; nextPutAll: '--- The full stack ---'; newLine.
	cnt := 0.

	preStack do:[:ctx|
		cnt := cnt + 1.
		cnt = 5 ifTrue: [
			strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; newLine].
		strm print: ctx; newLine.  "just class>>selector"
	].
! !

!WebServer methodsFor: 'errors' stamp: 'jmv 5/19/2015 23:48'!
handleError: ex request: aRequest
	"The default error handling during processing. Sends a 500 response."

	| errLabel errReport |
	errLabel := [ex description] on: Error do:[:ex2| 
		ex2 return: ex class name
	].
	errReport := [self errorReportFor: ex] on: Error do:[:ex2|
		ex2 return: '<error report failed>'.
	].
	[self log: errLabel] on: Error do:[:ex2| ex return].
	[self log: String newLineString, errReport] on: Error do:[:ex2| ex return].
	aRequest send500Response: errReport. ! !

!WebServer methodsFor: 'errors' stamp: 'jmv 12/12/2023 18:29:21'!
systemInfoOn: strm
	"Provides system information for a server error report"
	| info |
	strm nextPutAll: 'VM: ';
		nextPutAll:  WebUtils platformName;
		nextPutAll: ' - ';
		nextPutAll: WebUtils vmVersion;
		newLine.

	strm nextPutAll: 'Image: ';
		nextPutAll:  SystemVersion current versionString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']';
		newLine.

	strm newLine.

	"SecurityManager default printStateOn: strm."

	"Additional info available from the windows VM"
	WebUtils platformName = 'Win32' ifTrue:[
		10001 to: 10003 do: [ :i |
			info := (Smalltalk getSystemAttribute: i) ifNil:[''].
			info isEmpty ifFalse:[strm newLine; nextPutAll: info]]]! !

!WebServer methodsFor: 'errors' stamp: 'HAW 5/6/2020 12:06:46'!
useDebugErrorHandler

	self errorHandler: self debugErrorHandler ! !

!WebServer methodsFor: 'testing' stamp: 'ar 3/30/2010 22:09'!
isRunning
	"Is the server still running?"

	^self isStopped not! !

!WebServer methodsFor: 'testing' stamp: 'ar 3/30/2010 22:09'!
isStopped
	"Is the server still running?"

	^listenerProcess == nil or:[listenerProcess isTerminated]! !

!WebServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:27'!
listenOn: aPort
	"Starts the server listening on the given port"

	^self listenOn: aPort interface: nil! !

!WebServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:28'!
listenOn: aPort interface: anInterface
	"Starts the server listening on the given port / network interface"

	^self listenOn: aPort interface: anInterface backlogSize: 8! !

!WebServer methodsFor: 'listening' stamp: 'SvenVanCaekenberghe 6/14/2010 15:48'!
listenOn: aPort interface: anInterface backlogSize: backlog
	"Starts the server listening on the given port / network interface"
	
	| ipString |
	interface := anInterface.
	port := aPort.
	ipString := interface ifNil:['*'] ifNotNil:[
		String streamContents:[:s| interface do:[:b| s print: b] separatedBy:[s nextPut:$.]]].
	self log: 'listener starting (interface: ', ipString, ' port: ', port asString, ')'.
	listenerSocket := Socket newTCP.
	interface 
		ifNil:[listenerSocket listenOn: port backlogSize: backlog]
		ifNotNil:[listenerSocket listenOn: port backlogSize: backlog interface: interface].
	listenerSocket isWaitingForConnection ifFalse:[
		self error: 'Failed to listen(interface: ', interface printString, ' port: ', aPort printString,' )'.
	].
	self startListener.! !

!WebServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:28'!
listenerProcessPriority
	"The priority at which the listener process should run"

	^Processor lowIOPriority! !

!WebServer methodsFor: 'listening' stamp: 'ar 5/4/2010 09:22'!
runListener
	"Runs the server's listener loop. 
	Accepts incoming connections and handles them asynchronously"

	| connectionSocket |
	[[[listenerSocket notNil and:[listenerSocket isValid]] whileTrue:[
		"Waiting for five seconds means that WebServer will be
		back up after roughly five seconds when the system restarts."
		connectionSocket := listenerSocket waitForAcceptFor: 5.
		"Fork this off since or else errors in acceptClient: kill the listener"
		connectionSocket ifNotNil:[self asyncHandleConnectionFrom: connectionSocket].
	]] ensure:[
		"Logging here is only for the purpose of catching sockets going randomly bad"
		self log: 'listener terminating, socket: ', (listenerSocket ifNotNil:[:s| s statusString]).

		"Try to restart the listener"
		(listenerSocket notNil and:[listenerSocket isValid not]) ifTrue:[
			listenerSocket destroy.
			listenerProcess := nil.
			"Wait a little before retrying"
			(Delay forSeconds: 2) wait.
			^self listenOn: port interface: interface
		].
	]] on: Error do:[:ex| self log: ex].
! !

!WebServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:29'!
startListener
	"Starts the server's listener process"

	self stopListener.
	listenerProcess := [self runListener] newProcess.
	listenerProcess priority: self listenerProcessPriority.
	listenerProcess name: self class name, '''s listener process'.
	listenerProcess resume.
! !

!WebServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:29'!
stopListener
	"Stops the server's listener process"

	listenerProcess ifNotNil:[listenerProcess terminate].
	listenerProcess := nil.! !

!WebServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:09'!
sessionAt: key
	"Answer the session associated with the given key"

	^mutex critical:[sessions at: key ifAbsent:[nil]].! !

!WebServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:10'!
sessionAt: key ifAbsentPut: session
	"Answer the session associated with the given key"

	^mutex critical:[sessions at: key ifAbsentPut:[session]].! !

!WebServer methodsFor: 'sessions' stamp: 'ar 5/11/2010 23:34'!
sessionAt: key put: session
	"Store the session associated with the given key"

	^mutex critical:[sessions at: key put: session].! !

!WebServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:11'!
sessions
	"The sessions associated with the WebServer. These are not used by the server
	directly but rather provided so that client code has a convenient place to store
	session data."
	
	^sessions! !

!WebServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:11'!
sessions: aDictionary
	"The sessions associated with the WebServer. These are not used by the server
	directly but rather provided so that client code has a convenient place to store
	session data."
	
	sessions := aDictionary! !

!WebServer class methodsFor: 'benchmarks' stamp: 'jmv 6/26/2017 19:12:52'!
benchmark
	"Runs a WebClient/WebServer streaming benchmark to measure throughput"

	"
		WebServer benchmark
		MessageTally spyOn:[WebServer benchmark]
	"

	| amount time resp port |
	port := 8888.
	amount := 128 * 1024 * 1024.
	WebServer reset default listenOn: port.
	WebServer default addService: '/streaming' action:[:req|
		req stream200Response: DummyStream new size: amount.
	].
	time := [
		resp := WebClient httpGet: 'http://localhost:', port, '/streaming'.
		resp isSuccess ifFalse:[self error: resp status].
		resp streamTo: DummyStream new size: resp contentLength progress: nil.
		resp close.
	] timeToRun.
	WebServer reset.
	^(amount  * 1000 // time) printStringWithCommas, ' bytes/sec'
! !

!WebServer class methodsFor: 'benchmarks' stamp: 'ar 5/4/2010 21:27'!
requestBenchmark: persistent
	"Runs a WebClient/WebServer benchmark to measure request handling speed.
	Uses a persistent connection if requested."

	"
		WebServer requestBenchmark: false.
		WebServer requestBenchmark: true.
		MessageTally spyOn:[WebServer requestBenchmark: true]
	"

	| port |
	port := 8888.
	WebServer reset default listenOn: port.
	WebServer default addService: '/benchmark' action:[:req|
		req send200Response: ''.
	].
	^[WebClient 
		requestBenchmark: 'http://localhost:', port, '/benchmark'
		persistent: persistent] ensure:[WebServer reset].
! !

!WebServer class methodsFor: 'examples' stamp: 'jmv 5/26/2022 12:08:02'!
browseDir: directory request: request
	"Responds with a directory listing back to the original request"

	| entries listing url |
	url := request url.
	(url endsWith: '/') ifTrue:[url := url allButLast].

	entries := directory children sort:[:e1 :e2|
		e1 isDirectory = e2 isDirectory 
			ifTrue:[e1 name <= e2 name]
			ifFalse:[e2 isDirectory]].

	listing := String streamContents:[:s|
		s nextPutAll:'<html><head><title>Index of ', request url,'</title></head><body>'.
		s nextPutAll:'<h1>Index of ', request url,'</h1>'.
		s nextPutAll:'<pre>      Name                      Last modified      Size  Description<hr>'.
		s nextPutAll: '[up]  <a href="', (url copyUpToLast: $/),'">parent</a>'; cr; lf.
		entries do:[:e|
			s nextPutAll:(e isDirectory ifTrue:['[dir] '] ifFalse:['      ']).
			s nextPutAll: '<a href="', url, '/', e name, '">'.
			s nextPutAll: (e name truncateTo: 30).
			s nextPutAll: '</a>'.
			e name size < 30 ifTrue:[s nextPutAll: (String new: 30 - e name size withAll: $ )].
			s nextPutAll: (e modificationTime asString padded: #right to: 20 with: $ ).
			s nextPutAll: (e isDirectory ifTrue: [String new: 20] ifFalse: [e fileSize asString padded: #right to: 20 with: $ ]).
			s cr; lf.
		].
 	].

	request
		send200Response: listing asUtf8Bytes
		contentType: 'text/html; charset=utf-8'.

! !

!WebServer class methodsFor: 'examples' stamp: 'jmv 5/19/2015 23:52'!
browseFile: file request: request
	"Responds with a file back to the original request"

	| fileSize mimeTypes resp |
	file binary.
	fileSize := file size.
"	mimeTypes := file mimeTypes ifNil:[#('application/octet-stream')]."
	mimeTypes := nil ifNil:[#('application/octet-stream')].
	resp := request newResponse protocol: 'HTTP/1.1' code: 200.
	resp headerAt: 'Content-Type' put: mimeTypes first.
	request sendResponse: resp contentStream: file size: fileSize.! !

!WebServer class methodsFor: 'examples' stamp: 'jmv 3/18/2019 13:16:59'!
browseRequest: request
	"Handle an HTTP request for browsing some resource"

	| path fd file directoryEntry fileEntry |

	"Extract the file path from the request"
	path := request url findTokens: '/'.
	path ifEmpty: [ ^self browseDir: DirectoryEntry currentDirectory request: request ].

	"Find the directory entry for the resource"
	fd := path allButLast inject: DirectoryEntry currentDirectory into: [ :dir :part | dir / part ].
	
	directoryEntry _ fd / path last.
	directoryEntry exists ifTrue: [
		"Send file listing. We handle this synchronously for simplicity."
		self browseDir: directoryEntry request: request.
		^ self].
	fileEntry _ fd // path last.
	fileEntry exists
	ifFalse: [^request send404Response ]
	ifTrue: [
		"Send file content. Since files can be large, we fork this off.
		However, since we forked it, we need to handle two different
		possible conditions:
			- closing the file in case of error
			- handling errors when sending it
		This makes the code below a bit ugly"
		[[
			file := fileEntry readStream.
			[ self browseFile: file request: request ]
				ensure: [ file close ] 	"close file even in case of error"
		] on: Error do: []				"ignore errors altogether"
		] fork. 							"fork it"
	]! !

!WebServer class methodsFor: 'examples' stamp: 'HAW 5/6/2020 09:45:44'!
doItFrom: req

	| data result |
	
	req isPostRequest 
		ifTrue:[
			"We fetch all of the data instead of streaming it"
			data := req content. ]
		ifFalse: [ req isGetRequest 
			ifTrue: [ data := req rawUrl unescapePercents allButFirst ]
			ifFalse: [ ^req send404Response ]].
		
	"Compute the result"
	result := [Compiler evaluate: data] on: Error do:[:ex| ex return: ex asString].
	req send200Response: result asString 
	! !

!WebServer class methodsFor: 'examples' stamp: 'ar 2/23/2010 09:30'!
exampleAuth		"WebServer exampleAuth"
	"Simple example requiring basic auth"

	| server port realm |
	port := 9999.
	server := WebServer reset default.
	
	"The realm to use for the server"
	realm := 'Example Realm'.

	"Add a sample user (note: password is hashed, not stored verbatim)"
	server passwordAt: 'squeak' realm: realm put: 'squeak'.

	server listenOn: port.
	server addService: '/' action:[:req| 
		server 
			authenticate: req 
			realm: realm
			methods: #(digest basic) 
			do:[self browseRequest: req]].
! !

!WebServer class methodsFor: 'examples' stamp: 'ar 2/20/2010 14:54'!
exampleBrowse	"WebServer exampleBrowse"
	"This example implements a simple http server allowing to view 
	and download files (like browsing ftp sites etc)."

	| server port |
	port := 9999.
	server := WebServer reset default.
	server listenOn: port.
	server addService: '/' action:[:req| self browseRequest: req].
! !

!WebServer class methodsFor: 'examples' stamp: 'HAW 5/6/2020 09:40:26'!
exampleDoIt		"WebServer exampleDoIt"
	"This example executes doIts from a POST request.
	It's the simplest possible RPC interface to Squeak."

	| server port |

	port := 9999.
	server := WebServer reset default.
	server listenOn: port.
	server addService: '/' action:[:req| self doItFrom: req ].
		
! !

!WebServer class methodsFor: 'examples' stamp: 'ar 7/29/2010 18:49'!
exampleSSL: certName

	"This example implements a simple https server allowing to view 
	and download files (like browsing ftp sites etc):

		WebServer exampleSSL: 'Internet Widgits Pty'.
		WebServer exampleSSL: '/home/andreas/certs/testcert.pem'.

	"

	| server port |
	port := 8443.
	server := WebServer reset default.
	server accessLog: Transcript.
	server certName: certName.
	server listenOn: port.
	server addService: '/' action:[:req| self browseRequest: req].
! !

!WebServer class methodsFor: 'examples' stamp: 'jmv 4/13/2023 15:12:52'!
exampleSession		"WebServer exampleSession"
	"This example uses a simple session cookie to avoid 
	authentication for every request."

	| server port realm |
	realm := 'Example Realm'.	"authentication realm"
	port := 9999.
	server := WebServer reset default.
	server listenOn: port.

	"Add a sample user (note: password is hashed, not stored verbatim)"
	server passwordAt: 'squeak' realm: realm put: 'squeak'.

	"Register the login service (sets cookie when successful)"
	server addService: '/login' action:[:req| 
		server  authenticate: req  realm: realm methods: #(digest)  do:[ | id |
			"Set the cookie and redirect back to /"
			server sessionAt: (id := UUID new hex) put: ''.
			req send302Response: (req fields at: 'url' ifAbsent:['/'])
				do:[:resp| resp setCookie: 'session' value: id path: '/']]].

	"Register the normal browse service"
	server addService: '/' action:[:req|
		"Check if we have a valid session cookie. If not redirect to login."
		(server sessionAt: (req cookieAt: 'session')) notNil
			ifTrue:[self browseRequest: req]
			ifFalse:[req send302Response: '/login?url=', req url percentEscapeUrlField]].
! !

!WebServer class methodsFor: 'accessing' stamp: 'ar 2/23/2010 09:29'!
default
	"A default WebServer instance. Mostly for the examples."

	^Default ifNil:[Default := self new].! !

!WebServer class methodsFor: 'accessing' stamp: 'ar 2/23/2010 09:29'!
reset		"WebServer reset"
	"Reset the default WebServer instance."

	Default ifNotNil:[
		Default destroy.
		Default := nil.
	].! !

!WebServer class methodsFor: 'accessing' stamp: 'DSG 5/29/2012 17:57'!
serverDate
	"Returns the date string to be used in responses"
	
	"FIXME: check http spec for time format"

	| date |
	^String streamContents:[:s|
		date := DateAndTime now asUTC.
		s nextPutAll: date dayOfWeekAbbreviation, ', '.
		date printOn: s.
		s nextPutAll: ' GMT'.
	].! !

!WebServer class methodsFor: 'accessing' stamp: 'jmv 12/12/2023 18:31:59'!
serverString
	"Returns the server string to be used in responses"

	^ 'WebServer/1.5 (',
		SystemVersion current versionString,'; ',
		WebUtils platformName,')'.! !

!WebServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
forUrl: urlString
	"Answer the WebServer registered for a given URL string."

	^self forUrl: urlString ifAbsent:[self newForUrl: urlString]! !

!WebServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
forUrl: urlString ifAbsent: aBlock
	"Answer the WebServer registered for a given URL string.
	If no WebServer is registered, evaluate aBlock."

	^self registry at: urlString ifAbsent: aBlock! !

!WebServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 21:02'!
newForUrl: urlString
	"Create a new WebServer instance registered for a given URL string."

	| server |
	"Out with the old ...."
	server := self removeUrl: urlString.
	server ifNotNil:[server destroy].

	"... and in with the new."
	server := self new.
	server siteUrl: urlString.
	self registry at: urlString put: server.
	^server
! !

!WebServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 21:11'!
registry
	"Answer the WebServer site registry"

	^Registry ifNil:[Registry := Dictionary new]! !

!WebServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
removeUrl: urlString
	"Remove and answer the WebServer registered for a given URL string.
	If no WebServer was registered, return nil."

	^self removeUrl: urlString ifAbsent:[nil]! !

!WebServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
removeUrl: urlString ifAbsent: aBlock
	"Remove and answer the WebServer registered for a given URL string."

	^self registry removeKey: urlString ifAbsent: aBlock! !

!WebSocket methodsFor: 'private' stamp: 'ar 9/26/2011 11:41'!
handle: selector arguments: argList

	^self handle: selector arguments: argList ifAbsent:[nil]! !

!WebSocket methodsFor: 'private' stamp: 'ar 9/26/2011 11:41'!
handle: selector arguments: argList ifAbsent: aBlock

	^(handlers at: selector ifAbsent:[^aBlock value])
		valueWithPossibleArgs: argList.! !

!WebSocket methodsFor: 'private' stamp: 'ar 9/26/2011 10:47'!
handleClose
	"Handle a connection close"

	^self handle: 'close' arguments: (Array with: self)
! !

!WebSocket methodsFor: 'private' stamp: 'ar 9/26/2011 10:46'!
handleError: exception
	"Handle an error"

	^self handle: 'error' arguments: (Array with: exception with: self)! !

!WebSocket methodsFor: 'private' stamp: 'ar 9/26/2011 10:45'!
handleMessage: msgData type: msgType
	"Handle an incoming message of the given type"

	self handle: 'message' arguments: (Array with: msgData with: msgType with: self)! !

!WebSocket methodsFor: 'private' stamp: 'ar 7/8/2010 21:48'!
printOn: aStream
	"Print the receiver"

	aStream nextPutAll: self class name.
	aStream nextPutAll: '(', name, ')'.! !

!WebSocket methodsFor: 'initialize' stamp: 'ar 9/26/2011 13:06'!
close
	"Request graceful close"

	^self subclassResponsibility! !

!WebSocket methodsFor: 'initialize' stamp: 'ar 9/27/2011 13:42'!
initialize
	super initialize.
	handlers := Dictionary new.! !

!WebSocket methodsFor: 'initialize' stamp: 'DSG 3/28/2015 20:00'!
on: aStream
	"Create a new WebSocket instance using the given SocketStream"

	stream := aStream.
	name := String streamContents:[:s| 
		stream socket remoteAddress 
			do:[:b| s print: b] 
			separatedBy:[s nextPut: $.]].
	name := name, ':', stream socket remotePort asString.
	! !

!WebSocket methodsFor: 'running' stamp: 'ar 7/8/2010 20:00'!
fork
	"Fork the WebSocket's input process."

	process := [self run] newProcess.
	process resume.
! !

!WebSocket methodsFor: 'running' stamp: 'ar 9/26/2011 10:46'!
onClose: aBlock
	"Set the handler block to invoke when the socket is closed"

	handlers at: 'close' put: aBlock.! !

!WebSocket methodsFor: 'running' stamp: 'ar 9/26/2011 10:45'!
onError: aBlock
	"Set the handler block to invoke when an error occurs"

	handlers at: 'error' put: aBlock.! !

!WebSocket methodsFor: 'running' stamp: 'ar 9/26/2011 10:45'!
onMessage: aBlock
	"Set the handler block to invoke when a message is received"

	handlers at: 'message' put: aBlock.
! !

!WebSocket methodsFor: 'running' stamp: 'ar 9/26/2011 10:41'!
run
	"Run the WebSocket's input process."

	^self subclassResponsibility! !

!WebSocket methodsFor: 'read/write' stamp: 'ar 9/26/2011 10:42'!
send: aString
	"Writes one frame worth of data"

	^self subclassResponsibility! !

!WebSocket methodsFor: 'accessing' stamp: 'ar 7/8/2010 21:39'!
timeout
	"The underlying stream timeout"

	^stream timeout! !

!WebSocket methodsFor: 'accessing' stamp: 'ar 7/8/2010 21:40'!
timeout: seconds
	"Sets the underlying stream timeout"

	^stream timeout: seconds! !

!WebSocket class methodsFor: 'example' stamp: 'jmv 6/25/2017 20:16:51'!
example	"WebSocket example"

	"This is a very simple WebSocket example implementing a basic chat system.
	Each message sent by a client connection is broadcast to everyone."

	| mutex sockets |
	mutex := Mutex new.
	sockets := OrderedCollection new.
	WebServer reset default listenOn: 8080.

	WebServer default addService: '/' action:[:req|
		"Serves the main html chat page"
		req send200Response: self examplePage contentType: 'text/html'.
	].

	WebServer default addService: '/broadcast' action:[:req| | ws |
		"Implements the WebSocket broadcaster"
		ws := req asWebSocket.
		ws timeout: 300. "5 minutes timeout"
	
		"Transcript log: 'New ', ws requestLine."
		Transcript log: 'New '.
		
		ws onMessage:[:data|
			mutex critical:[sockets do:[:s| s send: data]].
		].
		ws onClose:[
			Transcript log: 'Closing ', ws requestLine.
			mutex critical:[sockets remove: ws ifAbsent:[]].
		].
		ws onError:[:ex|
			Transcript log: ws requestLine.
			Transcript log: ex description.
			Transcript log: ex signalerContext longStack.
		].
		mutex critical:[sockets add: ws].

		"We don't need to #fork here, in fact we shouldn't since it ensures
		that shutting down WebServer shuts down any WebSockets, too."
		ws run.
	].! !

!WebSocket class methodsFor: 'example' stamp: 'ar 7/8/2010 21:50'!
examplePage
	"The example HTML page for WebServer"

	^'<html>
<p id="status">WebSocket: Not connected</p> 
<form> 
    <input type="text" id="chat" placeholder="type and press enter to chat" /> 
</form> 
<p>To test, open two windows with Web Socket support, type a message above and press return.</p> 
<ul id="log"></ul> 

<script>

var conn = {};
var log = document.getElementById("log");
var chat = document.getElementById("chat");
var form = chat.form;
var state = document.getElementById("status"); 
 
if (window.WebSocket === undefined) {
  state.innerHTML = "(WebSockets not supported)";
} else {
    conn = new WebSocket("ws://localhost:8080/broadcast");
    conn.onopen = function () {
      state.innerHTML = "WebSocket: Connected";
    };
 
    conn.onmessage = function (event) {
      log.innerHTML = "<li>" + event.data + "</li>" + log.innerHTML;
    };
    
    conn.onclose = function (event) {
      state.innerHTML = "WebSocket: Closed";
    };

	form.addEventListener("submit", function (event) {
		event.preventDefault();
		if (conn.readyState === 1) {
			conn.send(chat.value);      
			chat.value = "";
		}
	}, false);
};
 
</script> 

</body>
</html>'! !

!WebSocket class methodsFor: 'instance creation' stamp: 'ar 7/8/2010 18:22'!
on: aStream
	"Create a new WebSocket instance using the given SocketStream"

	^self new on: aStream! !

!WebSocket00 methodsFor: 'running' stamp: 'jmv 1/9/2024 17:42:32'!
close
	"Request graceful close"

	stream isConnected ifTrue:[
		stream nextPut: (Character codePoint: 255).
		stream nextPutAll: (Character codePoint: 0).
		stream flush.
	].
! !

!WebSocket00 methodsFor: 'running' stamp: 'ar 7/8/2010 21:49'!
run
	"Run the WebSocket's input process."

	[[[stream isConnected] whileTrue:[
		self readMessage.
		"XXXX: Should this be moved into handleMessage:type: ???"
		frameType = 255 ifTrue:[stream close. ^self].
		self handleMessage: frameData type: frameType.
	]] on: Error do:[:ex| self handleError: ex]] ensure:[
		[stream socket destroy] on: Error do:[:ex|].
		self handleClose.
	].
! !

!WebSocket00 methodsFor: 'read/write' stamp: 'jmv 1/9/2024 17:38:41'!
readMessage
	"Reads one frame worth of data"

	| byte length |
	byte := stream next ifNil: [
		frameType := 255.
		^nil].
	byte isCharacter ifTrue: [ byte := byte codePoint ].
	frameType := byte.
	frameType < 128 ifTrue:[
		"Frame type < 128: UTF-8 encoded text with 0xFF frame delimiter"
		frameData := String fromUtf8Bytes: (stream upTo: (Character codePoint: 255)) asByteArray.
	] ifFalse:[
		"Frame type >= 128: Binary data with length prefix"
		length := 0.
		[
			byte := stream next.
			byte isCharacter ifTrue: [ byte := byte codePoint ].
			length := (length * 128) + (byte bitClear: 128).
			byte >= 128 ] whileTrue.
		frameData := ByteArray new: length.
		stream nextInto: frameData.
	].
! !

!WebSocket00 methodsFor: 'read/write' stamp: 'ar 9/26/2011 12:03'!
send: aString
	"Writes one frame worth of data"

	^self send: aString type: 0! !

!WebSocket00 methodsFor: 'read/write' stamp: 'jmv 1/9/2024 17:42:37'!
send: data type: type
	"Writes one frame worth of data using the given frame type"

	"Frame type"
	stream nextPut: (Character codePoint: type).
	type < 128 ifTrue:[
		"Frame type < 128: UTF-8 encoded text with 0xFF frame delimiter"
		stream nextPutAll: data asString asUtf8Bytes.
		stream nextPut: (Character codePoint: 255).
	] ifFalse:[ | prefix length |
		"Frame type >= 128: Binary data with length prefix"
		prefix := ByteArray new.
		length := data size.
		[length > 127]  whileTrue:[
			prefix := prefix copyWith: prefix \\ 128.
			length := length // 128.
		].
		prefix := prefix copyWith: length.
		stream nextPutAll: prefix reversed.
		stream nextPutAll: data.
		stream flush.
	].
	stream flush.
! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 13:06'!
close
	"Send a close control request"

	closing ifFalse:[
		closing := true.
		self send: #[] code: 8.
	].! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 13:07'!
handleClose: payload
	"Close is being sent by remote in process of shutting down connection"

	closing ifFalse:[^self close].

	"We're done. We've received the close and responded to it"
	stream close.

! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 11:37'!
handleControl: payload type: opcode
	"Handle a control opcode"

	opcode caseOf: {
		[8]	-> [self handleClose: payload].
		[9]	-> [self handlePing: payload].
		[10] -> [self handlePong: payload].
	} otherwise:[self error: 'Unknown control request: ', opcode].

! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 11:41'!
handlePing: payload

	self handle: 'ping' 
		arguments: (Array with: payload with: self)
		ifAbsent:[self pong: payload].
! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 11:40'!
handlePong: payload

	self handle: 'pong' arguments: (Array with: payload with: self).! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 12:49'!
onPing: aBlock
	"Set the handler block to invoke when a ping is received"

	handlers at: 'ping' put: aBlock.! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 12:49'!
onPong: aBlock
	"Set the handler block to invoke when a pong is received"

	handlers at: 'pong' put: aBlock.! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 12:03'!
ping: data
	"Send a ping control request"

	self send: data code: 9.! !

!WebSocket07 methodsFor: 'control requests' stamp: 'ar 9/26/2011 12:03'!
pong: data
	"Send a pong control request"

	self send: data code: 10.! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/26/2022 12:08:10'!
firstFragment: aStringOrBytes
	"Writes the first fragment of data"

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asUtf8Bytes code: 1 final: false]
		ifFalse:[^self send: aStringOrBytes code: 2 final: false].! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/26/2022 12:08:13'!
lastFragment: aStringOrBytes
	"Writes the last fragment of data.
	Note: Spec says that code is ZERO for final fragment."

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asUtf8Bytes code: 0 final: true]
		ifFalse:[^self send: aStringOrBytes code: 0 final: true].! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/26/2022 12:08:19'!
nextFragment: aStringOrBytes
	"Writes the first fragment of data
	Note: Spec says that code is ZERO for intermediate fragments."

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asUtf8Bytes code: 0 final: false]
		ifFalse:[^self send: aStringOrBytes code: 0 final: false].! !

!WebSocket07 methodsFor: 'read/write' stamp: 'ar 9/27/2011 13:31'!
readFrame
	"Reads one frame worth of data"

	^self readFrameFrom: stream! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 1/9/2024 17:40:31'!
readFrameFrom: aStream
	"Reads one frame worth of data"

	| byte length final mask opcode payload |
	byte := aStream next ifNil: [^nil].
	byte isCharacter ifTrue: [ byte := byte codePoint ].

	final := byte anyMask: 128.
	opcode := byte bitAnd: 15.

	byte := aStream next ifNil: [^nil].
	byte isCharacter ifTrue: [ byte := byte codePoint ].
	mask := byte anyMask: 128.
	length := byte bitAnd: 127.

	length = 126 ifTrue:[
		length := self nextNumber: 2 from: aStream.
	] ifFalse:[
		length = 127 ifTrue:[length := self nextNumber: 8 from: aStream].
	].

	mask 
		ifTrue:[mask := aStream next: 4] 
		ifFalse:[mask := nil].

	payload := ByteArray new: length.
	aStream next: length into: payload.

	"Unmask payload"
	mask ifNotNil:[self xor: payload with: mask asByteArray].

	"First see if this is a control frame which can be inserted into fragmented messages."
	opcode >= 8 ifTrue:[^self handleControl: payload type: opcode].

	"Now see if this is a fragmented message"
	frameType ifNil:[
		frameType := opcode.
		frameData := Array with: payload.
	] ifNotNil:[
		frameData := frameData copyWith: payload.
	].

	final ifTrue:[
		"This was the final fragment"
		frameData size = 1 ifTrue:[
			payload := frameData first
		] ifFalse:[
			payload := frameData allButFirst 
							inject: frameData first 
							into:[:each :next| each, next].
		].
		frameType = 1 ifTrue:[payload := String fromUtf8Bytes: payload asByteArray].
		self handleMessage: payload type: frameType.
		frameType := frameData := nil.
	].
! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/26/2022 12:08:22'!
send: aStringOrBytes
	"Writes one frame worth of data"

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asString asUtf8Bytes code: 1]
		ifFalse:[^self send: aStringOrBytes code: 2].! !

!WebSocket07 methodsFor: 'read/write' stamp: 'ar 9/26/2011 12:48'!
send: data code: opcode
	"Sends unfragmented data using the given opcode"

	^self send: data code: opcode final: true! !

!WebSocket07 methodsFor: 'read/write' stamp: 'ar 9/27/2011 13:47'!
send: data code: opcode final: isFinal
	"Sends unfragmented data using the given opcode"

	^self send: data code: opcode final: isFinal mask: self masking! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/7/2023 18:59:40'!
send: data code: opcode final: isFinal mask: doMask
	"Sends unfragmented data using the given opcode"

	| byte1 byte2 mask xLength copy |
	byte1 := opcode. "FIN = 1, RSV1=RSV2=RSV3=0"
	isFinal ifTrue:[byte1 := byte1 bitOr: 128].
	data size > 125 ifTrue:[
		data size > 16rFFFF ifTrue:[
			byte2 := 127.
			xLength := ByteArray new: 8.
			(WriteStream on: xLength)
				nextUint32Put: data size >> 32 bigEndian: true;
				nextUint32Put: (data size bitAnd: 16rFFFFFFFF) bigEndian: true.
		] ifFalse:[
			byte2 := 126.
			xLength := ByteArray new: 2.
			(WriteStream on: xLength) nextUint16Put: data size bigEndian: true.
		].
	] ifFalse:[
		byte2 := data size.
		xLength := #[].
	].
	doMask ifTrue:[
		byte2 := byte2 bitOr: 128. "Mask = 1"
		mask := ByteArray new: 4.
		1 to: 4 do:[:i| mask at: i put: 256 atRandom-1].
		copy := data copy asByteArray.
		self xor: copy with: mask.
	] ifFalse:[
		copy := data.
	].
	stream nextPut: byte1.
	stream nextPut: byte2.
	stream nextPutAll: xLength.
	mask ifNotNil:[stream nextPutAll: mask].
	stream nextPutAll: copy.
	stream flush.! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 1/10/2023 14:50:06'!
xor: data with: mask
	"XOR all the data using the given mask bytes.

	WARNING: Does inplace reversal; do not call with literal data."

	"This would be much faster w/ BitBlt but data may not be 
	4-byte aligned and so BB will barf."
	1 to: data size do:[:i|
		data at: i put: ((data at: i) bitXor: (mask at: i-1 \\ 4 + 1)).
	].
! !

!WebSocket07 methodsFor: 'initialize' stamp: 'klub 6/13/2012 17:00'!
initialize

	super initialize.
	closing := false.
	masking := true. " 	http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07#section-4.1 says:
   ''Frames sent from the client to the server are masked to
   avoid confusing network intermediaries, such as intercepting proxies.''"! !

!WebSocket07 methodsFor: 'accessing' stamp: 'ar 9/27/2011 13:47'!
masking
	"If true, outgoing frames are masked."

	^masking! !

!WebSocket07 methodsFor: 'accessing' stamp: 'ar 9/27/2011 13:47'!
masking: aBool
	"If true, outgoing frames are masked."

	masking := aBool! !

!WebSocket07 methodsFor: 'private' stamp: 'jmv 1/9/2024 17:39:32'!
nextNumber: n from: aStream
	"Answer the next n bytes as a positive Integer or LargePositiveInteger."
	| s |
	s := 0.
	1 to: n do: 	[ :i | | b |
		b := aStream next.
		b isCharacter ifTrue: [ b := b codePoint ].
		s := (s bitShift: 8) bitOr: b ].
	^ s normalize! !

!WebSocket07 methodsFor: 'running' stamp: 'ar 9/26/2011 12:40'!
run
	"Run the WebSocket's input process."

	[[[stream isConnected] whileTrue:[
		self readFrame.
	]] on: Error do:[:ex| self handleError: ex]] ensure:[
		[stream socket destroy] on: Error do:[:ex|].
		self handleClose.
	].
! !

!WebUtils class methodsFor: 'decoding' stamp: 'jmv 6/25/2017 19:16:15'!
decodeMultipartForm: aStream boundary: boundary do: aBlock
	"Parse the contents of a multipart/form-data submission.

	Evaluate aBlock with three parts: The headers, the (parsed) form-data
	arguments and the (undecoded) contents of the part. The sender is
	expected to take care of other issues such as content-transfer-encoding
	and similar headers."

	| skip headers content disposition index params |
	aStream upToAll: '--', boundary.
	[aStream atEnd or:[(skip := aStream next: 2) = '--']] whileFalse:[
		skip = String crlfString ifFalse:[self error: 'Error decoding multipart/form-data fields'].
		headers := Dictionary new.
		(WebUtils readHeadersFrom: aStream) do:[:hdr| headers add: hdr].
		content := aStream upToAll: String crlfString, '--', boundary.
		params := Dictionary new.
		disposition := headers at: 'content-disposition' ifAbsent:[''].
		#(name filename) do:[:arg| | len val |
			len := arg size + 2.
			index := disposition findString: arg,'='.
			index > 0 ifTrue:[
				val := disposition copyFrom: index + len to: (disposition indexOf: $" startingAt: index+len) - 1.
				params at: arg put: val.
			].
		].
		aBlock value: headers value: params value: content.
	].! !

!WebUtils class methodsFor: 'decoding' stamp: 'ar 2/21/2010 09:54'!
decodeUrlEncodedForm: aString
	"Decodes the fields embedded in the url ?var1=val1&var2=val2"

	^self decodeUrlEncodedForm: aString multipleValues: false! !

!WebUtils class methodsFor: 'decoding' stamp: 'DSG 5/24/2012 21:19'!
decodeUrlEncodedForm: aString do: aBlock
	"Decodes the fields embedded in the url ?var1=val1&var2=val2.
	Evaluates aBlock with the key/value pairs defined in the fields."

	| args keyval index key value |
	args := aString readStream.
	[args atEnd] whileFalse:[
		keyval := args upTo: $&.
		(index := keyval indexOf: $=) = 0 ifTrue:[
			key := keyval.
			value := ''.
		] ifFalse:[
			key := keyval copyFrom: 1 to: index-1.
			value := keyval copyFrom: index +1 to: keyval size.
		].
		key := key unescapePercents.
		value := value unescapePercents.
		aBlock value: key value: value.
	].
! !

!WebUtils class methodsFor: 'decoding' stamp: 'ar 7/20/2010 20:37'!
decodeUrlEncodedForm: aString multipleValues: aBool
	"Decodes the fields embedded in the url ?var1=val1&var2=val2"

	| fields |
	fields := Dictionary new.
	self decodeUrlEncodedForm: aString do:[:key :value|
		aBool 
			ifTrue:[fields at: key put: (fields at: key ifAbsent:['']), value]
			ifFalse:[fields at: key put: value].
	].
	^fields! !

!WebUtils class methodsFor: 'decoding' stamp: 'jmv 6/26/2017 19:54:45'!
encodeMultipartForm: fieldMap boundary: boundary
	"Encodes the fieldMap as multipart/form-data.

	The fieldMap may contain MIMEDocument instances to indicate the presence
	of a file to upload to the server. If the MIMEDocument is present, its
	content type and file name will be used for the upload.

	The fieldMap can be EITHER an array of associations OR a Dictionary of 
	key value pairs (the former is useful for providing multiple fields and/or 
	specifying the order of fields)."

	^String streamContents:[:stream|
		(fieldMap as: Dictionary) keysAndValuesDo:[:fieldName :fieldValue | | fieldContent |
			"Write multipart boundary and common headers"
			stream nextPutAll: '--', boundary; nextPutAll: String crlfString.
			stream nextPutAll: 'Content-Disposition: form-data; name="', fieldName, '"'.
			"Figure out if this is a file upload - This code was deleted. Cuis does not currently include MIMEDocument. See Squeak."
			fieldContent := fieldValue.
			stream nextPutAll: String crlfString; nextPutAll: String crlfString.
			stream nextPutAll: fieldContent asString.
			stream nextPutAll: String crlfString.
		].
		stream nextPutAll: '--', boundary, '--', String crlfString.
	].
! !

!WebUtils class methodsFor: 'decoding' stamp: 'jmv 4/13/2023 15:14:07'!
encodeUrlEncodedForm: fieldMap
	"Encodes the fieldsMap in the form of var1=val1&var2=val2.
	The fieldMap can be EITHER an array of associations OR a Dictionary of 
	key value pairs (the former is useful for providing multiple fields and/or 
	specifying the order of fields)."

	| associations |
	associations := (fieldMap isKindOf: Dictionary) ifTrue:[fieldMap associations] ifFalse:[fieldMap].
	^String streamContents:[:s|
		associations do:[:assoc|
			s nextPutAll: assoc key percentEscapeUrlField.
			assoc value ifNotNil:[
				s nextPutAll: '='.
				s nextPutAll: assoc value asString percentEscapeUrlField
			].
		] separatedBy:[s nextPutAll:'&'].
	].! !

!WebUtils class methodsFor: 'decoding' stamp: 'jmv 5/19/2015 21:52'!
multipartBoundary
	"Answer a string to be used as multpart boundary in posts"

	^'----squeak-', Date today printString,'-', Time localMillisecondClock printString, '-webclient-----'.! !

!WebUtils class methodsFor: 'proxy' stamp: 'CK 11/24/2017 13:59:29'!
defaultProxyServer
	"Answer the default proxy server to use."
	^ currentProxyServer ! !

!WebUtils class methodsFor: 'proxy' stamp: 'CK 11/24/2017 14:00:05'!
defaultProxyServer: aString
	"Set the default proxy server to use."
	currentProxyServer _ aString ! !

!WebUtils class methodsFor: 'proxy' stamp: 'DSG 12/4/2013 23:03'!
proxyExceptionsDo: aBlock
	"Evaluate aBlock with all registered proxy exceptions. 
	Uses the information from HTTPSocket."

	"HTTPSocket httpProxyExceptions do: aBlock."! !

!WebUtils class methodsFor: 'proxy' stamp: 'CK 11/27/2017 12:06:52'!
proxyServerFor: serverName
	"Answer the proxy server to use for the given server:port
	Returns the proxy server:port to use or nil if no proxy is required."

	| domain |
	domain := serverName copyUpTo: $:.
	self proxyExceptionsDo:[:pattern| (pattern match: domain) ifTrue:[^nil]].
	^self defaultProxyServer! !

!WebUtils class methodsFor: 'websockets' stamp: 'ar 7/20/2010 20:38'!
extractWebSocketKey: field
	"Decodes a WebSocket key:

		Sec-WebSocket-Key1: 18x 6]8vM;54 *(5:  {   U1]8  z [  8
		Sec-WebSocket-Key2: 1_ tx7X d  <  nw  334J702) 7]o}` 0

		For each of these fields, the server has to take the digits from the
		 value to obtain a number (in this case 1868545188 and 1733470270
		respectively), then divide that number by the number of spaces
		characters in the value (in this case 12 and 10) to obtain a 32-bit
		number (155712099 and 173347027).  These two resulting numbers are
		then used in the server handshake, as described below.
	"

	| digits spaces |
	digits := spaces := 0.
	field do:[:ch|
		ch isDigit ifTrue:[digits := digits * 10 + ch digitValue].
		ch = $ ifTrue:[spaces := spaces + 1].
	].
	^digits // spaces.
! !

!WebUtils class methodsFor: 'websockets' stamp: 'jmv 5/7/2023 11:08:48'!
webSocketHandshake: key1 with: key2 with: data
	"Do the actual WebSocket handshake computation"

	| bytes |
	bytes := ByteArray new: 16.
	bytes int32At: 1 put: key1 bigEndian: true.
	bytes int32At: 5 put: key2 bigEndian: true.
	bytes replaceFrom: 9 to: 16 with: data.
	^(self md5HashStream: bytes readStream) reversed! !

!WebUtils class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:30'!
webSocketHash07: key
	"Do the actual WebSocket hash computation"

	| uid |
	uid := '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'.
	^(ByteArray newFrom: (SecureHashAlgorithm new hashMessage: key, uid)) reversed base64Encoded! !

!WebUtils class methodsFor: 'auth' stamp: 'jmv 5/26/2022 12:07:05'!
handleAuth: aBlock
	"Utility method to handle web authentication requests interactively.

	Use like here:
		| wc |
		wc := WebClient new.
		WebUtils handleAuth:[wc httpGet: 'http://some.auth.required'].
	"

	| user pass |
	^[aBlock value] on: WebAuthRequired do:[:ex|
		"Allow outer handlers to take over authentication"
		ex isNested ifFalse:[
			user := FillInTheBlankMorph request:  ex message, 
				'\Please enter your user name:' initialAnswer:''.
			user ifNotEmpty:[
				"Just a little obfuscation to avoid completely plain passwords"
				pass := (FillInTheBlankMorph request:
					'Please enter the password for "', user,'":') asUtf8Bytes base64Encoded.
				 ex username: user password: [String fromUtf8Bytes: pass base64Decoded]]].
		ex pass].! !

!WebUtils class methodsFor: 'oauth' stamp: 'DSG 3/28/2015 23:22'!
hmacSha1: message key: signKey
	"Compute the SHA1 HMAC for the given message"

	| blockSize key ipad opad |
	blockSize := 64. "SHA1 block size"
	key := signKey asByteArray.
	key size > blockSize ifTrue:[key := self sha1Hash: key].
	key size < blockSize ifTrue:[key := key, (ByteArray new: blockSize - key size)].
	ipad := ByteArray new: blockSize withAll: 16r36.
	opad := ByteArray new: blockSize withAll: 16r5C.
	^self sha1Hash: 
		(key with: opad collect:[:b1 :b2| b1 bitXor: b2]), 
		(self sha1Hash: (key with: ipad collect:[:b1 :b2| b1 bitXor: b2]), message)! !

!WebUtils class methodsFor: 'oauth' stamp: 'jmv 4/13/2023 15:14:25'!
oAuthSign: webRequest url: requestUrl extra: postFields using: params
	"Sign the given webRequest using the set of oauth parameters provided"

	| epoch timestamp nonce normalized baseString query signature key header extraKeys baseUrl |

	"Allow providing an oauth_timestamp; HOWEVER this is only useful for
	tests etc. For actual API calls the timestamp *must* match real-time closely
	or else the request will be rejected".

	timestamp := params at: 'oauth_timestamp' ifAbsent:[
		"Timestamp is relative to Jan. 1st 1970 GMT"
		epoch := DateAndTime year: 1970 day: 1 hour: 0 minute: 0 second: 0 
			offset: (Duration seconds: 0).
		(DateAndTime now asUTC - epoch) totalSeconds.
	].

	"Same for nonces (although nonces can be reused)"
	nonce := params at: 'oauth_nonce' ifAbsent:[UUID new hex].

	"Assemble the required parameters. Start with the (POST) body fields."
	normalized := (postFields as: Dictionary) associations asOrderedCollection.

	"If present, extract any (GET) fields from the URL"
	(requestUrl copyAfter: $?) ifNotEmpty:[:fields|
		self decodeUrlEncodedForm: fields do:[:fkey :fval| normalized add: fkey -> fval]].

	"Keep the extra argument keys so we can exclude them below"
	extraKeys := Set newFrom: (normalized collect:[:assoc| assoc key]).
	normalized add:('oauth_nonce' -> nonce).
	normalized add: ('oauth_timestamp' -> timestamp asString).
	
	normalized add: (params associationAt: 'oauth_consumer_key').
	normalized add: (params associationAt: 'oauth_signature_method').
	normalized add: (params associationAt: 'oauth_version').

	(params includesKey: 'oauth_callback') ifTrue:[
		normalized add: (params associationAt: 'oauth_callback').
	].
	(params includesKey: 'oauth_token') ifTrue:[
		normalized add: (params associationAt: 'oauth_token').
	].
	(params includesKey: 'oauth_verifier') ifTrue:[
		normalized add: (params associationAt: 'oauth_verifier').
	].

	normalized := normalized sort:[:a1 :a2| 
		a1 key = a2 key 
			ifTrue:[a1 value <= a2 value]
			ifFalse:[a1 key <= a2 key]].

	query := String streamContents:[:s|
		normalized do:[:assoc|
			s nextPutAll: assoc key.
			s nextPutAll: '='.
			s nextPutAll: assoc value percentEscapeUrlField.
		] separatedBy:[s nextPutAll: '&'].
	].

	baseUrl := (requestUrl copyUpTo: $?) copyUpTo: $#.
	baseString := webRequest method, 
		'&', baseUrl percentEscapeUrlField, 
		'&',  query percentEscapeUrlField.

	key := (params at: 'oauth_consumer_secret'), 
			'&', (params at: 'oauth_token_secret' ifAbsent:['']).
	(params at: 'oauth_signature_method') caseOf: {
		['HMAC-SHA1'] -> [
			signature := (WebUtils hmacSha1: baseString key: key) base64Encoded
		].
	} otherwise:[self error: 'Unsupported signature method'].

	header := String streamContents:[:s|
		s nextPutAll: 'OAuth '.
		"Even though we need to include the extra args in the computation,
		they do NOT go into the OAuth header field."
		normalized do:[:assoc|
			(extraKeys includes: assoc key) 
				ifFalse:[ s nextPutAll: assoc key, '="', assoc value percentEscapeUrlField, '",']].
		s nextPutAll: 'oauth_signature="', signature percentEscapeUrlField, '"'.
	].

	webRequest headerAt: 'Authorization' put: header.! !

!WebUtils class methodsFor: 'oauth' stamp: 'ar 8/23/2010 21:55'!
oAuthSign: webRequest url: requestUrl using: params
	"Sign the given webRequest using the set of oauth parameters provided"

	^self oAuthSign: webRequest url: requestUrl extra: #() using: params! !

!WebUtils class methodsFor: 'oauth' stamp: 'ar 8/12/2010 16:31'!
sha1Hash: message
	"Compute the SHA1 hash for the given message. Slightly different from 
	SecureHashAlgorithm to produce fixed-length byte arrays."

	| sha hash |
	sha := SecureHashAlgorithm new hashMessage: message.
	hash := ByteArray new: 20.
	1 to: 20 do:[:i| hash at: i put: (sha digitAt: 21-i)].
	^hash
! !

!WebUtils class methodsFor: 'json-encode' stamp: 'ar 2/10/2012 13:39'!
jsonArray: anArray on: stream
	"Encodes an array"

	stream nextPut: $[.
	anArray 
		do:[:each| self jsonObj: each on: stream]
		separatedBy:[stream nextPutAll:', '].
	stream nextPut:$].
! !

!WebUtils class methodsFor: 'json-encode' stamp: 'jmv 1/10/2023 14:13:13'!
jsonChar: aCharacter on: stream
	"Writes a single encoded character"

	| codePoint |
	codePoint _ aCharacter codePoint.

	codePoint < 32 ifTrue:["Control character"
		codePoint caseOf: {
			[13]	-> [^stream nextPutAll: '\r'].
			[12]	-> [^stream nextPutAll: '\f'].
			[10]	-> [^stream nextPutAll: '\n'].
			[9]		-> [^stream nextPutAll: '\t'].
			[8]		-> [^stream nextPutAll: '\b'].
		} otherwise:[
			^stream nextPutAll: '\u'; nextPutAll: 
				((codePoint printStringBase: 16) padded: #left to: 4 with: $0)
		].
	].

	(codePoint >= 32 and:[codePoint <=127]) ifTrue:["Ascii character"
		(codePoint = 34 or:[codePoint = 92 or:[codePoint = 47]]) ifTrue:[stream nextPut: $\].
		^stream nextPut: aCharacter
	].
"just 16 bits??????"
	"Encode other characters (control chars, accents, umlauts, unicode)"
	stream nextPutAll:  
		'\u', (((codePoint bitAnd: 16rFFFF) printStringBase: 16) padded: #left to: 4 with: $0).
! !

!WebUtils class methodsFor: 'json-encode' stamp: 'ar 2/10/2012 13:41'!
jsonEncode: anObject
	"Encode the given object as JSON"

	^String streamContents:[:s| self jsonObj: anObject on: s]! !

!WebUtils class methodsFor: 'json-encode' stamp: 'ar 2/10/2012 13:40'!
jsonMap: aDictionary on: stream
	"Encodes a dictionary"

	stream nextPut: ${.
	"Sorting keys ensures deterministic order"
	aDictionary keys asArray sort do:[:key|
		self jsonString: key on: stream.
		stream nextPutAll:': '.
		self jsonObj: (aDictionary at: key) on: stream.
	] separatedBy:[stream nextPutAll: ', '].
	stream nextPut: $}.! !

!WebUtils class methodsFor: 'json-encode' stamp: 'ar 2/10/2012 13:38'!
jsonNumber: aNumber on: stream
	"Encodes a number"

	| value |
	value := aNumber.
	value isInteger ifFalse:[value := aNumber asFloat].
	stream print: aNumber.
! !

!WebUtils class methodsFor: 'json-encode' stamp: 'jmv 6/26/2017 19:34:55'!
jsonObj: anObject on: stream
	"Encode a generic object"

	anObject isString ifTrue:[^self jsonString: anObject on: stream].
	anObject isNumber ifTrue:[^self jsonNumber: anObject on: stream].
	anObject == nil ifTrue:[^stream nextPutAll: 'null'].
	anObject == true ifTrue:[^stream nextPutAll: 'true'].
	anObject == false ifTrue:[^stream nextPutAll: 'false'].
	anObject isArray ifTrue:[^self jsonArray: anObject on: stream].
	(anObject is: #Dictionary) ifTrue:[^self jsonMap: anObject on: stream].

	self error: 'Cannot encode: ', anObject! !

!WebUtils class methodsFor: 'json-encode' stamp: 'ar 2/10/2012 13:39'!
jsonString: aString on: stream
	"Encodes a string"

	stream nextPut: $".
	aString do:[:ch| self jsonChar: ch on: stream].
	stream nextPut: $".
! !

!WebUtils class methodsFor: 'json-decode' stamp: 'ar 2/10/2012 13:43'!
jsonArrayFrom: stream
	"Decodes a JSON [value, *] array from the stream"

	| ch result |
	(ch := stream next) = $[ ifFalse:[^self error: 'JSON Array expected'].
	stream skipSeparators.
	stream peek = $] ifTrue:[stream next. ^#()].
	result := WriteStream on: (Array new: 10).
	["Decode the next value"
	stream skipSeparators.
	result nextPut: (self jsonDecode: stream).
	stream skipSeparators.
	(ch := stream next) = $]] whileFalse:[
		ch = $, ifFalse:[^self error: 'Comma expected'].
	].

	^result contents! !

!WebUtils class methodsFor: 'json-decode' stamp: 'jmv 1/10/2023 14:08:23'!
jsonCharFrom: stream on: writeStream
	"Decodes a backslash-escaped character"

	| ch decoded |
	ch _ stream next.
	decoded _ ch caseOf: {
		[$u] 	-> 	[Character codePoint: (Integer readFrom: (stream next: 4) readStream base: 16)].
		[$r] 	-> 	[Character cr].
		[$n] 	-> 	[Character lf].
		[$t] 	-> 	[Character tab].
		[$b] 	->	[Character backspace].
		[$f] 	->	[Character newPage].
	} otherwise: [ch].
	writeStream nextPut: decoded! !

!WebUtils class methodsFor: 'json-decode' stamp: 'ar 2/10/2012 13:48'!
jsonDecode: stream
	"Decodes an arbitrary JSON encoded value from the given stream"

	stream skipSeparators.
	^stream peek caseOf: {
		[$"]		->	[self jsonStringFrom: stream].
		[$t]		->	[self jsonTrueFrom: stream].
		[$f]		->	[self jsonFalseFrom: stream].
		[$n]		->	[self jsonNullFrom: stream].
		[${]		-> 	[self jsonMapFrom: stream].
		[$[]		->	[self jsonArrayFrom: stream].
	} otherwise:[self jsonNumberFrom: stream].
! !

!WebUtils class methodsFor: 'json-decode' stamp: 'ar 2/10/2012 13:43'!
jsonFalseFrom: stream
	"Decodes 'false' from aStream"

	((stream next: 5) = 'false' 
		and:[stream atEnd or:[stream peek isAlphaNumeric not]]) 
			ifFalse:[^self error: 'Expected ''false'''].
	^false! !

!WebUtils class methodsFor: 'json-decode' stamp: 'ar 2/10/2012 13:45'!
jsonMapFrom: stream
	"Decodes a JSON {key:value, *} object from the stream"

	| map ch key value |
	map := Dictionary new.
	(ch := stream next) = ${ ifFalse:[^self error: 'JSON Object expected'].
	stream skipSeparators.
	stream peek = $} ifTrue:[^map].

	["Decode the next key:value pair"
	stream skipSeparators.
	key := self jsonStringFrom: stream.
	stream skipSeparators.
	stream next = $: ifFalse:[^self error: 'Key-value pair expected'].
	value := self jsonDecode: stream.
	map at: key put: value.
	stream skipSeparators.
	(ch := stream next) = $}] whileFalse:[
		ch = $, ifFalse:[^self error: 'Comma expected'].
	].

	^map! !

!WebUtils class methodsFor: 'json-decode' stamp: 'ar 2/10/2012 13:43'!
jsonNullFrom: stream
	"Decodes 'null' from aStream"

	((stream next: 4) = 'null'
		and:[stream atEnd or:[stream peek isAlphaNumeric not]]) 
			ifFalse:[^self error: 'Expected ''null'''].
	^nil! !

!WebUtils class methodsFor: 'json-decode' stamp: 'ar 2/10/2012 13:44'!
jsonNumberFrom: stream
	"Decodes a JSON number from the stream"

	| ascii integer fraction scale sign expSign exponent value ch |
	integer := fraction := 0. sign := scale := exponent := expSign := 1.
	ascii := stream next asciiValue.
	ascii = 45 "$- asciiValue" ifTrue:[
		sign := -1.
		ascii := stream next asciiValue.
	].
	"JSON requires at least one digit"
	(ascii >= 48 and:[ascii <= 57]) ifFalse:[^self error: 'Digit expected'].

	"Read the integer part"
	integer := ascii - 48.
	[ch := stream next ifNil:[^integer * sign].
	ascii := ch asciiValue.
	ascii >= 48 and:[ascii <= 57]] whileTrue:[
		integer := (integer * 10) + (ascii - 48).
	].

	ascii = 46 "$. asciiValue" ifTrue:[
		"Read the fraction part"
		[ch := stream next ifNil:[^(integer * scale + fraction * sign) asFloat / scale].
		ascii := ch asciiValue.
		ascii >= 48 and:[ascii <= 57]] whileTrue:[
			fraction := (fraction * 10) + (ascii - 48).
			scale := scale * 10.
		].
		value := (integer * scale + fraction * sign) asFloat / scale asFloat.
	] ifFalse:[value := integer * sign].

	(ascii =  69 "$E asciiValue" or:[ascii =  101 "$e asciiValue"]) ifTrue:[
		"Read exponent"
		ascii := stream next asciiValue.
		ascii = $- ifTrue:[
			expSign := -1.
			ascii := stream next asciiValue.
		] ifFalse:[ascii = $+ ifTrue:[ascii := stream next asciiValue]].
		exponent := ascii - 48.
		[ch := stream next ifNil:[^value * (10 raisedTo: expSign * exponent)].
		ascii := ch asciiValue.
		ascii >= 48 and:[ascii <= 57]] whileTrue:[
			exponent := (exponent * 10) + (ascii - 48).			
		].
		exponent := exponent * expSign.
	].

	"Skip back before last character since number might be part of a sequence
	like 1, 2, 3, 4, etc (which would eat the trailing comma)"
	ch isAlphaNumeric ifTrue:[^self error: 'Delimiter expected'].
	stream skip: -1.

	exponent = 1 ifFalse:[
		exponent < 0 ifTrue:[value := value asFloat].
		value := value * (10 raisedTo: exponent).
	].
	^value! !

!WebUtils class methodsFor: 'json-decode' stamp: 'jmv 6/26/2017 19:49:10'!
jsonStringFrom: stream
	"Decodes a JSON encoded string"

	| ch result |
	(ch := stream next) = $" 
		ifFalse:[^self error: 'String expected'].
	result := WriteStream on: (String new: 20).
	[(ch := stream next) == nil] whileFalse:[
		ch = $" ifTrue:[^result contents].
		ch = $\
			ifTrue: [ self jsonCharFrom: stream on: result ]
			ifFalse: [ result nextPut: ch ].
	].
	^self error: 'Unterminated string'! !

!WebUtils class methodsFor: 'json-decode' stamp: 'ar 2/10/2012 13:46'!
jsonTrueFrom: stream
	"Decodes 'true' from aStream"

	((stream next: 4) = 'true'
		and:[stream atEnd or:[stream peek isAlphaNumeric not]]) 
			 ifFalse:[^self error: 'Expected ''true'''].
	^true! !

!WebUtils class methodsFor: 'misc' stamp: 'DSG 3/28/2015 22:49'!
logEntryFor: request response: response
	"Create a log entry in common log format from the given request / response"

	| entry logdate logsize |

	"CLF prints date as [day/month/year:hour:min:sec zone]"
	logdate := String streamContents:[:s| | date |
		date := DateAndTime fromSeconds: Time localSecondClock.
		s nextPut: $[.
			date date printOn: s format: #( 1 2 3 $/ 2 1 2).
		s nextPut: $:.
			date time print24: true on: s.
		s nextPutAll:(' ',
			(date offset hours >= 0 ifTrue:['+'] ifFalse:['-']),
			(date offset hours abs asString padded: #left to: 2 with: $0),
			(date offset minutes abs asString padded: #left to: 2 with: $0)
		).
		s nextPut: $].
	].

	"CLF prints zero length as - "
	logsize := response contentLength ifNil:[0].
	logsize = 0 ifTrue:[logsize := '-'].

	entry := String streamContents:[:s|
		s 
			nextPutAll: (request remoteHost ifNil:[
				"Substitute with the host header"
				(request headerAt: 'host') copyUpTo: $:
			]);
			nextPutAll: ' -'; 	"RFC 1413 identity of client"
			nextPutAll: ' -';	"TODO: userid of authenticated user"
			nextPutAll: ' ', logdate;
			nextPutAll: ' "', request requestLine, '"';
			nextPutAll: ' ', response code asString;
			nextPutAll: ' ', logsize asString.
	].
	^entry
! !

!WebUtils class methodsFor: 'misc' stamp: 'pb 5/25/2016 01:29'!
logRequest: request response: response on: streamOrFilename
	"Log a request in common log format on the given stream / file."

	| entry |

	"Create the log entry for the request/response pair"
	entry := self logEntryFor: request response: response.

	"If the argument is a string, it represents the file name to log to"
	streamOrFilename isString
		ifTrue: [
			streamOrFilename asFileEntry appendStreamDo: [ :stream |
				stream nextPutAll: entry; newLine ]]
		ifFalse: [
			streamOrFilename nextPutAll: entry; newLine.
			streamOrFilename flush ]! !

!WebUtils class methodsFor: 'misc' stamp: 'jmv 8/16/2021 12:04:50'!
mcVersion
	"Answers the Monticello version of WebServer's MC package"

	| version |
	"
	Smalltalk at: #MCWorkingCopy ifPresent:[:mc|
		mc managersForClass: self do:[:wc|
			wc ancestors ifNotEmpty:[:ancestry| version := ancestry first name]]].
	"
	^version ifNil:[''].! !

!WebUtils class methodsFor: 'misc' stamp: 'ar 11/17/2011 10:16'!
platformName
	"Return the name of the platform we're running on."

	^Smalltalk getSystemAttribute: 1001! !

!WebUtils class methodsFor: 'misc' stamp: 'ar 11/17/2011 10:06'!
vmVersion	
	"Return a string identifying the interpreter version"

	^Smalltalk getSystemAttribute: 1004! !

!WebUtils class methodsFor: 'md5' stamp: 'ar 2/20/2010 12:45'!
md5Digest: aStringOrByteArray
	"This creates a little endian hex string to be used with various auth methods
	This is the same as htdigest (apache) uses for its md5 digest auth db"

	^(self md5HashStream: (ReadStream on: aStringOrByteArray asByteArray)) reversed hex! !

!WebUtils class methodsFor: 'md5' stamp: 'jmv 5/7/2023 11:06:05'!
md5HashStream: aStream
	"self md5HashStream: (ReadStream on: 'foo')"

	| start buffer bytes sz n words hash |
	hash := WordArray 
		with: 16r67452301 
		with: 16rEFCDAB89 
		with: 16r98BADCFE 
		with: 16r10325476.
	words := WordArray new: 16.
	buffer := ByteArray new: 64.
	start := aStream position.
	[aStream atEnd] whileFalse: [
		bytes := aStream nextInto: buffer.
		(bytes size < 64 or:[aStream atEnd]) ifTrue:[
			sz := bytes size.
			buffer replaceFrom: 1 to: sz with: bytes startingAt: 1.
			buffer from: sz+1 to: buffer size put: 0.
			sz < 56 ifTrue:[
				buffer at: sz + 1 put: 128. "trailing bit"
			] ifFalse:[
				"not enough room for the length, so just pad this one, then..."
				sz < 64 ifTrue:[buffer at: sz + 1 put: 128].
				1 to: 16 do:[:i| words at: i put: (buffer uint32At: i*4-3 bigEndian: false)].
				self md5Transform: words hash: hash.
				"process one additional block of padding ending with the length"
				buffer atAllPut: 0.
				sz = 64 ifTrue: [buffer at: 1 put: 128].
			].
			"Fill in the final 8 bytes with the 64-bit length in bits."
			n := (aStream position - start) * 8.
			7 to: 0 by: -1 do:[:i| buffer at: (buffer size - i) put: ((n bitShift: 7 - i * -8) bitAnd: 255)].
		].
		1 to: 16 do:[:i| words at: i put: (buffer uint32At: i*4-3 bigEndian: false)].
		self md5Transform: words hash: hash.
	].
	bytes := ByteArray new: 16.
	bytes uint32At: 1 put: (hash at: 4) bigEndian: true.
	bytes uint32At: 5 put: (hash at: 3) bigEndian: true.
	bytes uint32At: 9 put: (hash at: 2) bigEndian: true.
	bytes uint32At: 13 put: (hash at: 1) bigEndian: true.
	^bytes
! !

!WebUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
md5Transform: in hash: hash
	"This adds the incoming words to the existing hash"

	| a b c d |
	<primitive: 'primitiveMD5Transform' module: 'CroquetPlugin'>
	a := hash at: 1.
	b := hash at: 2.
	c := hash at: 3.
	d := hash at: 4.

	a := self step1: a x: b y: c z: d data: (in at:  1) add: 16rD76AA478 shift: 7.
	d := self step1: d x: a y: b z: c data: (in at:  2) add: 16rE8C7B756 shift: 12.
	c := self step1: c x: d y: a z: b data: (in at:  3) add: 16r242070DB shift: 17.
	b := self step1: b x: c y: d z: a data: (in at:  4) add: 16rC1BDCEEE shift: 22.
	a := self step1: a x: b y: c z: d data: (in at:  5) add: 16rF57C0FAF shift:  7.
	d := self step1: d x: a y: b z: c data: (in at:  6) add: 16r4787C62A shift: 12.
	c := self step1: c x: d y: a z: b data: (in at:  7) add: 16rA8304613 shift: 17.
	b := self step1: b x: c y: d z: a data: (in at:  8) add: 16rFD469501 shift: 22.
	a := self step1: a x: b y: c z: d data: (in at:  9) add: 16r698098D8 shift:  7.
	d := self step1: d x: a y: b z: c data: (in at: 10) add: 16r8B44F7AF shift: 12.
	c := self step1: c x: d y: a z: b data: (in at: 11) add: 16rFFFF5BB1 shift: 17.
	b := self step1: b x: c y: d z: a data: (in at: 12) add: 16r895CD7BE shift: 22.
	a := self step1: a x: b y: c z: d data: (in at: 13) add: 16r6B901122 shift:  7.
	d := self step1: d x: a y: b z: c data: (in at: 14) add: 16rFD987193 shift: 12.
	c := self step1: c x: d y: a z: b data: (in at: 15) add: 16rA679438E shift: 17.
	b := self step1: b x: c y: d z: a data: (in at: 16) add: 16r49B40821 shift: 22.

	a := self step2: a x: b y: c z: d data: (in at:  2) add: 16rF61E2562 shift:  5.
	d := self step2: d x: a y: b z: c data: (in at:  7) add: 16rC040B340 shift:  9.
	c := self step2: c x: d y: a z: b data: (in at: 12) add: 16r265E5A51 shift: 14.
	b := self step2: b x: c y: d z: a data: (in at:  1) add: 16rE9B6C7AA shift: 20.
	a := self step2: a x: b y: c z: d data: (in at:  6) add: 16rD62F105D shift:  5.
	d := self step2: d x: a y: b z: c data: (in at: 11) add: 16r02441453 shift:  9.
	c := self step2: c x: d y: a z: b data: (in at: 16) add: 16rD8A1E681 shift: 14.
	b := self step2: b x: c y: d z: a data: (in at:  5) add: 16rE7D3FBC8 shift: 20.
	a := self step2: a x: b y: c z: d data: (in at: 10) add: 16r21E1CDE6 shift:  5.
	d := self step2: d x: a y: b z: c data: (in at: 15) add: 16rC33707D6 shift:  9.
	c := self step2: c x: d y: a z: b data: (in at:  4) add: 16rF4D50D87 shift: 14.
	b := self step2: b x: c y: d z: a data: (in at:  9) add: 16r455A14ED shift: 20.
	a := self step2: a x: b y: c z: d data: (in at: 14) add: 16rA9E3E905 shift:  5.
	d := self step2: d x: a y: b z: c data: (in at:  3) add: 16rFCEFA3F8 shift:  9.
	c := self step2: c x: d y: a z: b data: (in at:  8) add: 16r676F02D9 shift: 14.
	b := self step2: b x: c y: d z: a data: (in at: 13) add: 16r8D2A4C8A shift: 20.

	a := self step3: a x: b y: c z: d data: (in at:  6) add: 16rFFFA3942 shift:  4.
	d := self step3: d x: a y: b z: c data: (in at:  9) add: 16r8771F681 shift: 11.
	c := self step3: c x: d y: a z: b data: (in at: 12) add: 16r6D9D6122 shift: 16.
	b := self step3: b x: c y: d z: a data: (in at: 15) add: 16rFDE5380C shift: 23.
	a := self step3: a x: b y: c z: d data: (in at:  2) add: 16rA4BEEA44 shift:  4.
	d := self step3: d x: a y: b z: c data: (in at:  5) add: 16r4BDECFA9 shift: 11.
	c := self step3: c x: d y: a z: b data: (in at:  8) add: 16rF6BB4B60 shift: 16.
	b := self step3: b x: c y: d z: a data: (in at: 11) add: 16rBEBFBC70 shift: 23.
	a := self step3: a x: b y: c z: d data: (in at: 14) add: 16r289B7EC6 shift:  4.
	d := self step3: d x: a y: b z: c data: (in at:  1) add: 16rEAA127FA shift: 11.
	c := self step3: c x: d y: a z: b data: (in at:  4) add: 16rD4EF3085 shift: 16.
	b := self step3: b x: c y: d z: a data: (in at:  7) add: 16r04881D05 shift: 23.
	a := self step3: a x: b y: c z: d data: (in at: 10) add: 16rD9D4D039 shift:  4.
	d := self step3: d x: a y: b z: c data: (in at: 13) add: 16rE6DB99E5 shift: 11.
	c := self step3: c x: d y: a z: b data: (in at: 16) add: 16r1FA27CF8 shift: 16.
	b := self step3: b x: c y: d z: a data: (in at:  3) add: 16rC4AC5665 shift: 23.

	a := self step4: a x: b y: c z: d data: (in at:  1) add: 16rF4292244 shift:  6.
	d := self step4: d x: a y: b z: c data: (in at:  8) add: 16r432AFF97 shift: 10.
	c := self step4: c x: d y: a z: b data: (in at: 15) add: 16rAB9423A7 shift: 15.
	b := self step4: b x: c y: d z: a data: (in at:  6) add: 16rFC93A039 shift: 21.
	a := self step4: a x: b y: c z: d data: (in at: 13) add: 16r655B59C3 shift:  6.
	d := self step4: d x: a y: b z: c data: (in at:  4) add: 16r8F0CCC92 shift: 10.
	c := self step4: c x: d y: a z: b data: (in at: 11) add: 16rFFEFF47D shift: 15.
	b := self step4: b x: c y: d z: a data: (in at:  2) add: 16r85845DD1 shift: 21.
	a := self step4: a x: b y: c z: d data: (in at:  9) add: 16r6FA87E4F shift:  6.
	d := self step4: d x: a y: b z: c data: (in at: 16) add: 16rFE2CE6E0 shift: 10.
	c := self step4: c x: d y: a z: b data: (in at:  7) add: 16rA3014314 shift: 15.
	b := self step4: b x: c y: d z: a data: (in at: 14) add: 16r4E0811A1 shift: 21.
	a := self step4: a x: b y: c z: d data: (in at:  5) add: 16rF7537E82 shift:  6.
	d := self step4: d x: a y: b z: c data: (in at: 12) add: 16rBD3AF235 shift: 10.
	c := self step4: c x: d y: a z: b data: (in at:  3) add: 16r2AD7D2BB shift: 15.
	b := self step4: b x: c y: d z: a data: (in at: 10) add: 16rEB86D391 shift: 21.

	a := (a + (hash at: 1)) bitAnd: 16rFFFFFFFF. hash at: 1 put: a.
	b := (b + (hash at: 2)) bitAnd: 16rFFFFFFFF. hash at: 2 put: b.
	c := (c + (hash at: 3)) bitAnd: 16rFFFFFFFF. hash at: 3 put: c.
	d := (d + (hash at: 4)) bitAnd: 16rFFFFFFFF. hash at: 4 put: d.

	^hash! !

!WebUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
rotate: value by: amount
	"Rotate value left by amount"

	| lowMask highMask |
	lowMask := (1 bitShift: 32-amount) - 1.
	highMask := 16rFFFFFFFF - lowMask.
	^((value bitAnd: lowMask) bitShift: amount) + 
		((value bitAnd: highMask) bitShift: amount-32)! !

!WebUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step1: w x: x y: y z: z data: data add: add shift: s
	"Step 1 in MD5 transformation"

	| f result |
	f := z bitXor: (x bitAnd: (y bitXor: z)).
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!WebUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step2: w x: x y: y z: z data: data add: add shift: s
	"Step 2 in MD5 transformation"

	| f result |
	f := y bitXor: (z bitAnd: (x bitXor: y)).
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!WebUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step3: w x: x y: y z: z data: data add: add shift: s
	"Step 3 in MD5 transformation"

	| f result |
	f := (x bitXor: y) bitXor: z.
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!WebUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step4: w x: x y: y z: z data: data add: add shift: s
	"Step 4 in MD5 transformation"

	| f result |
	f := y bitXor: (x bitOr: (z  bitXor: 16rFFFFFFFF)).
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!WebUtils class methodsFor: 'auth utils' stamp: 'DSG 12/5/2013 17:20'!
parseAuthParams: authHeader
	"Parse the auth header to find authentication parameters"

	| authParams stream key value|
	authParams := Dictionary new.
	stream := ReadStream on: authHeader.
	authParams at: 'method' put: (stream upTo: Character space) asLowercase.
	[stream skipSeparators.
	stream atEnd] whileFalse:[
		key := stream upTo: $=.
		stream skipSeparators.
		stream peek = $" 
			ifTrue:[value := stream next; upTo: $". stream upTo: $,]
			ifFalse:[value := stream upToAny: ' ,' asCharacterSet].
		authParams at: key asLowercase put: value.
	].
	^authParams! !

!WebUtils class methodsFor: 'quoting' stamp: 'ar 3/31/2010 08:43'!
quote: aString
	"Quote the given string, escaping as necessary any embedded quotes"

	^'"', (aString copyReplaceAll: '"' with: '\"'), '"'
! !

!WebUtils class methodsFor: 'quoting' stamp: 'ar 3/31/2010 08:45'!
unquote: aString
	"Unquote the given string, unescaping as necessary any embedded quotes"

	(aString beginsWith: '"') ifFalse:[^aString].
	(aString endsWith: '"') ifFalse:[^aString].
	^(aString copyReplaceAll: '\"' with: '"') allButFirst allButLast! !

!WebUtils class methodsFor: 'headers' stamp: 'jmv 6/25/2017 19:36:55'!
readHeadersFrom: aStream
	"Parse http headers and answer a collection of key -> value pairs."

	| headers nextLine existing |
	headers := OrderedCollection new.
	[nextLine := aStream upToAll: String crlfString.
	nextLine isEmpty] whileFalse:[
		nextLine first isSeparator ifTrue:["Continuation as per HTTP 1.1 spec"
			existing := headers last.
			existing value: (existing value, String space, nextLine withBlanksTrimmed).
		] ifFalse:["Regular header"
			headers add: 
				(nextLine copyUpTo: $:) asLowercase ->
					(nextLine copyAfter: $:) withBlanksTrimmed.
		].
	].
	^headers! !

!SqueakSSL methodsFor: 'convenience' stamp: 'jmv 4/13/2023 15:50:00'!
accept
	"Convenience API. Perform an SSL server handshake.
	Raises an error if something goes wrong."

	| inbuf outbuf count result |

	inbuf := ByteArray new: 4096.
	outbuf := ByteArray new: 4096.
	count := 0.

	[self isConnected] whileFalse:[
		"Read input"
		count := self readDataInto: inbuf.
		result := self accept: inbuf from: 1 to: count into: outbuf.

		"Check for errors first"
		result < -1 ifTrue:[^self error: 'SSL accept failed with code: ', result printString].

		"If a token has been produced in the handshake, send it to the remote"
		result > 0 ifTrue:[self writeData: outbuf count: result].
	].
! !

!SqueakSSL methodsFor: 'convenience' stamp: 'jmv 4/13/2023 15:50:14'!
connect
	"Convenience API. Perform an SSL client handshake. 
	Raises an error if something goes wrong."

	| inbuf outbuf count result |

	inbuf := ByteArray new: 4096.
	outbuf := ByteArray new: 4096.
	count := 0.

	"Begin the SSL handshake"
	[result := self connect: inbuf from: 1 to: count into: outbuf.
	result = 0] whileFalse:[
		"Check for errors first"
		result < -1 ifTrue:[^self error: 'SSL connect failed with code: ', result printString].

		"If a token has been produced in the handshake, send it to the remote"
		result > 0 ifTrue:[self writeData: outbuf count: result].

		"Read more input and repeat"
		count := self readDataInto: inbuf.
	].! !

!SqueakSSL methodsFor: 'convenience' stamp: 'ar 7/18/2010 14:18'!
decrypt: data
	"Convenience API. Decrypt incoming data and return the result.

	Warning: This method may produce more or less results than expected
	unless called with exactly one SSL/TLS frame."

	| buf count |
	buf := data class new: 4096.
	count := self decrypt: data from: 1 to: data size into: buf.
	count < 0 ifTrue:[self error: 'Decryption failed, code: ', count].
	^buf copyFrom: 1 to: count! !

!SqueakSSL methodsFor: 'convenience' stamp: 'ar 7/18/2010 14:18'!
encrypt: data
	"Convenience API. Encrypt incoming data and return the result."

	| buf count |
	buf := data class new: data size + 100.
	count := self encrypt: data from: 1 to: data size into: buf.
	count < 0 ifTrue:[self error: 'Decryption failed, code: ', count].
	^buf copyFrom: 1 to: count! !

!SqueakSSL methodsFor: 'convenience' stamp: 'ar 7/16/2010 23:04'!
receiveData
	"Convenience API. Receive data and decrypt it."

	| inbuf outbuf count |
	inbuf := String new: 4096.
	outbuf := String new: 4096.

	^String streamContents:[:s|
		"Read the next input bytes"
		count := self readDataInto: inbuf.
		"Push the input bytes into the SSL"
		count := self decrypt: inbuf from: 1 to: count into: outbuf.
		"And keep draining as long as output is being produced"
		[count > 0] whileTrue:[
			s next: count putAll: outbuf.
			count := self decrypt: inbuf from: 1 to: 0 into: outbuf.
		].
	].! !

!SqueakSSL methodsFor: 'convenience' stamp: 'ar 7/16/2010 23:04'!
sendData: inbuf
	"Convenience API. Encrypt and send data"

	| outbuf count |
	outbuf := inbuf class new: inbuf size + 100.
	count := self encrypt: inbuf from: 1 to: inbuf size into: outbuf.
	^self writeData: outbuf count: count.! !

!SqueakSSL methodsFor: 'operations' stamp: 'ar 7/16/2010 23:02'!
accept: srcBuf from: start to: stop into: dstBuf
	"Start or continue the server handshake using the given input token."

	^self primitiveSSL: handle accept: srcBuf startingAt: start count: stop-start+1 into: dstBuf! !

!SqueakSSL methodsFor: 'operations' stamp: 'eem 3/23/2016 16:55'!
connect: srcBuf from: start to: stop into: dstBuf
	"Start or continue the server handshake using the given input token."

	| r |
	r := self primitiveSSL: handle connect: srcBuf startingAt: start count: stop-start+1 into: dstBuf.
	r <= 0 ifTrue:
		[Processor yield].
	^r! !

!SqueakSSL methodsFor: 'operations' stamp: 'ar 7/16/2010 21:58'!
decrypt: srcBuf from: start to: stop into: dstBuf
	"Decrypt the input in srcBuf into the provided output buffer.

	Clients are expected to adhere to the following rules:
		* The size of dstBuf must be large enough for the largest encrypted packet.
		* Clients must not call this method with a huge srcBuf (tens of kb of data)
		* After having called this method with new input, clients must call it 
		   with NO input until all data has been 'drained' for example:
			count := squeakSSL decrypt: srcBuf into: dstBuf.
			[count > 0] whileTrue:[
				count := squeakSSL decrypt: #[] into: dstBuf.
			].
	"

	^self primitiveSSL: handle decrypt: srcBuf startingAt: start count: stop-start+1 into: dstBuf! !

!SqueakSSL methodsFor: 'operations' stamp: 'ar 7/16/2010 21:59'!
encrypt: srcBuf from: start to: stop into: dstBuf
	"Encrypt the input in srcBuf into the provided output buffer.
	The output buffer must be large enough to include the framing information."

	^self primitiveSSL: handle encrypt: srcBuf startingAt: start count: stop-start+1 into: dstBuf! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/18/2010 21:31'!
certName
	"The name of the (local) certificate to provide to the remote peer."

	^self primitiveSSL: handle getStringProperty: 1! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 11/16/2011 16:48'!
certName: aString
	"Sets the name of the (local) certificate to provide to the remote peer.
	OpenSSL:
		The name is the full path to a .pem file.
	WinSSL:
		The name is matched against the 'friendly name' of a certificate in the cert store.
	"

	^self primitiveSSL: handle setStringProperty: 1 toValue: (aString ifNil:[''])! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/18/2010 21:27'!
certState
	"Returns the certificate verification bits. The returned value indicates
	whether the certificate is valid. The two standard values are:

		0 - The certificate is valid.
		-1 - No certificate has been provided by the peer.

	Otherwise, the result is a bit mask of the following values:

		1 - If set, there is an unspecified issue with the cert (generic error)
		2 - If set, the root CA is untrusted (usually a self-signed cert)
		4 - If set, the certificate is expired.
		8 - If set, the certificate is used for the wrong purpose
		16 - If set, the CN of the certificate is invalid.
		32 - If set, the certificate was revoked.

	"
	^self primitiveSSL: handle getIntProperty: 3! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/18/2010 21:26'!
logLevel
	"Returns the log level of the ssl instance"

	^self primitiveSSL: handle getIntProperty: 1! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/18/2010 21:27'!
logLevel: aNumber
	"Sets the log level of the ssl instance"

	^self primitiveSSL: handle setIntProperty: 1 toValue: aNumber! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/18/2010 21:31'!
peerName
	"Returns the certificate name of the remote peer.
	The method only returns a name if the certificate has been verified."

	^self primitiveSSL: handle getStringProperty: 0! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/18/2010 21:25'!
pluginVersion
	"Returns the version of the plugin"

	^self primitiveSSL: handle getIntProperty: 0! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/15/2010 20:16'!
readBlock
	"The block used to read data where required. The block takes one argument,
	the buffer to fill with data and is expected to return the number of bytes read."

	^readBlock! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/15/2010 20:16'!
readBlock: aBlock
	"The block used to read data where required. The block takes one argument,
	the buffer to fill with data and is expected to return the number of bytes read."

	readBlock := aBlock! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ul 10/16/2014 10:35'!
serverName: aString
	"Sets the name to use with the Server Name Indication TLS extension. Which should be a valid FQDN. No WinSSL support yet."

	^[ self primitiveSSL: handle setStringProperty: 2 toValue: aString ]
		on: Error
		do: [ "nothing" ]! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/18/2010 21:27'!
sslState
	"Returns the current state of the SSL connection:
		0 - Unused.
		1 - In accept handshake.
		2 - In connect handshake.
		3 - Connected.
	"
	^self primitiveSSL: handle getIntProperty: 2
		! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/15/2010 20:17'!
writeBlock
	"The block used to write data where required. The block takes two arguments,
	the buffer and the number of bytes to be written from the buffer."

	^writeBlock! !

!SqueakSSL methodsFor: 'accessing' stamp: 'ar 7/15/2010 20:17'!
writeBlock: aBlock
	"The block used to write data where required. The block takes two arguments,
	the buffer and the number of bytes to be written from the buffer."

	writeBlock := aBlock! !

!SqueakSSL methodsFor: 'initialize' stamp: 'ar 7/16/2010 22:01'!
destroy
	"Destroys the underlying platform handle"

	handle ifNotNil:[
		self primitiveSSLDestroy: handle.
		handle := nil.
	].! !

!SqueakSSL methodsFor: 'initialize' stamp: 'ar 7/16/2010 23:10'!
initialize
	"Initialize the receiver"

	handle := self primitiveSSLCreate.
! !

!SqueakSSL methodsFor: 'initialize' stamp: 'ar 7/18/2010 20:37'!
on: aSocket
	"Convenience API. Set up SqueakSSL to operate on a standard TCP socket.
	Generally not very useful for real applications (it lacks error handling etc) 
	but very helpful for debugging and other experiments."

	self readBlock:[:inbuf|
		aSocket waitForDataIfClosed:[].
		aSocket receiveDataInto: inbuf.
	].
	self writeBlock:[:outbuf :count|
		aSocket sendData: (outbuf copyFrom: 1 to: count).
	].! !

!SqueakSSL methodsFor: 'testing' stamp: 'ar 7/16/2010 22:37'!
isConnected
	"Returns true if the SSL handshake has been completed"

	^self sslState = 3! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 21:57'!
primitiveSSL: sslHandle accept: srcbuf startingAt: start count: length into: dstbuf
	"Primitive. Starts or continues a server handshake using the provided data.
	Will eventually produce output to be sent to the server.
	Returns:
		> 0	 - Number of bytes to be sent to the server
		0	 - Success. The connection is established.
		-1 	 - More input is required.
		< -1 - Other errors
	"
	<primitive: 'primitiveAccept' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 21:57'!
primitiveSSL: sslHandle connect: srcbuf startingAt: start count: length into: dstbuf
	"Primitive. Starts or continues a client handshake using the provided data.
	Will eventually produce output to be sent to the server.
	Returns:
		> 0	 - Number of bytes to be sent to the server
		0	 - Success. The connection is established.
		-1 	 - More input is required.
		< -1 - Other errors
	"
	<primitive: 'primitiveConnect' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 21:58'!
primitiveSSL: sslHandle decrypt: srcbuf startingAt: start count: length into: dstbuf
	"Primitive. Takes incoming data for decryption and continues to decrypt data.
	Returns the number of bytes produced in the output"

	<primitive: 'primitiveDecrypt' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 21:59'!
primitiveSSL: sslHandle encrypt: srcbuf startingAt: start count: length into: dstbuf
	"Primitive. Encrypts the incoming buffer into the result buffer.
	Returns the number of bytes produced as a result."

	<primitive: 'primitiveEncrypt' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 22:00'!
primitiveSSL: sslHandle getIntProperty: propID
	"Primitive. Returns a string property from an SSL session."

	<primitive: 'primitiveGetIntProperty' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 21:59'!
primitiveSSL: sslHandle getStringProperty: propID
	"Primitive. Returns a string property from an SSL session."

	<primitive: 'primitiveGetStringProperty' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 1/12/2011 09:40'!
primitiveSSL: sslHandle setIntProperty: propID toValue: anInteger
	"Primitive. Sets a string property in an SSL session."

	<primitive: 'primitiveSetIntProperty' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 22:00'!
primitiveSSL: sslHandle setStringProperty: propID toValue: aString
	"Primitive. Sets a string property in an SSL session."

	<primitive: 'primitiveSetStringProperty' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 22:01'!
primitiveSSLCreate
	"Primitive. Creates and returns a new SSL handle"

	<primitive: 'primitiveCreate' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'primitives' stamp: 'ar 7/16/2010 22:01'!
primitiveSSLDestroy: sslHandle
	"Primitive. Destroys the SSL session handle"

	<primitive: 'primitiveDestroy' module: 'SqueakSSL'>
	^self primitiveFailed! !

!SqueakSSL methodsFor: 'private' stamp: 'ar 7/15/2010 20:08'!
readDataInto: aBuffer
	"Private. Read actual data into the given buffer.
	Return the number of bytes read."

	^readBlock value: aBuffer! !

!SqueakSSL methodsFor: 'private' stamp: 'ar 11/16/2011 16:31'!
setStringProperty: index to: aString
	"Private. Use with caution"

	^self primitiveSSL: handle setStringProperty: index toValue: aString! !

!SqueakSSL methodsFor: 'private' stamp: 'ar 7/15/2010 20:08'!
writeData: aBuffer count: count
	"Private. Write actual data from the given buffer."

	writeBlock value: aBuffer value: count! !

!SqueakSSL class methodsFor: 'utilities' stamp: 'jmv 4/13/2023 15:50:09'!
checkCert: certName
	"Attempt to verify the cert with the given name by performing 
	an SSL handshake. Raises an error if there is an issue with the cert,
	returns the peer name from the cert if successful."

	| sslClient sslServer inbuf outbuf result |
	inbuf := ByteArray new: 4096.
	outbuf := ByteArray new: 4096.

	["Perform the SSL handshake"
	sslClient := SqueakSSL new.
	sslServer := SqueakSSL new.
	sslServer certName: certName.

	result := 0.
	[result := sslClient connect: inbuf from: 1 to: result into: outbuf.
	result = 0] whileFalse:[
		result < -1 ifTrue:[^self error: 'SSL handshake failed (client code: ', result printString, ')'].
		result := sslServer accept: outbuf from: 1 to: result into: inbuf.
		result < -1 ifTrue:[^self error: 'SSL handshake failed (server code: ', result printString, ')'].
	].

	"Handshake complete. Check the cert status"
	sslClient certState = 0 ifFalse:[
		^self error: 'Certificate validation failed (code: ', sslClient certState, ')'.
	].

	"When successful, just return the peer name to the caller"
	^sslClient peerName
	
	] ensure:[
		sslClient ifNotNil:[sslClient destroy].
		sslServer ifNotNil:[sslServer destroy].
	].! !

!SqueakSSL class methodsFor: 'utilities' stamp: 'ar 11/17/2011 10:16'!
platformName
	"Return the name of the platform we're running on."

	^Smalltalk getSystemAttribute: 1001! !

!SqueakSSL class methodsFor: 'examples' stamp: 'ar 6/17/2012 14:58'!
ensureSampleCert
	"Ensure that we have a sample certificate for the tests"

	SqueakSSL platformName caseOf: {
		['unix'] -> [^self ensureSampleCertFile].
		['Win32'] -> [^self ensureSampleCertInStore].
	} otherwise: [^nil].
! !

!SqueakSSL class methodsFor: 'examples' stamp: 'jmv 6/25/2017 21:14:29'!
ensureSampleCertFile
	"On Unix, we can simply create a valid cert file"

	| certName file |
	SqueakSSL platformName = 'unix' ifFalse:[^self].
	certName := self name, 'Cert.pem'.
	file _ DirectoryEntry currentDirectory // certName.
	file exists ifFalse: [
		 file writeStreamDo: [ :stream |
			stream nextPutAll: self exampleCertFile ]].
	^file pathName! !

!SqueakSSL class methodsFor: 'examples' stamp: 'ar 6/17/2012 14:57'!
ensureSampleCertInStore
	"Ensure that we have a valid certificate in the Windows certificate store"

	SqueakSSL platformName = 'Win32' ifFalse:[^self].
	"Undocumented. Allows importing a pfx w/o password.
	For the sole purpose of being able to run tests reliably"
	SqueakSSL new setStringProperty: 10001 to: self exampleCertPFX.
	^'testcert'. "Friendly name of test cert"
! !

!SqueakSSL class methodsFor: 'examples' stamp: 'ar 6/17/2012 14:57'!
exampleCertFile
^'-----BEGIN RSA PRIVATE KEY-----
MIICXQIBAAKBgQDnCv/gxDCb2yq15qkNwYtdMOHfW609Ck7wfwjVgzSNg+Hw+1R4
+krWhYRsWoXZUcy9xPC9WhnFCFijcnROcWp7vByVukFkVPYgzk1OBFT484ZCLBme
08GqLSzZrjgu7c1Yu5M9MZQdZKObBvZzDFsnvFccfM7G5mX/FgATasYaLQIDAQAB
AoGBAMpUJ6B+LtNOKykAxir1w0Xo+OTRM/SwglC57tKMBAmp5MNUVbVb+w3B/yWk
YHLf35yQSwKHVOnnVThNkuzfBY+MBxnaZwCByKknB4viP1ihPmfwdtqW4QXt1CTH
53sc9BVPjs3Nn1eEVrc582RK0MhORmjvlz+GkTswXCiKD3tBAkEA+6/au8T8XUeM
y/KrtJ+U84seviw5nY93Yg7495n4ir1fojp4wFbWq1JTeM22zspZQOKzEsjxfHUi
UH3buH//OwJBAOsAlJdIZqTIJponBXho+jqLHqcZYXBz3znDzHZU1PLfyfq2DuVe
gt8UWa4VwlCZNtPi7g/iFPEcLOlf2XY3hbcCQFU7voVsNlKYknPW4JMwn87CREz+
yRw0o6dPjry7JdJGQ4a66n2oatZl8OKuN8Rb/lHc8+vepPkS6eX8WVZn8lUCQE2r
F3EYgLQdYoS4ONqe93S53hukC8w6v6A70iuZxfevdvXhjfLI1cAc3bbngh1ZRgGp
kry1H+7APSe0gg7MMukCQQD3jdsVoc4yhziMdpUMyw6R6vYCMJbMEr/tI6CJYBG4
lW+zdcLK2d6GNpZU80F49HOvxH4HMg1Qv+UUiuxT7jpG
-----END RSA PRIVATE KEY-----
-----BEGIN CERTIFICATE-----
MIICxTCCAi6gAwIBAgIJAN/0HUpkM5dvMA0GCSqGSIb3DQEBBQUAMEwxCzAJBgNV
BAYTAkdCMRIwEAYDVQQIEwlCZXJrc2hpcmUxEDAOBgNVBAcTB05ld2J1cnkxFzAV
BgNVBAoTDk15IENvbXBhbnkgTHRkMB4XDTExMDYwNjE0MzcyMFoXDTEyMDYwNTE0
MzcyMFowTDELMAkGA1UEBhMCR0IxEjAQBgNVBAgTCUJlcmtzaGlyZTEQMA4GA1UE
BxMHTmV3YnVyeTEXMBUGA1UEChMOTXkgQ29tcGFueSBMdGQwgZ8wDQYJKoZIhvcN
AQEBBQADgY0AMIGJAoGBAOcK/+DEMJvbKrXmqQ3Bi10w4d9brT0KTvB/CNWDNI2D
4fD7VHj6StaFhGxahdlRzL3E8L1aGcUIWKNydE5xanu8HJW6QWRU9iDOTU4EVPjz
hkIsGZ7TwaotLNmuOC7tzVi7kz0xlB1ko5sG9nMMWye8Vxx8zsbmZf8WABNqxhot
AgMBAAGjga4wgaswHQYDVR0OBBYEFGFwXmx2B6FB25yKMBm6g884lB2xMHwGA1Ud
IwR1MHOAFGFwXmx2B6FB25yKMBm6g884lB2xoVCkTjBMMQswCQYDVQQGEwJHQjES
MBAGA1UECBMJQmVya3NoaXJlMRAwDgYDVQQHEwdOZXdidXJ5MRcwFQYDVQQKEw5N
eSBDb21wYW55IEx0ZIIJAN/0HUpkM5dvMAwGA1UdEwQFMAMBAf8wDQYJKoZIhvcN
AQEFBQADgYEAbjMF7YzNQGovKD4NRjsnnKzQnUCTw6UquY2Oz/5SeLcPfLm8DudF
qppAjJjNpAgYC0yWoWcIxatYF/AsgGc2WL3hzI8oK7by6STfVi5RfLA6jS7lIDOv
4BUVsWZKADbEPsfiwed9b9MLLx8gpLLBrrr2rZpSyeDu4v16haV6wg8=
-----END CERTIFICATE-----
'! !

!SqueakSSL class methodsFor: 'examples' stamp: 'ar 6/17/2012 14:57'!
exampleCertPFX
	^ #[48 130 7 50 2 1 3 48 130 6 248 6 9 42 134 72 134 247 13 1 7 1 160 130 6 233 4 130 6 229 48 130 6 225 48 130 3 191 6 9 42 134 72 134 247 13 1 7 6 160 130 3 176 48 130 3 172 2 1 0 48 130 3 165 6 9 42 134 72 134 247 13 1 7 1 48 28 6 10 42 134 72 134 247 13 1 12 1 6 48 14 4 8 88 63 142 234 51 170 181 1 2 2 8 0 128 130 3 120 247 113 35 203 188 93 48 77 162 13 174 138 246 211 61 198 135 133 35 173 48 145 17 17 215 165 194 254 211 158 248 98 76 208 35 117 179 66 160 245 118 213 71 174 220 87 29 165 94 87 52 172 173 229 251 165 205 43 242 114 250 65 123 9 113 132 130 241 182 211 44 155 163 177 90 52 4 72 47 37 0 101 149 229 33 113 144 29 160 38 44 28 178 1 193 134 122 194 233 165 233 236 242 121 119 47 72 143 91 146 148 29 155 94 202 17 124 77 21 110 194 197 228 149 28 9 129 74 139 76 1 180 245 235 1 191 177 175 158 159 16 12 52 96 80 243 34 26 155 45 210 192 183 217 230 122 13 19 197 214 172 29 151 24 153 136 8 203 72 220 199 79 22 79 251 248 83 204 246 117 242 216 219 53 20 182 121 148 173 221 177 210 171 107 56 101 159 63 110 23 37 168 47 25 252 163 244 206 125 220 122 108 251 223 93 219 129 242 137 229 199 216 254 230 235 62 33 236 39 211 255 184 37 134 152 51 188 182 195 242 18 43 29 134 16 183 48 35 0 100 231 121 145 91 99 171 183 225 246 126 56 190 198 188 79 227 107 211 1 65 113 64 71 9 120 185 75 138 171 220 155 182 35 226 180 121 108 83 253 1 232 183 151 97 160 73 117 218 140 182 224 58 227 40 171 59 143 213 187 41 57 174 185 115 190 81 111 110 81 149 122 114 170 14 10 168 113 248 120 13 247 231 160 162 14 4 227 41 48 249 153 2 107 130 176 16 144 160 116 41 25 241 225 126 110 24 7 69 221 205 108 141 73 164 61 76 219 248 94 142 69 171 109 44 45 75 34 179 205 40 62 161 191 222 79 131 239 230 86 201 124 48 226 212 13 178 187 248 29 191 81 98 229 199 91 204 153 220 112 227 71 116 233 131 134 160 244 78 77 84 128 144 63 123 210 148 221 133 201 44 41 218 89 64 253 172 106 220 127 130 151 11 88 155 57 172 192 196 165 93 177 197 139 128 45 223 88 64 196 6 15 153 160 156 168 3 202 102 129 134 25 75 61 51 190 216 218 178 101 250 91 255 169 245 170 55 228 47 111 197 10 145 196 180 96 217 97 49 104 134 62 228 86 203 242 207 75 246 77 115 20 81 40 173 107 113 251 9 172 18 21 10 102 117 86 63 252 91 190 64 190 140 1 146 70 75 130 110 94 129 107 155 24 253 117 204 162 32 30 102 75 62 42 204 19 159 205 62 23 26 192 23 79 128 205 18 72 198 84 83 107 16 234 121 61 33 101 48 72 32 197 119 216 2 24 213 8 133 63 181 65 15 192 138 240 203 219 69 207 68 66 233 168 195 13 212 235 34 22 142 226 141 25 131 250 123 202 13 163 142 214 170 179 240 5 21 201 143 103 4 70 139 84 104 115 140 248 163 15 71 220 197 222 251 170 15 158 82 26 214 186 154 139 37 245 77 174 37 29 218 103 99 14 230 36 75 72 140 186 89 146 99 10 10 94 68 150 159 234 64 234 32 254 117 187 160 102 46 25 25 77 184 134 151 2 236 109 63 58 186 148 239 251 122 59 123 200 29 42 70 51 118 54 71 184 71 0 111 178 10 81 141 247 59 254 67 191 214 239 78 238 217 142 184 87 107 111 14 102 97 61 229 94 118 187 52 204 25 52 233 177 250 17 62 113 22 163 2 250 13 5 238 103 80 143 201 25 73 33 93 212 81 126 207 29 138 72 191 60 182 132 255 76 97 254 188 96 81 72 73 43 118 191 106 118 41 112 45 96 255 148 59 79 111 89 61 199 106 75 199 154 21 60 25 124 156 168 42 233 7 102 203 120 161 126 125 118 110 114 229 174 26 31 215 140 120 85 171 146 207 176 159 100 102 215 83 142 39 61 255 84 12 19 235 207 44 199 229 220 98 38 167 113 24 88 66 31 115 135 184 70 133 129 3 57 44 202 230 225 37 70 222 228 126 130 216 185 247 48 130 3 26 6 9 42 134 72 134 247 13 1 7 1 160 130 3 11 4 130 3 7 48 130 3 3 48 130 2 255 6 11 42 134 72 134 247 13 1 12 10 1 2 160 130 2 166 48 130 2 162 48 28 6 10 42 134 72 134 247 13 1 12 1 3 48 14 4 8 157 82 4 247 110 231 147 241 2 2 8 0 4 130 2 128 81 141 63 61 170 27 13 87 195 101 166 17 185 109 40 123 79 40 85 18 112 106 87 142 32 19 113 12 131 155 36 149 204 92 237 1 142 195 36 34 134 117 241 52 38 4 223 121 9 207 149 114 168 232 16 31 38 128 191 205 129 96 20 210 13 246 170 175 72 206 132 163 135 42 227 200 61 4 223 65 246 136 48 139 206 95 243 12 78 111 152 17 172 160 235 19 185 107 248 215 171 69 17 108 110 12 143 48 163 35 112 60 104 210 180 61 97 35 132 190 185 52 214 94 137 51 90 103 115 176 108 81 179 254 43 128 230 0 178 229 102 142 136 122 52 213 218 150 93 29 251 227 151 124 220 211 152 14 214 57 253 134 5 216 20 70 142 9 67 253 187 20 45 239 144 60 149 38 118 94 5 240 92 240 11 163 131 39 237 219 228 68 198 176 184 23 155 181 19 149 188 2 73 215 118 95 52 169 186 179 142 106 201 222 98 38 7 72 12 167 242 23 217 58 8 48 98 75 203 68 202 230 50 109 112 231 34 77 8 212 132 34 53 120 195 211 170 209 138 45 25 22 249 200 39 170 102 104 35 23 165 199 0 180 149 231 66 55 227 101 212 227 111 140 202 218 21 211 142 227 95 228 34 59 29 23 212 43 142 132 36 100 19 58 38 124 136 77 192 186 174 111 82 162 61 13 207 31 123 138 16 236 169 94 182 156 137 71 11 3 223 81 146 185 230 164 108 87 82 126 167 121 216 202 201 21 197 50 204 62 46 30 80 245 60 157 124 81 50 79 225 144 130 55 141 182 176 61 62 128 88 105 3 206 168 97 81 180 145 20 211 135 252 195 71 185 42 209 139 98 27 47 3 181 252 89 41 67 246 238 34 71 224 211 65 165 130 115 138 102 130 153 126 248 225 200 42 33 247 34 83 47 161 223 179 49 244 240 108 184 244 229 129 42 34 208 77 62 142 125 57 121 39 2 223 123 75 83 35 184 136 71 228 58 15 61 16 21 111 21 72 84 107 99 66 51 251 47 132 92 62 85 53 197 90 170 118 254 28 232 170 69 119 55 25 30 210 189 113 231 121 214 151 141 218 11 54 90 17 40 94 143 41 72 221 16 204 7 126 200 220 28 157 75 159 142 181 56 44 244 2 206 93 230 121 110 124 181 108 157 161 2 131 121 119 22 99 4 194 228 137 124 193 89 196 239 216 79 206 88 233 84 70 205 120 107 79 1 95 117 198 73 112 207 18 52 174 188 81 59 75 238 227 184 57 166 66 12 188 200 97 251 40 146 239 27 44 6 104 216 90 153 8 161 189 194 32 200 124 180 43 124 169 200 80 238 28 234 114 46 216 243 192 75 180 149 181 215 39 214 64 69 183 205 159 252 238 50 141 132 214 2 245 5 251 219 32 217 37 146 78 226 201 81 209 79 74 174 108 65 49 70 48 31 6 9 42 134 72 134 247 13 1 9 20 49 18 30 16 0 116 0 101 0 115 0 116 0 99 0 101 0 114 0 116 48 35 6 9 42 134 72 134 247 13 1 9 21 49 22 4 20 161 19 18 59 76 168 198 72 97 179 205 74 244 65 111 116 223 140 145 154 48 49 48 33 48 9 6 5 43 14 3 2 26 5 0 4 20 182 216 177 70 221 73 183 142 238 169 97 22 175 148 97 145 207 223 75 54 4 8 178 120 42 60 194 226 96 245 2 2 8 0]! !

!SqueakSSL class methodsFor: 'examples' stamp: 'jmv 6/25/2017 21:27:45'!
google: query
	"An example HTTPS query to encrypted.google.com.
	Example:
		SqueakSSL google: 'squeak'.
		SqueakSSL google: 'SqueakSSL'.
	"

	| hostName address socket ssl |

	"Change the host name to try an https request to some other host"
	hostName := 'encrypted.google.com'.

	address := NetNameResolver addressForName: hostName.
	socket := Socket newTCP.

	"Connect the TCP socket"
	socket connectTo: address port: 443.
	socket waitForConnectionFor: 10.

	"Set up SqueakSSL using the convenience APIs"
	ssl := SqueakSSL on: socket.

	["Let SqueakSSL handle the client handshake"
	ssl connect.

	"Verify that the cert is valid"
	ssl certState = 0 ifFalse:[
		self error: 'The certificate is invalid (code: ', ssl certState,')'.
	].

	"If the certificate is valid, make sure we're were we wanted to go"
	(ssl peerName match: hostName) ifFalse:[
		self error: 'Host name mismatch: ', ssl peerName.
	].

	"Send encrypted data"
	ssl sendData:
		'GET /search?q=', query,' HTTP/1.0', String crlfString,
		'Host: ', hostName, String crlfString,
		'Connection: close', String crlfString,
		String crlfString.

	"Wait for the response"
	^String streamContents:[:s|
		[socket isConnected | socket dataAvailable] 
			whileTrue:[s nextPutAll: ssl receiveData]].
	] ensure:[ssl destroy].
! !

!SqueakSSL class methodsFor: 'examples' stamp: 'jmv 6/25/2017 21:31:39'!
serverOn: port certName: certName
	"An HTTPS server example. Fires up a listener at the given port such that
	you can point a browser to that https url. Responds with a single line of text
	and closes the listener after the first connection.

		SqueakSSL 
			serverOn: 8443
			certName: 'Internet Widgits Pty'.

		SqueakSSL 
			serverOn: 8443 
			certName: '/home/andreas/certs/testcert.pem'.

	"
	| listener socket ssl |
	"Set up the listener socket"
	listener := Socket newTCP.
	listener listenOn: port backlogSize: 8.
	[socket := listener waitForAcceptFor: 30.
	socket == nil] whileTrue.
	listener destroy.

	"Set up SqueakSSL for the just accepted connection"
	[ssl := SqueakSSL on: socket.

	"The SSL needs the cert name."
	ssl certName: certName.

	"Let SqueakSSL do the server handshake"
	ssl accept.

	"Read out the HTTPS request"
	ssl receiveData.
	
	"And send the response"
	ssl sendData:
		'HTTP/1.0 200 OK', String crlfString,
		'Connection: close', String crlfString,
		'Content-Type: text/plain', String crlfString,
		'Server: SqueakSSL', String crlfString,
		String crlfString,
		'This is a successful SqueakSSL response.'.

	socket close.

	] ensure:[
		ssl destroy.
		socket destroy.
	].! !

!SqueakSSL class methodsFor: 'instance creation' stamp: 'ar 7/16/2010 22:59'!
on: aSocket
	"Convenience API. Create a SqueakSSL operating on a standard TCP socket.
	Generally not very useful for real applications (it lacks error handling etc) 
	but very helpful for debugging and other experiments."

	^self new on: aSocket! !

!SqueakSSL class methodsFor: 'instance creation' stamp: 'ar 6/17/2012 11:13'!
secureSocket
	"Answer the class to use as secure socket implementation.
	Provided here so that users only need a dependency on SqueakSSL."

	^SecureSocket! !

!SqueakSSL class methodsFor: 'instance creation' stamp: 'ar 6/17/2012 11:25'!
secureSocketStream
	"Answer the class to use as secure socket stream implementation.
	Provided here so that users only need a dependency on SqueakSSL."

	^SecureSocketStream! !

!String class methodsFor: '*WebClient' stamp: 'jmv 1/9/2024 17:28:20'!
invalidAsciiInUrlFields
	"Answer a CharacterSet with all ASCII characters that are invalid in an URL field.
	See https://en.wikipedia.org/wiki/URL_encoding
	Do include Reserved Characters (i.e. Characters that are used to form URLs)
	This is appropriate to prepare any ASCII String to be used as a field name or value in an URL.
	See senders.	"

	| answer |
	answer := ByteArray new: 256.
	(((0 to: 127) collect: [ :i | Character codePoint: i ]) reject: [ :c | c isAlphaNumeric  | ('.-_' includes: c)]) do: [ :char |
			answer at: char codePoint + 1 put: 1 ].
	^answer
	! !

!String class methodsFor: '*WebClient' stamp: 'jmv 1/9/2024 17:27:11'!
invalidAsciiInUrls
	"Answer a CharacterSet with all ASCII characters that are invalid in an URL.
	See https://en.wikipedia.org/wiki/URL_encoding
	Do not include Reserved Characters (i.e. Characters that are used to form URLs)
	This is appropriate for preparing a complete URL to be sent to network primitives.
	See senders.	"

	| answer |
	answer := ByteArray new: 256.
	((((0 to: 31) collect: [ :i | Character codePoint: i ]),
		{Character space. $". $<. $>. $\. $^. $`. ${. $|. $}. Character codePoint: 127})) do: [ :char |
			answer at: char codePoint + 1 put: 1 ].
	^answer! !

!String methodsFor: '*WebClient' stamp: 'jmv 1/9/2024 17:29:48'!
percentEscapeUrl
	"Answer an instance of String where all characters are ASCII.
	If we contain any ASCII character that is invalid in an URL, parentEscape it
	If we contain any non-ASCII character, percentEscape its UTF-8 bytes.
	See https://en.wikipedia.org/wiki/URL_encoding"

	| badChars |
	badChars := `String invalidAsciiInUrls`.
	(self isAscii and: [(self indexOfAnyIn: badChars startingAt: 1 ifAbsent: 0) = 0]) ifTrue: [
		^self ].
	^String percentEscapingNonAsciiAndThoseIn: badChars bytes: self asUtf8Bytes! !

!String methodsFor: '*WebClient' stamp: 'jmv 1/9/2024 17:29:50'!
percentEscapeUrlField
	"Answer an instance of String where all characters are ASCII.
	If we contain any ASCII character that is invalid in an URL field, parentEscape it
	If we contain any non-ASCII character, percentEscape its UTF-8 bytes.
	See https://en.wikipedia.org/wiki/URL_encoding"

	| badChars |
	badChars := `String invalidAsciiInUrlFields`.
	(self isAscii and: [(self indexOfAnyIn: badChars startingAt: 1 ifAbsent: 0) = 0]) ifTrue: [
		^self ].
	^String percentEscapingNonAsciiAndThoseIn: badChars bytes: self asUtf8Bytes! !

!String methodsFor: '*WebClient' stamp: 'jmv 1/9/2024 18:03:29'!
unescapePercents
	"change each %XY substring to the character with ASCII value XY in hex.
	This is the opposite of #percentEscapingNonAscii: and #percentEscapingNonAsciiAndThoseIn:bytes:
	(String percentEscapingNonAscii: '⊙' bytes) unescapePercents
	'%E2%8A%99' unescapePercents
	"

	| myBytes answerClass answerBytes c decodedByte pos oldPos specialCharsByteSet |
	myBytes := self asByteArray.
	specialCharsByteSet := `ByteArray new: 256 :: at: $+ asciiValue + 1 put: 1; at: $% asciiValue + 1 put: 1; yourself`.
	answerClass := String.
	answerBytes := ByteArray streamContents: [ :byteStream |
		oldPos := 1.

		[pos := myBytes indexOfAnyIn: specialCharsByteSet startingAt: oldPos ifAbsent: [ 0 ]. pos > 0 ]
		whileTrue: [
			byteStream nextPutAll: (myBytes copyFrom: oldPos to: pos - 1).
			c := self at: pos.
			c = $+ ifTrue: [byteStream nextPut: `$  asciiValue` ] ifFalse: [
				(c = $% and: [pos + 2 <= self size]) ifTrue: [
					decodedByte := (self at: pos+1) asUppercase digitValue * 16 +
						(self at: pos+2) asUppercase digitValue.
					pos := pos + 2.
					decodedByte > 255 ifTrue: [^self].	"not really an escaped string"
					decodedByte > 127 ifTrue: [ answerClass := UnicodeString ].
					byteStream nextPut: decodedByte ]
				ifFalse: [byteStream nextPut: c codePoint ]].
			oldPos := pos+1].
		byteStream nextPutAll: (myBytes copyFrom: oldPos to: self size).
	].
	^ answerClass fromUtf8Bytes: answerBytes! !

!UnicodeString methodsFor: '*WebClient' stamp: 'jmv 1/9/2024 17:30:43'!
percentEscapeUrl
	"Answer an instance of String where all characters are ASCII.
	If we contain any ASCII character that is invalid in an URL, parentEscape it
	If we contain any non-ASCII character, percentEscape its UTF-8 bytes.
	See https://en.wikipedia.org/wiki/URL_encoding"

	| badChars |
	badChars := `String invalidAsciiInUrls`.
	(self isAscii and: [(self indexOfAnyIn: badChars startingAt: 1 ifAbsent: 0) = 0]) ifTrue: [
		^self asByteString ].
	^String percentEscapingNonAsciiAndThoseIn: badChars bytes: self asUtf8Bytes! !

!UnicodeString methodsFor: '*WebClient' stamp: 'jmv 1/9/2024 17:30:30'!
percentEscapeUrlField
	"Answer an instance of String where all characters are ASCII.
	If we contain any ASCII character that is invalid in an URL field, parentEscape it
	If we contain any non-ASCII character, percentEscape its UTF-8 bytes.
	See https://en.wikipedia.org/wiki/URL_encoding"

	| badChars |
	badChars := `String invalidAsciiInUrlFields`.
	(self isAscii and: [(self indexOfAnyIn: badChars startingAt: 1 ifAbsent: 0) = 0]) ifTrue: [
		^self asByteString ].
	^String percentEscapingNonAsciiAndThoseIn: badChars bytes: self asUtf8Bytes! !
WebClient initialize!
WebMessage initialize!
