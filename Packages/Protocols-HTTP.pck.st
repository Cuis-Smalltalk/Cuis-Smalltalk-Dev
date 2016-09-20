'From Cuis 4.2 of 25 July 2013 [latest update: #2944] on 19 September 2016 at 10:07:02 pm'!
'Description - This package is a port and refactoring of WebClient from http://ss3.gemstone.com/ss/WebClient.html with moderate changes by David Graham.
	- WebClient-Core-topa.99
	- WebClient-Help-ar.10
	- WebClient-Tests-fbs.47
'!
!provides: 'Protocols-HTTP' 1 25!
!requires: 'Cryptography-DigitalSignatures' 1 1 nil!
!requires: 'Network-Kernel' 1 2 nil!
!requires: 'Identities-UUID' 1 1 nil!
!classDefinition: #HTTPChunkedStream category: #'Protocols-HTTP'!
ReadStream subclass: #HTTPChunkedStream
	instanceVariableNames: 'sourceStream chunkSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPChunkedStream class' category: #'Protocols-HTTP'!
HTTPChunkedStream class
	instanceVariableNames: ''!

!classDefinition: #HTTPAuthRequired category: #'Protocols-HTTP'!
Exception subclass: #HTTPAuthRequired
	instanceVariableNames: 'client request response authParams message'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPAuthRequired class' category: #'Protocols-HTTP'!
HTTPAuthRequired class
	instanceVariableNames: ''!

!classDefinition: #HTTPClientServerTest category: #'Protocols-HTTP-Tests'!
TestCase subclass: #HTTPClientServerTest
	instanceVariableNames: 'server user password oldAuthHandler oldProxyHandler port'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP-Tests'!
!classDefinition: 'HTTPClientServerTest class' category: #'Protocols-HTTP-Tests'!
HTTPClientServerTest class
	instanceVariableNames: ''!

!classDefinition: #HTTPClient category: #'Protocols-HTTP'!
Object subclass: #HTTPClient
	instanceVariableNames: 'flags server scheme timeout stream cookies proxyServer lastScheme lastServer lastPort maxRedirect redirections userAgent authParams proxyParams accessLog debugLog'
	classVariableNames: 'DebugLog FlagAcceptCookies FlagAllowAuth FlagAllowRedirect ProxyHandler'
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPClient class' category: #'Protocols-HTTP'!
HTTPClient class
	instanceVariableNames: ''!

!classDefinition: #HTTPCookie category: #'Protocols-HTTP'!
Object subclass: #HTTPCookie
	instanceVariableNames: 'name value path domain expiry version secure httpOnly comment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPCookie class' category: #'Protocols-HTTP'!
HTTPCookie class
	instanceVariableNames: ''!

!classDefinition: #HTTPMessage category: #'Protocols-HTTP'!
Object subclass: #HTTPMessage
	instanceVariableNames: 'stream protocol headers content'
	classVariableNames: 'StatusCodes'
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPMessage class' category: #'Protocols-HTTP'!
HTTPMessage class
	instanceVariableNames: ''!

!classDefinition: #HTTPRequest category: #'Protocols-HTTP'!
HTTPMessage subclass: #HTTPRequest
	instanceVariableNames: 'method request rawUrl server'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPRequest class' category: #'Protocols-HTTP'!
HTTPRequest class
	instanceVariableNames: ''!

!classDefinition: #HTTPResponse category: #'Protocols-HTTP'!
HTTPMessage subclass: #HTTPResponse
	instanceVariableNames: 'request status code url'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPResponse class' category: #'Protocols-HTTP'!
HTTPResponse class
	instanceVariableNames: ''!

!classDefinition: #HTTPServer category: #'Protocols-HTTP'!
Object subclass: #HTTPServer
	instanceVariableNames: 'listenerSocket listenerProcess interface port mutex connections entryPoints vault logAction nonceCache sessions serverString errorHandler accessLog certName siteUrl'
	classVariableNames: 'Default Registry'
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPServer class' category: #'Protocols-HTTP'!
HTTPServer class
	instanceVariableNames: ''!

!classDefinition: #HTTPUtils category: #'Protocols-HTTP'!
Object subclass: #HTTPUtils
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'HTTPUtils class' category: #'Protocols-HTTP'!
HTTPUtils class
	instanceVariableNames: ''!

!classDefinition: #WebSocket category: #'Protocols-HTTP'!
Object subclass: #WebSocket
	instanceVariableNames: 'name process stream handlers'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'WebSocket class' category: #'Protocols-HTTP'!
WebSocket class
	instanceVariableNames: ''!

!classDefinition: #WebSocket00 category: #'Protocols-HTTP'!
WebSocket subclass: #WebSocket00
	instanceVariableNames: 'frameType frameData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'WebSocket00 class' category: #'Protocols-HTTP'!
WebSocket00 class
	instanceVariableNames: ''!

!classDefinition: #WebSocket07 category: #'Protocols-HTTP'!
WebSocket subclass: #WebSocket07
	instanceVariableNames: 'frameType frameData closing masking'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Protocols-HTTP'!
!classDefinition: 'WebSocket07 class' category: #'Protocols-HTTP'!
WebSocket07 class
	instanceVariableNames: ''!


!HTTPChunkedStream commentStamp: 'ar 1/31/2012 14:00' prior: 0!
Can process chunked data.!

!HTTPAuthRequired commentStamp: 'ar 2/10/2012 12:39' prior: 0!
Exception signaled when authentication is required.!

!HTTPClient commentStamp: '<historical>' prior: 0!
HTTPClient provides a simple yet complete HTTP client implementation.

To retrieve the contents of a web page:
(HTTPClient httpGet: 'http://www.cuis-smalltalk.org/') content

INTRO
-------

The simplest form to use HTTPClient is by one of its convenience APIs:

	HTTPClient httpGet: 'http://www.squeak.org/'.
	HTTPClient httpPost: 'http://www.squeak.org/' content:'Hello Squeak' type: 'text/plain'.

For more elaborate use of headers and some other options in the request, a client can utilize modified variants:

	HTTPClient new httpGet: 'http://www.squeak.org/' do:[:req|
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
	client := HTTPClient new initializeFromUrl: url. 		"sets host etc"
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

The HTTPClient request methods return a HTTPResponse that the client can process:

	| resp |
	resp := HTTPClient httpGet: 'http://www.squeak.org/'.
	resp isSuccess ifFalse:[^self error: resp status].
	"Process the content from the response"
	^resp content

In addition, content can be streamed from the response so that it does not need to be downloaded all at once:

	| client resp file |
	client := HTTPClient new.
	[resp := client httpGet: 'http://www.squeak.org/'.
	resp isSuccess ifFalse:[^self error: resp status].
	"Stream the content from the response"
	file := FileStream newFileNamed: 'page.html'.
	resp streamTo: file 
			size: resp contentLength
			progress:[:total :amount].
	file close] ensure:[client close].

The progress block in the above can be omitted but has been included in this example to illustrate its usage. The block takes a total length (which can be nil if the length is not known) and the amount that has been loaded.

MULTIPLE REQUESTS
-------------------------

HTTPClient can and should be used for multiple requests to the same host. This will ensure persistent connections as well as having cookies processed properly within one session:

	| client resp |
	client := HTTPClient new.
	resp := client httpGet: 'http://www.squeak.org/'.
	resp := client httpGet: 'http://www.squeak.org/Download'.
	resp := client httpGet: 'http://www.squeak.org/Features'.
	client close.

One important issue to keep in mind is that because HTTPClient is optimized for persistent connections, you need to close it when you are done. That is not true for HTTPClient's class-side convenience APIs, which prefetch the response and close the socket. Generally speaking, whenever you say 'HTTPClient new' you need to close the client when you're done (however, you can do so by sending #close to a response you've received). For example:

	"Convenience API. Don't need to close, but prefetches result."
	HTTPClient httpGet: 'http://www.squeak.org'.

	| client resp |
	"Regular use. Create HTTPClient, return after header is read ..."
	client := HTTPClient new.
	[response := client httpGet: 'http://www.squeak.org/'.
	"... then fetch (or stream) the content ..."
	response content.
	] ensure:[
		"... and close the client when done."
		client close.
	].

AUTHENTICATION
---------------------

HTTPClient supports basic and digest authentication by default. HTTPClient delegates the retrieval of username/password to HTTPUtils which prompts the user for credentials. 

HTTPClient can either be supplied with specific credentials to be used or custom credentials handlers, for example:

	| client |
	client := HTTPClient new.
	client username: 'squeak'.
	client password: 'squeak'.
	client httpGet: 'http://www.squeak.org/protected'.

Proxy authentication works the same way as authentication but operates on a different authentication context to allow different sets of credentials to work.
!

!HTTPCookie commentStamp: 'ar 2/22/2010 05:28' prior: 0!
HTTPCookie represents an http cookie for use by HTTPClient.!

!HTTPMessage commentStamp: 'ar 2/23/2010 10:37' prior: 0!
A common base class for HTTPRequest and HTTPResponse.!

!HTTPRequest commentStamp: 'ar 2/23/2010 10:38' prior: 0!
Represents an HTTP request for HTTPClient/HTTPServer.!

!HTTPResponse commentStamp: 'ar 2/23/2010 10:39' prior: 0!
Represents an HTTP response for HTTPClient/HTTPServer.!

!HTTPServer commentStamp: '<historical>' prior: 0!
HTTPClient provides a simple yet complete HTTP server implementation.

Example:
(HTTPServer new listenOn: 8080) addService: '/hello' action: [ :req | req send200Response: 'Hello World!!']!

!HTTPUtils commentStamp: 'ar 2/23/2010 10:39' prior: 0!
HTTPUtils contains a number of utility methods used by HTTPClient/HTTPServer.!

!WebSocket commentStamp: '<historical>' prior: 0!
A wrapper object for the WebSocket API.!

!WebSocket00 commentStamp: 'ar 7/8/2010 18:21' prior: 0!
A wrapper object for the WebSocket API.!

!WebSocket07 commentStamp: '<historical>' prior: 0!
A wrapper object for the WebSocket API.!

!SmallInteger methodsFor: '*Protocols-HTTP' stamp: 'jmv 9/19/2016 22:04:27'!
numericValue
	"Shouldn't be needed!!"! !

!Character methodsFor: '*Protocols-HTTP' stamp: 'jmv 9/5/2016 21:01:57'!
isSafeForHTTP
	"whether a character is 'safe', or needs to be escaped when used, eg, in a URL"
	"[GG]  See http://www.faqs.org/rfcs/rfc1738.html. ~ is unsafe and has been removed"
	^ self codePoint < 128
		and: [self isAlphaNumeric
				or: ['.-_' includes: self]]! !

!String methodsFor: '*Protocols-HTTP' stamp: 'jmv 9/5/2016 22:10:02'!
encodeForHTTP
	"change dangerous characters to their %XX form, for use in HTTP transactions"

	| httpSafeStream |
	httpSafeStream := WriteStream on: (String new).
	self do: [ :c |
		c isSafeForHTTP
			ifTrue: [httpSafeStream nextPut: c]
			ifFalse: [
					httpSafeStream nextPut: $%.
					httpSafeStream nextPut: (c numericValue // 16) asHexDigit.
					httpSafeStream nextPut: (c numericValue \\ 16) asHexDigit.
			].
	].
	^ httpSafeStream contents! !

!String methodsFor: '*Protocols-HTTP' stamp: 'jmv 9/5/2016 20:49:10'!
unescapePercents
	"change each %XY substring to the character with ASCII value XY in hex.  This is the opposite of #encodeForHTTP"
	| ans c asciiVal pos oldPos specialChars |
	ans _ WriteStream on: String new.
	oldPos _ 1.
	specialChars _ '+%' asCharacterSet.

	[pos _ self indexOfAnyOf: specialChars startingAt: oldPos ifAbsent: [ 0 ]. pos > 0 ]
	whileTrue: [
		ans nextPutAll: (self copyFrom: oldPos to: pos - 1).
		c _ self at: pos.
		c = $+ ifTrue: [ans nextPut: $ ] ifFalse: [
			(c = $% and: [pos + 2 <= self size]) ifTrue: [
				asciiVal _ (self at: pos+1) asUppercase digitValue * 16 +
					(self at: pos+2) asUppercase digitValue.
				pos _ pos + 2.
				asciiVal > 255 ifTrue: [^self].	"not really an escaped string"
				ans nextPut: (Character numericValue: asciiVal)]
			ifFalse: [ans nextPut: c]].
		oldPos _ pos+1].
	ans nextPutAll: (self copyFrom: oldPos to: self size).
	^ ans contents! !

!HTTPChunkedStream methodsFor: 'testing' stamp: 'ar 1/31/2012 14:26'!
atEnd
	^chunkSize = 0 and:[super atEnd]! !

!HTTPChunkedStream methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:09'!
next
	"Answer the next decompressed object in the Stream represented by the
	receiver."

	<primitive: 65>
	position >= readLimit
		ifTrue: [^self pastEndRead]
		ifFalse: [^collection at: (position := position + 1)]! !

!HTTPChunkedStream methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:25'!
next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for simplicity"

	[(position + anInteger >= readLimit) and:[chunkSize ~= 0]] 
		whileTrue:[self nextChunk].

	^super next: anInteger
! !

!HTTPChunkedStream methodsFor: 'accessing' stamp: 'DSG 5/29/2012 13:40'!
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

!HTTPChunkedStream methodsFor: 'initialize' stamp: 'ar 1/31/2012 14:24'!
on: aStream

	sourceStream := aStream.
	collection := (aStream isBinary ifTrue:[ByteArray] ifFalse:[String]) new.
	position := readLimit := 0.! !

!HTTPChunkedStream methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:11'!
pastEndRead
	"Need more data"

	self nextChunk.
	^self next! !

!HTTPAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:14'!
client
	"The client causing the exception"

	^client! !

!HTTPAuthRequired methodsFor: 'initialize' stamp: 'ar 2/10/2012 13:06'!
client: aHTTPClient request: aHTTPRequest response: aHTTPResponse
	"Initializes the exception"

	client := aHTTPClient.
	request := aHTTPRequest.
	response := aHTTPResponse.
! !

!HTTPAuthRequired methodsFor: 'defaults' stamp: 'ar 2/10/2012 13:14'!
defaultAction

	self resume: false.! !

!HTTPAuthRequired methodsFor: 'defaults' stamp: 'ar 2/10/2012 13:07'!
defaultResumeValue
	"Resume with false by default"

	^false! !

!HTTPAuthRequired methodsFor: 'testing' stamp: 'ar 2/10/2012 13:09'!
isProxyAuth
	"Return true if proxy authorization is required"

	^response code = 407! !

!HTTPAuthRequired methodsFor: 'testing' stamp: 'ar 2/10/2012 13:09'!
isUnauthorized
	"Return true if authorization is required"

	^response code = 401! !

!HTTPAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:49'!
message
	"Answer a default message for the user to ask for input"

	self isProxyAuth ifTrue:[
		^'The proxy server at "', client proxyServerName, '" requires authentication.'.
	].

	^'The server at "', client serverName, '" requires authentication.'.! !

!HTTPAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:04'!
request
	"The request object causing the exception"

	^request! !

!HTTPAuthRequired methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:05'!
response
	"The response object causing the exception"

	^response! !

!HTTPAuthRequired methodsFor: 'initialize' stamp: 'ar 2/10/2012 13:13'!
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

!HTTPAuthRequired class methodsFor: 'instance creation' stamp: 'ar 2/10/2012 13:16'!
client: aHTTPClient request: aHTTPRequest response: aHTTPResponse
	"Creates a new exception"

	^(self new)
		client: aHTTPClient 
		request: aHTTPRequest 
		response: aHTTPResponse! !

!HTTPClientServerTest methodsFor: 'setup' stamp: 'SvenVanCaekenberghe 6/14/2010 15:49'!
localHostUrl
	^'http://localhost:', self port asString! !

!HTTPClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 21:56'!
oAuthParams
	"The base parameter set for oauth related tests.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	^Dictionary newFromPairs: {
		"Consumer key and secret"
		'oauth_consumer_key'. 		'abcd'.
		'oauth_consumer_secret'.	'efgh'.

		"Token key and secret"
		'oauth_token'.	'ijkl'.
		'oauth_token_secret'. 'mnop'.
		
		'oauth_timestamp'. '1281668113'.
		'oauth_nonce'.	'FWNkVaRJVzE'.
		
		"Twitter uses oauth 1.0 with HMAC-SHA1"
		'oauth_version'.				'1.0'.
		'oauth_signature_method'.	'HMAC-SHA1'.
	}.! !

!HTTPClientServerTest methodsFor: 'setup' stamp: 'dsg 3/12/2015 10:43'!
port

	"Use a random port to minimise chances of concurrently running test
	suites clashing."
	^ port
		ifNil: [port := (10000 to: 50000) atRandom]! !

!HTTPClientServerTest methodsFor: 'setup' stamp: 'ar 2/10/2012 13:27'!
setUp
	server := HTTPServer new listenOn: self port.
	server passwordAt: 'user' realm: 'test' put: 'pass'.
! !

!HTTPClientServerTest methodsFor: 'setup' stamp: 'ar 2/10/2012 13:27'!
tearDown
	server ifNotNil:[server destroy].
! !

!HTTPClientServerTest methodsFor: 'tests - auth' stamp: 'ar 2/10/2012 13:26'!
testAuthException
	"Test client and server handling of digest auth"

	| resp client |
	server addService: '/test/auth' action:[:req | 
		server authenticate: req realm: 'test' methods: #(digest) do:[
			req send200Response: 'ok'
		].
	].

	client := HTTPClient new.
	self should:[client httpGet: self localHostUrl, '/test/auth'] 
		raise: HTTPAuthRequired.

	client allowAuth: false.
	self shouldnt:[resp := client httpGet: self localHostUrl, '/test/auth'] 
		raise: HTTPAuthRequired.
	self assert: resp code = 401.
! !

!HTTPClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:29'!
testAuthRedirectSession
	"Run a test for a full auth-redirect-cookie loop"

	| loginOK finalUrl finalFields firstRedirect client resp |
	server addService: '/login' action:[:req | 
		server authenticate: req realm: 'test' methods: #(digest) do:[ | id |
			loginOK := true.
			server sessionAt: (id := UUID new hex) put: ''.
			req send302Response: (req fields at: 'url' ifAbsent:['/']) 
				do:[:reply| reply setCookie: 'session' value: id path: '/']]].
	server addService: '/action' action:[:req | 
		(server sessionAt: (req cookieAt: 'session')) ifNil:[
			firstRedirect := true.
			req send302Response: '/login?url=', req rawUrl encodeForHTTP.
		] ifNotNil:[
			finalUrl := req url.
			finalFields := req fields.
			req send200Response: 'ok'.
		].
	].
	firstRedirect := loginOK := false.
	server passwordAt: 'squeak' realm: 'test' put: 'foo'.
	client := HTTPClient new.
	client username: 'squeak'; password: 'foo'.
	resp := client httpGet: self localHostUrl, '/action/foo/bar?string=hello&number=42'.

	self assert: resp code = 200.
	self assert: firstRedirect.
	self assert: loginOK.
	self assert: finalUrl = '/action/foo/bar'.
	self assert: finalFields size = 2.
	self assert: (finalFields at: 'string') = 'hello'.
	self assert: (finalFields at: 'number') = '42'.
! !

!HTTPClientServerTest methodsFor: 'tests - auth' stamp: 'ar 2/10/2012 13:53'!
testBasicAuth
	"Test client and server basic auth"

	| resp reqHeader |
	server addService: '/test/auth' action:[:req | 
		server authenticate: req realm: 'test' methods: #(basic) do:[
			reqHeader := req headerAt: 'Authorization'.
			req send200Response: 'ok'
		].
	].
	resp := HTTPClient new httpGet: self localHostUrl, '/test/auth'.
	self assert: resp code = 401.

	[resp := HTTPClient httpGet: self localHostUrl, '/test/auth'] 
		on: HTTPAuthRequired do:[:ex| ex username: 'squeak' password: 'squeak'].
	self assert: resp code = 401.

	[resp := HTTPClient httpGet: self localHostUrl, '/test/auth']
		on: HTTPAuthRequired do:[:ex| ex username: 'user' password: 'pass'].
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (reqHeader beginsWith: 'Basic ').
! !

!HTTPClientServerTest methodsFor: 'tests - chunked' stamp: 'ar 8/31/2010 23:17'!
testChunkedLoopback
	"Test HTTP loopback streaming using chunked transfer-encoding"

	| queue response |
	queue := SharedQueue new.
	server addService: '/recv' action:[:req |

		"The /recv service establishes the write-end for the server.
		In a real environment we would access protect the request
		and also pass a token to be used to link the incoming /send
		request from the client."

		req sendResponse: 200 chunked:[:writeEnd|
			| chunk |
			"There is no reason to wait for the client to send a request,
			the protocol is entirely freestyle. Send something just because
			we can"
			writeEnd nextChunkPut: 'Initial response'.
			"And from here on echo any incoming data"
			[chunk := queue next.
			chunk == nil] whileFalse:[writeEnd nextChunkPut: chunk].
			"And some final data"
			writeEnd nextChunkPut: 'Final response'.
		].
	] methods: #('GET'). "only allow GET requests"

	server addService: '/send' action:[:req |

		"The /send service establishes the read-end for the server.
		Simply read the chunks as they come in and stick them in
		our loopback queue to send them back to the client."

		| chunk |
		[chunk := req nextChunk.
		chunk == nil] whileFalse:[queue nextPut: chunk].
		queue nextPut: nil. "end conversation"
		req send200Response: 'ok'.
	] methods: #('POST'). "only allow POST requests"

	"Establish the server response stream"
	response := HTTPClient new httpGet: self localHostUrl, '/recv'.
	self assert: response code = 200.
	self assert: (response headerAt: 'Transfer-Encoding') = 'chunked'.
	self assert: response nextChunk equals: 'Initial response'.

	"Establish the client request stream"
	HTTPClient 
		httpPostChunked: self localHostUrl, '/send'
		content:[:request|
			"We've set up both ends, try our loopback server"
			request nextChunkPut: 'Hello World'.
			self assert: response nextChunk equals: 'Hello World'.
			request nextChunkPut: 'The answer is 42'.
			self assert: response nextChunk equals: 'The answer is 42'.
		] type: nil.

	self assert: response nextChunk equals: 'Final response'.
	self assert: response nextChunk equals: nil.
! !

!HTTPClientServerTest methodsFor: 'tests - chunked' stamp: 'ar 8/31/2010 22:07'!
testChunkedRequest
	"Test HTTP post using chunked transfer-encoding"

	| resp request |
	server addService: '/test' action:[:req | 
		request := req.
		req send200Response: req content].

	resp := HTTPClient 
				httpPostChunked: self localHostUrl, '/test' 
				content:[:req|
					req nextChunkPut: 'Hello'.
					req nextChunkPut: 'World'.
					req nextChunkPut: 'Dude'.
				] type: nil.

	self assert: resp code = 200.
	self assert: resp content = 'HelloWorldDude'.
	self assert: (request headerAt: 'Transfer-Encoding') = 'chunked'! !

!HTTPClientServerTest methodsFor: 'tests - chunked' stamp: 'ar 8/31/2010 22:17'!
testChunkedResponse
	"Test HTTP response using chunked transfer-encoding"

	| resp |
	server addService: '/test' action:[:req | 
		req sendResponse: 200 chunked:[:response|
			response nextChunkPut: 'Hello'.
			response nextChunkPut: 'World'.
			response nextChunkPut: 'Dude'.
		].
	].

	resp := HTTPClient httpGet: self localHostUrl, '/test'.
	self assert: resp code = 200.
	self assert: resp content = 'HelloWorldDude'.
	self assert: (resp headerAt: 'Transfer-Encoding') = 'chunked'.! !

!HTTPClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/5/2010 19:28'!
testCookieDomainRules
	"Test cookie domain rules"

	"Request host matches domain"
	self assert: (HTTPClient new 
			acceptCookie: (HTTPCookie new domain: 'www.domain.com') 
			host: 'www.domain.com' 
			path: '/').

	"Request host matches subdomain"
	self assert: (HTTPClient new 
			acceptCookie: (HTTPCookie new domain: '.domain.com') 
			host: 'www.domain.com' 
			path: '/').

	"Request host does not match domain"
	self deny: (HTTPClient new 
			acceptCookie: (HTTPCookie new domain: 'www.domain.com') 
			host: 'ftp.domain.com' 
			path: '/').

	"Request host does not match subdomain"
	self deny: (HTTPClient new 
			acceptCookie: (HTTPCookie new domain: '.domain.com') 
			host: 'www.someother.org' 
			path: '/').

	"Request host has an extra subdomain"
	self deny: (HTTPClient new 
			acceptCookie: (HTTPCookie new domain: '.domain.com') 
			host: 'foo.bar.domain.com' 
			path: '/').
! !

!HTTPClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/21/2011 11:36'!
testCookieExpiryParsing
	"Test parsing of cookie expiry dates"

	| exp a b |
	exp := DateAndTime date: Date today time: Time now.
	a := HTTPCookie new.
	a name: 'test'.
	a expiry: exp.
	b := HTTPCookie new readFrom: a asString readStream.
	self assert: b expiry = exp asUTC.

	b := HTTPCookie new readFrom: (a asString copyReplaceAll: '-' with: ' ') readStream.
	self assert: b expiry = exp asUTC.
! !

!HTTPClientServerTest methodsFor: 'tests - cookies' stamp: 'jmv 9/5/2016 22:17:44'!
testCookieParsing
	"Test client and server handling of cookies"

	| resp client |
	"Sends the testcookie back if it's set"
	server addService: '/' action:[:req |
		req send200Response:(String streamContents:[:s|
			req cookiesDo:[:key :val| s nextPutAll: key, '=', val, String crString].
		]).
	].

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl,'/cookieA' do:[:req|
		req headerAt: 'Cookie' put: 'foo=bar; key=value, x=32, y=55'.
	].
	resp content; close.
	self assert: resp code = 200.
	self assert: resp content 
		equals: 	'foo=bar', String crString,
				'key=value', String crString,
				'x=32', String crString,
				'y=55', String crString.

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl,'/cookieA' do:[:req|
		req addHeader: 'Cookie' value: 'foo=bar'.
		req addHeader: 'Cookie' value: 'key=value'.
		req addHeader: 'Cookie' value: 'x=32'.
		req addHeader: 'Cookie' value: 'y=55'.
	].
	resp content; close.
	self assert: resp code = 200.
	self assert: resp content 
		equals: 	'foo=bar', String crString,
				'key=value', String crString,
				'x=32', String crString,
				'y=55', String crString.
! !

!HTTPClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/5/2010 19:36'!
testCookies
	"Test client and server handling of cookies"

	| resp client |
	"Sends the testcookie back if it's set"
	server addService: '/' action:[:req | 
		req send200Response: (req cookieAt: 'testcookie').
	].

	"Sets the cookie"
	server addService: '/cookie/set' action:[:req | 
		req send200Response: 'ok' contentType: 'text/plain' 
			do:[:reply| reply setCookie: 'testcookie' value: '123' path: '/cookie']].

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/cookie/set'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.

	resp := client httpGet: self localHostUrl, '/cookie/test'.
	self assert: resp code = 200.
	self assert: resp content = '123'.

	resp := client httpGet: self localHostUrl, '/nocookie/test'.
	self assert: resp code = 200.
	self assert: resp content = ''.
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 7/20/2010 20:38'!
testDecodeWebSocketKey
	"Ensure that decoding the Sec-WebSocket-Key fields works properly."

	| key1 key2 data |
	key1 := 155712099.
	key2 := 173347027.
	data := 'Tm[K T2u'.

	self assert: (HTTPUtils extractWebSocketKey: '18x 6]8vM;54 *(5:  {   U1]8  z [  8') = key1.
	self assert: (HTTPUtils extractWebSocketKey: '1_ tx7X d  <  nw  334J702) 7]o}` 0') = key2.

	self assert: (HTTPUtils webSocketHandshake: key1 with: key2 with: data) asString = 'fQJ,fN/4F4!!~K~MH'.! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:30'!
testDefault404
	"Test that a new server responds with 404 to anything"

	| resp |
	resp := HTTPClient httpGet: self localHostUrl.
	self assert: resp code = 404.
! !

!HTTPClientServerTest methodsFor: 'tests - auth' stamp: 'ar 2/10/2012 13:53'!
testDigestAuth
	"Test client and server handling of digest auth"

	| resp reqHeader |
	server addService: '/test/auth' action:[:req | 
		server authenticate: req realm: 'test' methods: #(digest) do:[
			reqHeader := req headerAt: 'Authorization'.
			req send200Response: 'ok'
		].
	].

	resp := HTTPClient new httpGet: self localHostUrl, '/test/auth'.
	self assert: resp code = 401.

	[resp := HTTPClient httpGet: self localHostUrl, '/test/auth']
		on: HTTPAuthRequired do:[:ex| ex username: 'squeak' password: 'squeak'].
	self assert: resp code = 401.

	[resp := HTTPClient httpGet: self localHostUrl, '/test/auth']
		on: HTTPAuthRequired do:[:ex| ex username: 'user' password: 'pass'].
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (reqHeader beginsWith: 'Digest ').
! !

!HTTPClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/5/2010 19:36'!
testDuplicateCookies
	"Tests deleting cookies in HTTPClient"

	| resp client |
	"Sends the testcookie back if it's set"
	server addService: '/' action:[:req | 
		req send200Response: (req headersAt: 'cookie') size asString.
	].

	"Sets the cookie"
	server addService: '/cookie/set' action:[:req | 
		req send200Response: 'ok' contentType: 'text/plain' 
			do:[:reply| reply setCookie: 'testcookie' value: '123' path: '/cookie']].

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/cookie/set'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.

	resp := client httpGet: self localHostUrl, '/cookie/set'.
	resp := client httpGet: self localHostUrl, '/cookie/set'.

	resp := client httpGet: self localHostUrl, '/cookie/test'.
	self assert: resp code = 200.
	self assert: resp content = '1'.
! !

!HTTPClientServerTest methodsFor: 'tests - fields' stamp: 'jmv 9/5/2016 22:34:15'!
testGetFields
	"Test client and server handling simple fields"

	| resp |
	server addService: '/fields' action:[:req | 
		req send200Response: (String streamContents:[:s|
			req fields keys asArray sort do:[:key | s nextPutAll: String crlfString; nextPutAll: key,'=', (req fields at: key)].
		]).
	].

	resp := HTTPClient httpGet: self localHostUrl, '/fields?foo=123&bar=yoho'.
	self assert: resp code = 200.
	self assert: resp content = (String streamContents: [ :s | s nextPutAll: String crlfString; nextPutAll: 'bar=yoho'; nextPutAll: String crlfString; nextPutAll: 'foo=123'])
! !

!HTTPClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/11/2010 21:20'!
testHmacSha1
	"Test the SHA1 HMAC algorithm"

	| key message |
	key := 'MCD8BKwGdgPHvAuvgvz4EQpqDAtx89grbuNMRd7Eh98&'.
	message := 'POST&https%3A%2F%2Fapi.twitter.com%2Foauth%2Frequest_token&oauth_callback%3Dhttp%253A%252F%252Flocalhost%253A3005%252Fthe_dance%252Fprocess_callback%253Fservice_provider_id%253D11%26oauth_consumer_key%3DGDdmIQH6jhtmLUypg82g%26oauth_nonce%3DQP70eNmVz8jvdPevU3oJD2AfF7R7odC2XJcn4XlZJqk%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1272323042%26oauth_version%3D1.0'.

	self assert: (HTTPUtils hmacSha1: message key: key) base64Encoded = '8wUi7m5HFQy76nowoCThusfgB+Q='! !

!HTTPClientServerTest methodsFor: 'tests - methods' stamp: 'ar 5/11/2010 20:42'!
testHttpDelete
	"Test for handling a DELETE request"

	| resp |
	server addService: '/nodelete' action:[:req | 
		req send200Response: 'ok'.
	].
	server addService: '/delete' action:[:req | 
		req send200Response: req method.
	] methods: {'GET'. 'PUT'. 'DELETE'}.

	resp := HTTPClient httpGet: self localHostUrl, '/nodelete'.
	self assert: resp code = 200.
	resp := HTTPClient httpDelete: self localHostUrl, '/nodelete'.
	self assert: resp code = 405.

	resp := HTTPClient httpGet: self localHostUrl, '/delete'.
	self assert: resp code = 200.
	resp := HTTPClient httpDelete: self localHostUrl, '/delete'.
	self assert: resp code = 200.

	resp := HTTPClient httpPost: self localHostUrl, '/delete' content:'' type: nil.
	self assert: resp code = 405.
! !

!HTTPClientServerTest methodsFor: 'tests - methods' stamp: 'ar 8/5/2010 22:32'!
testHttpHead
	"Test for handling a HEAD request"

	| resp |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send200Response: 'ok'
	].

	resp := HTTPClient httpHead: self localHostUrl, '/bar'.
	self assert: resp code = 200.
	self assert: resp content = ''.
	self deny: resp contentStream isDataAvailable..

	resp := HTTPClient httpHead: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp content = ''.
! !

!HTTPClientServerTest methodsFor: 'tests - methods' stamp: 'ar 5/11/2010 20:42'!
testHttpOptions
	"Test for handling the OPTIONS request"

	| resp |
	server addService: '/delete' action:[:req | 
		req send200Response: req method.
	] methods: {'GET'. 'PUT'. 'DELETE'}.

	resp := HTTPClient httpOptions: self localHostUrl, '/*'.
	self assert: resp code = 200.
	self assert: resp content = ''.
	self assert: (resp headerAt: 'allow') = 'HEAD,TRACE,OPTIONS,GET,POST'.

	resp := HTTPClient httpOptions: self localHostUrl, '/delete'.
	self assert: resp code = 200.
	self assert: resp content = ''.
	self assert: (resp headerAt: 'allow') = 'HEAD,TRACE,OPTIONS,GET,PUT,DELETE'.! !

!HTTPClientServerTest methodsFor: 'tests - methods' stamp: 'jmv 5/19/2015 22:36'!
testHttpTrace
	"Test for handling the TRACE request"

	| resp |
	resp := HTTPClient httpTrace: self localHostUrl, '/bar'.
	self assert: resp code = 200.
	self assert: resp contentType = 'message/http'.
	self assert: resp content = ('TRACE /bar HTTP/1.1' , String crlfString , 'user-agent: ' , HTTPClient new userAgent , String crlfString , 'host: localhost:' , self port printString , String crlfString , String crlfString)
! !

!HTTPClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 5/11/2010 20:42'!
testInvalidCookies
	"Test client and server handling of cookies"

	| resp client |
	"Sends the testcookie back if it's set"
	server addService: '/' action:[:req | 
		req send200Response: (req cookieAt: 'testcookie').
	].

	"Sets the cookie"
	server addService: '/setcookie' action:[:req | | domain path expires secure |
		domain := req fields at: 'domain' ifAbsent:[nil].
		path := req fields at: 'path' ifAbsent:[nil].
		expires := req fields at: 'expires' ifAbsent:[nil].
		secure := req fields at: 'secure' ifAbsent:['false'].
		req send200Response: 'ok' contentType: 'text/plain'  do:[:reply| 
			reply setCookie: 'testcookie' value: '123' path: path 
				expires: expires domain: domain secure: secure = 'true'.
		]
	].

	server addService: '/clearcookie' action:[:req |
		req send200Response: 'ok' contentType: 'text/plain'  do:[:reply| 
			reply setCookie: 'testcookie' value: '123' path: '/' 
				expires: DateAndTime new domain: nil secure: false.
		]
	].

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/setcookie?path=/'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.
	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self assert: resp content = '123'.

	resp := client httpGet: self localHostUrl, '/clearcookie'.
	self assert: resp code = 200.
	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self deny: resp content = '123'.

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/setcookie?domain=.foo.com'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.

	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self deny: resp content = '123'.

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/setcookie?domain=.com'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.
	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self deny: resp content = '123'.

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/setcookie?secure=true'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.
	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self deny: resp content = '123'.
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'SvenVanCaekenberghe 6/14/2010 15:55'!
testListenOnInterface
	"Test that listening on a particular interface listens only on that interface"

	| localHostAddr resp localHostName client |
	server destroy.		"kill old server"

	localHostAddr := NetNameResolver localHostAddress.
	localHostAddr = #(127 0 0 1) asByteArray ifTrue:[^self]. "skip test"

	localHostName := String streamContents:[:s|
		localHostAddr do:[:b| s print: b] separatedBy:[s nextPut: $.].
	].

	server := HTTPServer new listenOn: self port interface: localHostAddr.
	server addService: '/' action:[:req| req send200Response: 'ok'].

	client := HTTPClient new.
	client timeout: 1.
	self should:[resp := client httpGet: self localHostUrl, '/test'] raise: Error.

	resp := client httpGet: 'http://', localHostName, ':', self port asString, '/test'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'jmv 5/19/2015 22:38'!
testLogging200
	"Test logging a regular 200 a-ok response"

	| resp client log |
	server addService: '/test' action:[:req | req send200Response: 'ok'].
	server accessLog: String new writeStream.

	client := HTTPClient new.
	client accessLog: String new writeStream.
	[resp := client httpGet: self localHostUrl, '/test'] ensure:[client close].

	self assert: resp code = 200.

	log := server accessLog contents.
	self assert: ('127.0.0.1 - - [*] "GET /test HTTP/1.1" 200 2', String lfString match: log).

	log := client accessLog contents.
	self assert: ('localhost - - [*] "GET /test HTTP/1.1" 200 2', String lfString match: log).
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'jmv 5/19/2015 22:39'!
testLogging404
	"Test logging a 404 response"

	| resp client log |
	server accessLog: String new writeStream.
	client := HTTPClient new.
	client accessLog: String new writeStream.
	[resp := client httpGet: self localHostUrl] ensure:[client close].

	self assert: resp code = 404.

	log := server accessLog contents.
	self assert: ('127.0.0.1 - - [*] "GET / HTTP/1.1" 404 145', String lfString match: log).

	log := client accessLog contents.
	self assert: ('localhost - - [*] "GET / HTTP/1.1" 404 145', String lfString match: log).
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:32'!
testNestedAction
	"Test handling of nested actions"

	| resp |
	server addService: '/test' action:[:req | req send200Response: 'ok'].
	server addService: '/test/42' action:[:req | req send200Response: '42'].

	resp := HTTPClient httpGet: self localHostUrl, '/test'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.

	resp := HTTPClient httpGet: self localHostUrl, '/test/42'.
	self assert: resp code = 200.
	self assert: resp content = '42'.

	resp := HTTPClient httpGet: self localHostUrl, '/test/43'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!HTTPClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 8/10/2010 09:23'!
testNo302Redirect
	"Ensure compliance with RFC 2616 rules prohibiting auto redirect 
	for 302/307 responses."

	| resp |
	server addService: '/302' action:[:req | 
		req send3xxResponse: '/bar' code: 302
	] methods: #('GET' 'PUT' 'POST' 'DELETE').
	server addService: '/307' action:[:req | 
		req send3xxResponse: '/bar' code: 307
	] methods: #('GET' 'PUT' 'POST' 'DELETE').
	server addService: '/bar' action:[:req | 
		req send200Response: 'redirect ok'
	] methods: #('GET' 'PUT' 'POST' 'DELETE').

	resp := HTTPClient httpGet: self localHostUrl, '/302'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.

	resp := HTTPClient httpGet: self localHostUrl, '/307'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.

	resp := HTTPClient httpPost: self localHostUrl, '/302' content: '' type: 'text/plain'.
	self assert: resp code = 302.
	resp := HTTPClient httpPost: self localHostUrl, '/307' content: '' type: 'text/plain'.
	self assert: resp code = 307.

	resp := HTTPClient httpPost: self localHostUrl, '/302' content: '' type: 'text/plain'.
	self assert: resp code = 302.
	resp := HTTPClient httpPost: self localHostUrl, '/307' content: '' type: 'text/plain'.
	self assert: resp code = 307.

	resp := HTTPClient httpPut: self localHostUrl, '/302' content: '' type: 'text/plain'.
	self assert: resp code = 302.
	resp := HTTPClient httpPut: self localHostUrl, '/307' content: '' type: 'text/plain'.
	self assert: resp code = 307.

	resp := HTTPClient httpDelete: self localHostUrl, '/302'.
	self assert: resp code = 302.
	resp := HTTPClient httpDelete: self localHostUrl, '/307'.
	self assert: resp code = 307.

! !

!HTTPClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 2/10/2012 13:21'!
testPersistentAuthRedirectSession
	"Run a test for a full auth-redirect-cookie loop.
	Ensure that the connection is persistent for the entire loop."

	| loginOK finalUrl finalFields firstRedirect client resp url |
	server addService: '/login' action:[:req | 
		server authenticate: req realm: 'test' methods: #(digest) do:[ | id |
			loginOK := true.
			server sessionAt: (id := UUID new hex) put: ''.
			req send302Response: (req fields at: 'url' ifAbsent:['/']) unescapePercents
				do:[:reply| reply setCookie: 'session' value: id path: '/']]].
	server addService: '/action' action:[:req | 
		(server sessionAt: (req cookieAt: 'session')) ifNil:[
			firstRedirect := true.
			req send302Response: '/login?url=', req rawUrl encodeForHTTP.
		] ifNotNil:[
			finalUrl := req url.
			finalFields := req fields.
			req send200Response: 'ok'.
		].
	].
	firstRedirect := loginOK := false.
	server passwordAt: 'squeak' realm: 'test' put: 'foo'.

	client := HTTPClient new.
	client allowRedirect: false.
	url :=  self localHostUrl, '/action/foo/bar?string=hello&number=42'.
	[[resp := client httpGet: url] on: HTTPAuthRequired 
		do:[:ex| 	self assert: client == ex client.
				self assert: client isConnected.
				ex username: 'squeak' password: 'foo'.].
	resp code = 302] whileTrue:[
		self assert: client isConnected.
		url := resp headerAt: 'Location'.
	].
	self assert: client isConnected.
	self assert: resp code = 200.
	self assert: firstRedirect.
	self assert: loginOK.
	self assert: finalUrl = '/action/foo/bar'.
	self assert: finalFields size = 2.
	self assert: (finalFields at: 'string') = 'hello'.
	self assert: (finalFields at: 'number') = '42'.
! !

!HTTPClientServerTest methodsFor: 'tests - fields' stamp: 'jmv 9/5/2016 22:34:19'!
testPostFields
	"Test client and server handling fields in url-encoded encoded post requests"

	| resp |
	server addService: '/fields' action:[:req | 
		req send200Response: (String streamContents:[:s|
			req fields keys asArray sort do:[:key | s nextPutAll: String crlfString; nextPutAll: key,'=', (req fields at: key)].
		]).
	].

	resp := HTTPClient httpPost: self localHostUrl, '/fields' 
				content: 'foo=123&bar=yoho'
				type: 'application/x-www-form-urlencoded'.
	self assert: resp code = 200.
	self assert: resp content = (String streamContents: [ :s | s nextPutAll: String crlfString; nextPutAll: 'bar=yoho'; nextPutAll: String crlfString; nextPutAll: 'foo=123'])
! !

!HTTPClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:32'!
testRedirect
	"Test client and server handling of redirects"

	| resp client |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send200Response: 'redirect ok'
	].

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.

	client allowRedirect: false.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 302.

	client allowRedirect: true.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.
! !

!HTTPClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:32'!
testRedirectLoop
	"Test client handling of redirect loops"

	| resp |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send302Response: '/foo'
	].

	resp := HTTPClient httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 302.
! !

!HTTPClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:33'!
testRedirectTrailingSlash
	"Special test to ensure that a redirect from /foo to /foo/ works"

	| resp client |
	server addService: '/foo' action:[:req | 
		req send302Response: '/foo/'
	].
	server addService: '/foo/' action:[:req | 
		req send200Response: 'redirect ok'
	].

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 8/24/2010 21:42'!
testResponseUrl
	"Tests that the response url is pointing to the final location"

	| resp client |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send200Response: 'redirect ok'
	].

	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp url = (self localHostUrl, '/bar').

	client := HTTPClient new.
	client allowRedirect: false.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 302.
	self assert: resp url = (self localHostUrl, '/foo').
! !

!HTTPClientServerTest methodsFor: 'tests - https' stamp: 'ar 7/21/2010 20:59'!
testSchemeHandling
	"Ensure that we handle only http and https schemes"

	self shouldnt:[HTTPClient httpGet: 'http://localhost:', self port asString] raise: Error.
	self should:[HTTPClient httpGet: 'ftp://localhost:', self port asString] raise: Error.

! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 11/19/2011 18:20'!
testServerDestroy
	"Tests that connections get nuked when server gets killed"
	
	| client resp |
	server addService: '/test' action:[:req | req send200Response: 'ok'].
	client := HTTPClient new.
	resp := client httpGet: self localHostUrl, '/test'.

	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: client isConnected.
	self assert: server connections size = 1.

	server destroy.

	"Depending on OS the signal may not be delivered synchronously
	and raise ConnectionClosed in the (signaling) peek. Give it a bit of 
	time to deal with the close."
	(Delay forMilliseconds: 100) wait.

	"Make stream non-blocking otherwise peek can blow up in our face"
	resp contentStream shouldSignal: false.
	self assert: resp contentStream peek == nil.
	self deny: client isConnected
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:33'!
testServerError
	"Test server handling of errors"

	| resp |
	server addService: '/test' action:[:req | self error: 'boom'].

	resp := HTTPClient httpGet: self localHostUrl, '/test'.
	self assert: resp code = 500.
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 11/1/2010 21:09'!
testServerRegistry
	"Ensure that the HTTPServer registry functions as intended"

	| serverA serverB |
	["Create a new server"
	serverA := HTTPServer forUrl: 'http://foo.bar.com'.
	serverA listenOn: self port+1.
	self assert: serverA notNil.
	self assert: serverA isRunning.

	"Look it up again"
	serverB := HTTPServer forUrl: 'http://foo.bar.com'.
	self assert: serverA == serverB.

	"Override with a new one (should shutdown serverA)"
	serverB := HTTPServer newForUrl: 'http://foo.bar.com'.
	self deny: serverA == serverB.
	self deny: serverA isRunning.

	"Destroy (should de-register serverB)"
	serverB destroy.
	self assert: (HTTPServer forUrl: 'http://foo.bar.com' ifAbsent:[nil]) == nil
	] ensure:[
		serverA ifNotNil:[serverA destroy].
		serverB ifNotNil:[serverB destroy].
	].! !

!HTTPClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:03'!
testSignOAuthGet
	"Ensure that we can sign a request correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := HTTPRequest new.
	request method: 'GET'.
	HTTPUtils oAuthSign: request 
				url: 'http://host.net/resource'
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="dSI3zjYnriSnaB787UH2NDcS8Ss%3D"'.
! !

!HTTPClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:04'!
testSignOAuthGetDupFields
	"Ensure that we can sign a request with duplicate fields correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := HTTPRequest new.
	request method: 'GET'.
	HTTPUtils oAuthSign: request 
				url: 'http://host.net/resource?name=value&name=value'
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="30p67coAX8YbxCKAGfaimydYK6g%3D"'.
! !

!HTTPClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:07'!
testSignOAuthGetFields
	"Ensure that we can sign a GET request with fields correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := HTTPRequest new.
	request method: 'GET'.
	HTTPUtils oAuthSign: request 
				url: 'http://host.net/resource?name=value'
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="N6TOtNK6h3u9zqjqaF2kgpIVb%2F8%3D"'.
! !

!HTTPClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:04'!
testSignOAuthPostFields
	"Ensure that we can sign a POST request with fields correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := HTTPRequest new.
	request method: 'POST'.
	HTTPUtils oAuthSign: request 
				url: 'http://host.net/resource'
				extra:{'name' -> 'value'}
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="E7yVjmf%2F8UTF9ij15CtbBBhulMw%3D"'.
! !

!HTTPClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:04'!
testSignOAuthUrlEncoding
	"Ensure that we can sign a request requiring url-encoded oauth params.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| params request |
	"Consumer Key with url-encoded characters"
	params := self oAuthParams.
	params at: 'oauth_consumer_key' put: 'key with spaces'.

	request := HTTPRequest new.
	request method: 'GET'.
	HTTPUtils oAuthSign: request 
				url: 'http://host.net/resource'
				using: params.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="key%20with%20spaces",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="%2FxrAsx0Utt3V6ZbX00jWWpkqrvg%3D"'.! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 6/14/2010 21:06'!
testSimpleServerAction
	"Test client and server handling simple defaults"

	| resp srvr |
	server addService: '/test' action:[:req |
		srvr := req server.
		req send200Response: 'ok'].

	resp := HTTPClient httpGet: self localHostUrl, '/test'.
	self assert: srvr notNil.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:33'!
testStreaming
	"Run a test for a full auth-redirect-cookie loop"

	| resp amount stream |
	amount := 128*1024.
	server addService: '/streaming' action:[:req|
		req stream200Response: (String new: amount) readStream size: amount.
	].
	resp := HTTPClient httpGet: self localHostUrl, '/streaming'.
	self assert: resp isSuccess.
	stream := WriteStream on: String new.
	resp streamTo: stream size: resp contentLength progress: nil.
	self assert: stream position = amount.
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 6/19/2010 00:05'!
testTransientPostContent
	"Ensure that HTTPRequest>>content doesn't close the socket 
	when used in a transient post request"

	| resp client |
	server addService: '/test' action:[:req |
		req send200Response: req content].

	client := HTTPClient new.
	resp := client httpPost: self localHostUrl, '/test' content:'hello' type: nil do:[:req|
		req protocol: 'HTTP/1.0'
	].
	self assert: resp code = 200.
	self assert: resp content = 'hello'.
! !

!HTTPClientServerTest methodsFor: 'tests - misc' stamp: 'ar 9/4/2010 11:35'!
testUrlEncoding
	"Test the default URL encoding behavior"
	| resp |
	server addService: '/hello world' action:[:req|
		req send200Response: 'ok'.
	].
	resp := HTTPClient httpGet: self localHostUrl, '/hello%20world'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.

	resp := HTTPClient httpGet: (HTTPUtils urlEncode: self localHostUrl, '/hello world').
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 10:29'!
testWebSocketHash07
	"self run: #testWebSocketHash07"
	"From http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07"

	| hash key |
	key := 'dGhlIHNhbXBsZSBub25jZQ=='.
	hash := HTTPUtils webSocketHash07: key.
	self assert: hash = 's3pPLMBiTxaQ9kYGzzhZRbK+xOo='.! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets
	"Test the WebSocket interface"

	self testWebSockets:[
		HTTPClient webSocketTo: self localHostUrl,'/websockets'.
	].
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets00
	"Test the WebSocket interface"

	self testWebSockets:[
		HTTPClient webSocket00: self localHostUrl,'/websockets' protocol: 'sample'
	].
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets07
	"Test the WebSocket interface"

	self testWebSockets:[
		HTTPClient webSocket07: self localHostUrl,'/websockets' protocol: 'sample'
	].
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 11/17/2011 17:28'!
testWebSockets07ControlDecode
	"Tests from section 4.7 of the WebSockets spec"

	| ws ping pong |
	ws := WebSocket07 new.
	ws onPing:[:msg| ping := msg asString].
	ws onPong:[:msg| pong := msg asString].

	ping := pong := nil.
	ws readFrameFrom:
		#[16r89 16r05 16r48 16r65 16r6C 16r6C 16r6F] readStream.
		"contains a body of 'Hello', but the contents of the body are arbitrary)"
	self assert: ping = 'Hello'.
	self assert: pong = nil.

	ping := pong := nil.
	ws readFrameFrom:
		#[16r8A 16r05 16r48 16r65 16r6C 16r6C 16r6F] readStream.
		"contains a body of 'Hello', matching the body of the ping)"
	self assert: ping = nil.
	self assert: pong = 'Hello'.
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'DSG 3/28/2015 21:45'!
testWebSockets07ControlInterleave
	"Test the WebSocket 07 control interleave"

	| sema cws resp ping pong |
	ping := 'Hello Ping'.
	server addService: '/websockets' action:[:req | | sws |
		sws := req asWebSocket.
		sws onMessage:[:sdata| sws send: sdata].
		sws onPing:[:sdata| sws pong: sdata].
		sws onError:[:ex| 
			Transcript log: ex description.
			Transcript log: ex signalerContext longStack.
		].
		sws run.
	].

	sema := Semaphore new.
	cws := HTTPClient webSocket07: self localHostUrl,'/websockets' protocol: nil.
	cws onMessage:[:cdata| resp := cdata. sema signal].
	cws onPong:[:cdata| pong := cdata asString. sema signal.].
	cws fork.

	cws send: 'Ãœber-cool'.
	sema wait.
	self assert: (resp = 'Ãœber-cool').

	cws firstFragment: 'First,'.
	cws nextFragment: 'Next,'.
	cws lastFragment: 'Last.'.
	sema wait.
	self assert: (resp = 'First,Next,Last.').
	self deny: sema isSignaled.

	resp := nil.
	cws firstFragment: 'First,'.
	cws ping: ping.
	sema wait.

	self assert: ping = pong. "i.e., roundtrip finished"
	self assert: resp == nil. "i.e., fragment not delivered yet"
	self deny: sema isSignaled.

	cws nextFragment: 'Next,'.
	cws lastFragment: 'Last.'.
	sema wait.
	self assert: (resp = 'First,Next,Last.').


	cws close.! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 11/17/2011 17:23'!
testWebSockets07DataDecode
	"Tests from section 4.7 of the WebSockets spec"

	| data ws |
	ws := WebSocket07 new.
	ws onMessage:[:msg| data := msg].
	"A single-frame unmasked text message"
	data := nil.
	ws readFrameFrom: 
		#[16r81 16r05 16r48 16r65 16r6C 16r6C 16r6F] readStream.
	self assert: data = 'Hello'.

	"A single-frame masked text message"
	data := nil.
	ws readFrameFrom:
		#[16r81 16r85 16r37 16rFA 16r21 16r3D 16r7F 16r9F 16r4D 16r51 16r58] readStream.
	self assert: data = 'Hello'.

	"A fragmented unmasked text message"
	data := nil.	
	ws readFrameFrom: 
		#[16r01 16r03 16r48 16r65 16r6C] readStream. "contains 'Hel'"
	self assert: data = nil.
	ws readFrameFrom:
		#[16r80 16r02 16r6C 16r6F] readStream. "contains 'lo'"
	self assert: data = 'Hello'.
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/27/2011 13:48'!
testWebSockets07NoMask
	"Test the WebSocket interface"

	self testWebSockets:[
		(HTTPClient webSocket07: self localHostUrl,'/websockets' protocol: 'sample')
			masking: false;
			yourself].
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets68
	"Test the WebSocket interface"

	self testWebSockets:[
		HTTPClient webSocket68: self localHostUrl,'/websockets' protocol: 'sample'.
	].
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'DSG 3/28/2015 21:36'!
testWebSockets: aBlock
	"Test the WebSocket interface"

	| sema cws resp sws |
	server addService: '/websockets' action:[:req |
		sws := req asWebSocket.
		sws onError:[:ex| 
			Transcript log: ex description.
			Transcript log: ex signalerContext longStack.
		].
		sws onMessage:[:sdata| sws send: 'Response: ', sdata].
		sws run.
	].

	sema := Semaphore new.
	cws := aBlock value.
	cws onMessage:[:cdata| resp := cdata. sema signal].
	cws onClose:[resp := nil. sema signal].
	cws fork.

	cws send: 'Testing, one, two'.
	sema wait.
	self assert: (resp = 'Response: Testing, one, two').

	cws close.
	sema wait.
	self assert: (resp = nil).
! !

!HTTPClientServerTest methodsFor: 'tests - websockets' stamp: 'jmv 5/19/2015 22:31'!
testWebSocketsFraming
	"Test the WebSocket 00 framing"

	| sema cws resp frameType |
	server addService: '/websockets' action:[:req | | sws |
		sws := req asWebSocket.
		sws onMessage:[:sdata :type| sws send: sdata type: type].
		sws onError:[:ex| 
			Transcript log: ex description.
			Transcript log: ex signalerContext longStack.
		].
		sws run.
	].

	sema := Semaphore new.
	cws := HTTPClient webSocket00: self localHostUrl,'/websockets' protocol: nil.
	cws onMessage:[:cdata :type| resp := cdata. frameType := type. sema signal].
	cws onClose:[resp := nil. frameType := 255. sema signal].
	cws fork.

	cws send: 'Über-cool'.
	sema wait.
	self assert: (resp = 'Über-cool').
	self assert: (frameType = 0).

	cws send: 'Über-funny' type: 15.
	sema wait.
	self assert: (resp = 'Über-funny').
	self assert: (frameType = 15).

	cws send: (String new: 100) type: 0.
	sema wait.
	self assert: (resp = (String new: 100)).
	self assert: (frameType = 0).

	cws send: (ByteArray new: 100) type: 130.
	sema wait.
	self assert: (resp = (ByteArray new: 100)).
	self assert: (frameType = 130).

	cws close.
	sema wait.
	self assert: (resp = nil).
	self assert: (frameType = 255).! !

!HTTPClient methodsFor: 'cookies' stamp: 'ar 8/12/2010 21:40'!
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

!HTTPClient methodsFor: 'cookies' stamp: 'ar 4/1/2010 18:04'!
acceptCookies
	"Whether we should accept cookies"

	^flags anyMask: FlagAcceptCookies! !

!HTTPClient methodsFor: 'cookies' stamp: 'ar 4/1/2010 18:05'!
acceptCookies: aBool
	"Whether we should accept cookies"

	flags := aBool ifTrue:[flags bitOr: FlagAcceptCookies] ifFalse:[flags bitClear: FlagAcceptCookies].
! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:49'!
accessLog
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	^accessLog! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:49'!
accessLog: aStreamOrFilename
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	accessLog := aStreamOrFilename! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:03'!
allowAuth
	"If true, HTTPClient will attempt to handle authorization requests"

	^flags anyMask: FlagAllowAuth! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:02'!
allowAuth: aBool
	"If true, HTTPClient will attempt to handle authorization requests"

	flags := aBool ifTrue:[flags bitOr: FlagAllowAuth] ifFalse:[flags bitClear: FlagAllowAuth].
! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:01'!
allowRedirect
	"If true, HTTPClient will attempt to handle redirect responses"

	^flags anyMask: FlagAllowRedirect! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 4/1/2010 18:02'!
allowRedirect: aBool
	"If true, HTTPClient will attempt to handle redirect responses"

	flags := aBool ifTrue:[flags bitOr: FlagAllowRedirect] ifFalse:[flags bitClear: FlagAllowRedirect].
! !

!HTTPClient methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:45'!
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

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:13'!
authParams
	"The authentication parameters"

	^authParams! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/10/2012 13:13'!
authParams: aDictionary
	"The authentication parameters"

	authParams := aDictionary! !

!HTTPClient methodsFor: 'sending' stamp: 'ar 2/10/2012 13:12'!
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

	"Signal HTTPAuthRequired"
	(HTTPAuthRequired client: self request: request response: response)
		signal == true ifFalse:[^false].

	"And retry with the new credentials"
	^self authenticate: request from: response! !

!HTTPClient methodsFor: 'authentication' stamp: 'jmv 9/19/2016 21:02:13'!
basicAuth: request from: response header: header params: params
	"Provide basic authentication for the request"

	| user pass args |
	authParams at: #authMethod ifPresent:[:method| ^self]. "do not retry repeatedly"
	authParams at: #authMethod put: 'Basic'.

	args := HTTPUtils parseAuthParams: header.
	args at: 'realm' ifPresent:[:realm| authParams at: #authRealm put: realm].
	
	user := (params at: #username ifAbsent:[nil]) ifNil:[^self].
	pass := (params at: #password ifAbsent:[nil]) ifNil:[^self].
	authParams at: #authResponse put: (user, ':', pass value) asByteArray base64Encoded.
! !

!HTTPClient methodsFor: 'initialize' stamp: 'ar 2/20/2010 11:06'!
close
	"Close the client's stream"

	stream ifNotNil:[
		stream close.
		stream := nil.
	].
! !

!HTTPClient methodsFor: 'initialize' stamp: 'dsg 3/10/2015 16:32'!
connect
	"Connect the client to a web server. Returns nil if successful,
	a HTTPResponse if a proxy CONNECT request fails."

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

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/23/2010 10:31'!
cookies
	"The cookies that have been set for this session"

	^cookies! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/23/2010 10:31'!
cookies: aCollection
	"The cookies that have been set for this session"

	cookies := aCollection.! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:16'!
debugLog
	"If provided, HTTPClient will print all requests and responses to the debugLog,
	except from the content of the message. The debug log must be a stream of
	some sort; file names (like for the accessLog are not supported)."

	^debugLog! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:17'!
debugLog: aStream
	"If provided, HTTPClient will print all requests and responses to the debugLog,
	except from the content of the message. The debug log must be a stream of
	some sort; file names (like for the accessLog are not supported)."

	debugLog := aStream! !

!HTTPClient methodsFor: 'initialize' stamp: 'ar 7/21/2010 21:12'!
defaultPort
	"Return the default port to use if no port was specified.
	Since we only support https and http, default to port 80 unless https."

	^scheme = 'https' 
		ifTrue:[443]
		ifFalse:[80]! !

!HTTPClient methodsFor: 'initialize' stamp: 'DSG 5/13/2015 16:52'!
defaultUserAgent
	"Answer the default User-Agent string to use for HTTPClient"

	^'HTTPClient/1.5 (', 
		SystemVersion current version,'-', 
		SystemVersion current highestUpdate printString, '; ', 
		HTTPUtils platformName,')'.
! !

!HTTPClient methodsFor: 'initialize' stamp: 'ar 2/23/2010 06:20'!
destroy
	"Destroys the client"

	stream ifNotNil:[
		stream socket ifNotNil:[:s| s destroy].
		stream := nil.
	].
! !

!HTTPClient methodsFor: 'initialize' stamp: 'ar 3/27/2010 11:59'!
detectProxyServer
	"Automatically detect the proxy server to use"

	proxyServer := ProxyHandler proxyServerFor: self server.
! !

!HTTPClient methodsFor: 'authentication' stamp: 'ar 8/26/2010 07:48'!
digestAuth: request from: response header: authHeader params: params
	"Perform digest authentication"

	| realm nonce ha1 ha2 nc cnonce qop header uri md5 key args user pass |
	params at: #authMethod ifPresent:[:method| ^self]. "do not retry repeatedly"
	params at: #authMethod put: 'Digest'.

	args := HTTPUtils parseAuthParams: authHeader.
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
	ha1 := HTTPUtils md5Digest: key.
	ha2 := HTTPUtils md5Digest: request method, ':', uri.
	cnonce := UUID new hex.

	(args includesKey: 'qop') ifTrue:["use qop"
		qop := 'auth'. "the only qop we support"
		md5 := HTTPUtils md5Digest: ha1, ':', nonce, ':', nc, ':', cnonce, ':', qop, ':', ha2.
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
		md5 := HTTPUtils md5Digest: ha1, ':', nonce, ':', ha2.
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

!HTTPClient methodsFor: 'initialize' stamp: 'ar 4/1/2010 18:51'!
flushAuthState
	"Flush all the auth state"

	self flushAuthState: authParams.
	self flushAuthState: proxyParams.! !

!HTTPClient methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:46'!
flushAuthState: params
	"Fliush authentication state that should not been preserved 
	inbetween failed attempts to authenticate. 
	Subclasses can extend this method to support more auth methods."

	params removeKey: #authMethod ifAbsent:[].
! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:04'!
httpDelete: urlString
	"Sends an DELETE request"

	^self httpDelete: urlString do:[:req]
! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:06'!
httpDelete: urlString do: aBlock
	"Sends a DELETE request"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'DELETE'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request.! !

!HTTPClient methodsFor: 'methods' stamp: 'DSG 3/29/2015 17:32'!
httpGet: urlString
	"GET the response from the given url"
	"(HTTPClient httpGet: 'http://www.cuis-smalltalk.org/') content"

	^self httpGet: urlString do:[:req]
! !

!HTTPClient methodsFor: 'methods' stamp: 'DSG 3/29/2015 17:33'!
httpGet: urlString do: aBlock
	"GET the response from the given url"
	"(HTTPClient httpGet: 'http://www.cuis-smalltalk.org/') content"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'GET'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request
! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:05'!
httpHead: urlString
	"Sends a HEAD request"

	^self httpHead: urlString do:[:req]
! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:06'!
httpHead: urlString do: aBlock
	"Sends a HEAD request"

	| request |
	self initializeFromUrl: urlString.
	request := self requestWithUrl: urlString.
	request method: 'HEAD'.
	userAgent ifNotNil:[request headerAt: 'User-Agent' put: userAgent].
	aBlock value: request.
	^self sendRequest: request.! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:05'!
httpOptions: urlString
	"Sends an OPTIONS request"

	^self httpOptions: urlString do:[:req]
! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:06'!
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

!HTTPClient methodsFor: 'methods' stamp: 'ar 2/23/2010 08:56'!
httpPost: urlString content: postData type: contentType
	"POST the data to the given url"

	^self httpPost: urlString content: postData type: contentType do:[:req]! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:07'!
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

!HTTPClient methodsFor: 'methods' stamp: 'ar 8/31/2010 22:54'!
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

!HTTPClient methodsFor: 'methods' stamp: 'DSG 3/28/2015 17:58'!
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

!HTTPClient methodsFor: 'methods' stamp: 'ar 2/23/2010 08:57'!
httpPut: urlString content: postData type: contentType
	"PUT the data to the given url"

	^self httpPut: urlString content: postData type: contentType do:[:req]! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:07'!
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

!HTTPClient methodsFor: 'methods' stamp: 'ar 5/11/2010 20:05'!
httpTrace: urlString
	"Sends a TRACE request"

	^self httpTrace: urlString do:[:req]
! !

!HTTPClient methodsFor: 'methods' stamp: 'ar 9/4/2010 11:08'!
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

!HTTPClient methodsFor: 'initialize' stamp: 'ar 2/10/2012 13:33'!
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

!HTTPClient methodsFor: 'initialize' stamp: 'DSG 3/29/2015 09:00'!
initializeFromUrl: urlString
	"Initialize the client from a url string"

	| urlStream serverAndPort userAndPass |
	urlStream := urlString readStream.
	"scheme := (urlStream upToAll: '://') asLowercase."
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
	"self detectProxyServer".! !

!HTTPClient methodsFor: 'testing' stamp: 'ar 3/30/2010 19:18'!
isConnected
	"Returns true if the client is still connected"

	^stream notNil and:[stream isConnected]! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:12'!
maxRedirect
	"The max. number of redirects we allow for the SAME SITE before giving up.
	Redirects can happen for any number of reasons between any number of sites
	and we give up if we have seen the SAME SITE maxRedirect number of times."

	^maxRedirect! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:12'!
maxRedirect: aNumber
	"The max. number of redirects we allow for the SAME SITE before giving up.
	Redirects can happen for any number of reasons between any number of sites
	and we give up if we have seen the SAME SITE maxRedirect number of times."

	maxRedirect := aNumber! !

!HTTPClient methodsFor: 'initialize' stamp: 'ar 8/9/2010 08:30'!
newRequest
	"Create an return a new HTTPRequest.
	Subclasses can use this method to override the default request class."

	^HTTPRequest new! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 3/27/2010 12:01'!
password
	"The password for remote authentication.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	^authParams at: #password ifAbsent:[nil]! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 3/27/2010 01:32'!
password: aStringOrValuable
	"The password for remote authentication.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	^authParams at: #password put: aStringOrValuable! !

!HTTPClient methodsFor: 'printing' stamp: 'ar 8/9/2010 08:50'!
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

!HTTPClient methodsFor: 'initialize' stamp: 'ar 8/12/2010 21:12'!
proxyConnect
	"Send a proxy CONNECT request to connect to a remote host via the chosen proxy server"

	| uri request repeatAuth response |
	uri := self serverName,':', self serverPort.
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

!HTTPClient methodsFor: 'proxy' stamp: 'ar 2/10/2012 13:12'!
proxyParams
	"The proxy authentication parameters"

	^proxyParams! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 2/10/2012 13:12'!
proxyParams: aDictionary
	"The proxy authentication parameters"

	proxyParams := aDictionary! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:04'!
proxyPass
	"The password for an authenticating proxy.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	^proxyParams at: #password! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:04'!
proxyPass: aStringOrValuable
	"The password for an authenticating proxy.
	The password should generally not be a plain-text
	version but rather a block that can retrieve the password
	when required."

	proxyParams at: #password put: aStringOrValuable! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 2/20/2010 19:17'!
proxyServer
	"The proxy server to use for connections.
	The server should be specified as server:port if the proxy
	port to be used is different from the desitation port."

	^proxyServer! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 2/20/2010 19:17'!
proxyServer: aString
	"The proxy server to use for connections.
	The server should be specified as server:port if the proxy
	port to be used is different from the desitation port."

	proxyServer := aString! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 2/20/2010 19:22'!
proxyServerName
	"Returns the name part of the server:port description"

	^proxyServer copyUpTo: $:! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 2/20/2010 19:21'!
proxyServerPort
	"Returns the port of the proxyServer:port description"

	^(proxyServer copyAfter: $:) 
		ifEmpty:[self serverPort]
		ifNotEmpty:[:portString| portString asInteger].
! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:02'!
proxyUser
	"The user name for an authenticating proxy"

	^proxyParams at: #username ifAbsent:[nil]! !

!HTTPClient methodsFor: 'proxy' stamp: 'ar 3/24/2010 16:02'!
proxyUser: aString
	"The user name for an authenticating proxy"

	proxyParams at: #username put: aString! !

!HTTPClient methodsFor: 'sending' stamp: 'ar 9/4/2010 11:10'!
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

!HTTPClient methodsFor: 'initialize' stamp: 'ar 9/4/2010 11:10'!
requestWithUrl: urlString
	"Create and return a new HTTPRequest initialized with the given url."

	^self  newRequest initializeFromUrl: urlString! !

!HTTPClient methodsFor: 'initialize' stamp: 'dsg 3/10/2015 16:34'!
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

!HTTPClient methodsFor: 'accessing' stamp: 'ar 8/5/2010 19:50'!
scheme
	"The scheme used for the request (usually http or https)"

	^scheme! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 8/5/2010 19:50'!
scheme: aString
	"The scheme used for the request (usually http or https)"

	scheme := aString! !

!HTTPClient methodsFor: 'sending' stamp: 'ar 2/25/2010 22:16'!
sendRequest: request
	"Send an http request"

	^self sendRequest: request content: nil size: 0! !

!HTTPClient methodsFor: 'sending' stamp: 'DSG 5/25/2012 23:35'!
sendRequest: request content: contentStream size: streamSize
	"Send an http request"

	^self sendRequest: request contentBlock:[:aStream|
		contentStream ifNotNil:[
			"Upload content if provided"
			contentStream position: 0.
			request streamFrom: contentStream to: aStream size: streamSize 
				progress:[:total :amount|
					(ProgressBarMorph new) 
						value: amount / total.
						]]].
! !

!HTTPClient methodsFor: 'sending' stamp: 'ar 9/16/2010 21:52'!
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
			HTTPUtils logRequest: request response: response on: accessLog
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

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/21/2010 09:50'!
server
	"The server to use for connections.
	The server is specified as server:port if needed"

	^server! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/21/2010 09:50'!
server: aString
	"The server to use for connections.
	The server should be specified as server:port if needed"

	server := aString
! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/20/2010 19:22'!
serverName
	"Returns the name part of the server:port description"

	^server copyUpTo: $:! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/20/2010 19:21'!
serverPort
	"Returns the port of the server:port description"

	^(server copyAfter: $:) 
		ifEmpty:[self defaultPort]
		ifNotEmpty:[:portString| portString asInteger].
! !

!HTTPClient methodsFor: 'initialize' stamp: 'DSG 3/28/2015 15:50'!
sslConnect
	"Connect the client to a web server"

	| sqSSL |
	proxyServer ifNotNil:[ | resp |
		"If we have a proxy server, do the proxy connect"
		resp := self proxyConnect.
		resp isSuccess ifFalse:[^resp].
	].

	sqSSL := Smalltalk at: #TLSSocket ifAbsent:[self error: 'SecureSocket is missing'].
	"Convert the stream to a secure stream"
	stream := sqSSL secureSocketStream on: stream socket.
	stream timeout: timeout.
	"Do the SSL handshake"
	stream sslConnectTo: lastServer.
	"And cert verification (unless on OSX, where this does not work yet)"
	"HTTPUtils platformName = 'Mac OS'
		ifFalse: [stream verifyCert: self serverName]."
	"And cert verification"
	stream verifyCert: self serverName.
	^nil "indicating success"
! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:12'!
timeout
	"Timeout for the http operations"

	^timeout! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:12'!
timeout: aNumber
	"Timeout for the http operations"

	timeout := aNumber! !

!HTTPClient methodsFor: 'accessing' stamp: 'DSG 12/4/2013 22:34'!
userAgent
	"The User-Agent string sent to the server"

	userAgent ifNil: [ userAgent := self defaultUserAgent ].
	^userAgent! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:17'!
userAgent: aString
	"The User-Agent string sent to the server. 
	If no user agent should be sent, this value can be set to nil"

	userAgent := aString! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 3/24/2010 16:05'!
username
	"The username for remote authentication"

	^authParams at: #username ifAbsent:[nil]! !

!HTTPClient methodsFor: 'accessing' stamp: 'ar 3/24/2010 16:05'!
username: aString
	"The username for remote authentication"

	authParams at: #username put: aString! !

!HTTPClient methodsFor: 'sending' stamp: 'DSG 3/28/2015 16:37'!
writeCookiesFor: request on: stream
	"Write the cookies for a particular request"

	(cookies select:[:ck| (request acceptsCookie: ck from: self )]) ifNotEmpty:[:ckset|
		stream nextPutAll: 'Cookie: '.
		ckset 
			do:[:ck| stream nextPutAll: ck name,'=', ck value]
			separatedBy:[stream  nextPutAll:'; '].
		stream crlf.
	].
! !

!HTTPClient methodsFor: 'sending' stamp: 'DSG 5/24/2012 17:24'!
writeHeadersFor: request on: aStream
	"Write all the headers for the given request on aStream"

	request headersDo:[:key :value| aStream nextPutAll: key, ': ', value asString; nextPutAll: String crlfString].
! !

!HTTPClient methodsFor: 'sending' stamp: 'dsg 3/10/2015 16:40'!
writeRequest: request on: aStream
	"Write all the request headers on the given stream"

	self retryOnce: [
		self writeRequestLine: request on: aStream.
		self writeHeadersFor: request on: aStream.
		self writeCookiesFor: request on: aStream.
		aStream crlf.
		aStream flush].
! !

!HTTPClient methodsFor: 'sending' stamp: 'DSG 5/24/2012 17:23'!
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

!HTTPClient class methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:20'!
debugLog
	"The default debugLog, if any:
		HTTPClient debugLog: Transcript.
		HTTPClient debugLog: nil.
	"

	^DebugLog! !

!HTTPClient class methodsFor: 'accessing' stamp: 'ar 8/12/2010 21:20'!
debugLog: aStream
	"The default debugLog, if any:
		HTTPClient debugLog: Transcript.
		HTTPClient debugLog: nil.
	"

	DebugLog := aStream.! !

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:30'!
httpDelete: urlString
	"Sends a DELETE request"
	"HTTPClient httpDelete: 'http://ftp.squeak.org/trunk'"

	^self httpDo:[:client| client httpDelete: urlString].
! !

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:29'!
httpDo: aBlock
	"Simplified wrapper for running various methods"

	| client response |
	client := self new.
	response := HTTPUtils handleAuth: [aBlock value: client].
	response content; close.
	^response! !

!HTTPClient class methodsFor: 'methods' stamp: 'DSG 3/29/2015 17:31'!
httpGet: urlString
	"Get the response from the given url"
	"(HTTPClient httpGet: 'http://www.cuis-smalltalk.org/') content"

	^self httpDo:[:client| client httpGet: urlString].! !

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
httpHead: urlString
	"Sends a HEAD request"
	"HTTPClient httpHead: 'http://ftp.squeak.org/trunk'"

	^self httpDo:[:client| client httpHead: urlString].
! !

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
httpOptions: urlString
	"Sends an OPTIONS request"
	"HTTPClient httpOptions: 'http://ftp.squeak.org/*'"
	"HTTPClient httpOptions: 'http://ftp.squeak.org/4.1/Squeak4.1.zip'"

	^self httpDo:[:client| client httpOptions: urlString]
! !

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
httpPost: urlString content: postData type: contentType
	"Fire off an HTTP post request"

	^self httpDo:[:client| client httpPost: urlString content: postData type: contentType]
! !

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:31'!
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

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:32'!
httpPut: urlString content: postData type: contentType
	"Fire off an HTTP PUT request"

	^self httpDo:[:client| client httpPut: urlString content: postData type: contentType]! !

!HTTPClient class methodsFor: 'methods' stamp: 'ar 2/10/2012 13:32'!
httpTrace: urlString
	"Sends a TRACE request"
	"(HTTPClient httpTrace: 'http://lists.squeakfoundation.org') content"
	"(HTTPClient httpTrace: 'http://ftp.squeak.org/trunk') content"

	^self httpDo:[:client| client httpTrace: urlString]! !

!HTTPClient class methodsFor: 'class initialization' stamp: 'DSG 12/4/2013 22:19'!
initialize
	"HTTPClient initialize"

	FlagAcceptCookies := 1.
	FlagAllowAuth := 2.
	FlagAllowRedirect := 4.

	"Default proxy handler"
	ProxyHandler := HTTPUtils.
! !

!HTTPClient class methodsFor: 'accessing' stamp: 'ar 2/23/2010 22:26'!
proxyHandler
	"The currently registered proxy handler"

	^ProxyHandler! !

!HTTPClient class methodsFor: 'accessing' stamp: 'ar 2/23/2010 22:26'!
proxyHandler: anObject
	"The currently registered proxy handler"

	ProxyHandler := anObject! !

!HTTPClient class methodsFor: 'benchmark' stamp: 'jmv 5/19/2015 21:51'!
requestBenchmark: url persistent: aBool
	"Run a HTTPClient benchmark to measure request handling speed.
	Try using a persistent connection if requested.

	1) Launch HTTPServer  (or Seaside, or whatever):
	
		(HTTPServer reset default)
			listenOn: 8888;
			addService: '/' action:[:r| r send200Response: ''].

	2) Run the benchmark:

		MessageTally spyOn:[
			HTTPClient requestBenchmark: 'http://localhost:8888/' persistent: true.
		]
	"

	| time resp startTime endTime count client |
	count := 0.
	client := HTTPClient new.
	startTime := Time localMillisecondClock.
	[endTime := Time localMillisecondClock.
	(time := endTime - startTime) < 5000] whileTrue:[
		resp := client httpGet: url.
		resp isSuccess ifFalse:[self error: resp status].
		resp content.
		aBool ifFalse:[resp close].
		count := count + 1.
	].
	^(count  * 1000 // time) printStringWithCommas, ' requests/sec'
! !

!HTTPClient class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:39'!
webSocket00: serverUrl protocol: protocol
	"Attempt to connect to the given server url using the WebSocket api"

	| key1 key2 bytes hash client req resp |
	"Fixed keys from spec"
	key1 := 155712099.
	key2 := 173347027.
	bytes := ByteArray new: 8.
	(1 to: bytes size) do:[:i| bytes at: i put: (256 atRandom - 1)].
	hash := HTTPUtils webSocketHandshake: key1 with: key2 with: bytes.

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

!HTTPClient class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:39'!
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
	hash := HTTPUtils webSocketHash07: nonce.
	hash = (resp headerAt: 'Sec-WebSocket-Accept') ifFalse:[client close. ^nil].

	"Close of stream is left to caller"
	^WebSocket07 on: resp stream.
! !

!HTTPClient class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:39'!
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

!HTTPClient class methodsFor: 'websockets' stamp: 'DSG 3/28/2015 21:43'!
webSocketTo: serverUrl
	"Attempt to connect to the given server url using the WebSocket api"

	"Testing:

		1) Launch WebSocket example.
		2) Run the following code:
		
		| ws |
		ws := HTTPClient webSocketTo: 'http://localhost:8080/broadcast'.
		ws onMessage:[:data| Transcript log: data].
		ws fork.
		ws send: 'This is the first message'.
		ws send: 'This is the last message'.
		ws close.

	"

	^self webSocketTo: serverUrl protocol: nil! !

!HTTPClient class methodsFor: 'websockets' stamp: 'ar 7/9/2010 00:31'!
webSocketTo: serverUrl protocol: protocol
	"Attempt to connect to the given server url using the WebSocket api"

	^self webSocket00: serverUrl protocol: protocol! !

!HTTPCookie methodsFor: 'comparing' stamp: 'ar 5/11/2010 08:58'!
= aCookie
	"Compare the receiver with aCookie"

	^aCookie class == self class 
		and:[aCookie name = self name
		and:[aCookie domain = self domain
		and:[aCookie path = self path]]].! !

!HTTPCookie methodsFor: 'converting' stamp: 'ar 2/20/2010 11:06'!
asString
	"Create the string representation for this cookie"

	^String streamContents:[:s| self printOn: s].! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 8/5/2010 18:22'!
comment
	"Cookie comment"

	^comment! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 8/5/2010 18:22'!
comment: aString
	"Cookie comment"

	comment := aString! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
domain
	"Cookie domain"

	^domain! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
domain: aString
	"Cookie domain"

	domain := aString.! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:06'!
expiry
	"The expiry DateAndTime"

	^expiry! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:06'!
expiry: aDateAndTime
	"The expiry DateAndTime"

	expiry := aDateAndTime ifNotNil:[:dt| dt asUTC].! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 8/5/2010 18:21'!
expirySeconds: maxAge
	"Compute the cookie's expiry age from the given number of seconds"

	maxAge = 0 ifTrue:[self expiry: DateAndTime new].
	self expiry: DateAndTime now + maxAge seconds.! !

!HTTPCookie methodsFor: 'comparing' stamp: 'ar 5/11/2010 08:59'!
hash
	"Hash is re-implemented because #= is re-implemented"

	^name hash + domain hash + path hash.! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 3/31/2010 08:38'!
httpOnly
	"If the cookie should be httpOnly (not scriptable).
	See http://msdn2.microsoft.com/en-us/library/ms533046.aspx"

	^httpOnly! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 3/31/2010 08:38'!
httpOnly: aBool
	"If the cookie should be httpOnly (not scriptable).
	See http://msdn2.microsoft.com/en-us/library/ms533046.aspx"

	httpOnly := aBool! !

!HTTPCookie methodsFor: 'initialize' stamp: 'ar 3/31/2010 08:40'!
initialize
	"Initializes the receiver"
	super initialize.
	secure := false.
	httpOnly := false.
! !

!HTTPCookie methodsFor: 'testing' stamp: 'ar 5/11/2010 20:06'!
isExpired
	"Return true if this cookie is expired"
	
	expiry ifNil:[^false].
	^expiry asUTC < DateAndTime now asUTC! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
name
	"The name of the cookie"

	^name! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
name: aString
	"The name of the cookie"

	name := aString! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
path
	"Cookie path"

	^path! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
path: aString
	"Cookie path"

	path := aString! !

!HTTPCookie methodsFor: 'printing' stamp: 'DSG 5/24/2012 21:09'!
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
		aStream nextPutAll:'; path=', (HTTPUtils quote: path).
	].
	domain ifNotNil:[
		aStream nextPutAll:'; domain=', (HTTPUtils quote: domain).
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

!HTTPCookie methodsFor: 'initialize' stamp: 'DSG 3/28/2015 16:36'!
readExpiryFrom: aString
	"Read a cookie expiry date from the given string.
	The expiration date is specified in the 'Wdy, DD-Mon-YYYY HH:MM:SS GMT' format."
	
	| tokens |
	^[
		tokens := aString withBlanksTrimmed.
		(tokens copyAfterLast: Character space) = 'GMT'
			ifFalse:[^DateAndTime new].
		tokens := (tokens copyAfter: Character space) copyUpToLast: Character space.
		(DateAndTime readFrom: tokens readStream) utcOffset: Duration zero.
	] on: Error do:[:ex| ex return: DateAndTime new].! !

!HTTPCookie methodsFor: 'initialize' stamp: 'DSG 3/28/2015 21:44'!
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
			['path']		-> [self path: (HTTPUtils unquote: val)].
			['domain'] 	-> [self domain: (HTTPUtils unquote: val)].
			['secure'] 	-> [self secure: true].
			['version'] 	-> [self version: val].
			['httponly'] 	-> [self httpOnly: true].
			['comment']	 ->[self comment: val].
			['max-age']	->[self expirySeconds: val asNumber].
		} otherwise:[Transcript log: 'Unknown cookie field: ', key].
	].! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
secure
	"Secure cookie flag"

	^secure! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
secure: aBool
	"Secure cookie flag"

	secure := aBool! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:05'!
value
	"The value of the cookie"

	^value! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:06'!
value: aString
	"The value of the cookie"

	value := aString! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:06'!
version
	"Cookie version"

	^version! !

!HTTPCookie methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:06'!
version: aString
	"Cookie version"

	version := aString! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 16:31'!
addHeader: key value: value
	"Adds an ADDITIONAL header for the given key"

	headers add: key -> value.! !

!HTTPMessage methodsFor: 'converting' stamp: 'ar 2/23/2010 09:02'!
asString
	"Prints the receiver"

	^String streamContents:[:s| self writeOn: s].! !

!HTTPMessage methodsFor: 'streaming' stamp: 'ar 2/2/2012 15:55'!
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

!HTTPMessage methodsFor: 'initialize' stamp: 'ar 5/4/2010 20:02'!
close
	"Always closes regardless of connection header"

	stream ifNotNil:[stream close].! !

!HTTPMessage methodsFor: 'initialize' stamp: 'ar 5/6/2010 20:47'!
closeIfTransient
	"Close the underlying connection if it's not persistent"

	self isPersistent ifFalse:[self close].
! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 6/18/2010 23:52'!
content
	"Reads and caches available content and returns it."

	^content ifNil:[content := self getContent].
! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 3/30/2010 22:05'!
content: aString
	"Sets content for a response"

	content := aString.! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 2/23/2010 10:37'!
contentLength
	"Answers the numeric value of the Content-Length header, or nil if no header exist"

	^(self headerAt: 'Content-Length' ifAbsent:[^nil]) asInteger! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 5/4/2010 21:04'!
contentLength: aNumber
	"Sets the numeric value of the Content-Length header"

	^self headerAt: 'Content-Length' put: aNumber! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:01'!
contentStream
	"Answers a stream for the conent."

	^stream! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:02'!
contentType
	"Answer the content type for the receiver"

	^self headerAt: 'Content-Type' ifAbsent:['application/octet-stream']
! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 3/30/2010 21:58'!
contentType: aString
	"Sets the content type for the receiver"

	^self headerAt: 'Content-Type' put: aString! !

!HTTPMessage methodsFor: 'accessing' stamp: 'jmv 5/19/2015 23:51'!
contentWithProgress: progressBlock
	"Reads and caches available content and returns it."

	^content ifNil:[	| length |
		length := self contentLength.
		content := (stream isBinary ifTrue:[ByteArray] ifFalse:[String]) 
			streamContents:[:s| self streamFrom: stream to: s size: length progress: progressBlock]
			estimatedSize: (length ifNil:[1000]) ]! !

!HTTPMessage methodsFor: 'initialize' stamp: 'ar 5/4/2010 20:02'!
destroy
	"Destroys the underlying stream"

	stream ifNotNil:[stream destroy].! !

!HTTPMessage methodsFor: 'private' stamp: 'DSG 5/24/2012 21:33'!
getContent
	"Reads available content and returns it."

	| length size buffer |
	length := self contentLength.
	size := length ifNil:[1000].
	buffer := ((stream isBinary ifTrue:[ByteArray] ifFalse:[String]) 
		new: size) writeStream.
	self streamFrom: stream to: buffer size: length progress: nil.
	^buffer position = size
		ifTrue: [ buffer originalContents ]
		ifFalse: [ buffer contents ]! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:02'!
headerAt: key
	"Answers a single value for the given http header. 
	Do not use for headers that may appear multiple times, such at www-authenticate."

	^self headerAt: key ifAbsent:[''].! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headerAt: key ifAbsent: aBlock
	"Answers a single value for the given http header. 
	Do not use for headers that may appear multiple times, such at www-authenticate."

	self headersAt: key do:[:value| ^value].
	^aBlock value! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 12:18'!
headerAt: key put: value
	"Sets the value of a header, adding if necessary"

	headers do:[:assoc|
		(assoc key sameAs: key) ifTrue:[
			assoc value: value.
			^self
		].
	].
	self addHeader: key value: value.! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersAt: key
	"Answers a collection of values for the given http header. 
	Use for headers that may appear multiple times, such at www-authenticate."

	^self headersAt: key ifAbsent:[#()].! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersAt: aKey do: aBlock
	"Evaluate aBlock with all the headers matching aKey"

	self headersDo:[:key :value|
		(aKey sameAs: key) ifTrue:[ aBlock value: value].
	].! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersAt: key ifAbsent: aBlock
	"Answers a collection of values for the given http header. 
	Use for headers that may appear multiple times, such at www-authenticate."

	| hdrs |
	hdrs := OrderedCollection new.
	self headersAt: key do:[:value| hdrs add: value].
	^hdrs asArray ifEmpty:[aBlock value]
! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 2/20/2010 11:03'!
headersDo: aBlock
	"Evaluate aBlock with all of the headers key-value pairs"

	headers do:[:assoc|
		aBlock value: assoc key value: assoc value.
	].! !

!HTTPMessage methodsFor: 'testing' stamp: 'pmm 6/19/2010 08:05'!
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

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 7/20/2010 20:09'!
multipartBoundary
	"Answer the boundary of a multipart/form-data message"

	| header |
	header := self headerAt: 'Content-Type' ifAbsent:[^nil].
	(header beginsWith: 'multipart/form-data') ifFalse:[^nil].
	^(header copyFrom: (header findString: 'boundary=') + 9 to: header size)! !

!HTTPMessage methodsFor: 'streaming' stamp: 'DSG 3/29/2015 09:10'!
nextChunk
	"Answer the next chunk from a message using chunked transfer encoding."

	| chunkSize chunk |
	chunkSize := Integer readFrom: (stream upToAll: String crlfString) readStream base: 16.
	chunkSize = 0 ifFalse:[chunk := stream next: chunkSize].
	stream skip: 2. "CrLf"
	^chunk! !

!HTTPMessage methodsFor: 'streaming' stamp: 'DSG 3/28/2015 17:59'!
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

!HTTPMessage methodsFor: 'printing' stamp: 'ar 2/20/2010 11:05'!
printOn: aStream
	"Prints the receiver"

	aStream nextPutAll: self class name.
	aStream nextPut: $(.
	self writeOn: aStream.
	aStream nextPut: $).! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 16:31'!
protocol
	"Returns the requested protocol"

	^protocol! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 16:31'!
protocol: aString
	"Returns the requested protocol"

	protocol := aString! !

!HTTPMessage methodsFor: 'accessing' stamp: 'dsg 3/10/2015 16:29'!
remoteHost
	"Return the name of the remote host"

	| remote |
	(stream isKindOf: SocketStream) ifFalse:[^nil]. "don't have it"
	remote := stream socket remoteAddress.
	^ NetNameResolver stringFromAddress: remote.! !

!HTTPMessage methodsFor: 'headers' stamp: 'ar 3/26/2010 19:02'!
removeHeader: key
	"Removes ALL headers with the given key"

	headers := headers reject:[:assoc| assoc key sameAs: key].
! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:01'!
stream
	"Answers the underlying TCP stream"

	^stream! !

!HTTPMessage methodsFor: 'accessing' stamp: 'ar 2/20/2010 11:01'!
stream: aStream
	"Sets the underlying TCP stream"

	stream := aStream! !

!HTTPMessage methodsFor: 'streaming' stamp: 'ar 5/4/2010 15:44'!
streamFrom: srcStream size: size progress: aBlock
	"Stream the given source stream to the message's socket stream.
	Outbound. Can be used on both request/response depending on
	whether it is utilized by HTTPClient or HTTPServer."

	^self streamFrom: srcStream to: stream size: size progress: aBlock! !

!HTTPMessage methodsFor: 'streaming' stamp: 'ar 7/10/2010 13:51'!
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

!HTTPMessage methodsFor: 'streaming' stamp: 'ar 5/4/2010 15:46'!
streamTo: dstStream size: size progress: aBlock
	"Stream from the receiver's socket stream to the given destination stream.
	Inbound. Can be used on both request/response depending on
	whether it is utilized by HTTPClient or HTTPServer."
	content ifNil:[
		self streamFrom: stream to: dstStream size: size progress: aBlock
	] ifNotNil:[
		self streamFrom: content readStream to: dstStream size: size progress: aBlock
	].! !

!HTTPMessage methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:38'!
writeHeadersOn: aStream
	"Write the request"

	self headersDo:[:key :value| aStream nextPutAll: key, ': ', value asString; nextPutAll: String crlfString].
! !

!HTTPMessage methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:38'!
writeOn: aStream
	"Write the request"

	self writeHeadersOn: aStream.
	aStream nextPutAll: String crlfString.
! !

!HTTPMessage class methodsFor: 'class initialization' stamp: 'ar 2/21/2010 09:55'!
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

!HTTPMessage class methodsFor: 'utilities' stamp: 'dsg 3/12/2015 10:32'!
statusCodeAt: code
	"Return the textual description of a status code"

	^ StatusCodes
		at: code
		ifAbsent: ['Unknown error']! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 8/5/2010 19:51'!
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

!HTTPRequest methodsFor: 'converting' stamp: 'ar 9/26/2011 10:25'!
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

!HTTPRequest methodsFor: 'converting' stamp: 'ar 9/26/2011 10:39'!
asWebSocket00
	"Convert this request into a WebSocket"

	| field key1 key2 bytes resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 101.
	resp status: 'HTTP/1.1 101 Web Socket Protocol Handshake'.

	"Chrome 4/5 is *VERY* sensitive to the order of the fields"
	resp headerAt: 'Upgrade' put: 'WebSocket'.
	resp headerAt: 'Connection' put: 'Upgrade'.

	field := self headerAt: 'Sec-WebSocket-Key1'.
	key1 := HTTPUtils extractWebSocketKey: field.
	field := self headerAt: 'Sec-WebSocket-Key2'.
	key2 := HTTPUtils extractWebSocketKey: field.
	bytes := HTTPUtils webSocketHandshake: key1 with: key2 with: (stream next: 8).

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

!HTTPRequest methodsFor: 'converting' stamp: 'DSG 12/4/2013 22:46'!
asWebSocket07
	"Convert this request into a WebSocket"

	| resp hash nonce |
	resp := self newResponse protocol: 'HTTP/1.1' code: 101.
	resp status: 'HTTP/1.1 101 Web Socket Protocol Handshake'.

	"Chrome 4/5 is *VERY* sensitive to the order of the fields"
	resp headerAt: 'Upgrade' put: 'WebSocket'.
	resp headerAt: 'Connection' put: 'Upgrade'.

	nonce := self headerAt: 'Sec-WebSocket-Key'.
	hash := HTTPUtils webSocketHash07: nonce.

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
		''A client MUST close a connection if it detects a masked frame.''
		So not using masking is compatible with the 07 protocol and is required by RFC 6455."! !

!HTTPRequest methodsFor: 'converting' stamp: 'ar 9/26/2011 10:40'!
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

!HTTPRequest methodsFor: 'sending' stamp: 'klub 7/24/2011 19:25'!
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

!HTTPRequest methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:34'!
cookieAt: aString
	"Answer the value for a cookie with the given name"

	^self cookieAt: aString ifAbsent:['']! !

!HTTPRequest methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
cookieAt: aString ifAbsent: aBlock
	"Answer the value for a cookie with the given name"

	self cookiesDo:[:name :value| name = aString ifTrue:[^value]].
	^aBlock value! !

!HTTPRequest methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
cookies
	"Answer all the cookies defined in the request"

	| cookies |
	cookies := Dictionary new.
	self cookiesDo:[:name :value| cookies at: name put: value].
	^cookies! !

!HTTPRequest methodsFor: 'cookies' stamp: 'ar 8/5/2010 19:56'!
cookiesDo: aBlock
	"Parse the cookie headers and evaluate aBlock with name / value pairs"

	self headersAt: 'Cookie' do:[:hdr|
		(hdr findTokens: ';,') do:[:token|
			aBlock 
				value: (token copyUpTo: $=) withBlanksTrimmed 
				value: (token copyAfter: $=) withBlanksTrimmed]].! !

!HTTPRequest methodsFor: 'fields' stamp: 'ar 7/20/2010 20:08'!
fields
	"Answer the fields for the given request."

	| fields |
	fields := Dictionary new.
	self getFields associationsDo:[:a| fields add: a].
	self postFields associationsDo:[:a| fields add: a].
	self multipartFields associationsDo:[:a| fields add: a].
	^fields
! !

!HTTPRequest methodsFor: 'fields' stamp: 'ar 2/21/2010 09:55'!
getFields
	"Decodes the fields embedded in the url ?var1=val1&var2=val2"

	| args |
	args := (rawUrl copyAfter: $?) ifEmpty:[^Dictionary new].
	^HTTPUtils decodeUrlEncodedForm: args multipleValues: false
! !

!HTTPRequest methodsFor: 'initialize' stamp: 'ar 2/23/2010 09:02'!
initialize
	"Initialize the receiver"

	super initialize.
	method := 'GET'.
	protocol := 'HTTP/1.1'.
	headers := OrderedCollection new.! !

!HTTPRequest methodsFor: 'initialize' stamp: 'ar 9/4/2010 11:09'!
initializeFromUrl: urlString
	"Initialize the client from a http url or string"

	| urlStream |
	urlStream := urlString readStream.
	"Skip past scheme if present"
	urlStream upToAll: '://'.
	urlStream atEnd 
		ifTrue:[rawUrl := urlString]
		ifFalse:[rawUrl := urlStream upTo: $/; upToEnd].
	(rawUrl beginsWith: '/') ifFalse:[rawUrl := '/', rawUrl].
! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:18'!
isDeleteRequest
	"Returns true if this is an HTTP DELETE request"

	^self method = 'DELETE'! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 2/20/2010 10:40'!
isGetRequest
	"Returns true if this is an HTTP GET request"

	^self method = 'GET'! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 2/20/2010 10:41'!
isHeadRequest
	"Returns true if this is an HTTP HEAD request"

	^self method = 'HEAD'! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:19'!
isOptionsRequest
	"Returns true if this is an HTTP OPTIONS request"

	^self method = 'OPTIONS'! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 2/20/2010 10:41'!
isPostRequest
	"Returns true if this is an HTTP POST request"

	^self method = 'POST'! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:18'!
isPutRequest
	"Returns true if this is an HTTP PUT request"

	^self method = 'PUT'! !

!HTTPRequest methodsFor: 'testing' stamp: 'ar 6/23/2010 15:18'!
isTraceRequest
	"Returns true if this is an HTTP TRACE request"

	^self method = 'TRACE'! !

!HTTPRequest methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:40'!
method
	"The request method (GET, POST etc)"

	^method! !

!HTTPRequest methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:40'!
method: aString
	"The request method (GET, POST etc)"

	method := aString! !

!HTTPRequest methodsFor: 'fields' stamp: 'ar 7/28/2010 21:01'!
multipartFields
	"Answers the multipart fields of a post request"

	| fields |
	fields := Dictionary new.
	self multipartFieldsDo:[:hdrs :params :cntnt|
		params at: 'name' ifPresent:[:fieldName| fields at: fieldName put: cntnt].
	].
	^fields! !

!HTTPRequest methodsFor: 'fields' stamp: 'ar 7/20/2010 20:08'!
multipartFieldsDo: aBlock
	"Evaluate aBlock with the multipart fields of a post request.
	The block is expected to take three arguments:
		* The headers from the part being processed
		* The parameters from the content-disposition header
		* The content from the part being processed
	"

	^(self isPostRequest and: [
		self contentType beginsWith: 'multipart/form-data'])
			ifTrue: [HTTPUtils decodeMultipartForm: self content readStream boundary: self multipartBoundary do: aBlock]
			ifFalse: [Dictionary new]! !

!HTTPRequest methodsFor: 'initialize' stamp: 'ar 4/1/2010 16:03'!
newResponse
	"Create a new response entitiy.
	Subclasses can override this method to provide a different response class"

	^(HTTPResponse new)
		request: self;
		yourself! !

!HTTPRequest methodsFor: 'initialize' stamp: 'ar 7/8/2010 19:11'!
newWebSocketOn: aStream
	"Create a new socket entitiy.
	Subclasses can override this method to provide a different response class"

	^WebSocket on: aStream! !

!HTTPRequest methodsFor: 'fields' stamp: 'klub 6/22/2011 16:41'!
postFields
	"Answers the multipart fields of a post request"

	^(self isPostRequest and: [
		self contentType beginsWith: 'application/x-www-form-urlencoded'])
			ifTrue: [HTTPUtils decodeUrlEncodedForm: self content multipleValues: true]
			ifFalse: [Dictionary new]! !

!HTTPRequest methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:40'!
rawUrl
	"The raw (unparsed) url"

	^rawUrl! !

!HTTPRequest methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:07'!
rawUrl: aString
	"The raw (unparsed) url"

	rawUrl := aString
! !

!HTTPRequest methodsFor: 'initialize' stamp: 'DSG 5/29/2012 16:47'!
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
	headers := HTTPUtils readHeadersFrom: stream.
! !

!HTTPRequest methodsFor: 'accessing' stamp: 'ar 7/28/2010 19:49'!
requestLine
	"The actual request line as sent by the client"

	^request ifNil:[self method, ' ', self rawUrl, ' ', self protocol]! !

!HTTPRequest methodsFor: 'responses' stamp: 'DSG 12/5/2013 10:57'!
send200Response: aString
	"Send a 200 OK response"

	^self 
		send200Response: (aString asUtf8) 
		contentType: 'text/plain; charset=utf-8'.! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/20/2010 15:42'!
send200Response: aString contentType: contentType
	"Send a 200 OK response"

	^self send200Response: aString contentType: contentType do:[:resp]! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 16:32'!
send200Response: aString contentType: contentType do: aBlock
	"Send a 200 OK response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 200.
	resp headerAt: 'Content-Type' put: contentType.
	aBlock value: resp.
	^self sendResponse: resp content: aString.! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:26'!
send301Response: location
	"Send a 301 permanent redirect response"

	^self send301Response: location do:[:resp].! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:27'!
send301Response: location do: aBlock
	"Send a 301 permanent redirect response"

	^self send3xxResponse: location code: 301 do: aBlock! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:26'!
send302Response: location
	"Send a 302 temporary redirect response"

	^self send302Response: location do:[:resp].! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:26'!
send302Response: location do: aBlock
	"Send a 302 temporary redirect response"

	^self send3xxResponse: location code: 302 do: aBlock! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/20/2010 15:42'!
send3xxResponse: location code: statusCode
	"Send a 3xx redirect response"

	^self send3xxResponse: location code: statusCode do:[:resp].! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 7/23/2010 09:33'!
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

!HTTPRequest methodsFor: 'responses' stamp: 'DSG 3/28/2015 21:13'!
send400Response
	"Send a 400 bad request response"

	| message |
	message := '<html><head><title>400 Bad Request</title></head><body>',
		'<h1>Bad Request</h1>
		<p>The server encountered a request it did not understand.
		</body></html>'.
	
	self sendResponseCode: 400 
		content: message asUtf8
		type: 'text/plain; charset=utf-8'
		close: true! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/20/2010 15:43'!
send404Response
	"Send a 404 not found response"

	^self send404Response: '<html><head><title>404 Not Found</title></head><body><h1>404 Not Found</h1><p>The requested URL ',self rawUrl,' was not found on this server.</p></body></html>'.
! !

!HTTPRequest methodsFor: 'responses' stamp: 'DSG 12/5/2013 10:57'!
send404Response: body
	"Send a 404 not found response"

	^self 
		send404Response: (body asUtf8) 
		do: [ :resp | resp headerAt: 'Content-Type' put: 'text/html; charset=utf-8' ]! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 16:32'!
send404Response: body do: aBlock
	"Send a 404 not found response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 404.
	resp headerAt: 'Content-Type' put: 'text/html; charset=utf-8'.
	aBlock value: resp.
	^self sendResponse: resp content: body.
! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 20:08'!
send405Response: allowed
	"Send a 405 method not allowed response"

	^self send405Response: allowed content: '<html><head><title>405 Method Not Allowed</title></head><body><h1>405 Method Not Allowed</h1><p>The requested method ',self method,' is not allowed for the URL', self rawUrl, '</p></body></html>'.! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 20:08'!
send405Response: allowed content: body
	"Send a 405 method not allowed response"
	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 405.
	resp headerAt: 'Content-Type' put: 'text/html; charset=utf-8'.
	resp headerAt: 'allow' put: (String streamContents:[:s|
		allowed do:[:m| s nextPutAll: m] separatedBy:[s nextPut: $,]
	]).
	^self sendResponse: resp content: body.! !

!HTTPRequest methodsFor: 'responses' stamp: 'jmv 5/19/2015 22:03'!
send500Response: errReport
	"Send a 500 internal server error response"

	| errLog |
	errLog := '<html><head><title>500 Internal Server Error</title></head><body>',
		'<h1>Internal Server Error</h1><pre>',
		(errReport  withLineEndings: String crlfString),
		'</pre></body></html>'.

	self sendResponseCode: 500
				content: errLog asUtf8
				type: 'text/html; charset=utf-8'
				close: true.
! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 20:08'!
sendOptionsResponse: allowed
	"Send a 200 OK response for an OPTIONS request"

	^self send200Response: '' contentType: 'application/octet-stream' do:[:resp|
		resp headerAt: 'allow' put: (String streamContents:[:s|
			allowed do:[:m| s nextPutAll: m] separatedBy:[s nextPut: $,]
		]).
	].! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 8/31/2010 22:16'!
sendResponse: code chunked: chunkBlock
	"Send a chunked response"

	^self sendResponse: code chunked: chunkBlock do:[:resp]! !

!HTTPRequest methodsFor: 'responses' stamp: 'DSG 3/28/2015 18:00'!
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

!HTTPRequest methodsFor: 'sending' stamp: 'ar 2/23/2010 22:04'!
sendResponse: resp content: aString
	"Send a HTTPResponse with content."

	^self sendResponse: resp contentStream: aString readStream size: aString size! !

!HTTPRequest methodsFor: 'sending' stamp: 'ar 8/31/2010 22:11'!
sendResponse: resp contentBlock: contentBlock
	"Sends a HTTPResponse, streaming its contents from aStream.
	If a size is provided, insert a Content-Length header, otherwise
	ensure that the connection is transient."

	"Add Date and Server headers"
	resp headerAt: 'Date' put: (server ifNil:[HTTPServer]) serverDate.
	resp headerAt: 'Server' put: (server ifNil:[HTTPServer]) serverString.

	self closeIfTransient: resp after:[
		"Ignore all network errors while sending the response"
		[resp writeOn: stream.
		stream flush.
		self isHeadRequest ifFalse:[contentBlock value: stream].
		] on: NetworkError do:["ignore"].

		"Make sure we log before closing the connection"
		server ifNotNil:[server logRequest: self response: resp].
	].! !

!HTTPRequest methodsFor: 'sending' stamp: 'ar 8/31/2010 22:12'!
sendResponse: resp contentStream: aStream size: streamSize
	"Sends a HTTPResponse, streaming its contents from aStream.
	If a size is provided, insert a Content-Length header, otherwise
	ensure that the connection is transient."

	streamSize 
		ifNil:[self headerAt: 'Connection' put: 'close'] "mark transient"
		ifNotNil:[resp headerAt: 'Content-Length' put: streamSize].

	^self sendResponse: resp contentBlock:[:sockStream|
		resp streamFrom: aStream to: sockStream size: streamSize progress: nil
	]! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 7/28/2010 20:22'!
sendResponseCode: code content: aString type: contentType close: aBool
	"Send a 500 Internal server error response"

	^self sendResponseCode: code content: aString type: contentType do:[:hdr|
		aBool ifTrue:[hdr headerAt: 'Connection' put: 'close'].
	].! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 7/28/2010 20:19'!
sendResponseCode: code content: aString type: contentType do: aBlock
	"Send a 500 Internal server error response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: code.
	contentType ifNotNil:[resp headerAt: 'Content-Type' put: contentType].
	aBlock value: resp.
	^self sendResponse: resp content: aString.! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 5/11/2010 23:35'!
sendXmlResponse: xmlString
	"Send an a-ok 200 xml-ish response"

	^self send200Response: xmlString contentType: 'application/xml'! !

!HTTPRequest methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:55'!
server
	"If non-nil, the HTTPServer instance associated with the request."

	^server! !

!HTTPRequest methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:55'!
server: aHTTPServer
	"If non-nil, the HTTPServer instance associated with the request."

	server := aHTTPServer! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:48'!
stream200Response: aStream
	"Stream a 200 OK response"

	^self stream200Response: aStream size: aStream size! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:53'!
stream200Response: aStream size: streamSize
	"Stream a 200 OK response"

	^self stream200Response: aStream size: streamSize type: 'application/octet-stream'! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 03:53'!
stream200Response: aStream size: streamSize type: contentType
	"Stream a 200 OK response"

	^self stream200Response: aStream size: streamSize type: contentType do:[:resp].! !

!HTTPRequest methodsFor: 'responses' stamp: 'ar 2/25/2010 18:14'!
stream200Response: aStream size: streamSize type: contentType do: aBlock
	"Stream a 200 OK response"

	| resp |
	resp := self newResponse protocol: 'HTTP/1.1' code: 200.
	resp headerAt: 'Content-Type' put: contentType.
	aBlock value: resp.
	^self sendResponse: resp contentStream: aStream size: streamSize.! !

!HTTPRequest methodsFor: 'accessing' stamp: 'DSG 5/24/2012 21:18'!
url
	"The url without fragment or arguments"

	^((rawUrl copyUpTo: $#) copyUpTo: $?) unescapePercents! !

!HTTPRequest methodsFor: 'printing' stamp: 'ar 2/20/2010 16:33'!
writeOn: aStream
	"Write the request"

	self writeRequestOn: aStream.
	super writeOn: aStream! !

!HTTPRequest methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:37'!
writeRequestOn: aStream
	"Write the request"

	aStream nextPutAll: method; space; nextPutAll: rawUrl; space; nextPutAll: protocol; nextPutAll: String crlfString.
! !

!HTTPRequest class methodsFor: 'instance creation' stamp: 'ar 2/20/2010 11:23'!
readFrom: aStream
	"Create a HTTPRequest from a given stream"

	^self new readFrom: aStream! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:39'!
code
	"The HTTP response code"

	^code! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 6/18/2010 23:51'!
content
	"Reimplemented to close the socket if the request is transient"

	content ifNil:[
		content := self getContent.
		self closeIfTransient.
	].
	^content! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 1/31/2012 14:02'!
contentStream
	"Answers a stream for the conent."

	(self headerAt: 'transfer-encoding') = 'chunked' ifTrue:[
		^HTTPChunkedStream on: stream.
	].
	^super contentStream! !

!HTTPResponse methodsFor: 'private' stamp: 'dsg 3/10/2015 16:27'!
getContent
	"Do not read any content if this was a HEAD request or code is 204 (no content)"

	(request method = 'HEAD' or: [code = 204]) ifTrue:[^''].
	^super getContent! !

!HTTPResponse methodsFor: 'testing' stamp: 'ar 7/9/2010 00:27'!
isInformational
	"Is this a 1xx response?"

	^self code between: 100 and: 199
! !

!HTTPResponse methodsFor: 'testing' stamp: 'ar 8/10/2010 08:56'!
isRedirect
	"Is this a redirect response?"

	^self code between: 300 and: 399
! !

!HTTPResponse methodsFor: 'testing' stamp: 'ar 2/23/2010 10:13'!
isSuccess
	"Is this a successful response?"

	^self code between: 200 and: 299
! !

!HTTPResponse methodsFor: 'initialize' stamp: 'SvenVanCaekenberghe 6/14/2010 15:48'!
protocol: respProtocol code: respCode
	"Initialize a response from protocol and code"

	code := respCode.
	protocol := respProtocol.
	status := protocol, ' ', code asString, ' ', (self class statusCodeAt: code).
	headers := OrderedCollection new.! !

!HTTPResponse methodsFor: 'initialize' stamp: 'DSG 5/24/2012 17:34'!
readFrom: aStream
	"Parse an HTTP response from the given stream"

	| tokens |
	stream := aStream.
	status := stream upToAll: String crlfString.
	tokens := status findTokens: ' '.
	tokens size < 2 ifTrue:[^self error:'Invalid response: ', status].
	protocol := tokens first.
	code := tokens second asNumber.
	headers := HTTPUtils readHeadersFrom: stream.
! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 4/1/2010 16:02'!
request
	"The original request for this response"

	^request! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 4/1/2010 16:03'!
request: aRequest
	"The original request for this response"

	request := aRequest! !

!HTTPResponse methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
setCookie: aCookie
	"Adds a Set-Cookie header"

	^self addHeader: 'Set-Cookie' value: aCookie asString! !

!HTTPResponse methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
setCookie: name value: value path: path 
	"Adds a Set-Cookie header"

	^self setCookie: ((HTTPCookie new)
			name: name;
			value: value;
			path: path;
		yourself)! !

!HTTPResponse methodsFor: 'cookies' stamp: 'ar 2/20/2010 19:35'!
setCookie: name value: value path: path expires: date domain: domain secure: aBool
	"Adds a Set-Cookie header"

	^self setCookie: ((HTTPCookie new)
			name: name;
			value: value;
			path: path;
			expiry: date;
			domain: domain;
			secure: aBool;
		yourself)! !

!HTTPResponse methodsFor: 'cookies' stamp: 'ar 2/25/2010 18:16'!
setCookieAt: key
	"Evaluate aBlock with all the cookies deined in the request"

	self setCookiesDo:[:cookie|
		cookie name = key ifTrue:[^cookie].
	].
	^nil! !

!HTTPResponse methodsFor: 'cookies' stamp: 'ar 2/21/2010 09:51'!
setCookiesDo: aBlock
	"Evaluate aBlock with all the cookies deined in the request"

	self headersAt: 'Set-Cookie' do:[:hdr|
		aBlock value: (HTTPCookie new readFrom: hdr readStream)
	].
! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:39'!
status
	"The status line from the response"

	^status! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 2/20/2010 10:39'!
status: aString
	"The status line from the response"

	status := aString! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 8/24/2010 21:37'!
url
	"The url of the resource at which a request was terminated after a series
	of redirects. Without redirects, this will be the same as the request url."

	^url! !

!HTTPResponse methodsFor: 'accessing' stamp: 'ar 8/24/2010 21:37'!
url: urlString
	"The url of the resource at which a request was terminated after a series
	of redirects. Without redirects, this will be the same as the request url."

	url := urlString! !

!HTTPResponse methodsFor: 'printing' stamp: 'ar 2/20/2010 16:33'!
writeOn: aStream
	"Store this response on aStream"

	self writeStatusOn: aStream.
	super writeOn: aStream.
! !

!HTTPResponse methodsFor: 'printing' stamp: 'DSG 5/24/2012 17:39'!
writeStatusOn: aStream
	"Store this response on aStream"

	aStream nextPutAll: status; nextPutAll: String crlfString.
! !

!HTTPResponse class methodsFor: 'instance creation' stamp: 'ar 2/20/2010 11:24'!
protocol: protocol code: code
	"Create a response using the given protocol / code"

	^self new protocol: protocol code: code! !

!HTTPServer methodsFor: 'logging' stamp: 'ar 7/28/2010 19:56'!
accessLog
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	^accessLog! !

!HTTPServer methodsFor: 'logging' stamp: 'ar 7/28/2010 19:56'!
accessLog: aStreamOrFilename
	"The standard access log. Can be either a stream or a string; if the access log
	is a string then the string is assumed to be a file name and the file will be opened 
	and closed for each request."

	accessLog := aStreamOrFilename! !

!HTTPServer methodsFor: 'services' stamp: 'ar 5/11/2010 20:10'!
addService: entryPoint action: aBlockOrMessage
	"Add a service to the web server. The service is specified by an entryPoint, 
	denoting the url under which the service can be requested 
	(such as /my/great/service) and an action which is a message 
	send taking a single argument, the HTTPRequest."

	^self addService: entryPoint 
		action: aBlockOrMessage 
		methods: self defaultHttpMethods.! !

!HTTPServer methodsFor: 'services' stamp: 'ar 5/11/2010 20:10'!
addService: entryPoint action: aBlockOrMessage methods: methods
	"Add a service to the web server. The service is specified by an entryPoint, 
	denoting the url under which the service can be requested 
	(such as /my/great/service) and an action which is a message 
	send taking a single argument, the HTTPRequest.
	
	The list of methods describes what HTTP methods the given url supports."

	mutex critical:[
		entryPoints at: entryPoint asLowercase put: {methods. aBlockOrMessage}.
	].! !

!HTTPServer methodsFor: 'handling' stamp: 'dsg 3/12/2015 10:36'!
asyncHandleConnectionFrom: aSocket
	"Handle an incoming connection asynchronously to avoid blocking the listener."

	| handlerProcess |
	handlerProcess := [[
		[self handleConnectionFrom: aSocket] on: Error do: [:ex | aSocket destroy]]
				ensure: [aSocket destroy.
					mutex
						critical: ["If we get here, then handlerProcess is
							initialized, so it's safe to use that variable.
							Using Processor activeProcess is not
							possible, because this block may be
							evaluated by another process. Doing this
							outside the #ensure: block is not safe."
							connections
								remove: handlerProcess
								ifAbsent: []]]] newProcess.

	mutex critical:[connections add: handlerProcess].

	handlerProcess resume.
! !

!HTTPServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:43'!
authAccept: method request: request realm: realm header: authHeader
	"Performs the requested authentication method. 
	Returns true if successfully authenticated.
	Subclasses can extend this method to support more auth methods."

	^method asLowercase caseOf: {
		['basic'] 	-> [self basicAuth: request realm: realm header: authHeader].
		['digest'] 	-> [self digestAuth: request realm: realm header: authHeader].
	} otherwise:[false].
! !

!HTTPServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:43'!
authHeader: method request: request realm: realm
	"Answer the authentication header for the given method.
	Subclasses can extend this method to support more auth methods."

	^method asLowercase caseOf: {
		['basic'] 	-> ['Basic realm="', realm,'"'].
		['digest'] 	-> ['Digest realm="', realm,'", nonce="', self newNonce,'", qop="auth"'].
	} otherwise:[nil].
	
	! !

!HTTPServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:43'!
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

!HTTPServer methodsFor: 'authentication' stamp: 'jmv 9/19/2016 21:33:53'!
basicAuth: request realm: realm header: authHeader
	"Perform Basic auth for the given request"

	| data user pass hash |
	data := (authHeader copyAfter: $ ) base64Decoded asString.
	user := data copyUpTo: $:.
	pass := data copyAfter: $:.
	data atAllPut: $*. data := nil.

	hash := self hashUser: user password: pass realm: realm.
	pass atAllPut: $*. pass := nil.

	^(self passwordHashAt: user, ':', realm) = hash
! !

!HTTPServer methodsFor: 'initialize' stamp: 'ar 5/11/2010 20:10'!
builtinHttpMethods
	"Returns the list of HTTP methods that the server handles built-in,
	i.e., without passing them to the registered service."

	^#('HEAD' 'TRACE' 'OPTIONS')! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 7/29/2010 18:49'!
certName
	"The cert name to use for a secure (SSL/TLS) web server."

	^certName! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 7/29/2010 18:49'!
certName: aString
	"The cert name to use for a secure (SSL/TLS) web server."

	certName := aString.! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 5/11/2010 20:22'!
connections
	"The active connection processes. Can only be examined after aquiring
	the HTTPServer's mutex."

	^connections! !

!HTTPServer methodsFor: 'initialize' stamp: 'ar 2/23/2010 12:10'!
critical: aBlock
	"Evaluate aBlock protected byt the server's mutex.
	Necessary if code needs to manipulate internal server state."

	^mutex critical: aBlock
! !

!HTTPServer methodsFor: 'initialize' stamp: 'ar 5/11/2010 20:10'!
defaultHttpMethods
	"Answer the list of HTTP methods that should be be supported by default.
	The methods must be implemented by the request handler."

	^#('GET' 'POST')! !

!HTTPServer methodsFor: 'initialize' stamp: 'ar 11/1/2010 21:07'!
destroy
	"Destroys the receiver"

	self stopListener.
	self destroyConnections.
	listenerSocket ifNotNil:[listenerSocket destroy].

	"De-register the HTTPServer"
	(self class forUrl: self siteUrl ifAbsent:[self]) == self 
		ifTrue:[self class removeUrl: siteUrl]
! !

!HTTPServer methodsFor: 'initialize' stamp: 'DSG 3/28/2015 16:06'!
destroyConnections
	"Destroy all current connections."

	| copy |
	copy := mutex critical: [ connections copy ].
	" #terminate will trigger the #ensure: block in #asyncHandleConnectionFrom:, which will block if we don't release the mutex "
	copy do: [ :p | p terminate ].
	" make sure all of them were really removed "
	mutex critical: [ connections removeAll: connections ]
! !

!HTTPServer methodsFor: 'authentication' stamp: 'DSG 12/5/2013 17:11'!
digestAuth: request realm: realm header: authHeader
	"Authenticates an incoming request using Digest auth."

	| user nonce uri response ha1 ha2 md5 qop nc cnonce 
	  nonceData nonceCounter nonceTimeout params |

	params := HTTPUtils parseAuthParams: authHeader.

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
	ha2 := HTTPUtils md5Digest: request method, ':', uri.
	md5 := HTTPUtils md5Digest: ha1, ':', nonce, ':', nc, ':', cnonce, ':', qop, ':', ha2.
	^md5 = response
! !

!HTTPServer methodsFor: 'handling' stamp: 'ar 2/23/2010 22:34'!
dispatchRequest: request
	"Look up the handler for a given request and dispatch to it"

	^self dispatchRequest: request url: request url! !

!HTTPServer methodsFor: 'handling' stamp: 'DSG 5/29/2012 17:29'!
dispatchRequest: request url: dispatchUrl
	"Look up the handler for a given url and dispatch to it"

	| entryUrl action |
	"Handle TRACE requests right away"
	request method = 'TRACE' ifTrue:[
		^request send200Response: request asString contentType: 'message/http'.
	].

	"Look up the entry point for the request"
	action := nil.
	entryUrl := dispatchUrl asLowercase.
	(entryUrl beginsWith: '/') ifFalse:[entryUrl := '/', entryUrl].
	
	mutex critical:[
		[entryUrl notEmpty and:[action == nil]] whileTrue: [
			action := entryPoints at: entryUrl ifAbsent:[nil].
			entryUrl := entryUrl copyUpToLast: $/.
		].
		action ifNil:[action := entryPoints at: '/' ifAbsent:[nil]].
	].

	"Handle OPTIONS requests"
	request method = 'OPTIONS' ifTrue:[
		"HEAD, TRACE, and OPTIONS are always supported"
		request sendOptionsResponse: self builtinHttpMethods,
			(action ifNil:[self defaultHttpMethods] ifNotNil:[action first])
	].

	"Handle 404 not found"
	action ifNil:[^request send404Response].

	"Handle 405 method not allowed"
	(request method = 'HEAD' "builtin" 
		or:[(action at: 1) includes: request method]) ifFalse:[
			^request send405Response: self builtinHttpMethods, action first.
		].

	^[self invokeAction: (action at: 2) request: request] 
		on: Error do:[:ex|
			errorHandler 
				ifNil:[self handleError: ex request: request]
				ifNotNil:[errorHandler value: ex value: request]
		].
! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:24'!
errorHandler
	"The user-supplied error handler. If present the handler takes two arguments:
		* The error being caught by HTTPServer
		* The request the error occurred in.
	The user-supplied error handler can do whatever it wants, including calling
	HTTPServer's #handleError:request: method which is the default."

	^errorHandler! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 7/28/2010 20:24'!
errorHandler: aBlock
	"The user-supplied error handler. If present the handler takes two arguments:
		* The error being caught by HTTPServer
		* The request the error occurred in.
	The user-supplied error handler can do whatever it wants, including calling
	HTTPServer's #handleError:request: method which is the default."

	errorHandler := aBlock.! !

!HTTPServer methodsFor: 'errors' stamp: 'ar 2/20/2010 10:35'!
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

!HTTPServer methodsFor: 'errors' stamp: 'jmv 5/19/2015 21:55'!
errorReportFor: anError stack: preStack on: strm
	"Provide a report for an error that's been caught during processing"

	self errorStackFor: anError stack: preStack on: strm.
	strm newLine; nextPutAll: '-----------------------------------------------------------------'; newLine; newLine.
	self systemInfoOn: strm.
! !

!HTTPServer methodsFor: 'errors' stamp: 'jmv 5/19/2015 21:55'!
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

!HTTPServer methodsFor: 'authentication' stamp: 'DSG 12/5/2013 17:11'!
flushNonceCache
	"Flushes expired nonces from the cache"

	| deadline |
	deadline := Time localSecondClock.
	mutex critical:[
		nonceCache := nonceCache reject:[:nonceData| deadline > nonceData second]
	].
! !

!HTTPServer methodsFor: 'handling' stamp: 'DSG 5/29/2012 16:48'!
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
			ssl := Smalltalk at: #SSecureSocket ifAbsent:[self error: 'SecureSocket not installed'].
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

!HTTPServer methodsFor: 'errors' stamp: 'jmv 5/19/2015 23:48'!
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

!HTTPServer methodsFor: 'authentication' stamp: 'ar 3/30/2010 20:36'!
hashUser: user password: pass realm: realm
	"Creates a hash for the given username password pair.
	This method is the ha1 part of digest auth and can be used for both digest
	as well as basic auth."

	^HTTPUtils md5Digest: user, ':', realm, ':', pass! !

!HTTPServer methodsFor: 'initialize' stamp: 'DSG 3/28/2015 20:16'!
initialize
	"Initialize the receiver"

	mutex := Mutex new.
	entryPoints := Dictionary new.
	connections := IdentitySet new.
	vault := Dictionary new.
	nonceCache := Dictionary new.
	sessions := Dictionary new.
	self logAction:[:aString| Transcript log: aString].
! !

!HTTPServer methodsFor: 'handling' stamp: 'DSG 5/29/2012 17:25'!
invokeAction: action request: request
	"Invokes the given action. Subclasses can override this method to serialize
	actions if necessary. At this point, the request header has been read from
	the network, but not its content."

	^action valueWithArguments: {request}
! !

!HTTPServer methodsFor: 'testing' stamp: 'ar 3/30/2010 22:09'!
isRunning
	"Is the server still running?"

	^self isStopped not! !

!HTTPServer methodsFor: 'testing' stamp: 'ar 3/30/2010 22:09'!
isStopped
	"Is the server still running?"

	^listenerProcess == nil or:[listenerProcess isTerminated]! !

!HTTPServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:27'!
listenOn: aPort
	"Starts the server listening on the given port"

	^self listenOn: aPort interface: nil! !

!HTTPServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:28'!
listenOn: aPort interface: anInterface
	"Starts the server listening on the given port / network interface"

	^self listenOn: aPort interface: anInterface backlogSize: 8! !

!HTTPServer methodsFor: 'listening' stamp: 'SvenVanCaekenberghe 6/14/2010 15:48'!
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

!HTTPServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:01'!
listenerInterface
	"Returns the listener interface for the server"

	^interface! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:01'!
listenerPort
	"Returns the listener port for the server"

	^port! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:00'!
listenerProcess
	"Returns the socket listener process for the server."

	^listenerProcess! !

!HTTPServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:28'!
listenerProcessPriority
	"The priority at which the listener process should run"

	^Processor lowIOPriority! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 03:00'!
listenerSocket
	"Returns the listener socket for the server."

	^listenerSocket! !

!HTTPServer methodsFor: 'initialize' stamp: 'ar 2/22/2010 05:35'!
log: aString
	"Log the given information."

	logAction ifNotNil:[logAction valueWithArguments: {aString}].
! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 05:35'!
logAction
	"The current log action (a block or message)"

	^logAction! !

!HTTPServer methodsFor: 'accessing' stamp: 'DSG 3/28/2015 21:44'!
logAction: aBlockOrMessage
	"The current log action (a block or message).
	Example:
		HTTPServer new logAction: [:aString| Transcript show: aString].
		HTTPServer new logAction: (MessageSend receiver: Transcript selector: #log).
	"

	logAction := aBlockOrMessage! !

!HTTPServer methodsFor: 'logging' stamp: 'ar 7/28/2010 20:02'!
logRequest: request response: response
	"Log a request after it has been successfully processed"

	accessLog ifNil:[^self].

	"Do not use the standard exception handler for errors logging; 
	it would possibly send a 500 response. Really this should be handled
	by a different process with its own error handler."

	[HTTPUtils logRequest: request response: response on: accessLog]
		on: Error do:[:ex| self log: 'Error in logging: ', ex description].
! !

!HTTPServer methodsFor: 'authentication' stamp: 'DSG 12/5/2013 17:12'!
newNonce
	"Create a new nonce to be used for a client authentication session"

	| nonce |
	nonce := UUID new hex.
	mutex critical:[
		nonceCache at: nonce put: {''. Time localSecondClock + self nonceDeadline}.
	].
	^nonce! !

!HTTPServer methodsFor: 'handling' stamp: 'ar 7/28/2010 20:27'!
newRequest
	"Answer a new request.
	Subclasses should override this method to use a different request class."

	^(HTTPRequest new)
		server: self;
		yourself! !

!HTTPServer methodsFor: 'authentication' stamp: 'ar 2/23/2010 09:20'!
nonceDeadline
	"Answer the number of seconds a nonce is considered valid for digest auth
	by the server. After this period, the client will have to reauthenticate.
	The default is 30 minutes."

	^30*60 ! !

!HTTPServer methodsFor: 'authentication' stamp: 'ar 2/20/2010 12:54'!
passwordAt: username realm: realm put: password
	"Store the password hash for the key."

	| hash |
	hash := self hashUser: username password: password realm: realm.
	self passwordHashAt: (username,':',realm) put: hash! !

!HTTPServer methodsFor: 'authentication' stamp: 'ar 2/20/2010 12:51'!
passwordHashAt: key
	"Answer the password hash for the given key"

	^vault at: key ifAbsent:['']! !

!HTTPServer methodsFor: 'authentication' stamp: 'ar 2/20/2010 12:01'!
passwordHashAt: key put: hash
	"Store the password hash for the key."

	^vault at: key put: hash! !

!HTTPServer methodsFor: 'services' stamp: 'ar 2/22/2010 02:52'!
removeService: entryPoint 
	"Remove a service from the HTTP service provider.  See #addService:action:"

	mutex critical:[
		entryPoints removeKey: entryPoint asLowercase ifAbsent: [].
	].! !

!HTTPServer methodsFor: 'listening' stamp: 'ar 5/4/2010 09:22'!
runListener
	"Runs the server's listener loop. 
	Accepts incoming connections and handles them asynchronously"

	| connectionSocket |
	[[[listenerSocket notNil and:[listenerSocket isValid]] whileTrue:[
		"Waiting for five seconds means that HTTPServer will be
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

!HTTPServer methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:53'!
serverDate
	"Returns the date string to be used in responses"

	^self class serverDate! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:54'!
serverString
	"Returns the server string to be used in responses.
	The response is cached for efficiency."

	^serverString ifNil:[serverString := self class serverString].! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 6/14/2010 20:57'!
serverString: aString
	"Set the server string to be used in responses"

	^serverString := aString! !

!HTTPServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:09'!
sessionAt: key
	"Answer the session associated with the given key"

	^mutex critical:[sessions at: key ifAbsent:[nil]].! !

!HTTPServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:10'!
sessionAt: key ifAbsentPut: session
	"Answer the session associated with the given key"

	^mutex critical:[sessions at: key ifAbsentPut:[session]].! !

!HTTPServer methodsFor: 'sessions' stamp: 'ar 5/11/2010 23:34'!
sessionAt: key put: session
	"Store the session associated with the given key"

	^mutex critical:[sessions at: key put: session].! !

!HTTPServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:11'!
sessions
	"The sessions associated with the HTTPServer. These are not used by the server
	directly but rather provided so that client code has a convenient place to store
	session data."
	
	^sessions! !

!HTTPServer methodsFor: 'sessions' stamp: 'ar 2/23/2010 12:11'!
sessions: aDictionary
	"The sessions associated with the HTTPServer. These are not used by the server
	directly but rather provided so that client code has a convenient place to store
	session data."
	
	sessions := aDictionary! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 11/1/2010 21:01'!
siteUrl
	"The site URL for HTTPServer, i.e., http://www.mysite.com or http://www.mysite.com/server (when used in a proxied environment) or https://www.mysite.com (when using SSL). Used to allow a client to derive the 'correct' full URL for a running server instance."

	^siteUrl! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 11/1/2010 21:13'!
siteUrl: aString
	"The site URL for HTTPServer, i.e., http://www.mysite.com or http://www.mysite.com/server (when used in a proxied environment) or https://www.mysite.com (when using SSL). Used to allow a client to derive the 'correct' full URL for a running server instance."

	siteUrl := aString! !

!HTTPServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:29'!
startListener
	"Starts the server's listener process"

	self stopListener.
	listenerProcess := [self runListener] newProcess.
	listenerProcess priority: self listenerProcessPriority.
	listenerProcess name: self class name, '''s listener process'.
	listenerProcess resume.
! !

!HTTPServer methodsFor: 'listening' stamp: 'ar 2/20/2010 10:29'!
stopListener
	"Stops the server's listener process"

	listenerProcess ifNotNil:[listenerProcess terminate].
	listenerProcess := nil.! !

!HTTPServer methodsFor: 'errors' stamp: 'jmv 6/17/2015 18:00'!
systemInfoOn: strm
	"Provides system information for a server error report"
	| info |
	strm nextPutAll: 'VM: ';
		nextPutAll:  HTTPUtils platformName;
		nextPutAll: ' - ';
		nextPutAll: HTTPUtils vmVersion;
		newLine.

	strm nextPutAll: 'Image: ';
		nextPutAll:  SystemVersion current version asString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']';
		newLine.

	strm newLine.

	"SecurityManager default printStateOn: strm."

	"Additional info available from the windows VM"
	HTTPUtils platformName = 'Win32' ifTrue:[
		10001 to: 10003 do: [ :i |
			info := (Smalltalk getSystemAttribute: i) ifNil:[''].
			info isEmpty ifFalse:[strm newLine; nextPutAll: info]]]! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 2/20/2010 12:03'!
vault
	"The vault used for storing password hashes."

	^vault! !

!HTTPServer methodsFor: 'accessing' stamp: 'ar 2/22/2010 02:51'!
vault: anObject
	"The vault used for storing password hashes.

	Clients can provide their own storage mechanism. 
	Subclasses can also override the lookup and authentication mechanism."

	vault := anObject! !

!HTTPServer class methodsFor: 'examples' stamp: 'DSG 3/28/2015 18:47'!
browseDir: directory request: request
	"Responds with a directory listing back to the original request"

	| entries listing url |
	url := request url.
	(url endsWith: '/') ifTrue:[url := url allButLast].

	entries := directory entries sort:[:e1 :e2|
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
			s nextPutAll: (e modificationDateAndTime asString padded: #right to: 20 with: $ ).
			s nextPutAll: (e fileSize asString padded: #right to: 20 with: $ ).
			s cr; lf.
		].
 	].

	request 
		"send200Response: (listing convertToWithConverter: UTF8TextConverter new)"
		send200Response: listing
		contentType: 'text/html; charset=utf-8'.

! !

!HTTPServer class methodsFor: 'examples' stamp: 'jmv 5/19/2015 23:52'!
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

!HTTPServer class methodsFor: 'examples' stamp: 'pb 5/25/2016 01:51'!
browseRequest: request
	"Handle an HTTP request for browsing some resource"

	| path fd entry file |

	"Extract the file path from the request"
	path := request url findTokens: '/'.
	path ifEmpty: [ ^self browseDir: DirectoryEntry currentDirectory request: request ].

	"Find the directory entry for the resource"
	fd := path allButLast inject: DirectoryEntry currentDirectory into: [ :dir :part | dir / part ].
	entry := fd entryAt: path last ifAbsent: [ ^request send404Response ].

	"Reply with the proper resource"
	entry isDirectory ifTrue:[
		"Send file listing. We handle this synchronously for simplicity."
		self browseDir: (fd / entry name) request: request.
	] ifFalse:[
		"Send file content. Since files can be large, we fork this off.
		However, since we forked it, we need to handle two different
		possible conditions:
			- closing the file in case of error
			- handling errors when sending it
		This makes the code below a bit ugly"
		[[
			file := (fd // entry name) readStream.
			[ self browseFile: file request: request ]
				ensure: [ file close ] 	"close file even in case of error"
		] on: Error do: []				"ignore errors altogether"
		] fork. 							"fork it"
	]! !

!HTTPServer class methodsFor: 'accessing' stamp: 'ar 2/23/2010 09:29'!
default
	"A default HTTPServer instance. Mostly for the examples."

	^Default ifNil:[Default := self new].! !

!HTTPServer class methodsFor: 'examples' stamp: 'ar 2/23/2010 09:30'!
exampleAuth		"HTTPServer exampleAuth"
	"Simple example requiring basic auth"

	| server port realm |
	port := 9999.
	server := HTTPServer reset default.
	
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

!HTTPServer class methodsFor: 'examples' stamp: 'ar 2/20/2010 14:54'!
exampleBrowse	"HTTPServer exampleBrowse"
	"This example implements a simple http server allowing to view 
	and download files (like browsing ftp sites etc)."

	| server port |
	port := 9999.
	server := HTTPServer reset default.
	server listenOn: port.
	server addService: '/' action:[:req| self browseRequest: req].
! !

!HTTPServer class methodsFor: 'examples' stamp: 'ar 2/22/2010 05:41'!
exampleDoIt		"HTTPServer exampleDoIt"
	"This example executes doIts from a POST request.
	It's the simplest possible RPC interface to Squeak."

	| server port |
	port := 9999.
	server := HTTPServer reset default.
	server listenOn: port.
	server addService: '/' action:[:req| 
		| data result |
		req isPostRequest ifTrue:[
			"We fetch all of the data instead of streaming it"
			data := req content.
			"Compute the result"
			result := [Compiler evaluate: data] on: Error do:[:ex| ex return: ex asString].
			req send200Response: result asString 
		] ifFalse:[req send404Response].
	].
! !

!HTTPServer class methodsFor: 'examples' stamp: 'ar 7/29/2010 18:49'!
exampleSSL: certName

	"This example implements a simple https server allowing to view 
	and download files (like browsing ftp sites etc):

		HTTPServer exampleSSL: 'Internet Widgits Pty'.
		HTTPServer exampleSSL: '/home/andreas/certs/testcert.pem'.

	"

	| server port |
	port := 8443.
	server := HTTPServer reset default.
	server accessLog: Transcript.
	server certName: certName.
	server listenOn: port.
	server addService: '/' action:[:req| self browseRequest: req].
! !

!HTTPServer class methodsFor: 'examples' stamp: 'ar 2/25/2010 03:28'!
exampleSession		"HTTPServer exampleSession"
	"This example uses a simple session cookie to avoid 
	authentication for every request."

	| server port realm |
	realm := 'Example Realm'.	"authentication realm"
	port := 9999.
	server := HTTPServer reset default.
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
			ifFalse:[req send302Response: '/login?url=', req url encodeForHTTP]].
! !

!HTTPServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
forUrl: urlString
	"Answer the HTTPServer registered for a given URL string."

	^self forUrl: urlString ifAbsent:[self newForUrl: urlString]! !

!HTTPServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
forUrl: urlString ifAbsent: aBlock
	"Answer the HTTPServer registered for a given URL string.
	If no HTTPServer is registered, evaluate aBlock."

	^self registry at: urlString ifAbsent: aBlock! !

!HTTPServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 21:02'!
newForUrl: urlString
	"Create a new HTTPServer instance registered for a given URL string."

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

!HTTPServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 21:11'!
registry
	"Answer the HTTPServer site registry"

	^Registry ifNil:[Registry := Dictionary new]! !

!HTTPServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
removeUrl: urlString
	"Remove and answer the HTTPServer registered for a given URL string.
	If no HTTPServer was registered, return nil."

	^self removeUrl: urlString ifAbsent:[nil]! !

!HTTPServer class methodsFor: 'registry' stamp: 'ar 11/1/2010 20:59'!
removeUrl: urlString ifAbsent: aBlock
	"Remove and answer the HTTPServer registered for a given URL string."

	^self registry removeKey: urlString ifAbsent: aBlock! !

!HTTPServer class methodsFor: 'accessing' stamp: 'ar 2/23/2010 09:29'!
reset		"HTTPServer reset"
	"Reset the default HTTPServer instance."

	Default ifNotNil:[
		Default destroy.
		Default := nil.
	].! !

!HTTPServer class methodsFor: 'accessing' stamp: 'DSG 5/29/2012 17:57'!
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

!HTTPServer class methodsFor: 'accessing' stamp: 'DSG 5/13/2015 16:52'!
serverString
	"Returns the server string to be used in responses"

	^'HTTPServer/1.0 (',
		SystemVersion current version,'-', 
		SystemVersion current highestUpdate printString, '; ', 
		HTTPUtils platformName,')'.
! !

!HTTPUtils class methodsFor: 'decoding' stamp: 'jmv 5/19/2015 22:36'!
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
		(HTTPUtils readHeadersFrom: aStream) do:[:hdr| headers add: hdr].
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

!HTTPUtils class methodsFor: 'decoding' stamp: 'ar 2/21/2010 09:54'!
decodeUrlEncodedForm: aString
	"Decodes the fields embedded in the url ?var1=val1&var2=val2"

	^self decodeUrlEncodedForm: aString multipleValues: false! !

!HTTPUtils class methodsFor: 'decoding' stamp: 'DSG 5/24/2012 21:19'!
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

!HTTPUtils class methodsFor: 'decoding' stamp: 'ar 7/20/2010 20:37'!
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

!HTTPUtils class methodsFor: 'proxy' stamp: 'DSG 12/4/2013 23:02'!
defaultProxyServer
	"Answer the default proxy server to use."
	^''! !

!HTTPUtils class methodsFor: 'decoding' stamp: 'jmv 5/19/2015 23:52'!
encodeUrlEncodedForm: fieldMap
	"Encodes the fieldsMap in the form of var1=val1&var2=val2.
	The fieldMap can be EITHER an array of associations OR a Dictionary of 
	key value pairs (the former is useful for providing multiple fields and/or 
	specifying the order of fields)."

	| associations |
	associations := (fieldMap isKindOf: Dictionary) ifTrue:[fieldMap associations] ifFalse:[fieldMap].
	^String streamContents:[:s|
		associations do:[:assoc|
			s nextPutAll: assoc key encodeForHTTP.
			assoc value ifNotNil:[
				s nextPutAll: '='.
				s nextPutAll: assoc value asString encodeForHTTP
			].
		] separatedBy:[s nextPutAll:'&'].
	].! !

!HTTPUtils class methodsFor: 'websockets' stamp: 'ar 7/20/2010 20:38'!
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

!HTTPUtils class methodsFor: 'auth' stamp: 'jmv 9/19/2016 21:34:22'!
handleAuth: aBlock
	"Utility method to handle web authentication requests interactively.

	Use like here:
		| wc |
		wc := HTTPClient new.
		HTTPUtils handleAuth:[wc httpGet: 'http://some.auth.required'].
	"

	| user pass |
	^[aBlock value] on: HTTPAuthRequired do:[:ex|
		"Allow outer handlers to take over authentication"
		ex isNested ifFalse:[
			user := FillInTheBlankMorph request:  ex message, 
				'\Please enter your user name:'.
			user ifNotEmpty:[
				"Just a little obfuscation to avoid completely plain passwords"
				pass := (FillInTheBlankMorph request:
					'Please enter the password for "', user,'":') asByteArray base64Encoded.
				 ex username: user password: [pass base64Decoded asString]]].
		ex pass].! !

!HTTPUtils class methodsFor: 'oauth' stamp: 'DSG 3/28/2015 23:22'!
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

!HTTPUtils class methodsFor: 'misc' stamp: 'DSG 3/28/2015 22:49'!
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

!HTTPUtils class methodsFor: 'misc' stamp: 'pb 5/25/2016 01:29'!
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

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/20/2010 12:45'!
md5Digest: aStringOrByteArray
	"This creates a little endian hex string to be used with various auth methods
	This is the same as htdigest (apache) uses for its md5 digest auth db"

	^(self md5HashStream: (ReadStream on: aStringOrByteArray asByteArray)) reversed hex! !

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/25/2010 23:29'!
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
				1 to: 16 do:[:i| words at: i put: (buffer unsignedLongAt: i*4-3 bigEndian: false)].
				self md5Transform: words hash: hash.
				"process one additional block of padding ending with the length"
				buffer atAllPut: 0.
				sz = 64 ifTrue: [buffer at: 1 put: 128].
			].
			"Fill in the final 8 bytes with the 64-bit length in bits."
			n := (aStream position - start) * 8.
			7 to: 0 by: -1 do:[:i| buffer at: (buffer size - i) put: ((n bitShift: 7 - i * -8) bitAnd: 255)].
		].
		1 to: 16 do:[:i| words at: i put: (buffer unsignedLongAt: i*4-3 bigEndian: false)].
		self md5Transform: words hash: hash.
	].
	bytes := ByteArray new: 16.
	bytes unsignedLongAt: 1 put: (hash at: 4) bigEndian: true.
	bytes unsignedLongAt: 5 put: (hash at: 3) bigEndian: true.
	bytes unsignedLongAt: 9 put: (hash at: 2) bigEndian: true.
	bytes unsignedLongAt: 13 put: (hash at: 1) bigEndian: true.
	^bytes
! !

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
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

!HTTPUtils class methodsFor: 'decoding' stamp: 'jmv 5/19/2015 21:52'!
multipartBoundary
	"Answer a string to be used as multpart boundary in posts"

	^'----squeak-', Date today printString,'-', Time localMillisecondClock printString, '-webclient-----'.! !

!HTTPUtils class methodsFor: 'oauth' stamp: 'DSG 3/29/2015 08:41'!
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
			s nextPutAll: assoc value encodeForHTTP.
		] separatedBy:[s nextPutAll: '&'].
	].

	baseUrl := (requestUrl copyUpTo: $?) copyUpTo: $#.
	baseString := webRequest method, 
		'&', baseUrl encodeForHTTP, 
		'&',  query encodeForHTTP.

	key := (params at: 'oauth_consumer_secret'), 
			'&', (params at: 'oauth_token_secret' ifAbsent:['']).
	(params at: 'oauth_signature_method') caseOf: {
		['HMAC-SHA1'] -> [
			signature := (HTTPUtils hmacSha1: baseString key: key) base64Encoded
		].
	} otherwise:[self error: 'Unsupported signature method'].

	header := String streamContents:[:s|
		s nextPutAll: 'OAuth '.
		"Even though we need to include the extra args in the computation,
		they do NOT go into the OAuth header field."
		normalized do:[:assoc|
			(extraKeys includes: assoc key) 
				ifFalse:[ s nextPutAll: assoc key, '="', assoc value encodeForHTTP, '",']].
		s nextPutAll: 'oauth_signature="', signature encodeForHTTP, '"'.
	].

	webRequest headerAt: 'Authorization' put: header.! !

!HTTPUtils class methodsFor: 'oauth' stamp: 'ar 8/23/2010 21:55'!
oAuthSign: webRequest url: requestUrl using: params
	"Sign the given webRequest using the set of oauth parameters provided"

	^self oAuthSign: webRequest url: requestUrl extra: #() using: params! !

!HTTPUtils class methodsFor: 'auth utils' stamp: 'DSG 12/5/2013 17:20'!
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

!HTTPUtils class methodsFor: 'misc' stamp: 'ar 11/17/2011 10:16'!
platformName
	"Return the name of the platform we're running on."

	^Smalltalk getSystemAttribute: 1001! !

!HTTPUtils class methodsFor: 'proxy' stamp: 'DSG 12/4/2013 23:03'!
proxyExceptionsDo: aBlock
	"Evaluate aBlock with all registered proxy exceptions. 
	Uses the information from HTTPSocket."

	"HTTPSocket httpProxyExceptions do: aBlock."! !

!HTTPUtils class methodsFor: 'proxy' stamp: 'ar 2/23/2010 06:20'!
proxyServerFor: serverName
	"Answer the proxy server to use for the given server:port
	Returns the proxy server:port to use or nil if no proxy is required."

	| domain |
	domain := serverName copyUpTo: $:.
	self proxyExceptionsDo:[:pattern| (pattern match: domain) ifTrue:[^nil]].
	^self defaultProxyServer ifEmpty:[nil]! !

!HTTPUtils class methodsFor: 'quoting' stamp: 'ar 3/31/2010 08:43'!
quote: aString
	"Quote the given string, escaping as necessary any embedded quotes"

	^'"', (aString copyReplaceAll: '"' with: '\"'), '"'
! !

!HTTPUtils class methodsFor: 'headers' stamp: 'DSG 5/24/2012 17:35'!
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

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
rotate: value by: amount
	"Rotate value left by amount"

	| lowMask highMask |
	lowMask := (1 bitShift: 32-amount) - 1.
	highMask := 16rFFFFFFFF - lowMask.
	^((value bitAnd: lowMask) bitShift: amount) + 
		((value bitAnd: highMask) bitShift: amount-32)! !

!HTTPUtils class methodsFor: 'oauth' stamp: 'ar 8/12/2010 16:31'!
sha1Hash: message
	"Compute the SHA1 hash for the given message. Slightly different from 
	SecureHashAlgorithm to produce fixed-length byte arrays."

	| sha hash |
	sha := SecureHashAlgorithm new hashMessage: message.
	hash := ByteArray new: 20.
	1 to: 20 do:[:i| hash at: i put: (sha digitAt: 21-i)].
	^hash
! !

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step1: w x: x y: y z: z data: data add: add shift: s
	"Step 1 in MD5 transformation"

	| f result |
	f := z bitXor: (x bitAnd: (y bitXor: z)).
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step2: w x: x y: y z: z data: data add: add shift: s
	"Step 2 in MD5 transformation"

	| f result |
	f := y bitXor: (z bitAnd: (x bitXor: y)).
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step3: w x: x y: y z: z data: data add: add shift: s
	"Step 3 in MD5 transformation"

	| f result |
	f := (x bitXor: y) bitXor: z.
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!HTTPUtils class methodsFor: 'md5' stamp: 'ar 2/23/2010 09:28'!
step4: w x: x y: y z: z data: data add: add shift: s
	"Step 4 in MD5 transformation"

	| f result |
	f := y bitXor: (x bitOr: (z  bitXor: 16rFFFFFFFF)).
	result := w + f + data + add.
	result := self rotate: result by: s.
	^result + x bitAnd: 16rFFFFFFFF! !

!HTTPUtils class methodsFor: 'quoting' stamp: 'ar 3/31/2010 08:45'!
unquote: aString
	"Unquote the given string, unescaping as necessary any embedded quotes"

	(aString beginsWith: '"') ifFalse:[^aString].
	(aString endsWith: '"') ifFalse:[^aString].
	^(aString copyReplaceAll: '\"' with: '"') allButFirst allButLast! !

!HTTPUtils class methodsFor: 'misc' stamp: 'DSG 3/28/2015 19:59'!
urlEncode: urlString
	"For backwards compatibility only. Use 'urlString urlEncoded' if you can."

	^urlString encodeForHTTP! !

!HTTPUtils class methodsFor: 'misc' stamp: 'ar 11/17/2011 10:06'!
vmVersion	
	"Return a string identifying the interpreter version"

	^Smalltalk getSystemAttribute: 1004! !

!HTTPUtils class methodsFor: 'websockets' stamp: 'ar 7/8/2010 18:59'!
webSocketHandshake: key1 with: key2 with: data
	"Do the actual WebSocket handshake computation"

	| bytes |
	bytes := ByteArray new: 16.
	bytes longAt: 1 put: key1 bigEndian: true.
	bytes longAt: 5 put: key2 bigEndian: true.
	bytes replaceFrom: 9 to: 16 with: data.
	^(self md5HashStream: bytes readStream) reversed! !

!HTTPUtils class methodsFor: 'websockets' stamp: 'ar 9/26/2011 10:30'!
webSocketHash07: key
	"Do the actual WebSocket hash computation"

	| uid |
	uid := '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'.
	^(ByteArray newFrom: (SecureHashAlgorithm new hashMessage: key, uid)) reversed base64Encoded! !

!WebSocket methodsFor: 'initialize' stamp: 'ar 9/26/2011 13:06'!
close
	"Request graceful close"

	^self subclassResponsibility! !

!WebSocket methodsFor: 'running' stamp: 'ar 7/8/2010 20:00'!
fork
	"Fork the WebSocket's input process."

	process := [self run] newProcess.
	process resume.
! !

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

!WebSocket methodsFor: 'private' stamp: 'ar 7/8/2010 21:48'!
printOn: aStream
	"Print the receiver"

	aStream nextPutAll: self class name.
	aStream nextPutAll: '(', name, ')'.! !

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

!WebSocket class methodsFor: 'example' stamp: 'dsg 3/11/2015 13:18'!
example	"WebSocket example"

	"This is a very simple WebSocket example implementing a basic chat system.
	Each message sent by a client connection is broadcast to everyone."

	| mutex sockets |
	mutex := Mutex new.
	sockets := OrderedCollection new.
	HTTPServer reset default listenOn: 8080.

	HTTPServer default addService: '/' action:[:req|
		"Serves the main html chat page"
		req send200Response: self examplePage contentType: 'text/html'.
	].

	HTTPServer default addService: '/broadcast' action:[:req| | ws |
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
		that shutting down HTTPServer shuts down any WebSockets, too."
		ws run.
	].! !

!WebSocket class methodsFor: 'example' stamp: 'ar 7/8/2010 21:50'!
examplePage
	"The example HTML page for HTTPServer"

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

!WebSocket00 methodsFor: 'running' stamp: 'jmv 9/5/2016 20:49:42'!
close
	"Request graceful close"

	stream isConnected ifTrue:[
		stream nextPut: (Character numericValue: 255).
		stream nextPutAll: (Character numericValue: 0).
		stream flush.
	].
! !

!WebSocket00 methodsFor: 'read/write' stamp: 'jmv 9/5/2016 22:30:06'!
readMessage
	"Reads one frame worth of data"

	| byte length |
	byte := stream next ifNil:[
		frameType := 255.
		^nil].
	frameType := byte numericValue.
	frameType < 128 ifTrue:[
		"Frame type < 128: UTF-8 encoded text with 0xFF frame delimiter"
		frameData := String fromUtf8: (stream upTo: 255 asCharacter) asByteArray.
	] ifFalse:[
		"Frame type >= 128: Binary data with length prefix"
		length := 0.
		[byte := stream next numericValue.
		length := (length * 128) + (byte bitClear: 128).
		byte >= 128] whileTrue.
		frameData := ByteArray new: length.
		stream nextInto: frameData.
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

!WebSocket00 methodsFor: 'read/write' stamp: 'ar 9/26/2011 12:03'!
send: aString
	"Writes one frame worth of data"

	^self send: aString type: 0! !

!WebSocket00 methodsFor: 'read/write' stamp: 'jmv 9/19/2016 21:54:06'!
send: data type: type
	"Writes one frame worth of data using the given frame type"

	"Frame type"
	stream nextPut: (Character numericValue: type).
	type < 128 ifTrue:[
		"Frame type < 128: UTF-8 encoded text with 0xFF frame delimiter"
		stream nextPutAll: data asString asUtf8.
		stream nextPut: (Character numericValue: 255).
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

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/19/2015 22:45'!
firstFragment: aStringOrBytes
	"Writes the first fragment of data"

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asUtf8 code: 1 final: false]
		ifFalse:[^self send: aStringOrBytes code: 2 final: false].! !

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

!WebSocket07 methodsFor: 'initialize' stamp: 'DSG 12/4/2013 22:47'!
initialize

	super initialize.
	closing := false.
	masking := true.
	"http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07#section-4.1 says
	''Frames sent from the client to the server are masked to avoid confusing network intermediaries, such as intercepting proxies.''"! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/19/2015 22:45'!
lastFragment: aStringOrBytes
	"Writes the last fragment of data.
	Note: Spec says that code is ZERO for final fragment."

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asUtf8 code: 0 final: true]
		ifFalse:[^self send: aStringOrBytes code: 0 final: true].! !

!WebSocket07 methodsFor: 'accessing' stamp: 'ar 9/27/2011 13:47'!
masking
	"If true, outgoing frames are masked."

	^masking! !

!WebSocket07 methodsFor: 'accessing' stamp: 'ar 9/27/2011 13:47'!
masking: aBool
	"If true, outgoing frames are masked."

	masking := aBool! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 5/19/2015 22:45'!
nextFragment: aStringOrBytes
	"Writes the first fragment of data
	Note: Spec says that code is ZERO for intermediate fragments."

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asUtf8 code: 0 final: false]
		ifFalse:[^self send: aStringOrBytes code: 0 final: false].! !

!WebSocket07 methodsFor: 'private' stamp: 'jmv 9/5/2016 21:35:24'!
nextNumber: n from: aStream
	"Answer the next n bytes as a positive Integer or LargePositiveInteger."
	| s |
	s := 0.
	1 to: n do: 
		[:i | s := (s bitShift: 8) bitOr: aStream next numericValue].
	^ s normalize! !

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

!WebSocket07 methodsFor: 'read/write' stamp: 'ar 9/27/2011 13:31'!
readFrame
	"Reads one frame worth of data"

	^self readFrameFrom: stream! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 9/5/2016 22:30:33'!
readFrameFrom: aStream
	"Reads one frame worth of data"

	| byte length final mask opcode payload |
	byte := (aStream next ifNil:[^nil]) numericValue.

	final := byte anyMask: 128.
	opcode := byte bitAnd: 15.

	byte := (aStream next ifNil:[^nil]) numericValue.
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
	mask ifNotNil:[self xor: payload with: mask].

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
		frameType = 1 ifTrue:[payload := String fromUtf8: payload asByteArray].
		self handleMessage: payload type: frameType.
		frameType := frameData := nil.
	].
! !

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

!WebSocket07 methodsFor: 'read/write' stamp: 'DSG 3/28/2015 21:49'!
send: aStringOrBytes
	"Writes one frame worth of data"

	aStringOrBytes isString 
		ifTrue:[^self send: aStringOrBytes asString asUtf8 code: 1]
		ifFalse:[^self send: aStringOrBytes code: 2].! !

!WebSocket07 methodsFor: 'read/write' stamp: 'ar 9/26/2011 12:48'!
send: data code: opcode
	"Sends unfragmented data using the given opcode"

	^self send: data code: opcode final: true! !

!WebSocket07 methodsFor: 'read/write' stamp: 'ar 9/27/2011 13:47'!
send: data code: opcode final: isFinal
	"Sends unfragmented data using the given opcode"

	^self send: data code: opcode final: isFinal mask: self masking! !

!WebSocket07 methodsFor: 'read/write' stamp: 'jmv 2/26/2016 17:11'!
send: data code: opcode final: isFinal mask: doMask
	"Sends unfragmented data using the given opcode"

	| byte1 byte2 mask xLength copy |
	byte1 := opcode. "FIN = 1, RSV1=RSV2=RSV3=0"
	isFinal ifTrue:[byte1 := byte1 bitOr: 128].
	data size > 125 ifTrue:[
		data size > 16rFFFF ifTrue:[
			byte2 := 127.
			xLength := ByteArray new: 8.
			(WriteStream on: xLength) nextNumber: 8 put: data size.
		] ifFalse:[
			byte2 := 126.
			xLength := ByteArray new: 2.
			(WriteStream on: xLength) nextUnsignedInt16Put: data size bigEndian: true.
		].
	] ifFalse:[
		byte2 := data size.
		xLength := #[].
	].
	doMask ifTrue:[
		byte2 := byte2 bitOr: 128. "Mask = 1"
		mask := ByteArray new: 4.
		1 to: 4 do:[:i| mask at: i put: 256 atRandom-1].
		copy := data copy.
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

!WebSocket07 methodsFor: 'read/write' stamp: 'ar 9/26/2011 13:09'!
xor: data with: mask
	"XOR all the data using the given mask bytes.

	WARNING: Does inplace reversal; do not call with literal data."

	"This would be much faster w/ BitBlt but data may not be 
	4-byte aligned and so BB will barf."
	1 to: data size do:[:i|
		data byteAt: i put: ((data byteAt: i) bitXor: (mask byteAt: i-1 \\ 4 + 1)).
	].
! !
HTTPClient initialize!
HTTPMessage initialize!
