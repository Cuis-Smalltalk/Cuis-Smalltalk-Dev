'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:33:15 pm'!
'Description '!
!provides: 'Tests-WebClient' 1 0!
!requires: 'WebClient' 1 19 nil!
SystemOrganization addCategory: 'Tests-WebClient'!


!classDefinition: #WebClientServerTest category: 'Tests-WebClient'!
TestCase subclass: #WebClientServerTest
	instanceVariableNames: 'server user password oldAuthHandler oldProxyHandler port'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-WebClient'!
!classDefinition: 'WebClientServerTest class' category: 'Tests-WebClient'!
WebClientServerTest class
	instanceVariableNames: ''!


!WebClientServerTest commentStamp: 'ar 2/24/2010 00:13' prior: 0!
Tests for both WebClient and WebServer.!

!WebClientServerTest methodsFor: 'tests - json' stamp: 'ar 2/10/2012 13:47'!
decode: aString
	"Decodes the given string"
	
	^WebUtils jsonDecode: aString readStream! !

!WebClientServerTest methodsFor: 'tests - json' stamp: 'ar 2/10/2012 13:47'!
encode: anObject
	"Encodes the given object"
	
	^WebUtils jsonEncode: anObject! !

!WebClientServerTest methodsFor: 'tests - json' stamp: 'ar 2/10/2012 13:46'!
testArrays
	"Test array encodings"

	self assert: (self decode: '[]') = #().
	self assert: (self decode: '[[]]') = #(#()).
	self assert: (self decode: '[[], []]') = #(#() #()).
	self assert: (self decode: '["hello", "world", 123]') = #('hello' 'world' 123).
	self assert: (self decode: '[["true", false, null]]') = #(('true' false nil)).

	self assert: (self encode: #()) =  '[]'.
	self assert: (self encode: #(#())) = '[[]]'.
	self assert: (self encode: #(#() #()) ) = '[[], []]'.
	self assert: (self encode: #('hello' 'world' 123)) = '["hello", "world", 123]'.
	self assert: (self encode: #(('true' false nil))) = '[["true", false, null]]'.

	self should: [self decode: '['] raise: Error.
	self should: [self decode: '[}'] raise: Error.
	self should: [self decode: '{[}'] raise: Error.
	self should: [self decode: '[[[]]'] raise: Error.
! !

!WebClientServerTest methodsFor: 'tests - json' stamp: 'ar 2/10/2012 13:47'!
testDictionaries
	"Test dictionary encodings"

	self assert: (self decode: '{}') = (Dictionary new).

	self assert: (self decode: '{"foo" : "bar"}')
		equals: (Dictionary newFromPairs: {
			'foo'. 'bar'
		}).

	self assert: (self decode: '{"stuff" : [[], 42, "hello"]}')
		equals: (Dictionary newFromPairs: {
			'stuff'. #(() 42 'hello')
		}).

	self assert: (self decode: '{"x" : 42, "y": "77", "z": 0.1}')
		equals: (Dictionary newFromPairs: {
			'x'. 42.
			'y'. '77'.
			'z'. 0.1
		}).


	self assert: (self encode: Dictionary new) = '{}'.

	self assert: (self encode: (Dictionary newFromPairs: {
			'foo'. 'bar'
		})) equals: '{"foo": "bar"}'.

	self assert: (self encode: (Dictionary newFromPairs: {
			'stuff'. #(() 42 'hello')
		})) equals: '{"stuff": [[], 42, "hello"]}'.

	self assert: (self encode: (Dictionary newFromPairs: {
			'x'. 42.
			'y'. '77'.
			'z'. 0.1
		})) equals: '{"x": 42, "y": "77", "z": 0.1}'.

	self should: [self decode: '{'] raise: Error.
	self should: [self decode: '{]'] raise: Error.
	self should: [self decode: '[{]'] raise: Error.
	self should: [self decode: '{"a"}'] raise: Error.
	self should: [self decode: '{42: "hello"}'] raise: Error.
	self should: [self decode: '{"a" : 42,}'] raise: Error.
	self should: [self decode: '{"a" : 42 "b": 33}'] raise: Error.

	self should: [self encode: (Dictionary newFromPairs: {1. 1})] raise: Error.! !

!WebClientServerTest methodsFor: 'tests - json' stamp: 'ar 2/10/2012 13:47'!
testNilTrueFalse
	"Test encodings of nil, true, false"

	self assert: (self decode: 'true') = true.
	self assert: (self decode: 'false') = false.
	self assert: (self decode: 'null') = nil.

	self assert: (self encode: true) = 'true'.
	self assert: (self encode: false) = 'false'.
	self assert: (self encode: nil) = 'null'.

	self should: [self decode: 'nul'] raise: Error.
	self should: [self decode: 'nullll'] raise: Error.
	self should: [self decode: 'tru'] raise: Error.
	self should: [self decode: 'falsef'] raise: Error.
	self should: [self decode: 'truefalse'] raise: Error.
! !

!WebClientServerTest methodsFor: 'tests - json' stamp: 'topa 4/20/2015 14:49'!
testNumbers
	"Test the encodings of numbers"

	self assert: 42 equals: (self decode: '42').
	self assert: -123 equals: (self decode: '-0123').
	self assert: 42.3 equals: (self decode: '42.3').
	self assert: -42.3e44 equals: (self decode: '-42.3e44').
	self assert: -0.0 equals: (self decode: '-0.0e0').

	self assert: '42' equals: (self encode: 42).
	self assert: '-123' equals: (self encode: -123).
	self assert: '42.3' equals: (self encode: 42.3).
	self assert: '-4.23e45' equals: (self encode: -42.3e44).
	self assert: '-0.0' equals: (self encode: -0.0e0).

	self should: [self decode: '0x123'] raise: Error.
	self should: [self decode: '-.e'] raise: Error.
! !

!WebClientServerTest methodsFor: 'tests - json' stamp: 'jmv 6/26/2017 19:58:18'!
testStrings
	"Test string encodings"

	self assert: (self decode: '"Hello World"') = 'Hello World'.
	self assert: (self decode: '"\"Hello World\""') = '"Hello World"'.
	self assert: (self decode: '"foo\\bar\/baz"') = 'foo\bar/baz'.
	self assert: (self decode: '""') = ''.
	self assert: (self decode: '"foo \u0026 bar"') = 'foo & bar'.
	self assert: (self decode: '"\r\n"') = String crlfString.
	self assert: (self decode: '"\u041F\u0440\u0430\u0432\u0434\u0430"')
		equals: ( '&#x041F;&#x0440;&#x0430;&#x0432;&#x0434;&#x0430;' ).

	self assert: (self encode: 'Hello World') = '"Hello World"'.
	self assert: (self encode: '"Hello World"') = '"\"Hello World\""'.
	self assert: (self encode: 'foo\bar/baz') = '"foo\\bar\/baz"'.
	self assert: (self encode: '') = '""'.
false ifTrue: [
	self assert: (self encode: 'foo &#x100; bar') = '"foo \u0101 bar"' ].
	self assert: (self encode: String crlfString) = '"\r\n"'.

	self assert: (self encode: (String fromUtf8: #[208 159 209 128 208 176 208 178 208 180 208 176]))
		equals:  '"&#1055;&#1088;&#1072;&#1074;&#1076;&#1072;"' .

	self should: [self decode: '"hello'] raise: Error.
	self should: [self decode: '"\'] raise: Error.
	self should: [self decode: '"\"'] raise: Error.! !

!WebClientServerTest methodsFor: 'setup' stamp: 'SvenVanCaekenberghe 6/14/2010 15:49'!
localHostUrl
	^'http://localhost:', self port asString! !

!WebClientServerTest methodsFor: 'setup' stamp: 'fbs 4/4/2013 22:25'!
port
	"Use a random port to minimise chances of concurrently running test suites clashing."
	^ port
		ifNil: [port := (10000 to: 50000) atRandom]! !

!WebClientServerTest methodsFor: 'setup' stamp: 'ar 2/10/2012 13:27'!
setUp
	server := WebServer new listenOn: self port.
	server passwordAt: 'user' realm: 'test' put: 'pass'.
! !

!WebClientServerTest methodsFor: 'setup' stamp: 'ar 2/10/2012 13:27'!
tearDown
	server ifNotNil:[server destroy].
! !

!WebClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 21:56'!
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

!WebClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/11/2010 21:20'!
testHmacSha1
	"Test the SHA1 HMAC algorithm"

	| key message |
	key := 'MCD8BKwGdgPHvAuvgvz4EQpqDAtx89grbuNMRd7Eh98&'.
	message := 'POST&https%3A%2F%2Fapi.twitter.com%2Foauth%2Frequest_token&oauth_callback%3Dhttp%253A%252F%252Flocalhost%253A3005%252Fthe_dance%252Fprocess_callback%253Fservice_provider_id%253D11%26oauth_consumer_key%3DGDdmIQH6jhtmLUypg82g%26oauth_nonce%3DQP70eNmVz8jvdPevU3oJD2AfF7R7odC2XJcn4XlZJqk%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1272323042%26oauth_version%3D1.0'.

	self assert: (WebUtils hmacSha1: message key: key) base64Encoded = '8wUi7m5HFQy76nowoCThusfgB+Q='! !

!WebClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:03'!
testSignOAuthGet
	"Ensure that we can sign a request correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := WebRequest new.
	request method: 'GET'.
	WebUtils oAuthSign: request 
				url: 'http://host.net/resource'
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="dSI3zjYnriSnaB787UH2NDcS8Ss%3D"'.
! !

!WebClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:04'!
testSignOAuthGetDupFields
	"Ensure that we can sign a request with duplicate fields correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := WebRequest new.
	request method: 'GET'.
	WebUtils oAuthSign: request 
				url: 'http://host.net/resource?name=value&name=value'
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="30p67coAX8YbxCKAGfaimydYK6g%3D"'.
! !

!WebClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:07'!
testSignOAuthGetFields
	"Ensure that we can sign a GET request with fields correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := WebRequest new.
	request method: 'GET'.
	WebUtils oAuthSign: request 
				url: 'http://host.net/resource?name=value'
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="N6TOtNK6h3u9zqjqaF2kgpIVb%2F8%3D"'.
! !

!WebClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:04'!
testSignOAuthPostFields
	"Ensure that we can sign a POST request with fields correctly using OAuth.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| request |
	request := WebRequest new.
	request method: 'POST'.
	WebUtils oAuthSign: request 
				url: 'http://host.net/resource'
				extra:{'name' -> 'value'}
				using: self oAuthParams.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="abcd",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="E7yVjmf%2F8UTF9ij15CtbBBhulMw%3D"'.
! !

!WebClientServerTest methodsFor: 'tests - oauth' stamp: 'ar 8/23/2010 22:04'!
testSignOAuthUrlEncoding
	"Ensure that we can sign a request requiring url-encoded oauth params.
	Example values taken from 
		http://oauth.googlecode.com/svn/code/javascript/example/signature.html
	"

	| params request |
	"Consumer Key with url-encoded characters"
	params := self oAuthParams.
	params at: 'oauth_consumer_key' put: 'key with spaces'.

	request := WebRequest new.
	request method: 'GET'.
	WebUtils oAuthSign: request 
				url: 'http://host.net/resource'
				using: params.
	self assert: (request headerAt: 'Authorization')
		equals: 'OAuth oauth_consumer_key="key%20with%20spaces",oauth_nonce="FWNkVaRJVzE",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1281668113",oauth_token="ijkl",oauth_version="1.0",oauth_signature="%2FxrAsx0Utt3V6ZbX00jWWpkqrvg%3D"'.! !

!WebClientServerTest methodsFor: 'tests - auth' stamp: 'ar 2/10/2012 13:26'!
testAuthException
	"Test client and server handling of digest auth"

	| resp client |
	server addService: '/test/auth' action:[:req | 
		server authenticate: req realm: 'test' methods: #(digest) do:[
			req send200Response: 'ok'
		].
	].

	client := WebClient new.
	self should:[client httpGet: self localHostUrl, '/test/auth'] 
		raise: WebAuthRequired.

	client allowAuth: false.
	self shouldnt:[resp := client httpGet: self localHostUrl, '/test/auth'] 
		raise: WebAuthRequired.
	self assert: resp code = 401.
! !

!WebClientServerTest methodsFor: 'tests - auth' stamp: 'ar 2/10/2012 13:53'!
testBasicAuth
	"Test client and server basic auth"

	| resp reqHeader |
	server addService: '/test/auth' action:[:req | 
		server authenticate: req realm: 'test' methods: #(basic) do:[
			reqHeader := req headerAt: 'Authorization'.
			req send200Response: 'ok'
		].
	].
	resp := WebClient new httpGet: self localHostUrl, '/test/auth'.
	self assert: resp code = 401.

	[resp := WebClient httpGet: self localHostUrl, '/test/auth'] 
		on: WebAuthRequired do:[:ex| ex username: 'squeak' password: 'squeak'].
	self assert: resp code = 401.

	[resp := WebClient httpGet: self localHostUrl, '/test/auth']
		on: WebAuthRequired do:[:ex| ex username: 'user' password: 'pass'].
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (reqHeader beginsWith: 'Basic ').
! !

!WebClientServerTest methodsFor: 'tests - auth' stamp: 'ar 2/10/2012 13:53'!
testDigestAuth
	"Test client and server handling of digest auth"

	| resp reqHeader |
	server addService: '/test/auth' action:[:req | 
		server authenticate: req realm: 'test' methods: #(digest) do:[
			reqHeader := req headerAt: 'Authorization'.
			req send200Response: 'ok'
		].
	].

	resp := WebClient new httpGet: self localHostUrl, '/test/auth'.
	self assert: resp code = 401.

	[resp := WebClient httpGet: self localHostUrl, '/test/auth']
		on: WebAuthRequired do:[:ex| ex username: 'squeak' password: 'squeak'].
	self assert: resp code = 401.

	[resp := WebClient httpGet: self localHostUrl, '/test/auth']
		on: WebAuthRequired do:[:ex| ex username: 'user' password: 'pass'].
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (reqHeader beginsWith: 'Digest ').
! !

!WebClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:29'!
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
	client := WebClient new.
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

!WebClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 8/10/2010 09:23'!
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

	resp := WebClient httpGet: self localHostUrl, '/302'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.

	resp := WebClient httpGet: self localHostUrl, '/307'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.

	resp := WebClient httpPost: self localHostUrl, '/302' content: '' type: 'text/plain'.
	self assert: resp code = 302.
	resp := WebClient httpPost: self localHostUrl, '/307' content: '' type: 'text/plain'.
	self assert: resp code = 307.

	resp := WebClient httpPost: self localHostUrl, '/302' content: '' type: 'text/plain'.
	self assert: resp code = 302.
	resp := WebClient httpPost: self localHostUrl, '/307' content: '' type: 'text/plain'.
	self assert: resp code = 307.

	resp := WebClient httpPut: self localHostUrl, '/302' content: '' type: 'text/plain'.
	self assert: resp code = 302.
	resp := WebClient httpPut: self localHostUrl, '/307' content: '' type: 'text/plain'.
	self assert: resp code = 307.

	resp := WebClient httpDelete: self localHostUrl, '/302'.
	self assert: resp code = 302.
	resp := WebClient httpDelete: self localHostUrl, '/307'.
	self assert: resp code = 307.

! !

!WebClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 2/10/2012 13:21'!
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

	client := WebClient new.
	client allowRedirect: false.
	url :=  self localHostUrl, '/action/foo/bar?string=hello&number=42'.
	[[resp := client httpGet: url] on: WebAuthRequired 
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

!WebClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:32'!
testRedirect
	"Test client and server handling of redirects"

	| resp client |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send200Response: 'redirect ok'
	].

	client := WebClient new.
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

!WebClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:32'!
testRedirectLoop
	"Test client handling of redirect loops"

	| resp |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send302Response: '/foo'
	].

	resp := WebClient httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 302.
! !

!WebClientServerTest methodsFor: 'tests - redirect' stamp: 'ar 5/7/2010 20:33'!
testRedirectTrailingSlash
	"Special test to ensure that a redirect from /foo to /foo/ works"

	| resp client |
	server addService: '/foo' action:[:req | 
		req send302Response: '/foo/'
	].
	server addService: '/foo/' action:[:req | 
		req send200Response: 'redirect ok'
	].

	client := WebClient new.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp content = 'redirect ok'.
! !

!WebClientServerTest methodsFor: 'tests - chunked' stamp: 'ar 8/31/2010 23:17'!
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
	response := WebClient new httpGet: self localHostUrl, '/recv'.
	self assert: response code = 200.
	self assert: (response headerAt: 'Transfer-Encoding') = 'chunked'.
	self assert: response nextChunk equals: 'Initial response'.

	"Establish the client request stream"
	WebClient 
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

!WebClientServerTest methodsFor: 'tests - chunked' stamp: 'ar 8/31/2010 22:07'!
testChunkedRequest
	"Test HTTP post using chunked transfer-encoding"

	| resp request |
	server addService: '/test' action:[:req | 
		request := req.
		req send200Response: req content].

	resp := WebClient 
				httpPostChunked: self localHostUrl, '/test' 
				content:[:req|
					req nextChunkPut: 'Hello'.
					req nextChunkPut: 'World'.
					req nextChunkPut: 'Dude'.
				] type: nil.

	self assert: resp code = 200.
	self assert: resp content = 'HelloWorldDude'.
	self assert: (request headerAt: 'Transfer-Encoding') = 'chunked'! !

!WebClientServerTest methodsFor: 'tests - chunked' stamp: 'ar 8/31/2010 22:17'!
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

	resp := WebClient httpGet: self localHostUrl, '/test'.
	self assert: resp code = 200.
	self assert: resp content = 'HelloWorldDude'.
	self assert: (resp headerAt: 'Transfer-Encoding') = 'chunked'.! !

!WebClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/5/2010 19:28'!
testCookieDomainRules
	"Test cookie domain rules"

	"Request host matches domain"
	self assert: (WebClient new 
			acceptCookie: (WebCookie new domain: 'www.domain.com') 
			host: 'www.domain.com' 
			path: '/').

	"Request host matches subdomain"
	self assert: (WebClient new 
			acceptCookie: (WebCookie new domain: '.domain.com') 
			host: 'www.domain.com' 
			path: '/').

	"Request host does not match domain"
	self deny: (WebClient new 
			acceptCookie: (WebCookie new domain: 'www.domain.com') 
			host: 'ftp.domain.com' 
			path: '/').

	"Request host does not match subdomain"
	self deny: (WebClient new 
			acceptCookie: (WebCookie new domain: '.domain.com') 
			host: 'www.someother.org' 
			path: '/').

	"Request host has an extra subdomain"
	self deny: (WebClient new 
			acceptCookie: (WebCookie new domain: '.domain.com') 
			host: 'foo.bar.domain.com' 
			path: '/').
! !

!WebClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/21/2011 11:36'!
testCookieExpiryParsing
	"Test parsing of cookie expiry dates"

	| exp a b |
	exp := DateAndTime date: Date today time: Time now.
	a := WebCookie new.
	a name: 'test'.
	a expiry: exp.
	b := WebCookie new readFrom: a asString readStream.
	self assert: b expiry = exp asUTC.

	b := WebCookie new readFrom: (a asString copyReplaceAll: '-' with: ' ') readStream.
	self assert: b expiry = exp asUTC.
! !

!WebClientServerTest methodsFor: 'tests - cookies' stamp: 'jmv 6/26/2017 19:41:44'!
testCookieParsing
	"Test client and server handling of cookies"

	| resp client |
	"Sends the testcookie back if it's set"
	server addService: '/' action:[:req |
		req send200Response:(String streamContents:[:s|
			req cookiesDo:[:key :val| s nextPutAll: key, '=', val; cr].
		]).
	].

	client := WebClient new.
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

	client := WebClient new.
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
				'y=55', String crString.! !

!WebClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/5/2010 19:36'!
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

	client := WebClient new.
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

!WebClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 8/5/2010 19:36'!
testDuplicateCookies
	"Tests deleting cookies in WebClient"

	| resp client |
	"Sends the testcookie back if it's set"
	server addService: '/' action:[:req | 
		req send200Response: (req headersAt: 'cookie') size asString.
	].

	"Sets the cookie"
	server addService: '/cookie/set' action:[:req | 
		req send200Response: 'ok' contentType: 'text/plain' 
			do:[:reply| reply setCookie: 'testcookie' value: '123' path: '/cookie']].

	client := WebClient new.
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

!WebClientServerTest methodsFor: 'tests - cookies' stamp: 'ar 5/11/2010 20:42'!
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

	client := WebClient new.
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

	client := WebClient new.
	resp := client httpGet: self localHostUrl, '/setcookie?domain=.foo.com'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.

	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self deny: resp content = '123'.

	client := WebClient new.
	resp := client httpGet: self localHostUrl, '/setcookie?domain=.com'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.
	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self deny: resp content = '123'.

	client := WebClient new.
	resp := client httpGet: self localHostUrl, '/setcookie?secure=true'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
	self assert: (resp setCookieAt: 'testcookie') value = '123'.
	resp := client httpGet: self localHostUrl, '/'.
	self assert: resp code = 200.
	self deny: resp content = '123'.
! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 7/20/2010 20:38'!
testDecodeWebSocketKey
	"Ensure that decoding the Sec-WebSocket-Key fields works properly."

	| key1 key2 data |
	key1 := 155712099.
	key2 := 173347027.
	data := 'Tm[K T2u'.

	self assert: (WebUtils extractWebSocketKey: '18x 6]8vM;54 *(5:  {   U1]8  z [  8') = key1.
	self assert: (WebUtils extractWebSocketKey: '1_ tx7X d  <  nw  334J702) 7]o}` 0') = key2.

	self assert: (WebUtils webSocketHandshake: key1 with: key2 with: data) asString = 'fQJ,fN/4F4!!~K~MH'.! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 10:29'!
testWebSocketHash07
	"self run: #testWebSocketHash07"
	"From http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07"

	| hash key |
	key := 'dGhlIHNhbXBsZSBub25jZQ=='.
	hash := WebUtils webSocketHash07: key.
	self assert: hash = 's3pPLMBiTxaQ9kYGzzhZRbK+xOo='.! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets
	"Test the WebSocket interface"

	self testWebSockets:[
		WebClient webSocketTo: self localHostUrl,'/websockets'.
	].
! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets00
	"Test the WebSocket interface"

	self testWebSockets:[
		WebClient webSocket00: self localHostUrl,'/websockets' protocol: 'sample'
	].
! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets07
	"Test the WebSocket interface"

	self testWebSockets:[
		WebClient webSocket07: self localHostUrl,'/websockets' protocol: 'sample'
	].
! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 11/17/2011 17:28'!
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

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'jmv 6/26/2017 19:39:28'!
testWebSockets07ControlInterleave
	"Test the WebSocket 07 control interleave"

	| sema cws resp ping pong |
	ping := 'Hello Ping'.
	server addService: '/websockets' action:[:req | | sws |
		sws := req asWebSocket.
		sws onMessage:[:sdata| sws send: sdata].
		sws onPing:[:sdata| sws pong: sdata].
		sws onError:[:ex| 
			Transcript cr; show: ex description.
			Transcript cr; show: ex signalerContext longStack.
		].
		sws run.
	].

	sema := Semaphore new.
	cws := WebClient webSocket07: self localHostUrl,'/websockets' protocol: nil.
	cws onMessage:[:cdata| resp := cdata. sema signal].
	cws onPong:[:cdata| pong := cdata asString. sema signal.].
	cws fork.

	cws send: 'Ûber-cool'.
	sema wait.
	self assert: (resp = 'Ûber-cool').

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

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 11/17/2011 17:23'!
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

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/27/2011 13:48'!
testWebSockets07NoMask
	"Test the WebSocket interface"

	self testWebSockets:[
		(WebClient webSocket07: self localHostUrl,'/websockets' protocol: 'sample')
			masking: false;
			yourself].
! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:07'!
testWebSockets68
	"Test the WebSocket interface"

	self testWebSockets:[
		WebClient webSocket68: self localHostUrl,'/websockets' protocol: 'sample'.
	].
! !

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'ar 9/26/2011 12:50'!
testWebSockets: aBlock
	"Test the WebSocket interface"

	| sema cws resp sws |
	server addService: '/websockets' action:[:req |
		sws := req asWebSocket.
		sws onError:[:ex| 
			Transcript cr; show: ex description.
			Transcript cr; show: ex signalerContext longStack.
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

!WebClientServerTest methodsFor: 'tests - websockets' stamp: 'jmv 6/26/2017 19:39:38'!
testWebSocketsFraming
	"Test the WebSocket 00 framing"

	| sema cws resp frameType |
	server addService: '/websockets' action:[:req | | sws |
		sws := req asWebSocket.
		sws onMessage:[:sdata :type| sws send: sdata type: type].
		sws onError:[:ex| 
			Transcript cr; show: ex description.
			Transcript cr; show: ex signalerContext longStack.
		].
		sws run.
	].

	sema := Semaphore new.
	cws := WebClient webSocket00: self localHostUrl,'/websockets' protocol: nil.
	cws onMessage:[:cdata :type| resp := cdata. frameType := type. sema signal].
	cws onClose:[resp := nil. frameType := 255. sema signal].
	cws fork.

	cws send: 'Ûber-cool'.
	sema wait.
	self assert: (resp = 'Ûber-cool').
	self assert: (frameType = 0).

	cws send: 'Ûber-funny' type: 15.
	sema wait.
	self assert: (resp = 'Ûber-funny').
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

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:30'!
testDefault404
	"Test that a new server responds with 404 to anything"

	| resp |
	resp := WebClient httpGet: self localHostUrl.
	self assert: resp code = 404.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 6/1/2010 20:22'!
testHtmlSubmit
	"Ensure that we have round-trip conversion for html submit"

	| fieldDict fieldList |
	fieldDict := Dictionary newFromPairs: {'foo'. 123. 'bar'. 'yoho'}.
	fieldList := {'foo' -> 123. 'bar' -> 'yoho'}.
	self testHtmlSubmitUsing: fieldDict.
	self testHtmlSubmitUsing: fieldList.! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 6/1/2010 20:22'!
testHtmlSubmitUsing: fields
	"Ensure that we have round-trip conversion for html submit"

	| resp |
	server addService: '/fields' action:[:req | 
		req send200Response: (String streamContents:[:s|
			req fields keys asArray sort do:[:key | s cr; nextPutAll: key,'=', (req fields at: key)].
		]).
	].
	resp := WebClient htmlSubmit: (self localHostUrl, '/fields') fields: fields.
	self assert: resp code = 200.
	self assert: resp content = '
bar=yoho
foo=123'.

	resp := WebClient htmlSubmit: (self localHostUrl, '/fields') fields: fields method: 'POST'.
	self assert: resp code = 200.
	self assert: resp content = '
bar=yoho
foo=123'.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'dtl 7/17/2012 19:40'!
testListenOnInterface
	"Test that listening on a particular interface listens only on that interface"

	| localHostAddr resp localHostName client |
	server destroy.		"kill old server"

	localHostAddr := NetNameResolver localHostAddress.
	localHostAddr asByteArray = #(127 0 0 1) asByteArray ifTrue:[^self]. "skip test"

	localHostName := NetNameResolver stringFromAddress: localHostAddr.

	server := WebServer new listenOn: self port interface: localHostAddr.
	server addService: '/' action:[:req| req send200Response: 'ok'].

	client := WebClient new.
	client timeout: 1.
	self should:[resp := client httpGet: self localHostUrl, '/test'] raise: Error.

	resp := client httpGet: 'http://', localHostName, ':', self port asString, '/test'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'jmv 6/26/2017 20:00:44'!
testLogging200
	"Test logging a regular 200 a-ok response"

	| resp client log |
	server addService: '/test' action:[:req | req send200Response: 'ok'].
	server accessLog: String new writeStream.

	client := WebClient new.
	client accessLog: String new writeStream.
	[resp := client httpGet: self localHostUrl, '/test'] ensure:[client close].

	self assert: resp code = 200.

	log := server accessLog contents.
	self assert: ('127.0.0.1 - - [*] "GET /test HTTP/1.1" 200 2', String newLineString match: log).

	log := client accessLog contents.
	self assert: ('localhost - - [*] "GET /test HTTP/1.1" 200 2', String newLineString match: log).
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'jmv 6/26/2017 20:01:17'!
testLogging404
	"Test logging a 404 response"

	| resp client log |
	server accessLog: String new writeStream.
	client := WebClient new.
	client accessLog: String new writeStream.
	[resp := client httpGet: self localHostUrl] ensure:[client close].

	self assert: resp code = 404.

	log := server accessLog contents.
	self assert: ('127.0.0.1 - - [*] "GET / HTTP/1.1" 404 145', String newLineString match: log).

	log := client accessLog contents.
	self assert: ('localhost - - [*] "GET / HTTP/1.1" 404 145', String newLineString match: log).
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:32'!
testNestedAction
	"Test handling of nested actions"

	| resp |
	server addService: '/test' action:[:req | req send200Response: 'ok'].
	server addService: '/test/42' action:[:req | req send200Response: '42'].

	resp := WebClient httpGet: self localHostUrl, '/test'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.

	resp := WebClient httpGet: self localHostUrl, '/test/42'.
	self assert: resp code = 200.
	self assert: resp content = '42'.

	resp := WebClient httpGet: self localHostUrl, '/test/43'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 8/24/2010 21:42'!
testResponseUrl
	"Tests that the response url is pointing to the final location"

	| resp client |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send200Response: 'redirect ok'
	].

	client := WebClient new.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp url = (self localHostUrl, '/bar').

	client := WebClient new.
	client allowRedirect: false.
	resp := client httpGet: self localHostUrl, '/foo'.
	self assert: resp code = 302.
	self assert: resp url = (self localHostUrl, '/foo').
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 11/19/2011 18:20'!
testServerDestroy
	"Tests that connections get nuked when server gets killed"
	
	| client resp |
	server addService: '/test' action:[:req | req send200Response: 'ok'].
	client := WebClient new.
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

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:33'!
testServerError
	"Test server handling of errors"

	| resp |
	server addService: '/test' action:[:req | self error: 'boom'].

	resp := WebClient httpGet: self localHostUrl, '/test'.
	self assert: resp code = 500.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 11/1/2010 21:09'!
testServerRegistry
	"Ensure that the WebServer registry functions as intended"

	| serverA serverB |
	["Create a new server"
	serverA := WebServer forUrl: 'http://foo.bar.com'.
	serverA listenOn: self port+1.
	self assert: serverA notNil.
	self assert: serverA isRunning.

	"Look it up again"
	serverB := WebServer forUrl: 'http://foo.bar.com'.
	self assert: serverA == serverB.

	"Override with a new one (should shutdown serverA)"
	serverB := WebServer newForUrl: 'http://foo.bar.com'.
	self deny: serverA == serverB.
	self deny: serverA isRunning.

	"Destroy (should de-register serverB)"
	serverB destroy.
	self assert: (WebServer forUrl: 'http://foo.bar.com' ifAbsent:[nil]) == nil
	] ensure:[
		serverA ifNotNil:[serverA destroy].
		serverB ifNotNil:[serverB destroy].
	].! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 6/14/2010 21:06'!
testSimpleServerAction
	"Test client and server handling simple defaults"

	| resp srvr |
	server addService: '/test' action:[:req |
		srvr := req server.
		req send200Response: 'ok'].

	resp := WebClient httpGet: self localHostUrl, '/test'.
	self assert: srvr notNil.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 5/7/2010 20:33'!
testStreaming
	"Run a test for a full auth-redirect-cookie loop"

	| resp amount stream |
	amount := 128*1024.
	server addService: '/streaming' action:[:req|
		req stream200Response: (String new: amount) readStream size: amount.
	].
	resp := WebClient httpGet: self localHostUrl, '/streaming'.
	self assert: resp isSuccess.
	stream := WriteStream on: String new.
	resp streamTo: stream size: resp contentLength progress: nil.
	self assert: stream position = amount.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 6/19/2010 00:05'!
testTransientPostContent
	"Ensure that WebRequest>>content doesn't close the socket 
	when used in a transient post request"

	| resp client |
	server addService: '/test' action:[:req |
		req send200Response: req content].

	client := WebClient new.
	resp := client httpPost: self localHostUrl, '/test' content:'hello' type: nil do:[:req|
		req protocol: 'HTTP/1.0'
	].
	self assert: resp code = 200.
	self assert: resp content = 'hello'.
! !

!WebClientServerTest methodsFor: 'tests - misc' stamp: 'ar 9/4/2010 11:35'!
testUrlEncoding
	"Test the default URL encoding behavior"
	| resp |
	server addService: '/hello world' action:[:req|
		req send200Response: 'ok'.
	].
	resp := WebClient httpGet: self localHostUrl, '/hello%20world'.
	self assert: resp code = 200.
	self assert: resp content = 'ok'.

	resp := WebClient httpGet: (WebUtils urlEncode: self localHostUrl, '/hello world').
	self assert: resp code = 200.
	self assert: resp content = 'ok'.
! !

!WebClientServerTest methodsFor: 'tests - fields' stamp: 'ar 7/20/2010 20:12'!
testGetFields
	"Test client and server handling simple fields"

	| resp |
	server addService: '/fields' action:[:req | 
		req send200Response: (String streamContents:[:s|
			req fields keys asArray sort do:[:key | s cr; nextPutAll: key,'=', (req fields at: key)].
		]).
	].

	resp := WebClient httpGet: self localHostUrl, '/fields?foo=123&bar=yoho'.
	self assert: resp code = 200.
	self assert: resp content = '
bar=yoho
foo=123'.
! !

!WebClientServerTest methodsFor: 'tests - fields' stamp: 'ar 7/20/2010 20:12'!
testMultipartFields
	"Test client and server handling multipart/form-data fields"

	| resp |
	server addService: '/fields' action:[:req | 
		req send200Response: (String streamContents:[:s|
			req fields keys asArray sort do:[:key | s cr; nextPutAll: key,'=', (req fields at: key)].
		]).
	].

	resp := WebClient httpPost: self localHostUrl,'/fields'  multipartFields: {
		'foo' -> 123.
		'bar' -> 'yoho'.
	}.
	self assert: resp code = 200.
	self assert: resp content = '
bar=yoho
foo=123'.
! !

!WebClientServerTest methodsFor: 'tests - fields' stamp: 'ar 7/20/2010 20:11'!
testPostFields
	"Test client and server handling fields in url-encoded encoded post requests"

	| resp |
	server addService: '/fields' action:[:req | 
		req send200Response: (String streamContents:[:s|
			req fields keys asArray sort do:[:key | s cr; nextPutAll: key,'=', (req fields at: key)].
		]).
	].

	resp := WebClient httpPost: self localHostUrl, '/fields' 
				content: 'foo=123&bar=yoho'
				type: 'application/x-www-form-urlencoded'.
	self assert: resp code = 200.
	self assert: resp content = '
bar=yoho
foo=123'.
! !

!WebClientServerTest methodsFor: 'tests - methods' stamp: 'ar 5/11/2010 20:42'!
testHttpDelete
	"Test for handling a DELETE request"

	| resp |
	server addService: '/nodelete' action:[:req | 
		req send200Response: 'ok'.
	].
	server addService: '/delete' action:[:req | 
		req send200Response: req method.
	] methods: {'GET'. 'PUT'. 'DELETE'}.

	resp := WebClient httpGet: self localHostUrl, '/nodelete'.
	self assert: resp code = 200.
	resp := WebClient httpDelete: self localHostUrl, '/nodelete'.
	self assert: resp code = 405.

	resp := WebClient httpGet: self localHostUrl, '/delete'.
	self assert: resp code = 200.
	resp := WebClient httpDelete: self localHostUrl, '/delete'.
	self assert: resp code = 200.

	resp := WebClient httpPost: self localHostUrl, '/delete' content:'' type: nil.
	self assert: resp code = 405.
! !

!WebClientServerTest methodsFor: 'tests - methods' stamp: 'ar 8/5/2010 22:32'!
testHttpHead
	"Test for handling a HEAD request"

	| resp |
	server addService: '/foo' action:[:req | 
		req send302Response: '/bar'
	].
	server addService: '/bar' action:[:req | 
		req send200Response: 'ok'
	].

	resp := WebClient httpHead: self localHostUrl, '/bar'.
	self assert: resp code = 200.
	self assert: resp content = ''.
	self deny: resp contentStream isDataAvailable..

	resp := WebClient httpHead: self localHostUrl, '/foo'.
	self assert: resp code = 200.
	self assert: resp content = ''.
! !

!WebClientServerTest methodsFor: 'tests - methods' stamp: 'ar 5/11/2010 20:42'!
testHttpOptions
	"Test for handling the OPTIONS request"

	| resp |
	server addService: '/delete' action:[:req | 
		req send200Response: req method.
	] methods: {'GET'. 'PUT'. 'DELETE'}.

	resp := WebClient httpOptions: self localHostUrl, '/*'.
	self assert: resp code = 200.
	self assert: resp content = ''.
	self assert: (resp headerAt: 'allow') = 'HEAD,TRACE,OPTIONS,GET,POST'.

	resp := WebClient httpOptions: self localHostUrl, '/delete'.
	self assert: resp code = 200.
	self assert: resp content = ''.
	self assert: (resp headerAt: 'allow') = 'HEAD,TRACE,OPTIONS,GET,PUT,DELETE'.! !

!WebClientServerTest methodsFor: 'tests - methods' stamp: 'jmv 6/26/2017 19:25:31'!
testHttpTrace
	"Test for handling the TRACE request"

	| resp |
	resp := WebClient httpTrace: self localHostUrl, '/bar'.
	self assert: resp code = 200.
	self assert: resp contentType = 'message/http'.
	self assert: resp content =
		( 'TRACE /bar HTTP/1.1', String crlfString,
		'user-agent: ', WebClient new userAgent, String crlfString,
		'host: localhost:', self port printString, String crlfString, String crlfString	)! !

!WebClientServerTest methodsFor: 'tests - https' stamp: 'ar 7/21/2010 20:59'!
testSchemeHandling
	"Ensure that we handle only http and https schemes"

	self shouldnt:[WebClient httpGet: 'http://localhost:', self port asString] raise: Error.
	self should:[WebClient httpGet: 'ftp://localhost:', self port asString] raise: Error.

! !
