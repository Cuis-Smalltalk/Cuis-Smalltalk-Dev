Class {
	#name : 'ReadStreamTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Streams',
	#package : 'BaseImageTests',
	#tag : 'Collections-Streams'
}

{ #category : 'tests' }
ReadStreamTest >> testMatchStart [
	| string matchStart |
	string := UnicodeString fromUtf8Bytes: #[57 194 160 226 130 172 64 226 130 172 226 130 172].

	1 to: string size do: [ :i |
		i to: string size do: [ :j |
			matchStart := string readStream matchStart: (string copyFrom: i to: j).
			self assert: (string at: matchStart) = (string at: i)]].
]

{ #category : 'tests' }
ReadStreamTest >> testUpTo1ShortRecords [
	". this test ensures the upTo: delim method behaves as specified by the ANSI standard:
	Delimiter is considered a separator (and therefore not required for the last chunk)."
	| stream |
	stream _ ReadStream on: 'record-1Xrecord-2Xrecord-incomplete'. 
	self assert:  ((stream upTo: $X) =  'record-1').
	self assert:  ((stream upTo: $X) =  'record-2').
	self assert:  ((stream upTo: $X) =  'record-incomplete').
	self assert:  ((stream upTo: $X) =  '').
	". the stream has been all consumed" 
	self assert:  (stream position = 35). 
]

{ #category : 'tests' }
ReadStreamTest >> testUpTo3LongUnterminatedRecord [
	"Long input, no delimiter found, expected to return all the data chunk up to the end of file. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars! ' ].
	stream _ ReadStream on: longString.
	read _ stream upTo: $X.
	self assert: read = longString.
]

{ #category : 'tests' }
ReadStreamTest >> testUpTo4LongUnterminatedRecord [
	"Big chunk, not delimiter found, expected return all the chunk  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	read _ stream upTo: $X.
	self assert: read size = 5000.
]

{ #category : 'tests' }
ReadStreamTest >> testUpTo5TerminatedAndUnterminatedLongRecords [
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time the second block up to EOF.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				i > 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	" first scan, the delimiter is found but not printed.  "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan. the delimiter is not found, all second chunk is returned  "
	read _ stream upTo: $X.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.
]

{ #category : 'tests' }
ReadStreamTest >> testUpTo6TerminatedLongRecords [
	"Two big chunks, one delimiter in the middle, one at the end. expected to return 
      two big chunks in two read, delimiters excluded.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				((Interval
					from: 3001
					to: 5999) includes: i) ifTrue: [ strm nextPut: $b ].
				i = 6000 ifTrue: [ strm nextPut: $X ]]].
	stream _ ReadStream on: stuff.
	" first scan, delimiter is found, return all the block delimiter excluded "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan, return chunk, delimiter excluded.  "
	read _ stream upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $b.
]

{ #category : 'tests' }
ReadStreamTest >> testUpToAll [
	| string prefix |

	prefix := 'https://webmail.gandi.net/SOGo/so/juan@cuis.st/Mail/view#!/Mail/0/INBOX/7020'
		readStream upToAll: '://'.
	self assert: prefix = 'https'.

	string := UnicodeString fromUtf8Bytes: #[57 194 160 226 130 172 64 226 130 172 226 130 172].
	1 to: string size do: [ :i |
		i to: string size do: [ :j |
			prefix := string readStream upToAll: (string copyFrom: i to: j).
			self assert: (string at: prefix size + 1) = (string at: i)]].
]

{ #category : 'tests' }
ReadStreamTest >> testUpToTerminator1ShortRecords [
	"
	. Read a file stream up to 'delim' in a strict way. 
	. If delim is found returns everything up to the first occurrence of 'delim' included.
	. if delim is not found returns nil and set the FileStream position where it was before
	the call was made. This ensures if another process writes into the file another delim
	limited token we will fully read it on next upTo call.
	This means Delimiter is a Terminator: a chunk is only considered well formed if it ends with it.
	"
	| stream |
	stream _ ReadStream on: 'record-1Xrecord-2Xrecord-incomplete'. 
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  nil).
	". we are not at the end of the stream, but just after the last delim was found.
	we are ready to receive other delim limitated tokens. if they get written.
	"
	self assert:  (stream position = 18).
]

{ #category : 'tests' }
ReadStreamTest >> testUpToTerminator3LongUnterminatedRecord [
	"Long input, no delimiter found, expected to return nil. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars! ' ].
	stream _ ReadStream on: longString.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
]

{ #category : 'tests' }
ReadStreamTest >> testUpToTerminator4LongUnterminatedRecord [
	"Big chunk, not delimiter found, expected return nil  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
]

{ #category : 'tests' }
ReadStreamTest >> testUpToTerminator5TerminatedAndUnterminatedLongRecords [
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time nil.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				i > 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadStream on: stuff.
	" first scan, delimiter is found, return all the block delimiter included "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter not found, returns nil  "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
]

{ #category : 'tests' }
ReadStreamTest >> testUpToTerminator6TerminatedLongRecords [
	"Two big chunks, one delimiter in the middle, one at the end expected to return 
      two big chunks in two read, delimiters included.  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 6000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i = 3000 ifTrue: [ strm nextPut: $X ].
				((Interval
					from: 3001
					to: 5999) includes: i) ifTrue: [ strm nextPut: $b ].
				i = 6000 ifTrue: [ strm nextPut: $X ]]].
	stream _ ReadStream on: stuff.
	" first scan, delimiter is found, return all the block delimiter included "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter found, return chunk, delimiter included   "
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.
]
