Class {
	#name : 'ReadWriteStreamTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Streams',
	#package : 'BaseImageTests',
	#tag : 'Collections-Streams'
}

{ #category : 'tests' }
ReadWriteStreamTest >> testUpTo1ShortRecords [
	". this test ensures the upTo: delim method behaves as specified by the ANSI standard:
	Delimiter is considered a separator (and therefore not required for the last chunk)."
	| stream |
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: 'record-1Xrecord-2Xrecord-incomplete'.
	stream position: 0.
	self assert:  ((stream upTo: $X) =  'record-1').
	self assert:  ((stream upTo: $X) =  'record-2').
	self assert:  ((stream upTo: $X) =  'record-incomplete').
	self assert:  ((stream upTo: $X) =  '').
	". the stream has been all consumed" 
	self assert:  (stream position = 35). 
]

{ #category : 'tests' }
ReadWriteStreamTest >> testUpTo3LongUnterminatedRecord [
	"Long input, no delimiter found, expected to return all the data chunk up to the end of file. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars! ' ].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: longString.
	stream position: 0.
	read _ stream upTo: $X.
	self assert: read = longString.
]

{ #category : 'tests' }
ReadWriteStreamTest >> testUpTo4LongUnterminatedRecord [
	"Big chunk, not delimiter found, expected return all the chunk  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	read _ stream upTo: $X.
	self assert: read size = 5000.
]

{ #category : 'tests' }
ReadWriteStreamTest >> testUpTo5TerminatedAndUnterminatedLongRecords [
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
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
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
ReadWriteStreamTest >> testUpTo6TerminatedLongRecords [
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
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
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
ReadWriteStreamTest >> testUpToTerminator1ShortRecords [
	"
	. Read a file stream up to 'delim' in a strict way. 
	. If delim is found returns everything up to the first occurrence of 'delim' included.
	. if delim is not found returns nil and set the FileStream position where it was before
	the call was made. This ensures if another process writes into the file another delim
	limited token we will fully read it on next upTo call.
	This means Delimiter is a Terminator: a chunk is only considered well formed if it ends with it.
	"
	| stream |
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: 'record-1Xrecord-2Xrecord-incomplete'.
	stream position: 0.
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((stream upTo: $X delimiterIsTerminator: true) =  nil).
	". we are not at the end of the stream, but just after the last delim was found.
	we are ready to receive other delim limitated tokens. if they get written.
	"
	self assert:  (stream position = 18).
]

{ #category : 'tests' }
ReadWriteStreamTest >> testUpToTerminator3LongUnterminatedRecord [
	"Long input, no delimiter found, expected to return nil. "
	| longString stream read |
	longString _ (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars! ' ].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: longString.
	stream position: 0.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
]

{ #category : 'tests' }
ReadWriteStreamTest >> testUpToTerminator4LongUnterminatedRecord [
	"Big chunk, not delimiter found, expected return nil  "
	| stuff stream read |
	stuff _ String streamContents: [ :strm |
		1
			to: 5000
			do: [ :i |
				i < 3000 ifTrue: [ strm nextPut: $a ].
				i >= 3000 ifTrue: [ strm nextPut: $b ]]].
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
	read _ stream
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
]

{ #category : 'tests' }
ReadWriteStreamTest >> testUpToTerminator5TerminatedAndUnterminatedLongRecords [
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
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
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
ReadWriteStreamTest >> testUpToTerminator6TerminatedLongRecords [
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
	stream _ ReadWriteStream on: ''. 
	stream nextPutAll: stuff.
	stream position: 0.
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
