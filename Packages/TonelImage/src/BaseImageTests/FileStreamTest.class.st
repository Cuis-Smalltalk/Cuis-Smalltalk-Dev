Class {
	#name : 'FileStreamTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Files',
	#package : 'BaseImageTests',
	#tag : 'System-Files'
}

{ #category : 'tests' }
FileStreamTest >> testAtEnd [
	| path fs fragment1 fragment2 |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	
	fragment1 := 'Some stuff.'.
	path asFileEntry fileContents: fragment1.
	fs := path asFileEntry readStream.
	self deny: fs atEnd.
	self assert: fs upToEnd =  'Some stuff.'.
	self assert: fs atEnd.
	self assert: fs size = fragment1 size.

	fragment2 := 'Some stuff'.
	path asFileEntry appendContents: fragment2.
	self assert: fs size = (fragment1 size + fragment2 size).
	self deny: fs atEnd.
	self assert: fs upToEnd =  fragment2.
	self assert: fs atEnd.
	
	fs close .
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpTo1ShortRecords [
	". this test ensures the upTo: delim method behaves as specified by the ANSI standard:
	Delimiter is considered a separator (and therefore not required for the last chunk)."
	| path fs |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	path asFileEntry fileContents: 'record-1Xrecord-2Xrecord-incomplete'. 
	fs := path asFileEntry readStream . 
	self assert:  ((fs upTo: $X) =  'record-1').
	self assert:  ((fs upTo: $X) =  'record-2').
	self assert:  ((fs upTo: $X) =  'record-incomplete').
	self assert:  ((fs upTo: $X) =  '').
	". the stream has been all consumed" 
	self assert:  (fs position = 35). 
	fs close . 
	path asFileEntry delete.

	
]

{ #category : 'tests' }
FileStreamTest >> testUpTo2IncompleteRecords [
	"
	. This test stresses the motivation for introducing upTo:delimiterIsTerminator:
	. if two separate processes are reading and writing records into the same file 
	upTo can potentially create a bit a of a mess by reading in half records and complete records.
	. please compare to the equivalent testUpToTerminatorX to see what is probably the behaviour your are looking for 
	"
	| path fs |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. } .
	" a process starts to write data into a file but it does not end cleanly the writing "
	path asFileEntry fileContents: 'record-1Xrec'. 
	fs := path asFileEntry readStream . 
	" upTo reads 'record-1' and the second time reads half a token "
	self assert:  ((fs upTo: $X) =  'record-1').
	self assert:  ((fs upTo: $X) =  'rec').
	". the writing process comes back online and terminates its record writing "
	path asFileEntry appendContents: 'ord-2Xrecord-3X'.
	". upTo reads  an half token and then a complete one  "
	self assert:  ((fs upTo: $X) =  'ord-2').
	self assert:  ((fs upTo: $X) =  'record-3').
	". when there is nothing more to read upTo returns the empty string " 
	self assert:  ((fs upTo: $X) =  '').
	fs close .
	path asFileEntry delete.

	
]

{ #category : 'tests' }
FileStreamTest >> testUpTo3LongUnterminatedRecord [
	"Long input, no delimiter found, expected to return all the data chunk up to the end of file. "
	| path longString fs read |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	longString := (1 to: 100)
		inject: ''
		into: [ :prev :each |
			prev , 'A lot of stuff, needs over 2000 chars! ' ].
	path asFileEntry fileContents: longString.
	fs := path asFileEntry readStream.
	read := fs upTo: $X.
	self assert: read = longString.
	fs close.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpTo4LongUnterminatedRecord [
	"Big chunk, not delimiter found, expected return all the chunk  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 5000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i >= 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	read := fs upTo: $X.
	self assert: read size = 5000.
	fs close.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpTo5TerminatedAndUnterminatedLongRecords [
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time the second block up to EOF.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			i > 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, the delimiter is found but not printed.  "
	read := fs upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan. the delimiter is not found, all second chunk is returned  "
	read := fs upTo: $X.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.
	fs close.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpTo6TerminatedLongRecords [
	"Two big chunks, one delimiter in the middle, one at the end. expected to return 
      two big chunks in two read, delimiters excluded.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			((Interval
				from: 3001
				to: 5999) includes: i) ifTrue: [ fs nextPut: $b ].
			i = 6000 ifTrue: [ fs nextPut: $X ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, delimiter is found, return all the block delimiter excluded "
	read := fs upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $a.
	" second scan, return chunk, delimiter excluded.  "
	read := fs upTo: $X.
	self assert: read size = 2999.
	self assert: (read at: 1) = $b.
	fs close.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpToAll [
	| string path fromFile fromString |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. } .
	string := UnicodeString fromUtf8Bytes: #[57 57 194 160 226 130 172].
	DirectoryEntry userBaseDirectory at: path put: string.
	self assert: ((DirectoryEntry userBaseDirectory at: path) = string).
	fromString := string readStream upToAll: (string copyFrom: 3 to: 4).
	fromFile := (DirectoryEntry userBaseDirectory // path) readStream upToAll: (string copyFrom: 3 to: 4).
	self assert: fromFile = fromString.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpToTerminator1ShortRecords [
	"
	. Read a file stream up to 'delim' in a strict way. 
	. If delim is found returns everything up to the first occurrence of 'delim' included.
	. if delim is not found returns nil and set the FileStream position where it was before
	the call was made. This ensures if another process writes into the file another delim
	limited token we will fully read it on next upTo call.
	This means Delimiter is a Terminator: a chunk is only considered well formed if it ends with it.
	"
	| path fs |
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	path asFileEntry fileContents: 'record-1Xrecord-2Xrecord-incomplete'. 
	fs := path asFileEntry readStream . 
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  nil).
	". we are not at the end of the stream, but just after the last delim was found.
	we are ready to receive other delim limitated tokens. if they get written.
	"
	self assert:  (fs position = 18).
	fs close . 
	path asFileEntry delete .
	
]

{ #category : 'tests' }
FileStreamTest >> testUpToTerminator2IncompleteRecords [
	"
	. This test stresses the motivation for introducing upTo:delimiterIsTerminator: 
	. if two separate processes are reading and writing records into the same file 
	upTo:delimiterIsTerminator:true will not loose any token and will never return half written tokens.
	"
	|path fs|
	path := 'test-{1}.txt' format: {(Float pi * 10e10) floor atRandom. }.
	" a process starts to write data into a file but it does not end cleanly the writing "
	path asFileEntry fileContents: 'record-1Xrec'. 
	fs := path asFileEntry readStream . 
	" upTo:strict reads upto what it can find and ignores the rest "
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-1X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  nil).
	". the writing process comes back online and terminates its record writing "
	path asFileEntry appendContents: 'ord-2Xrecord-3X'.
	". upTo:strict is ready and reads just completed tokens and brand new ones.  "
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-2X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  'record-3X').
	self assert:  ((fs upTo: $X delimiterIsTerminator: true) =  nil).
	fs close .
	path asFileEntry delete.

	
]

{ #category : 'tests' }
FileStreamTest >> testUpToTerminator3LongUnterminatedRecord [
	"Long input, no delimiter found, expected to return nil. "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	path asFileEntry fileContents:
		((1 to: 100)
			inject: ''
			into: [ :prev :each |
				prev , 'A lot of stuff, needs over 2000 chars! ' ]).
	fs := path asFileEntry readStream.
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
	fs close.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpToTerminator4LongUnterminatedRecord [
	"Big chunk, not delimiter found, expected return nil  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 5000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i >= 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
	fs close.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpToTerminator5TerminatedAndUnterminatedLongRecords [
	"Two big chunks, one delimiter in the middle, expected to return 
      the first time a big chunk, the second time nil.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			i > 3000 ifTrue: [ fs nextPut: $b ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, delimiter is found, return all the block delimiter included "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter not found, returns nil  "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read = nil.
	fs close.
	path asFileEntry delete.
]

{ #category : 'tests' }
FileStreamTest >> testUpToTerminator6TerminatedLongRecords [
	"Two big chunks, one delimiter in the middle, one at the end expected to return 
      two big chunks in two read, delimiters included.  "
	| path fs read |
	path := 'test-{1}.txt' format:
		{(Float pi * 100000000000) floor atRandom}.
	fs := path asFileEntry forceWriteStream.
	1
		to: 6000
		do: [ :i |
			i < 3000 ifTrue: [ fs nextPut: $a ].
			i = 3000 ifTrue: [ fs nextPut: $X ].
			((Interval
				from: 3001
				to: 5999) includes: i) ifTrue: [ fs nextPut: $b ].
			i = 6000 ifTrue: [ fs nextPut: $X ]].
	fs close.
	fs := path asFileEntry readStream.
	" first scan, delimiter is found, return all the block delimiter included "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $a.
	" second scan, delimiter found, return chunk, delimiter included   "
	read := fs
		upTo: $X
		delimiterIsTerminator: true.
	self assert: read size = 3000.
	self assert: (read at: 1) = $b.
	fs close.
	path asFileEntry delete.
]
