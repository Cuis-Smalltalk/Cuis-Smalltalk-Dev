"
I represent an accessor for a sequence of objects (a collection) that are externally named by indices so that the point of access can be repositioned. I am abstract in that I do not implement the messages next and nextPut: which are inherited from my superclass Stream.
"
Class {
	#name : 'PositionableStream',
	#superclass : 'Stream',
	#instVars : [
		'collection',
		'position',
		'readLimit'
	],
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'instance creation' }
PositionableStream class >> on: aCollection  [
	"Answer a new instance of me, streaming over aCollection."

	^self basicNew on: aCollection
]

{ #category : 'testing' }
PositionableStream >> atEnd [
	"Answer whether the receiver can access any more objects."

	^position >= readLimit
]

{ #category : 'testing' }
PositionableStream >> atStart [
	"Answer whether the receiver is at the start position."

	^position = 0.
]

{ #category : 'reading' }
PositionableStream >> back [
	"Go back one element and return it."
	self position = 0 ifTrue: [ self error: 'CantGoBack' ].
	self skipBack.
	^ self peek
]

{ #category : 'fileIn/Out' }
PositionableStream >> backChunk [
	"Answer the contents of the receiver back to the previous terminator character.
	Doubled delimiter indicate an embedded delimiter character."
	
	| answer ch firstPosition delimiter prevCh count delimiterCount |
	delimiter := $!.
	"Go to end of chunk, not including delimiter."
	ch := self back.
	ch = delimiter ifTrue: [ ch := self back ].

	count := 1.
	prevCh := $$.
	"Go back until non-doubled delimiter, or beginning."
	[ch notNil and: [ ch ~= delimiter or: [self peekBack = delimiter or: [prevCh = delimiter]]]] whileTrue: [
		count := count + 1.
		prevCh := ch.
		ch := self back. ].
	"Skip initial delimiter"
	ch = delimiter ifTrue: [
		self next.
		count := count-1 ].

	"This is where chunk starts. Undouble doubled delimiters."
	firstPosition := self position.
	answer := String streamContents: [ :strm |
		delimiterCount := 0.
		count timesRepeat: [
			ch := self next.
			delimiterCount \\ 2 = 0 ifTrue: [
				strm nextPut: ch ].
			delimiterCount := ch = delimiter
				ifTrue: [ delimiterCount+1]
				ifFalse: [0]]].

	"Re position at start."
	self position: firstPosition.
	^ answer
]

{ #category : 'fileIn/Out' }
PositionableStream >> checkForPreamble: chunk [
	"As we don't support preambles and postscripts in Packages, assume any preamble or postscript belongs in the BaseSystem.
	Note: In packages, replace preamble by prerequisites, and postscript by class initialize methods."

	| changeSet newPreamble newPostscript |
	(chunk beginsWith: '"Change Set:')
		ifTrue: [
			changeSet := ChangeSet changeSetForBaseSystem.
			newPreamble := changeSet preambleString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPreamble |
					oldPreamble, '.', String newLineString, chunk ].
			changeSet preambleString: newPreamble.
			'Preamble added to ChangeSet ', changeSet name.
			].
	(chunk beginsWith: '"Postscript:')
		ifTrue: [
			changeSet := ChangeSet changeSetForBaseSystem.
			newPostscript := changeSet postscriptString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPostscript |
					oldPostscript, '.', String newLineString, chunk ].
			changeSet postscriptString: newPostscript.
			'Postscript added to ChangeSet ', changeSet name.
			].
							

]

{ #category : 'private' }
PositionableStream >> collectionSpecies [
	"Answer the species of collection into which the receiver can stream"
	
	^collection species
]

{ #category : 'fileIn/Out' }
PositionableStream >> compileNextChunk [
		
	(self peekFor: $!) 
		ifTrue: [ self compileNextChunkWhenStartsWithExclamationMark ]
		ifFalse: [ self compileNextChunkWhenDoesNotStartWithExclamationMark ]
]

{ #category : 'fileIn/Out' }
PositionableStream >> compileNextChunkHandlingExceptions  [

	[ self compileNextChunk ]
		on: InMidstOfFileinNotification, UndeclaredVariableWarning, PoolDefinitionNotification 
		do: [ :ex | ex resume: true ]
]

{ #category : 'fileIn/Out' }
PositionableStream >> compileNextChunkWhenDoesNotStartWithExclamationMark [

	| chunk |
	
	chunk := self nextChunk.
	self checkForPreamble: chunk.
	self evaluate: [ Compiler evaluate: chunk logged: true ] printingErrorWith: chunk
		
]

{ #category : 'fileIn/Out' }
PositionableStream >> compileNextChunkWhenStartsWithExclamationMark [

	| chunk |
	
	chunk := self nextChunk.
	
	"These are the ones that should do nothing, 
	because next line is a doit that does the stuff
	(or because it is handled elsewhere)"
	
	(chunk isEmpty or: [(chunk beginsWith: 'description:  ') 
		or: [ ChangeList knownFileInPreambles anySatisfy: [ :aPreamble | chunk beginsWith: aPreamble ]]])
			ifFalse: [
				self evaluate: [ (Compiler evaluate: chunk logged: false) scanFrom: self ] printingErrorWith: chunk ].
]

{ #category : 'reading' }
PositionableStream >> contents [
	"Answer with a copy of my collection from 1 to readLimit."

	^ (collection copyFrom: 1 to: readLimit) asStreamResult
]

{ #category : 'reading' }
PositionableStream >> contentsOfEntireFile [
	"For non-file streams"
	^ self contents
]

{ #category : 'reading' }
PositionableStream >> crLfNextLine [
	"Answer next line (may be empty), or nil if at end.
	Support any line ending convention"

	| answer lineSeparators c |
	self atEnd ifTrue: [^nil].
	lineSeparators := {Character cr. Character lf}.
	answer := self upToAny: lineSeparators.
	c := self peek.
	c = Character cr ifTrue: [self next. c := self peek].
	c = Character lf ifTrue: [self next].
	^answer
]

{ #category : 'fileIn/Out' }
PositionableStream >> evaluate: aBlock printingErrorWith: chunk [

	aBlock
		on: Error
		do: [ :ex |
			ex print.
			('while evaluating: ', chunk) print.
			ex pass ]
				
]

{ #category : 'fileIn/Out' }
PositionableStream >> fileIn [
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimiters. The expressions are read and passed to the 
	Compiler."

	self fileInAnnouncing: 'Reading ' , self name.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ]
]

{ #category : 'fileIn/Out' }
PositionableStream >> fileInAnnouncing: announcement [
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimiters. The expressions are read and passed to the 
	Compiler. Put up a progress report with the given announcement as the title."

	Utilities logsUserChanges: false.

	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :barBlock | self fileInInformingTo: barBlock ].

	"Note:  The main purpose of this banner is to flush the changes file."
	Utilities logsUserChanges: true.
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	
]

{ #category : 'fileIn/Out' }
PositionableStream >> fileInInformingTo: barBlock  [
	
	[ self atEnd ] whileFalse: [
		barBlock value: self position.
		self skipSeparators.
		self compileNextChunkHandlingExceptions ]
]

{ #category : 'testing' }
PositionableStream >> isBinary [
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray
]

{ #category : 'testing' }
PositionableStream >> isEmpty [
	"Answer whether the receiver's contents has no elements."

	"Returns true if both the set of past and future sequence values of
the receiver are empty. Otherwise returns false"

	^ self atEnd and: [position = 0]
]

{ #category : 'testing' }
PositionableStream >> isFileStream [
	^false
]

{ #category : 'testing' }
PositionableStream >> isText [
	"Return true if the receiver is a Text stream"
	^collection is: #Text
]

{ #category : 'positioning' }
PositionableStream >> match: subCollection [
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

	^(self matchStart: subCollection) notNil

]

{ #category : 'positioning' }
PositionableStream >> matchStart: subCollection [
	"Set the access position of the receiver to be past the next occurrence of the subCollection.
	If collection is found, answer match start position. If not, answer nil.
	No wildcards, and case does matter."

	| pattern startForNextMatch matchStart p |
	pattern := subCollection readStream.
	startForNextMatch := nil.
	[pattern atEnd] whileFalse: [
		self atEnd ifTrue: [^ nil].
		p := self position.
		self next = pattern next
			ifTrue: [
				pattern position = 1 ifTrue: [
					startForNextMatch := self position.
					matchStart := p+1 ]]
			ifFalse: [
				pattern position: 0.
				startForNextMatch ifNotNil: [
					self position: startForNextMatch.
					startForNextMatch := nil ]]].
	^matchStart
]

{ #category : 'reading' }
PositionableStream >> next: anInteger  [
	"Answer the next anInteger elements of my collection. Must override 
	because default uses self contents species, which might involve a large 
	collection."

	| newArray |
	newArray := self collectionSpecies new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: self next].
	^newArray
]

{ #category : 'reading' }
PositionableStream >> next: n into: aCollection [
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1
]

{ #category : 'reading' }
PositionableStream >> next: n into: aCollection startingAt: startIndex [
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| obj |
	0 to: n-1 do: [ :i |
		(obj := self next) ifNil: [ ^aCollection copyFrom: 1 to: startIndex+i-1 ].
		aCollection at: startIndex+i put: obj].
	^aCollection
]

{ #category : 'writing' }
PositionableStream >> next: anInteger putAll: aCollection [
	"Store the next anInteger elements from the given collection."
	^self next: anInteger putAll: aCollection startingAt: 1
]

{ #category : 'writing' }
PositionableStream >> next: anInteger putAll: aCollection startingAt: startIndex [
	"Store the next anInteger elements from the given collection."
	(startIndex = 1 and:[anInteger = aCollection size])
		ifTrue:[^self nextPutAll: aCollection].
	^self nextPutAll: (aCollection copyFrom: startIndex to: startIndex+anInteger-1)
]

{ #category : 'reading' }
PositionableStream >> nextAvailable: aNumber [
	^self next: aNumber
]

{ #category : 'fileIn/Out' }
PositionableStream >> nextChunk [
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator |
	terminator := $!.
	^self collectionSpecies streamContents: [ :strm |
		| ch done |
		self skipSeparators.
		done := false.
		[done not and: [(ch := self next) ~~ nil]] whileTrue: [
			done := false.
			(ch = terminator) ifTrue: [
				self peek = terminator ifTrue: [
					self next.  "skip doubled terminator"
				] ifFalse: [
					done := true  "terminator is not doubled; we're done!"
				].
			].
			done ifFalse: [
				strm nextPut: ch ].
		]
	]
]

{ #category : 'reading' }
PositionableStream >> nextInto: aCollection [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
]

{ #category : 'reading' }
PositionableStream >> nextInto: aCollection startingAt: startIndex [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex.
]

{ #category : 'reading' }
PositionableStream >> nextKeyword [
	^self
		untilAnySatisfying: [:c | (c isValidInIdentifiers or: [c == $:]) not]
]

{ #category : 'reading' }
PositionableStream >> nextLine [
	"Answer next line (may be empty), or nil if at end"

"	self atEnd ifTrue: [^nil].
	^self upTo: Character cr
"
	^self crLfNextLine
]

{ #category : 'reading' }
PositionableStream >> nextWordsInto: aBitmap  [
	"Fill the word based buffer from my collection. 
	Stored on stream as Big Endian. Optimized for speed. 
	Read in BigEndian, then restoreEndianness."
	| blt pos source byteSize |
	collection class isBytes
		ifFalse: [^ self next: aBitmap size into: aBitmap startingAt: 1].

	byteSize := aBitmap byteSize.
	"is the test on collection basicSize \\ 4 necessary?"
	((self position bitAnd: 3) = 0 and: [ (collection basicSize bitAnd: 3) = 0])
		ifTrue: [source := collection.
			pos := self position.
			self skip: byteSize]
		ifFalse: ["forced to copy it into a buffer"
			source := self next: byteSize.
			pos := 0].

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt
				toForm: (Form new hackBits: aBitmap))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.

	"And do whatever the bitmap needs to do to convert from big-endian order."
	aBitmap restoreEndianness.

	^ aBitmap 	"May be WordArray, ColorArray, etc"

]

{ #category : 'testing' }
PositionableStream >> notEmpty [
	"Answer whether the receiver contains any elements."

	^ self isEmpty not
]

{ #category : 'private' }
PositionableStream >> on: aCollection [
	"Initialize the stream to read over all of aCollection.
	The collection is considered to have useful content."

	collection := aCollection.
	readLimit := aCollection size.
	position := 0.
	self reset.
]

{ #category : 'accessing' }
PositionableStream >> originalContents [
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection
]

{ #category : 'positioning' }
PositionableStream >> padTo: nBytes put: aCharacter  [
	"Pad using the argument, aCharacter, to the next boundary of nBytes characters."
	| rem |
	rem := nBytes - (self position \\ nBytes).
	rem = nBytes ifTrue: [^ 0].
	self next: rem put: aCharacter.
]

{ #category : 'positioning' }
PositionableStream >> padToNextLongPut: char  [
	"Make position be on long word boundary, writing the padding 
	character, char, if necessary."
	[self position \\ 4 = 0]
		whileFalse: [self nextPut: char]
]

{ #category : 'reading' }
PositionableStream >> peek [
	"Answer what would be returned if the message next were sent to the 
	receiver. If the receiver is at the end, answer nil."

	| nextObject |
	position < readLimit ifTrue: [
		^collection at: position+1 ].
	self atEnd ifTrue: [^nil].
	nextObject := self next.
	position := position - 1.
	^nextObject
]

{ #category : 'reading' }
PositionableStream >> peekBack [
	"Return the element at the previous position, without changing position."

	self position = 0 ifTrue: [ self error: 'CantGoBack' ].
	self skipBack.
	^self next
]

{ #category : 'reading' }
PositionableStream >> peekFor: anObject  [
	"Answer false and do not move the position if self next ~= anObject or if the
	receiver is at the end. Answer true and increment position if self next = anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject := self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position := position - 1.
	^false
]

{ #category : 'positioning' }
PositionableStream >> position [
	"Answer the current position of accessing the stream."
	^position
]

{ #category : 'positioning' }
PositionableStream >> position: anInteger  [
	"Set position to anInteger as long as anInteger is within the bounds of the 
	receiver's contents.  If it is not, cause an error."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [position := anInteger]
		ifFalse: [self positionError]
]

{ #category : 'private' }
PositionableStream >> positionError [
	"Since I am not necessarily writable, it is up to my subclasses to override 
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'
]

{ #category : 'positioning' }
PositionableStream >> reset [
	"Set the receiver's position to the beginning of the sequence of objects."

	position := 0
]

{ #category : 'positioning' }
PositionableStream >> resetContents [
	"Set the position and limits to 0."

	position := 0.
	readLimit := 0
]

{ #category : 'private' }
PositionableStream >> setFrom: newStart to: newStop [

	position := newStart - 1.
	readLimit := newStop
]

{ #category : 'positioning' }
PositionableStream >> setToEnd [
	"Set the position of the receiver to the end of the sequence of objects."

	position := readLimit
]

{ #category : 'positioning' }
PositionableStream >> skip [
	"Skips one object in the receiver's future sequence values."

	self skip: 1
]

{ #category : 'positioning' }
PositionableStream >> skip: n [
	"Skips the next amount objects in the receiver's future sequence values."

	self position: (self position + n min: self size).
]

{ #category : 'positioning' }
PositionableStream >> skipBack [
	"Skips back one object in the receiver's future sequence values."

	self skip: -1.
]

{ #category : 'fileIn/Out' }
PositionableStream >> skipSeparators [
	| prevPosition |
	[self atEnd]
		whileFalse: [
			prevPosition := self position.
			self next isSeparator ifFalse: [
				self position: prevPosition.
				^ self ]]
]

{ #category : 'positioning' }
PositionableStream >> skipTo: anObject  [
	"Set the access position of the receiver to be past the next occurrence of 
	anObject. Answer whether anObject is found."

	[self atEnd]
		whileFalse: [self next = anObject ifTrue: [^true]].
	^false
]

{ #category : 'fileIn/Out' }
PositionableStream >> unCommand [
	"If this read stream is at a <, then skip up to just after the next >.  For removing html commands."
	| char |
	[self peek == $<] whileTrue: ["begin a block"
		[self atEnd == false and: [self next ~~ $>]] whileTrue.
		"absorb characters"
		].
 
]

{ #category : 'reading' }
PositionableStream >> untilAnySatisfying: aBlock [

	^self collectionSpecies streamContents: [ :write |
		[
			self atEnd not and: [
				(aBlock value: self peek) not]]
			whileTrue: [write nextPut: self next]].
]

{ #category : 'gui' }
PositionableStream >> untilEnd: aBlock displayingProgress: aString [
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :barBlock |
			[ self atEnd ] whileFalse: [
				barBlock value: self position.
				aBlock value ]]
]

{ #category : 'reading' }
PositionableStream >> upTo: delimiter  [
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of delimiter in the receiver. If 
	delimiter is not in the collection, answer the entire rest of the receiver."

	^self upTo: delimiter delimiterIsTerminator: false.
]

{ #category : 'reading' }
PositionableStream >> upTo: delimiter delimiterIsTerminator: delimiterIsTerminator [
	"Answer a subcollection from the current access position to the 
	occurrence of delimiter in the receiver. 

	If delimiterIsTerminator is false, delimiter is considered a separator:
		- Skip delimiter, but don't include it in the answer.
		- If delimiter is not found, answer the entire rest of the receiver.

	If delimiterIsTerminator is true, answer will end with delimeter:
		- Read delimiter, include it in the answer.
		- If delimiter is not found, answer nil and don't advance receiver at all.
	This is especially useful if we are appended new stuff while simultaneusly being read."

	| prevPosition answer |
	prevPosition := self position.
	answer := self collectionSpecies streamContents: [ :newStream | | element |
		[self atEnd or: [(element := self next) = delimiter]]
			whileFalse: [newStream nextPut: element].
		delimiterIsTerminator ifTrue: [
			element = delimiter
				ifTrue: [newStream nextPut: element]
				ifFalse: [
					self position: prevPosition.
					^ nil ]]].
	^answer.
]

{ #category : 'reading' }
PositionableStream >> upToAll: aCollection [
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	| startPos endMatch result |
	startPos := self position.
	(self matchStart: aCollection)
		ifNotNil: [ :matchStart |
			endMatch := self position.
			self position: startPos.
			result := self collectionSpecies streamContents: [ :ws | | n |
				[
					n := self next.
					self position < matchStart] whileTrue: [
						ws nextPut: n ]].
			self position: endMatch.
			^ result ]
		ifNil: [
			self position: startPos.
			^ self upToEnd ].
]

{ #category : 'reading' }
PositionableStream >> upToAny: aCollection [
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."

	^self collectionSpecies streamContents: [ :strm | | element prevPos |
		[self atEnd or: [
			prevPos := self position.
			element := self next.
			aCollection includes: element ]]
				whileFalse: [strm nextPut: element].
		(aCollection includes: element)
			ifTrue: [self position: prevPos]].
]

{ #category : 'mode' }
PositionableStream >> useBytes [
	collection class = ByteArray ifFalse: [
		self error: 'Can not #useBytes' ]
]
