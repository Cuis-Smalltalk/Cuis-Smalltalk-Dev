"
I represent an accessor for a sequence of objects that can only store objects in the sequence.
Instances are created on: aCollection, which is considered to be empty or filled with nil. Any contents will be ovewriten.
Instances can also be create with: aCollection. In this case, new contents will be added after this initial stuff.
"
Class {
	#name : 'WriteStream',
	#superclass : 'PositionableStream',
	#instVars : [
		'writeLimit'
	],
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'instance creation' }
WriteStream class >> with: aCollection  [
	"Answer an instance of me on the argument, aCollection, positioned to 
	store objects at the end of aCollection.
	In the case of ReadWriteStream, for read operations, the stream appears empty
	until stuff is added, or position is set to before end. See #readingOn:"

	^self basicNew with: aCollection
]

{ #category : 'instance creation' }
WriteStream class >> with: aCollection from: firstIndex to: lastIndex  [
	"Answer an instance of me on the subcollection of the argument, 
	aCollection, determined by the indices firstIndex and lastIndex. Position 
	the instance to store at the end of the subcollection."

	^self basicNew with: (aCollection copyFrom: firstIndex to: lastIndex)
]

{ #category : 'private' }
WriteStream >> braceArray [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ collection
]

{ #category : 'private' }
WriteStream >> braceArray: anArray [
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	collection := anArray.
	position := 0.
	readLimit := 0.
	writeLimit := anArray size.
]

{ #category : 'reading' }
WriteStream >> contents [
	readLimit := readLimit max: position.
	^(collection copyFrom: 1 to: position) asStreamResult
]

{ #category : 'character writing' }
WriteStream >> cr [
	"Append a cr character to the receiver.
	Use this method when you specifically need a cr character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character cr
]

{ #category : 'character writing' }
WriteStream >> ensureASpace [
	"Append a space character to the receiver IFF there is not one on the end."

	(position > 0 and: [(collection at: position) = Character space]) ifTrue: [^self].
	self nextPut: Character space
]

{ #category : 'character writing' }
WriteStream >> ensureNoSpace [
	"If there is not one on the end, remove it."

	(position > 0 and: [(collection at: position) = Character space]) 
		ifTrue: [self skipBack].
]

{ #category : 'private' }
WriteStream >> growTo: anInteger [
	"Grow the collection by creating a new bigger collection and then
	copy over the contents from the old one. We grow by doubling the size.

	anInteger is the required minimal new size of the collection "

	| oldSize newSize |
	oldSize := collection size.
	newSize := anInteger + (oldSize max: 20).
	collection := collection grownTo: newSize.
	writeLimit := collection size
]

{ #category : 'private' }
WriteStream >> isCompatibleWithContents: aCollection [

	collection class == aCollection class
		ifTrue: [ ^ true ].

	(aCollection isByteString and: [ (collection is: #Text) or: [ collection is: #ArrayOfCharactersAndCodePoints ]])
		ifTrue: [ ^ true ].

	^ false
]

{ #category : 'testing' }
WriteStream >> isEmpty [
	"Answer whether the receiver's contents has no elements."

	^position = 0
]

{ #category : 'testing' }
WriteStream >> isWriteOnly [
	^true
]

{ #category : 'character writing' }
WriteStream >> lf [
	"Append a lf character to the receiver.
	Use this method when you specifically need a lf character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character lf
]

{ #category : 'character writing' }
WriteStream >> newLine [
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter
]

{ #category : 'character writing' }
WriteStream >> newLineTab [
	"Append a newLine character, followed by a tab character, to the receiver."

	self
		nextPut: Character newLineCharacter;
		nextPut: Character tab.
]

{ #category : 'character writing' }
WriteStream >> newLineTab: anInteger [
	"Append a newLine character, followed by anInteger tab characters, to the receiver."

	self nextPut: Character newLineCharacter.
	anInteger timesRepeat: [self nextPut: Character tab]
]

{ #category : 'character writing' }
WriteStream >> newLineTab: times when: aCondition [

	aCondition ifTrue: [ self newLineTab: times ]

	
]

{ #category : 'character writing' }
WriteStream >> newLineWhen: aCondition [

	aCondition ifTrue: [ self newLine ]
]

{ #category : 'reading' }
WriteStream >> next [
	self shouldNotImplement
]

{ #category : 'reading' }
WriteStream >> next: n [
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super next: n
]

{ #category : 'writing' }
WriteStream >> next: n into: aCollection startingAt: startIndex [
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super next: n into: aCollection startingAt: startIndex
]

{ #category : 'writing' }
WriteStream >> next: anInteger putAll: aCollection startingAt: startIndex [
	"Store the next anInteger elements from the given collection."


	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex].

	newEnd := position + anInteger.
	newEnd > writeLimit ifTrue:
		[self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: startIndex.
	position := newEnd.

	^aCollection
]

{ #category : 'fileIn/Out' }
WriteStream >> nextChunkPut: aString [
	"Append the argument, aString, to the receiver, doubling embedded terminators."

	| i remainder terminator |
	terminator := $!.
	remainder := aString.
	[(i := remainder indexOf: terminator) = 0] whileFalse:
		[self nextPutAll: (remainder copyFrom: 1 to: i).
		self nextPut: terminator.  "double imbedded terminators"
		remainder := remainder copyFrom: i+1 to: remainder size].
	self nextPutAll: remainder; nextPut: terminator
]

{ #category : 'writing' }
WriteStream >> nextPut: anObject  [
	"Insert the argument at the next position in the Stream represented by the receiver."

	(anObject isCharacter and: [anObject codePoint > 255 and: [collection canHoldAnyCodePoint not]]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: [
			position := position + 1.
			^collection at: position put: anObject].
]

{ #category : 'writing' }
WriteStream >> nextPutAll: aCollection [

	| toBeAdded newEnd |
	toBeAdded := aCollection.
	(toBeAdded canHoldAnyCodePoint == collection canHoldAnyCodePoint) ifFalse: [
		toBeAdded canHoldAnyCodePoint
			ifTrue: [collection := collection thatCanHoldAnyCodePoint]
			ifFalse: [toBeAdded := toBeAdded asUnicodeStringOrText]].

	(self isCompatibleWithContents: toBeAdded)
		ifFalse: [^ super nextPutAll: toBeAdded].

	newEnd := position + toBeAdded size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: toBeAdded startingAt: 1.
	position := newEnd.
]

{ #category : 'writing' }
WriteStream >> nextPutAllString: aString withAttributes: attributesArray [

	| toBeAdded newEnd |
	toBeAdded := aString.
	(toBeAdded canHoldAnyCodePoint == collection canHoldAnyCodePoint) ifFalse: [
		toBeAdded canHoldAnyCodePoint
			ifTrue: [collection := collection thatCanHoldAnyCodePoint]
			ifFalse: [toBeAdded := toBeAdded asUnicodeStringOrText]].
	(self isCompatibleWithContents: toBeAdded)
		ifFalse: [ ^ self nextPutAll: toBeAdded ].

	newEnd := position + toBeAdded size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: toBeAdded
		attributes: attributesArray
		startingAt: 1.
	position := newEnd
]

{ #category : 'character writing' }
WriteStream >> nextPutKeyword: keyword withArg: argValue [
	"Emit a keyword/value pair in the alternate syntax"

	self nextPutAll: (keyword copyWithout: $:);
		nextPut: $(;
		store: argValue;
		nextPut: $)
]

{ #category : 'reading' }
WriteStream >> nextWordsInto: aBitmap [
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super nextWordsInto: aBitmap
]

{ #category : 'private' }
WriteStream >> on: aCollection [
	"Initialize the stream to write over all of aCollection.
	The collection contents are considered useless (most likely nil).
	Overwrite them.
	In the case of ReadWriteStream, for read operations, stream appears empty until stuff is added."

	super on: aCollection thatCanBeModified.
	readLimit := 0.
	writeLimit := aCollection size
]

{ #category : 'services' }
WriteStream >> padToEndIfCantTruncate [
	"Only makes sense for file streams with existing content.
	See inheritance"
]

{ #category : 'private' }
WriteStream >> pastEndPut: anObject [
	"Grow the collection.
	Then we put <anObject> at the current write position."

	self growTo: collection size + 1.
	collection at: (position := position + 1) put: anObject
]

{ #category : 'reading' }
WriteStream >> peek [
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super peek
]

{ #category : 'character writing' }
WriteStream >> peekLast [
	"Return that item just put at the end of the stream"

	^ position > 0 
		ifTrue: [collection at: position]
		ifFalse: [nil]
]

{ #category : 'positioning' }
WriteStream >> position: anInteger  [
	readLimit := readLimit max: position.
	super position: anInteger
]

{ #category : 'positioning' }
WriteStream >> reset [
	readLimit := readLimit max: position.
	position := 0
]

{ #category : 'positioning' }
WriteStream >> resetToStart [
	readLimit := position := 0.
]

{ #category : 'positioning' }
WriteStream >> setToEnd  [
	"Refer to the comment in PositionableStream|setToEnd."

	readLimit := readLimit max: position.
	super setToEnd.
]

{ #category : 'accessing' }
WriteStream >> size [
	^readLimit := readLimit max: position
]

{ #category : 'writing' }
WriteStream >> space [
	"Append a space character to the receiver."

	self nextPut: Character space
]

{ #category : 'character writing' }
WriteStream >> space: anInteger  [
	"Append anInteger space characters to the receiver."

	anInteger timesRepeat: [self space]
]

{ #category : 'character writing' }
WriteStream >> space: times when: aCondition [

	aCondition ifTrue: [ self space: times ]
	
	
]

{ #category : 'character writing' }
WriteStream >> spaceWhen: aCondition [
 
	aCondition ifTrue: [ self space ]
]

{ #category : 'printing' }
WriteStream >> store: anObject  [
	"Have anObject print on me for rereading."

	anObject storeOn: self
]

{ #category : 'character writing' }
WriteStream >> tab [
	"Append a tab character to the receiver."

	self nextPut: Character tab
]

{ #category : 'character writing' }
WriteStream >> tab: anInteger  [
	"Append anInteger tab characters to the receiver."

	anInteger timesRepeat: [self tab]
]

{ #category : 'character writing' }
WriteStream >> tab: times when: aCondition [

	aCondition ifTrue: [ self tab: times ]
	
	
]

{ #category : 'character writing' }
WriteStream >> tabWhen: aCondition [

	aCondition ifTrue: [ self tab ]
]

{ #category : 'reading' }
WriteStream >> upTo: delimiter delimiterIsTerminator: delimiterIsTerminator [
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
]

{ #category : 'private' }
WriteStream >> with: aCollection [
	"Initialize the stream to append after existing elements in aCollection.
	In the case of ReadWriteStream, for read operations, the stream appears empty
	until stuff is added, or position is set to before end. See #readingOn:"

	super on: aCollection.
	position := readLimit := writeLimit := aCollection size
]

{ #category : 'private' }
WriteStream >> withAttribute: aTextAttribute do: streamBlock [
	| pos1 val |

	(collection is: #Text) ifFalse: [
		^streamBlock value ].

	pos1 := self position.
	val := streamBlock value.
	collection addAttribute: aTextAttribute from: pos1+1 to: self position.
	^ val
]

{ #category : 'private' }
WriteStream >> withAttributes: attributes do: streamBlock  [
	| pos1 val |

	(collection is: #Text) ifFalse: [
		^streamBlock value ].

	pos1 := self position.
	val := streamBlock value.
	attributes do: [:attribute |
		collection
			addAttribute: attribute
			from: pos1 + 1
			to: self position].
	^ val
]

{ #category : 'fileIn/Out' }
WriteStream >> writeSystemTimeStamp [
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !s"
		(self collectionSpecies streamContents: [:s | Smalltalk writeSystemTimeStamp: s]) printString.
	self newLine
]
