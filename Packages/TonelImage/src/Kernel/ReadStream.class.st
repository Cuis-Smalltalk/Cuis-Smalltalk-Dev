"
I represent an accessor for a sequence of objects that can only read objects from the sequence.
Instances are created on: aCollection. Its elements will be iterated by us.
"
Class {
	#name : 'ReadStream',
	#superclass : 'PositionableStream',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'instance creation' }
ReadStream class >> on: aCollection from: firstIndex to: lastIndex  [
	"Answer with a new instance streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex
]

{ #category : 'testing' }
ReadStream >> isReadOnly [
	^ true
]

{ #category : 'file stream compatibility' }
ReadStream >> localName [
	^'ReadStream'
]

{ #category : 'reading' }
ReadStream >> next [
	"Answer the next object in the Stream represented by the receiver."

	^position >= readLimit
		ifFalse: [collection at: (position := position + 1)]
]

{ #category : 'reading' }
ReadStream >> next: anInteger  [
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans

]

{ #category : 'reading' }
ReadStream >> next: n into: aCollection startingAt: startIndex [
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]
]

{ #category : 'writing' }
ReadStream >> next: anInteger putAll: aString startingAt: startIndex [
	self shouldNotImplement
]

{ #category : 'writing' }
ReadStream >> nextPut: anObject  [
	self shouldNotImplement
]

{ #category : 'writing' }
ReadStream >> nextPutAll: aCollection [
	self shouldNotImplement
]

{ #category : 'private' }
ReadStream >> on: aCollection from: firstIndex to: lastIndex  [
	| len |
	collection := aCollection.
	readLimit :=  lastIndex > (len := collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position := firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]
]

{ #category : 'reading' }
ReadStream >> readInto: byteArray startingAt: startIndex count: count [
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max := (readLimit - position) min: count.
	byteArray 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	^max
]

{ #category : 'reading' }
ReadStream >> readStream [
	"polymorphic with SequenceableCollection.  Return self"

	^ self
]

{ #category : 'reading' }
ReadStream >> size [
	"Compatibility with other streams (e.g., FileStream)"
	^readLimit
]

{ #category : 'reading' }
ReadStream >> upTo: delimiter delimiterIsTerminator: delimiterIsTerminator [
	"See comment at PositionableStream.
	Fast version using indexOf:"

	| start end |

	start := position+1.
	end := collection indexOf: delimiter startingAt: start ifAbsent: [0].

	"not present"	
	end = 0 ifTrue: [
		^ delimiterIsTerminator
			ifTrue: [
				self position: start-1.
				nil ]
			ifFalse: [self upToEnd]].

	"skip to the end and return the data passed over"
	position := end.
	^collection copyFrom: start to: (delimiterIsTerminator ifTrue: [end] ifFalse: [end-1])
]

{ #category : 'reading' }
ReadStream >> upToEnd [
	| start |

	start := position+1.
	position := collection size.
	^collection copyFrom: start to: position
]
