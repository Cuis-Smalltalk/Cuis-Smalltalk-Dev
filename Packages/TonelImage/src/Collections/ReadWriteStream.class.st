"
I represent an accessor for a sequence of objects. My instances can both read and store objects.
Instances are created on: aCollection, which is considered to be empty or filled with nil. Any contents will be ovewriten, and are not available for reading.
Instances can also be create with: aCollection. In this case, new contents will be added after this initial stuff. Reading them is only possible if position: is set appropriately (for instance aReadWriteStream position: 1).
"
Class {
	#name : 'ReadWriteStream',
	#superclass : 'WriteStream',
	#category : 'Collections-Streams',
	#package : 'Collections',
	#tag : 'Streams'
}

{ #category : 'testing' }
ReadWriteStream >> = other [

	"Any object is equal to itself"
	self == other ifTrue: [ ^ true ].

	(self class == ReadWriteStream and: [other class == ReadWriteStream]) ifFalse: [
		^ false ].

	^ self position = other position and: [ self contents = other contents ]
]

{ #category : 'accessing' }
ReadWriteStream >> contents [
	"Answer with a copy of my collection from 1 to readLimit."

	readLimit := readLimit max: position.
	^ (collection copyFrom: 1 to: readLimit) asStreamResult
]

{ #category : 'fileIn/Out' }
ReadWriteStream >> fileOutObject: theObject [
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	| srefStream |
	self writeSystemTimeStamp.

	"Append the object's raw data"
	srefStream := SmartRefStream on: self.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me"

]

{ #category : 'testing' }
ReadWriteStream >> hash [

	self class == ReadWriteStream ifFalse: [^ super hash].
	^ (self position + readLimit + 53) hash
]

{ #category : 'testing' }
ReadWriteStream >> isWriteOnly [
	^false
]

{ #category : 'accessing' }
ReadWriteStream >> name [
	^ 'a stream'   "for fileIn compatibility"
]

{ #category : 'accessing' }
ReadWriteStream >> next [
	"Return the next object in the Stream represented by the receiver."

	"treat me as a FIFO"
	^ position >= readLimit
		ifFalse: [collection at: (position := position + 1)]
]

{ #category : 'accessing' }
ReadWriteStream >> next: anInteger  [
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |
	readLimit := readLimit max: position.

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans

]

{ #category : 'converting' }
ReadWriteStream >> readStream [
	"polymorphic with SequenceableCollection.  Return self"

	^ self
]

{ #category : 'accessing' }
ReadWriteStream >> truncateAtPosition [
	"Truncate the receiver at current position.
	For example, this should evaluate to true:
		| s |
		s := ReadWriteStream on: #().
		s nextPutAll: 'abcdefg'.
		s reset.
		s next; next.
		s nextPut: $z.
		s truncateAtPosition.
		s atEnd
	"
	readLimit := position
]
