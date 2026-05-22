Class {
	#name : 'Utf8EncodedWriteStream',
	#superclass : 'WriteStream',
	#instVars : [
		'lastWrittenElement',
		'numberOfCodePointsWritten'
	],
	#category : 'Collections-Streams',
	#package : 'Collections',
	#tag : 'Streams'
}

{ #category : 'private' }
Utf8EncodedWriteStream >> byteContents [
	readLimit := readLimit max: position.
	^collection copyFrom: 1 to: position.
]

{ #category : 'accessing' }
Utf8EncodedWriteStream >> collectionSpecies [

	self shouldNotImplement
]

{ #category : 'accessing' }
Utf8EncodedWriteStream >> contents [
	"Answer the UnicodeString built so far."

	^UnicodeString fromUtf8Bytes: self byteContents codePointCount: numberOfCodePointsWritten.
]

{ #category : 'private' }
Utf8EncodedWriteStream >> isCompatibleWithContents: aCollection [

	aCollection isString
		ifTrue: [ ^true ].

	(aCollection is: #ByteArray)
		ifTrue: [ ^true ].

	^ false
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> next: anInteger putAll: aStringOrBytes startingAt: startIndex [
	"Argument can be String, UnicodeString or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	super next: anInteger putAll: aStringOrBytes startingAt: startIndex.
	lastWrittenElement := aStringOrBytes at: startIndex+anInteger-1.
	numberOfCodePointsWritten := nil.	"We can no longer know"
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> nextPut: aCodePointOrCharacterOrByte [
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character, CodePoint or Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCodePointOrCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCodePointOrCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCodePointOrCharacterOrByte codePoint].
	lastWrittenElement := aCodePointOrCharacterOrByte.
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> nextPutAll: aStringOrBytes [
	"Argument can be String, UnicodeString or ByteArray.
	Write to this stream. Both kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes := aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes := aStringOrBytes asUtf8Bytes.
		numberOfCodePointsWritten ifNotNil: [
			"As aStringOrBytes is a kind of String, not utf-8 byteArray, we can use its size."
			numberOfCodePointsWritten := numberOfCodePointsWritten + aStringOrBytes size ]].
	super nextPutAll: bytes.
	lastWrittenElement := aStringOrBytes last.
]

{ #category : 'private' }
Utf8EncodedWriteStream >> nextPutByte: aByte  [
	"Insert the argument at the next position in the Stream represented by the receiver."

	numberOfCodePointsWritten := nil.	"We can no longer know"
	position >= writeLimit
		ifTrue: [^ self pastEndPutByte: aByte]
		ifFalse: [
			position := position + 1.
			^collection at: position put: aByte]
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> nextPutCodePoint: aNumber [
	"UTF-8 encoded"

	| n |
	"Save in a temp because #nextPutByte: smashes the ivar."
	n := numberOfCodePointsWritten ifNotNil: [ numberOfCodePointsWritten + 1 ].
	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOf: aNumber.
	numberOfCodePointsWritten := n.
]

{ #category : 'private' }
Utf8EncodedWriteStream >> on: aCollection [
	"Initialize the stream to write over all of aCollection.
	The collection contents are considered useless.
	Overwrite them."

	super on: aCollection.
	lastWrittenElement := nil.
	numberOfCodePointsWritten := 0.
]

{ #category : 'private' }
Utf8EncodedWriteStream >> pastEndPut: anObject [
	self shouldNotImplement
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> pastEndPutByte: aByte [
	"Grow the collection.
	Then we put <anObject> at the current write position."

	self growTo: collection size + 1.
	collection at: (position := position + 1) put: aByte
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> peekLast [

	^lastWrittenElement
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> position: pos [
	"Argument is a byte position"

	super position: pos.
	lastWrittenElement := nil.
	numberOfCodePointsWritten := nil.
]

{ #category : 'positioning' }
Utf8EncodedWriteStream >> reset [
	super reset.
	lastWrittenElement := nil.
	numberOfCodePointsWritten := 0.
]

{ #category : 'positioning' }
Utf8EncodedWriteStream >> resetToStart [
	super resetToStart.
	lastWrittenElement := nil.
	numberOfCodePointsWritten := 0.
]

{ #category : 'positioning' }
Utf8EncodedWriteStream >> setToEnd  [
	super setToEnd.
	lastWrittenElement := nil.
	numberOfCodePointsWritten := nil.
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> skip [
	"Leave an empty byte position."

	self skip: 1
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> skip: n [
	"Skips the next amount bytes in the byte store."

	self position: (self position + n min: self size).
]

{ #category : 'read, write, position' }
Utf8EncodedWriteStream >> skipBack [
	"Overwrite last written byte."

	self skip: -1.
]
