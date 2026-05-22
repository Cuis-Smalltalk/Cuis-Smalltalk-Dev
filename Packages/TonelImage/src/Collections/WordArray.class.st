"
WordArrays store 32-bit unsigned Integer values, between 0 and 16rFFFFFFFF.

"
Class {
	#name : 'WordArray',
	#superclass : 'ArrayedCollection',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'as yet unclassified' }
WordArray class >> bobsTest [
	| wa s1 s2 wa2 answer rawData |
"
WordArray bobsTest
"
	answer := OrderedCollection new.
	wa := WordArray with: 16r01020304 with: 16r05060708.
	{false. true} do: [ :pad |
		0 to: 3 do: [ :skip |
			s1 := ReadWriteStream on: #[].

			s1 next: skip put: 0.		"start at varying positions"
			wa writeOn: s1.
			pad ifTrue: [s1 next: 4-skip put: 0].	"force length to be multiple of 4"

			rawData := s1 contents.
			s2 := ReadWriteStream with: rawData.
			s2 reset.
			s2 skip: skip.			"get to beginning of object"
			wa2 := WordArray newFromStream: s2.
			answer add: {
				rawData size. 
				skip. 
				wa2 = wa. 
				wa2 asArray collect: [ :each | each printStringBase: 16]
			}
		].
	].
	^answer explore
]

{ #category : 'accessing' }
WordArray >> atAllPut: value [
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value
]

{ #category : 'accessing' }
WordArray >> bytesAt: index [
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	((WordArray with: 16rFF32791B) bytesAt: 1) hex
	"

	| bytes word |
	bytes := ByteArray new: 4.
	word := self at: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes
]

{ #category : 'accessing' }
WordArray >> bytesAt: index put: aByteArray [
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	((WordArray new: 1) bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF]) first hex
	"

	| word |
	word := 0.
	4 to: 1 by: -1 do: [ :i | word := word * 256 + (aByteArray at: i) ].
	self at: index put: word
]

{ #category : 'accessing' }
WordArray >> defaultElement [
	"Return the default element of the receiver"
	^0
]

{ #category : 'printing' }
WordArray >> printElementsOn: aStream [
	"Print my elements on aStream."

	aStream nextPut: $(.
	self do: [ :element | aStream print: (element printStringBase: 16 length: 8 padded: true) ] separatedBy: [ aStream space ].
	aStream nextPut: $)
]

{ #category : 'private' }
WordArray >> replaceFrom: start to: stop with: replacement startingAt: repStart  [

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart 
]
