"
Abstract superclass for objects whose slots are 32 bit values.
- Each can be Float or Integer, but always 32 bit.
- They have a fixed size, defined by the class.
- They don't have collection protocol.
"
Class {
	#name : 'ThirtyTwoBitSlotsObject',
	#superclass : 'Object',
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
ThirtyTwoBitSlotsObject class >> new [
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numSlots
]

{ #category : 'object serialization' }
ThirtyTwoBitSlotsObject class >> newFromStream: s [
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |
	len := s nextInt32BigEndian: true.
	^ s nextWordsInto: (self basicNew: len)
]

{ #category : 'instance creation' }
ThirtyTwoBitSlotsObject class >> numSlots [
	^0
]

{ #category : 'accessing' }
ThirtyTwoBitSlotsObject >> byteSize [
	^self size * 4
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> bytesAt: index [
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	((ThirtyTwoBitSlotsObject new: 1) at: 1 put: 16rFF32791B ; bytesAt: 1) hex
	"

	| bytes word |
	bytes := ByteArray new: 4.
	word := self basicAt: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> bytesAt: index put: aByteArray [
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	((ThirtyTwoBitSlotsObject new: 1) bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF]; basicAt: 1) hex
	"

	| word |
	word := 0.
	4 to: 1 by: -1 do: [ :i | word := word * 256 + (aByteArray at: i) ].
	self basicAt: index put: word
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> float32SlotAt: index [
	"Used to be:
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>"
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> float32SlotAt: index put: value [
	"Used to be:
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>"
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self float32SlotAt: index put: value asFloat ].
	^value
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> int32SlotAt: index [
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word := self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> int32SlotAt: index put: anInteger [
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger
]

{ #category : 'object serialization' }
ThirtyTwoBitSlotsObject >> restoreEndianness [
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	Smalltalk isLittleEndian ifTrue: [
		BitBlt swapBytesIn32BitWords: self ]
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> uint32SlotAt: index [

	^self basicAt: index
]

{ #category : 'private' }
ThirtyTwoBitSlotsObject >> uint32SlotAt: index put: anInteger [

	^self basicAt: index put: anInteger
]

{ #category : 'object serialization' }
ThirtyTwoBitSlotsObject >> writeOn: aStream  [
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	aStream nextInt32Put: self basicSize bigEndian: true.
	aStream nextUint32WordsPutAll: self.
]
