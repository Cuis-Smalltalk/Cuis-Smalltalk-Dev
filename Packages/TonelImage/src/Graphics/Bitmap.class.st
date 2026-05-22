"
My instances provide contiguous storage of bits, primarily to hold the graphical data of Forms. Forms and their subclasses provide the additional structural information as to how the bits should be interpreted in two dimensions.
"
Class {
	#name : 'Bitmap',
	#superclass : 'ArrayedCollection',
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'instance creation' }
Bitmap class >> newFromStream: s [
	| len |
	len := s nextInt32BigEndian: true.
	^ s nextWordsInto: (self new: len)
]

{ #category : 'converting' }
Bitmap >> asByteArray [
	"Faster way to make a byte array from me.
	copyFromByteArray:, if receiver is BigEndian makes equal Bitmap.
	Assume receiver bytes-in-word mapping is BigEndian:
		Most significant bye of first word in self goes to first position in result.
	This means that for a BigEndian 8bpp Form, pixels are in the right order in the ByteArray
	
	Form lena asGrayForm bits asByteArray copyFrom: 1 to: 4.
	(Form lena asGrayForm asFormOfDepth: 8) bits asByteArray copyFrom: 1 to: 4.
	(0 to: 3) collect: [ :x | ((Form lena asGrayForm colorAt: x@0) luminance * 255) rounded ].
	"
	| f bytes hack |
	f := Form extent: 4@self size depth: 8 bits: self.
	bytes := ByteArray new: self size * 4.
	hack := Form new hackBits: bytes.
	Smalltalk isLittleEndian ifTrue: [hack swapEndianness].
	hack copyBits: f boundingBox
		from: f
		at: `0@0`
		clippingBox: hack boundingBox
		rule: Form over.

	"f displayOn: hack."
	^ bytes
]

{ #category : 'accessing' }
Bitmap >> atAllPut: value [
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."
	<primitive: 145>
	super atAllPut: value.
]

{ #category : 'accessing' }
Bitmap >> bitPatternForDepth: depth [
	"The raw call on BitBlt needs a Bitmap to represent this color.  I already am Bitmap like.  I am already adjusted for a specific depth.  Interpret me as an array of (32/depth) Color pixelValues.  BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary. 6/18/96 tk"

	^ self
]

{ #category : 'accessing' }
Bitmap >> byteAt: byteAddress bigEndian: isBigEndian [
	"Extract a byte from a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:.  See Form pixelAt:  7/1/96 tk"
	| lowBits shift |
	lowBits := byteAddress - 1 bitAnd: 3.
	shift := isBigEndian 
		ifTrue: [ (lowBits - 3) * 8 ]
		ifFalse: [ (0 - lowBits) * 8 ].
	^((self at: byteAddress - 1 - lowBits // 4 + 1)
		bitShift: shift)
		bitAnd: 16rFF
]

{ #category : 'accessing' }
Bitmap >> byteAt: byteAddress put: byte bigEndian: isBigEndian [
	"Insert a byte into a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:put:.  See Form pixelAt:put:  7/1/96 tk"
	| longWord shift lowBits longAddr |
	(byte < 0 or:[byte > 255]) ifTrue:[^self errorImproperStore].
	lowBits := byteAddress - 1 bitAnd: 3.
	longWord := self at: (longAddr := (byteAddress - 1 - lowBits) // 4 + 1).
	shift := isBigEndian 
		ifTrue: [ (3 - lowBits) * 8 ]
		ifFalse: [ (lowBits) * 8 ].
	longWord := longWord - (longWord bitAnd: (16rFF bitShift: shift)) 
		+ (byte bitShift: shift).
	self at: longAddr put: longWord.
	^ byte
]

{ #category : 'accessing' }
Bitmap >> bytesAt: index [
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
Bitmap >> bytesAt: index put: aByteArray [
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
Bitmap >> defaultElement [
	"Return the default element of the receiver"
	^0
]

{ #category : 'accessing' }
Bitmap >> integerAt: index [
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

{ #category : 'accessing' }
Bitmap >> integerAt: index put: anInteger [
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

{ #category : 'testing' }
Bitmap >> isColormap [
	"Bitmaps were used as color maps for BitBlt.
	This method allows to recognize real color maps."
	^false
]

{ #category : 'accessing' }
Bitmap >> pixelValueForDepth: depth [
	"Self is being used to represent a single color.  Answer bits that appear in ONE pixel of this color in a Bitmap of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32.  Returns an integer.  First pixel only.  "

	^ (self at: 1) bitAnd: (1 bitShift: depth) - 1
]

{ #category : 'accessing' }
Bitmap >> primFill: aPositiveInteger [
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."

	<primitive: 145>
	self errorImproperStore.
]

{ #category : 'printing' }
Bitmap >> printOn: aStream [
	self printNameOn: aStream.
	aStream nextPutAll: ' of length '; print: self size
]

{ #category : 'accessing' }
Bitmap >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]
