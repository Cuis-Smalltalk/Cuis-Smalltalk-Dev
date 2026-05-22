"
I represent an ArrayedCollection whose elements are integers between 0 and 255.

We have a literal syntax:
	#[1 2 3 4]
"
Class {
	#name : 'ByteArray',
	#superclass : 'ArrayedCollection',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'services' }
ByteArray class >> findFirstInBytes: aByteArrayOrByteString inSet: aByteSetByteArray startingAt: start [
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>

	aByteSetByteArray size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aByteArrayOrByteString size.
	[ i <= stringSize and: [ (aByteSetByteArray at: (aByteArrayOrByteString basicAt: i) +1) = 0 ] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i
]

{ #category : 'byte based hash' }
ByteArray class >> hashBytes: aByteArray startingWith: speciesHash [
	"Answer the hash of a byte-indexed collection,
	using speciesHash as the initial value.
	See SmallInteger>>hashMultiply.

	The primitive should be renamed at a
	suitable point in the future"

	| byteArraySize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	byteArraySize := aByteArray size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: byteArraySize do: [ :pos |
		hash := hash + (aByteArray basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash
]

{ #category : 'services' }
ByteArray class >> indexOfByte: anInteger inBytes: aByteArrayOrByteString startingAt: start [

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>

	start > 0 ifFalse: [ ^ 0 ].
	stringSize := aByteArrayOrByteString size.
	start to: stringSize do: [:pos |
		(aByteArrayOrByteString basicAt: pos) = anInteger ifTrue: [^ pos]].
	^ 0

]

{ #category : 'byte based hash' }
ByteArray class >> minSizeForHashQuick [
	"Answer the size above which we prefer using the #hashQuick method"

	^256
]

{ #category : 'instance creation' }
ByteArray class >> newBits: bitCount [
	"
	(ByteArray newBits: 8) bitAt: 8 put: 1; bitAt: 8
	(ByteArray newBits: 9) bitAt: 9 put: 1; bitAt: 9
	"
	^self new: bitCount + 7 // 8
]

{ #category : 'instance creation' }
ByteArray class >> newFrom: aCollection  [
	"Answer an instance of me containing the same elements as aCollection.
	Argument can also be a LargeInteger!"
	| newArray |
	newArray := self new: aCollection size.
	1 to: aCollection size do: [:i | newArray at: i put: (aCollection at: i)].
	^ newArray

"
	ByteArray newFrom: 1000 factorial
	
	| ba li |
	ba := ByteArray newFrom: 1000 factorial.
	li := LargePositiveInteger new: ba size.
	li replaceFrom: 1 to: ba size with: ba startingAt: 1.
	li / 999 factorial
"
]

{ #category : 'instance creation' }
ByteArray class >> readHexFrom: aString [
	"Create a byte array from an hexadecimal representation

        ByteArray readHexFrom: '7A2BD507'
                => #[122 43 213 7]
	"
	^(self new: aString size // 2) readHexFrom: aString readStream
]

{ #category : 'stream creation' }
ByteArray class >> streamUtf8Contents: blockWithArg [
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, UnicodeStrings.
	Answer is an instance of us, holding UTF-8 encoded bytes."

	| stream |	
	stream := Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream byteContents
]

{ #category : 'comparing' }
ByteArray >> = another [
	"Make good use of String primitives"

	self == another ifTrue: [ ^ true ].
	self species == another species ifFalse: [^ false].
	^String is: self equalTo: another 
]

{ #category : 'converting' }
ByteArray >> asByteArray [
	^self
]

{ #category : 'converting' }
ByteArray >> asHex [
	"Answer an hexa decimal representation of the receiver
	
	        #[122 43 213 7] asHex
		=> '7A 2B D5 07'
	"

	^String streamContents: [ :strm |
		self
			do: [ :byte | strm
				nextPut: ('0123456789ABCDEF' at: byte//16+1);
				nextPut: ('0123456789ABCDEF' at: byte\\16+1)]
			separatedBy: [strm space ]].
]

{ #category : 'converting' }
ByteArray >> asString [
	"Convert to a String with Characters for each byte.
	Fast code uses primitive that avoids character conversion"

	^ (String new: self size) replaceFrom: 1 to: self size with: self
]

{ #category : 'converting' }
ByteArray >> asUtf8BytesOrByteStringOrByteArray [
	"Sometimes, a String or ByteArray will do, but if we have an UnicodeString, we
	want ASCII or UTF-8 bytes."
	
	^self
]

{ #category : 'accessing' }
ByteArray >> atAllPut: value [
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value
]

{ #category : 'converting' }
ByteArray >> base64Encoded [
	"Encode the receiver as base64"
	"'Hello World' asByteArray base64Encoded"
	^(Base64MimeConverter mimeEncode: self readStream) contents
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> bitAt: bitIndex [
	"Answer the bit (0 or 1) at a bit index.
	This way, the receiver behaves as a BitArray.
	Note: There is no error raised if you access the possible extra bits at the end if size is not multiple of 8."
	| bitPosition index |
	index := bitIndex - 1 // 8 + 1.
	bitPosition := bitIndex - 1 \\ 8 + 1.
	^ self bitAt: index bitPosition: bitPosition
]

{ #category : 'private' }
ByteArray >> bitAt: index bitPosition: bitPosition [
	"Answer the bit (0 or 1) at byte at index, at bitPosition.
	The bits are indexed starting at 1 for the least significant bit"
	^(self at: index) bitAt: bitPosition
]

{ #category : 'private' }
ByteArray >> bitAt: index bitPosition: bitPosition put: aBit [
	"Set the bit (0 or 1) at byte at index, at bitPosition. 
	The bit value should be 0 or 1, otherwise raise an Error.
	The bits are indexed starting at 1 for the least significant bit"
	self at: index put: ((self at: index) bitAt: bitPosition put: aBit)
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> bitAt: bitIndex put: aBit [
	"Set the bit (0 or 1) at a bit index. This way, the receiver behaves as a BitArray
	Note: There is no error raised if you access the possible extra bits at the end if size is not multiple of 8.
	#[1 0 0 ] bitAt: 1
	#[0 1  0 ] bitAt: 9
	#[0 0 128 ] bitAt: 24
	"
	| bitPosition index |
	index := bitIndex - 1 // 8 + 1.
	bitPosition := bitIndex - 1 \\ 8 + 1.
	self bitAt: index bitPosition: bitPosition put: aBit
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> bitBooleanAt: bitIndex [
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	^ (self bitAt: bitIndex) = 1
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> bitBooleanAt: bitIndex put: aBoolean [
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	self bitAt: bitIndex put: (aBoolean ifTrue: [1] ifFalse: [0])
]

{ #category : 'accessing' }
ByteArray >> bitSize [
	"Note:
	(ByteArray newBits: 9) bitSize = 16 
	"
	^self size * 8
]

{ #category : 'private' }
ByteArray >> defaultElement [
	^0
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> float32At: index bigEndian: bool  [
	"Return a 32 bit float starting from the given byte index.
	We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float."
	| w |
	w := self uint32At: index bigEndian: bool.
	^ Float fromIEEE32Bit: w
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> float32At: index put: value bigEndian: bool  [
	"Store a 32 bit float starting from the given byte index.
	Round value to a 32bit Float, and store it."
	| w |
	w := value asIEEE32BitWord.
	self uint32At: index put: w bigEndian: bool.
	^ value
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> float64At: index bigEndian: bool  [
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self uint32At: index bigEndian: bool.
	w2 := self uint32At: index + 4 bigEndian: bool.
	dbl := Float new. 
	bool
		ifTrue: [
			dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [
			dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> float64At: index put: value bigEndian: bool  [
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self uint32At: index put: w1 bigEndian: bool.
	self uint32At: index + 4 put: w2 bigEndian: bool.
	^ value
]

{ #category : 'comparing' }
ByteArray >> hash [
	"#hash is implemented, because #= is implemented"
	self size > ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: ByteArray hash ].
	^self class
		hashBytes: self
		startingWith: ByteArray hash
]

{ #category : 'converting' }
ByteArray >> hex [
	"Answer an hexa decimal representation of the receiver
	
	        #[122 43 213 7] hex
                => ' 7A2BD507'
	"
	| string v index map |
	map := '0123456789ABCDEF'.
	string := String new: self size * 2. "hex"
	index := 0.
	1 to: self size do:[:i| 
		v := self at: i.
		string at: (index := index + 1) put: (map at: (v bitShift: -4) + 1). 
		string at: (index := index + 1) put: (map at: (v bitAnd: 15) + 1).
	].
	^string
]

{ #category : 'accessing' }
ByteArray >> indexOf: anInteger startingAt: start ifAbsent: aBlock [

	| ans |
	(anInteger isInteger and: [
		anInteger >= 0 and: [
		anInteger <= 255 ] ]) ifFalse: [ ^0 ].
	ans := ByteArray indexOfByte: anInteger inBytes: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]
]

{ #category : 'accessing' }
ByteArray >> indexOfAnyIn: aByteSetByteArray startingAt: start ifAbsent: aBlock [
	"returns the index of the first character in the given set, starting from start.
	aByteSetByteArray is a ByteArray of size 256 with a 1 for byte values considered to be included."

	| answer |
	answer := ByteArray findFirstInBytes: self inSet: aByteSetByteArray startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int16At: index bigEndian: bigEndian [
	"Return a 16-bit signed integer quantity starting from the given byte index"

	| result |
	result := bigEndian
		ifFalse: [ ((self at: index + 1) bitShift: 8) + (self at: index) ]
		ifTrue: [ ((self at: index) bitShift: 8) + (self at: index + 1) ].
	result < 16r8000 ifTrue: [ ^result ].
	^result - 16r10000
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int16At: index put: value bigEndian: bigEndian [
	"Store a 16-bit signed integer quantity starting from the given byte index"
	
	| unsignedValue |
	(unsignedValue := value) < 0 ifTrue: [
		unsignedValue := unsignedValue + 16r10000 ].
	bigEndian ifFalse: [
		self 
			at: index + 1 put: (unsignedValue bitShift: -8);
			at: index put: (unsignedValue bitAnd: 16rFF).
		^value ].
	self
		at: index put: (unsignedValue bitShift: -8);
		at: index + 1 put: (unsignedValue bitAnd: 16rFF).
	^value
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int32At: index bigEndian: bigEndian [
	"Return a 32-bit integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"
	
	| byte result |
	bigEndian ifFalse: [
		(byte := self at: index + 3) <= 16r7F ifTrue: [ "Is the result non-negative?"
			(byte <= 16r3F or: [SmallInteger maxVal >  1073741823]) ifTrue: [
				^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
			^(LargePositiveInteger new: 4)
				replaceFrom: 1
					to: 4
					with: self
					startingAt: index;
				normalize ].
		"Negative"
		(byte >= 16rC0 or: [SmallInteger maxVal >  1073741823]) ifTrue: [
			^-1 - (((((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index) bitXor: 16rFF)) ].
		(result := LargeNegativeInteger new: 4)
			digitAt: 4 put: ((self at: index + 3) bitXor: 16rFF);
			digitAt: 3 put: ((self at: index + 2) bitXor: 16rFF);
			digitAt: 2 put: ((self at: index + 1) bitXor: 16rFF).
		(byte := ((self at: index) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
			^result
				digitAt: 1 put: byte;
				normalize ].
		^result
			digitAt: 1 put: 16rFF;
			- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this." ].
	(byte := self at: index) <= 16r7F ifTrue: [ "Is the result non-negative?"
		(byte <= 16r3F or: [SmallInteger maxVal >  1073741823]) ifTrue: [
			^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3) ].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize ].
	"Negative"
	(16rC0 <= byte or: [SmallInteger maxVal >  1073741823]) ifTrue: [
		^-1 - (((((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index + 3) bitXor: 16rFF)) ].
	(result := LargeNegativeInteger new: 4)
		digitAt: 4 put: (byte bitXor: 16rFF);
		digitAt: 3 put: ((self at: index + 1) bitXor: 16rFF);
		digitAt: 2 put: ((self at: index + 2) bitXor: 16rFF).
	(byte := ((self at: index + 3) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
		^result
			digitAt: 1 put: byte;
			normalize ].
	^result 
		digitAt: 1 put: 16rFF;
		- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this."
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int32At: index put: value bigEndian: bigEndian [
	"Store a 32-bit signed integer quantity starting from the given byte index"
	
	| v v2 |
	value isLarge ifTrue: [
		bigEndian ifFalse: [
			value positive ifTrue: [
				self 
					replaceFrom: index
					to: index + 3
					with: value
					startingAt: 1.
				^value ].
			v := 0.
			[ v <= 3 and: [ (v2 := ((value digitAt: v + 1) bitXor: 16rFF) + 1) = 16r100 ] ] whileTrue: [
				self at: index + v put: 0.
				v := v + 1 ].
			self at: index + v put: v2.
			v := v + 1.
			[ v <= 3 ] whileTrue: [
				self at: index + v put: ((value digitAt: (v := v + 1)) bitXor: 16rFF) ].
			^value ].
		value positive ifTrue: [
			self
				at: index put: (value digitAt: 4);
				at: index + 1 put: (value digitAt: 3);
				at: index + 2 put: (value digitAt: 2);
				at: index + 3 put: (value digitAt: 1).
			^value ].
		v := 3.
		[ 0 <= v and: [ (v2 := ((value digitAt: 4 - v) bitXor: 16rFF) + 1) = 16r100 ] ] whileTrue: [
			self at: index + v put: 0.
			v := v - 1 ].
		self at: index + v put: v2.
		[ 0 <= (v := v - 1) ] whileTrue: [
			self at: index + v put: ((value digitAt: 4 - v) bitXor: 16rFF) ].
		^value ].
	v := value bitShift: -24.
	0 <= (v := (v bitAnd: 16r7F) - (v bitAnd: 16r80)) ifFalse: [
		v := v + 16r100 ].
	bigEndian ifFalse: [
		self 
			at: index put: (value bitAnd: 16rFF);
			at: index + 1 put: ((value bitShift: -8) bitAnd: 16rFF);
			at: index + 2 put: ((value bitShift: -16) bitAnd: 16rFF);
			at: index + 3 put: v.
		^value ].
	self
		at: index put: v;
		at: index + 1 put: ((value bitShift: -16) bitAnd: 16rFF);
		at: index + 2 put: ((value bitShift: -8) bitAnd: 16rFF);
		at: index + 3 put: (value bitAnd: 16rFF).
	^value
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int64At: index bigEndian: bigEndian [
	"Return a 64-bit signed integer quantity starting from the given byte index."

	| value |
	value := self uint64At: index bigEndian: bigEndian.
	value digitLength < 8 ifTrue: [ ^value ].
	(value digitAt: 8) < 16r80 ifTrue: [ ^value ].
	^value - 16r10000000000000000
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int64At: index put: value bigEndian: bigEndian [
	"Store a 64-bit signed integer quantity starting from the given byte index."
	
	^self
		uint64At: index
		put: (value negative
			ifFalse: [ value ]
			ifTrue: [ value + 16r10000000000000000 ])
		bigEndian: bigEndian
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int8At: index bigEndian: dummyArgument [
	"Return a 8-bit signed integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	| result |
	result := self at: index.
	result < 16r80 ifTrue: [ ^result ].
	^result - 16r100
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> int8At: index put: value bigEndian: dummyArgument [
	"Store a 8-bit signed integer quantity at the given byte index
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	| unsignedValue |
	(unsignedValue := value) < 0 ifTrue: [
		unsignedValue := unsignedValue + 16r100 ].
	self at: index put: unsignedValue.
	^value
]

{ #category : 'testing' }
ByteArray >> is: aSymbol [
	^#ByteArray = aSymbol or: [ super is: aSymbol ]
]

{ #category : 'testing' }
ByteArray >> isLiteral [
	"so that #(1 #[1 2 3] 5) prints itself"
	^self class == ByteArray
]

{ #category : 'printing' }
ByteArray >> printAsLiteralOn: aStream [
	aStream nextPut: $#; nextPut: $[.
	self do: [:each| each storeOn: aStream]
		separatedBy: [aStream nextPut: $ ].
	aStream nextPut: $]
]

{ #category : 'printing' }
ByteArray >> printOn: aStream [
	self shouldBePrintedAsLiteral ifFalse:
		[super printOn: aStream.
		 aStream space].
	self printAsLiteralOn: aStream
]

{ #category : 'initialization' }
ByteArray >> readHexFrom: aStream [
	"Initialize the receiver from a hexadecimal string representation
		ByteArray readHexFrom: '1E1e'
		ByteArray readHexFrom: '1e1E'
		ByteArray readHexFrom: '1e1e'
	Note: lowercase hex digits are supported here, because we are reading strictly bytes.
	But are not really general, because lowercase e is used to denote scientific notation:
		16r1e0 = 1
		16r1E0 = 480
	"
	| map v ch value |
	map := '0123456789ABCDEF'.
	1 to: self size do: [ :i |
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		 (v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := v bitShift: 4.
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		(v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := value + v.
		self at: i put: value.
	]
]

{ #category : 'system primitives' }
ByteArray >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'printing' }
ByteArray >> storeOn: aStream [
	self shouldBePrintedAsLiteral
		ifTrue: [self printAsLiteralOn: aStream]
		ifFalse: [super storeOn: aStream]
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint16At: index bigEndian: bigEndian [
	"Return a 16-bit unsigned integer quantity starting from the given byte index"

	bigEndian ifFalse: [ ^((self at: index + 1) bitShift: 8) + (self at: index) ].
	^((self at: index) bitShift: 8) + (self at: index + 1)
	
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint16At: index put: value bigEndian: bigEndian [
	"Store a 16-bit unsigned integer quantity starting from the given byte index"
	
	bigEndian ifFalse: [
		self 
			at: index + 1 put: (value bitShift: -8);
			at: index put: (value bitAnd: 16rFF).
		^value ].
	self
		at: index put: (value bitShift: -8);
		at: index+1 put: (value bitAnd: 16rFF).
	^value
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint32At: index bigEndian: bigEndian [
	"Return a 32-bit unsigned integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"
	| byte |
	bigEndian ifTrue:
		[((byte := self at: index) <= 16r3F
		 or: [SmallInteger maxVal >  1073741823]) ifTrue:
			[^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3)].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize].
	((byte := self at: index + 3) <= 16r3F
	 or: [SmallInteger maxVal >  1073741823]) ifTrue:
		[^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index)].
	^(LargePositiveInteger new: 4)
		replaceFrom: 1 to: 4 with: self startingAt: index;
		normalize
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint32At: index put: value bigEndian: bigEndian [
	"Store a 32-bit unsigned integer quantity starting from the given byte index"
	
	value isLarge
		ifTrue: [
			bigEndian ifFalse: [
				self
					replaceFrom: index
					to: index + 3
					with: value
					startingAt: 1.
				^value ].
			self
				at: index put: (value digitAt: 4);
				at: index + 1 put: (value digitAt: 3);
				at: index + 2 put: (value digitAt: 2);
				at: index +3 put: (value digitAt: 1) ]
		ifFalse: [
			bigEndian ifFalse: [
				self 
					at: index put: (value bitAnd: 16rFF);
					at: index + 1 put: ((value bitShift: -8) bitAnd: 16rFF);
					at: index + 2 put: ((value bitShift: -16) bitAnd: 16rFF);
					at: index + 3 put: (value bitShift: -24).
				^value ].
			self 
				at: index put: (value bitShift: -24);
				at: index + 1 put: ((value bitShift: -16) bitAnd: 16rFF);
				at: index + 2 put: ((value bitShift: -8) bitAnd: 16rFF);
				at: index + 3 put: (value bitAnd: 16rFF) ].
	^value
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint64At: index bigEndian: bigEndian [
	"Return a 64-bit unsigned integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"

	| byte |
	SmallInteger maxVal >  1073741823 ifTrue:
		[bigEndian
			ifTrue: "64-bit SmallIntegers have a 3 bit tag and a sign bit, so the most positive value has 16rF as its top byte."
				[(byte := self at: index) <= 16rF ifTrue:
					[^((((((((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3)) bitShift: 8)
						+ (self at: index + 4) bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 7)]]
			ifFalse:
				[(byte := self at: index + 7) <= 16rF ifTrue:
					[^((((((((byte bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 4)) bitShift: 8)
						+ (self at: index + 3) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index)]]].
	bigEndian ifFalse: [
		(byte := self at: index + 7) = 0 ifFalse: [
			^(LargePositiveInteger new: 8)
				replaceFrom: 1 to: 8 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 6) = 0 ifFalse: [
			^(LargePositiveInteger new: 7)
				replaceFrom: 1 to: 7 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 5) = 0 ifFalse: [
			^(LargePositiveInteger new: 6)
				replaceFrom: 1 to: 6 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 4) = 0 ifFalse: [
			^(LargePositiveInteger new: 5)
				replaceFrom: 1 to: 5 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 3) <= 16r3F ifFalse: [
			^(LargePositiveInteger new: 4)
				replaceFrom: 1 to: 4 with: self startingAt: index;
				normalize ].
		^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
	(byte := self at: index) = 0 ifFalse: [
		^(LargePositiveInteger new: 8)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: (self at: index + 2);
			digitAt: 7 put: (self at: index + 1);
			digitAt: 8 put: byte;
			normalize ].
	(byte := self at: index + 1) = 0 ifFalse: [	
		^(LargePositiveInteger new: 7)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: (self at: index + 2);
			digitAt: 7 put: byte;
			normalize ].
	(byte := self at: index + 2) = 0 ifFalse: [	
		^(LargePositiveInteger new: 6)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: byte;
			normalize ].
	(byte := self at: index + 3) = 0 ifFalse: [	
		^(LargePositiveInteger new: 5)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: byte;
			normalize ].		
	(byte := self at: index + 4) <= 16r3F ifFalse: [
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: byte;
			normalize ].
	^(((byte bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 7)
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint64At: index put: value bigEndian: bigEndian [
	"Store a 64-bit unsigned integer quantity starting from the given byte index"
	
	| i j |
	value isLarge ifTrue: [
		i := value digitLength.
		bigEndian ifFalse: [
			self
				replaceFrom: index
					to: index + i - 1
					with: value 
					startingAt: 1;
				replaceFrom: index + i
					to: index + 7
					with: #[0 0 0 0 0 0 0 0]
					startingAt: 1.
			^value ].
		j := index + 8.
		i <= 7 ifTrue: [
			self
				replaceFrom: index
				to: j - i - 1
				with: #[0 0 0 0 0 0 0 0]
				startingAt: 1 ].
		[ 1 <= i ] whileTrue: [
			self at: j - i put: (value digitAt: i).
			i := i - 1 ].
		^value ].
	bigEndian ifFalse: [
		j := index - 1.
		i := value.
		[ 1 <= i ] whileTrue: [
			self at: (j := j + 1) put: (i bitAnd: 16rFF).
			i := i bitShift: -8 ].
		self replaceFrom: j + 1
			to: index + 7
			with: #[0 0 0 0 0 0 0 0]
			startingAt: 1.
		^value ].
	j := index + 8.
	i := value.
	[ 1 <= i ] whileTrue: [
		self at: (j := j - 1) put: (i bitAnd: 16rFF).
		i := i bitShift: -8 ].
	self replaceFrom: index
		to: j - 1
		with: #[0 0 0 0 0 0 0 0]
		startingAt: 1.
	^value
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint8At: index bigEndian: dummyArgument [
	"Return a 8-bit unsigned integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	^self at: index
]

{ #category : 'accessing - Numeric Types' }
ByteArray >> uint8At: index put: value bigEndian: dummyArgument [
	"Store a 8-bit unsigned integer quantity at the given byte index.
	Endiannes is irrelevant for 8 bit quantities. dummyArgument is included for consistency,
	and because #int8At: is implemented in FFI package."

	^self at: index put: value
]
