'From Cuis 6.0 [latest update: #5789] on 6 May 2023 at 5:59:31 pm'!

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 17:30:23'!
float32SlotAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 17:31:19'!
float32SlotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self float32SlotAt: index put: value asFloat ].
	^value! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 17:32:08'!
int32SlotAt: index
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word := self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 17:32:13'!
int32SlotAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word _ 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/6/2023 16:52:01'!
uint32SlotAt: index

	^self basicAt: index! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/6/2023 16:52:06'!
uint32SlotAt: index put: anInteger

	^self basicAt: index put: anInteger! !


!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 17:27:44'!
float32At: index bigEndian: bool 
	"Return a 32 bit float starting from the given byte index.
	We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float."
	| w |
	w := self uint32At: index bigEndian: bool.
	^ Float fromIEEE32Bit: w! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 17:28:17'!
float32At: index put: value bigEndian: bool 
	"Store a 32 bit float starting from the given byte index.
	Round value to a 32bit Float, and store it."
	| w |
	w := value asIEEE32BitWord.
	self uint32At: index put: w bigEndian: bool.
	^ value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 17:27:48'!
float64At: index bigEndian: bool 
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
	^ dbl! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 17:28:21'!
float64At: index put: value bigEndian: bool 
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self uint32At: index put: w1 bigEndian: bool.
	self uint32At: index + 4 put: w2 bigEndian: bool.
	^ value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/6/2015 23:16'!
int16At: index bigEndian: bigEndian
	"Return a 16-bit signed integer quantity starting from the given byte index"

	| result |
	result := bigEndian
		ifFalse: [ ((self at: index + 1) bitShift: 8) + (self at: index) ]
		ifTrue: [ ((self at: index) bitShift: 8) + (self at: index + 1) ].
	result < 16r8000 ifTrue: [ ^result ].
	^result - 16r10000! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/6/2015 23:16'!
int16At: index put: value bigEndian: bigEndian
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
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/9/2015 20:28'!
int32At: index bigEndian: bigEndian
	"Return a 32-bit integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"
	
	| byte result |
	bigEndian ifFalse: [
		(byte := self at: index + 3) <= 16r7F ifTrue: [ "Is the result non-negative?"
			byte <= 16r3F ifTrue: [
				^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
			^(LargePositiveInteger new: 4)
				replaceFrom: 1
					to: 4
					with: self
					startingAt: index;
				normalize ].
		"Negative"
		byte >= 16rC0 ifTrue: [
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
		byte <= 16r3F ifTrue: [
			^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3) ].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize ].
	"Negative"
	16rC0 <= byte ifTrue: [
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
		- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this."! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/6/2015 17:13'!
int32At: index put: value bigEndian: bigEndian
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
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 17:27:16'!
int64At: index bigEndian: bigEndian
	"Return a 64-bit signed integer quantity starting from the given byte index."

	| value |
	value := self uint64At: index bigEndian: bigEndian.
	value digitLength < 8 ifTrue: [ ^value ].
	(value digitAt: 8) < 16r80 ifTrue: [ ^value ].
	^value - 16r10000000000000000! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 17:27:27'!
int64At: index put: value bigEndian: bigEndian
	"Store a 64-bit signed integer quantity starting from the given byte index."
	
	^self
		uint64At: index
		put: (value negative
			ifFalse: [ value ]
			ifTrue: [ value + 16r10000000000000000 ])
		bigEndian: bigEndian! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 20:05:57'!
int8At: index
	"Return a 8-bit signed integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities"

	| result |
	result := self at: index.
	result < 16r80 ifTrue: [ ^result ].
	^result - 16r100! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 20:06:05'!
int8At: index put: value
	"Store a 8-bit signed integer quantity at the given byte index
	Endiannes is irrelevant for 8 bit quantities"

	| unsignedValue |
	(unsignedValue := value) < 0 ifTrue: [
		unsignedValue := unsignedValue + 16r100 ].
	self at: index put: unsignedValue.
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/3/2015 01:33'!
uint16At: index bigEndian: bigEndian
	"Return a 16-bit unsigned integer quantity starting from the given byte index"

	bigEndian ifFalse: [ ^((self at: index + 1) bitShift: 8) + (self at: index) ].
	^((self at: index) bitShift: 8) + (self at: index + 1)
	! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/3/2015 15:29'!
uint16At: index put: value bigEndian: bigEndian
	"Store a 16-bit unsigned integer quantity starting from the given byte index"
	
	bigEndian ifFalse: [
		self 
			at: index + 1 put: (value bitShift: -8);
			at: index put: (value bitAnd: 16rFF).
		^value ].
	self
		at: index put: (value bitShift: -8);
		at: index+1 put: (value bitAnd: 16rFF).
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'eem 2/22/2016 09:20'!
uint32At: index bigEndian: bigEndian
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
		normalize! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/3/2015 01:33'!
uint32At: index put: value bigEndian: bigEndian
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
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'eem 2/22/2016 09:36'!
uint64At: index bigEndian: bigEndian
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
	^(((byte bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 7)! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'ul 9/9/2015 20:18'!
uint64At: index put: value bigEndian: bigEndian
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
	^value! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 20:05:52'!
uint8At: index
	"Return a 8-bit unsigned integer quantity from the given byte index.
	Endiannes is irrelevant for 8 bit quantities"

	^self at: index! !

!ByteArray methodsFor: 'access - Numeric Types' stamp: 'jmv 5/5/2023 20:06:11'!
uint8At: index put: value
	"Store a 8-bit unsigned integer quantity at the given byte index.
	Endiannes is irrelevant for 8 bit quantities"

	^self at: index put: value! !


!Stream methodsFor: 'testing' stamp: 'jmv 5/6/2023 16:31:01'!
nextUint32WordsPutAll: aCollection
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	aCollection class isPointers | aCollection class isWords not 
		ifTrue: [^self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do:[:i|
		self nextUint32Put: (aCollection basicAt: i) bigEndian: true ].
	^aCollection! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:11:46'!
nextFloat64BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: false ])
			readStream nextFloat64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: true ])
			readStream nextFloat64BigEndian: true
	"
	| bytes |
	bytes := self next: 8.
	^ bytes float64At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:23:04'!
nextFloat64Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextFloat64Put: Float pi bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 8.
	bytes float64At: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:34:51'!
nextInt16BigEndian: bigEndian
	"Answer the next  signed, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: false ]) 
			readStream nextInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: true ]) 
			readStream nextInt16BigEndian: true
	"
	| bytes |
	bytes := self next: 2.
	^ bytes int16At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:35:32'!
nextInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 16-bit integer on this (binary) stream.

	(16r10000-12345) hex '16rCFC7'

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt16Put: -12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes int16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:36:17'!
nextInt32BigEndian: bigEndian
	"Answer the next signed, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: false ]) 
			readStream nextInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: true ]) 
			readStream nextInt32BigEndian: true
	"
	| bytes |
	bytes := self next: 4.
	^ bytes int32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:36:30'!
nextInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 32-bit integer on this (binary) stream.

	(16r100000000-123456) hex '16rFFFE1DC0'

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextInt32Put: -123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes int32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:37:24'!
nextUint16BigEndian: bigEndian
	"Answer the next unsigned, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: false ]) 
			readStream nextUint16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: true ]) 
			readStream nextUint16BigEndian: true
	"
	| bytes |
	bytes := self next: 2.
	^ bytes uint16At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:37:38'!
nextUint16Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 16-bit integer on this (binary) stream.

	12345 hex  '16r3039'

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint16Put: 12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 2.
	bytes uint16At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:38:20'!
nextUint32BigEndian: bigEndian
	"Answer the next unsigned, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: false ]) 
			readStream nextUint32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: true ]) 
			readStream nextUint32BigEndian: true
	"
	| bytes |
	bytes := self next: 4.
	^ bytes uint32At: 1 bigEndian: bigEndian! !

!Stream methodsFor: 'normalized access' stamp: 'jmv 5/5/2023 17:38:44'!
nextUint32Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 32-bit integer on this (binary) stream.

	123456 hex '16r1E240'

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUint32Put: 123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes := ByteArray new: 4.
	bytes uint32At: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes! !


!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 5/5/2023 17:39:01'!
nextUint32
	"Read a 32-bit unsigned quantity from the stream.
	Big Endian"

	^ stream nextUint32BigEndian: true! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'jmv 5/5/2023 17:39:17'!
nextUint32Put: aNumber
	"Write out a 32-bit integer as 32 bits.
	Big Endian"

	stream nextUint32Put: aNumber bigEndian: true.
	^aNumber! !


!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/6/2023 16:32:20'!
nextInt16

	| value |
	value := fontData int16At: offset bigEndian: true.
	offset := offset + 2.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/6/2023 16:36:37'!
nextInt32

	| value |
	value := fontData int32At: offset bigEndian: true.
	offset := offset + 4.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/6/2023 16:34:12'!
nextUint16

	| value |
	value := fontData uint16At: offset bigEndian: true.
	offset := offset + 2.
	^value! !

!TTFontTableDirEntry methodsFor: 'accessing' stamp: 'jmv 5/6/2023 16:36:52'!
nextUint32

	| value |
	value := fontData uint32At: offset bigEndian: true.
	offset := offset + 4.
	^value! !

