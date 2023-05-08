'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 4:15:33 pm'!
'Description '!
!provides: 'Collections-CompactArrays' 1 13!
SystemOrganization addCategory: 'Collections-CompactArrays'!


!classDefinition: #Float32PointArray category: 'Collections-CompactArrays'!
Float32Array variableWordSubclass: #Float32PointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-CompactArrays'!
!classDefinition: 'Float32PointArray class' category: 'Collections-CompactArrays'!
Float32PointArray class
	instanceVariableNames: ''!

!classDefinition: #PointArray category: 'Collections-CompactArrays'!
IntegerArray variableWordSubclass: #PointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-CompactArrays'!
!classDefinition: 'PointArray class' category: 'Collections-CompactArrays'!
PointArray class
	instanceVariableNames: ''!

!classDefinition: #ShortIntegerArray category: 'Collections-CompactArrays'!
ArrayedCollection variableWordSubclass: #ShortIntegerArray
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder'
	poolDictionaries: ''
	category: 'Collections-CompactArrays'!
!classDefinition: 'ShortIntegerArray class' category: 'Collections-CompactArrays'!
ShortIntegerArray class
	instanceVariableNames: ''!

!classDefinition: #ShortPointArray category: 'Collections-CompactArrays'!
ShortIntegerArray variableWordSubclass: #ShortPointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-CompactArrays'!
!classDefinition: 'ShortPointArray class' category: 'Collections-CompactArrays'!
ShortPointArray class
	instanceVariableNames: ''!

!classDefinition: #ShortWordArray category: 'Collections-CompactArrays'!
ShortIntegerArray variableWordSubclass: #ShortWordArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-CompactArrays'!
!classDefinition: 'ShortWordArray class' category: 'Collections-CompactArrays'!
ShortWordArray class
	instanceVariableNames: ''!

!classDefinition: #ShortRunArray category: 'Collections-CompactArrays'!
ArrayedCollection variableWordSubclass: #ShortRunArray
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder'
	poolDictionaries: ''
	category: 'Collections-CompactArrays'!
!classDefinition: 'ShortRunArray class' category: 'Collections-CompactArrays'!
ShortRunArray class
	instanceVariableNames: ''!


!Float32PointArray commentStamp: '<historical>' prior: 0!
This class stores points with 32 bit Floating point elements.

{1@2. 3@4} as: Float32PointArray!

!PointArray commentStamp: '<historical>' prior: 0!
This class stores 32bit Integer points in place. It is used to pass data efficiently to the primitive level during high-bandwidth 2D graphics operations.!

!ShortIntegerArray commentStamp: '<historical>' prior: 0!
ShortIntegerArray is an array for efficiently representing signed integers in the 16bit range, i.e. between -16r8000 and 16r7FFF.

Additionaly, using #unsignedAt:put: and #signedAt:put:, it supports unsigned 16 bits integers, between 0 and 16rFFFF. You can also alternatively ShortWordArray to store unsigned 16 bit integers with the usual #at: and #at:put: protocol.

Array size must be even, as two vales are stored in each word.!

!ShortPointArray commentStamp: '<historical>' prior: 0!
This class stores points that are in short integer range (e.g., -32767 <= value <= 32768). It is used to pass data efficiently to the primitive level during high-bandwidth 2D graphics operations.!

!ShortWordArray commentStamp: '<historical>' prior: 0!
ShortWordArrays store 16-bit unsigned Integer values, between 0 and 16rFFFF.!

!ShortRunArray commentStamp: '<historical>' prior: 0!
This class is run-length encoded representation of short integer (e.g., 16bit signed integer values)!

!ShortIntegerArray class methodsFor: 'class initialization' stamp: 'jmv 12/18/2015 15:29'!
initClassCachedState
	"Check if the word order has changed from the last save"

	((LastSaveOrder at: 1) = 42 and:[(LastSaveOrder at: 2) = 13]) 
		ifTrue:[^self]. "Okay"
	((LastSaveOrder at: 2) = 42 and:[(LastSaveOrder at: 1) = 13]) 
		ifTrue:[^self swapShortObjects]. "Reverse guys"
	^self error:'This must never happen'! !

!ShortRunArray class methodsFor: 'class initialization' stamp: 'jmv 12/18/2015 15:29'!
initClassCachedState
	"Check if the word order has changed from the last save"

	((LastSaveOrder valueAtRun: 1) = 42 and:[(LastSaveOrder lengthAtRun: 1) = 3]) 
		ifTrue:[^self]. "Okay"
	((LastSaveOrder lengthAtRun: 1) = 42 and:[(LastSaveOrder valueAtRun: 1) = 3]) 
		ifTrue:[^self swapRuns]. "Reverse guys"
	^self error:'This must never happen'! !

!ShortRunArray methodsFor: 'printing' stamp: 'ar 11/3/1998 17:41'!
printOn: aStream
	aStream nextPutAll: self class name; nextPutAll:' ( '.
	self lengthsAndValuesDo:[:runLength :runValue |
		aStream
			nextPutAll:' (';
			print: runLength;
			space;
			print: runValue;
			nextPut:$).
	].
	aStream nextPutAll:' )'.! !

!Float32PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:11:30'!
at: index
	"Return the element (e.g., point) at the given index"
	^(self floatAt: index * 2 - 1) @ (self floatAt: index * 2)! !

!Float32PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:11:49'!
at: index put: aPoint
	"Store the argument aPoint at the given index"
	self floatAt: index * 2 - 1 put: aPoint x.
	self floatAt: index * 2 put: aPoint y.
	^aPoint! !

!Float32PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:11:57'!
at: index putX: aNumber
	"Store the argument aPoint at the given index"
	self floatAt: index * 2 - 1 put: aNumber! !

!Float32PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:12:05'!
at: index putY: aNumber
	"Store the argument aPoint at the given index"
	self floatAt: index * 2 put: aNumber! !

!Float32PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:04:36'!
bytesPerElement

	^ 8! !

!Float32PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:02:36'!
defaultElement
	"Return the default element of the receiver"
	^`0.0@0.0`! !

!Float32PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:12:17'!
size
	"Return the number of elements in the receiver"
	^ super size // 2! !

!Float32PointArray methodsFor: 'printing' stamp: 'jmv 2/28/2020 14:59:07'!
printElementsOn: aStream
	"FloatArray elements are answered as 64 bit Float, but are really 32 bit Float.
	When printing, print them as 32 bit Float."
	aStream nextPut: ${.
	self do: [ :point |
		point x printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream nextPut: $@.
		point y printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream nextPut: $..
		aStream space].
	self isEmpty ifFalse: [aStream skip: -2].
	aStream nextPut: $}! !

!Float32PointArray class methodsFor: 'instance creation' stamp: 'jmv 2/28/2020 12:13:28'!
new: n
	^super new: n*2! !

!PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:26:03'!
at: index
	"Return the element (e.g., point) at the given index"
	^(self integerAt: index * 2 - 1) @ (self integerAt: index * 2)! !

!PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:26:16'!
at: index put: aPoint
	"Store the argument aPoint at the given index"
	self integerAt: index * 2 - 1 put: aPoint x asInteger.
	self integerAt: index * 2 put: aPoint y asInteger.
	^aPoint! !

!PointArray methodsFor: 'accessing' stamp: 'jmv 5/16/2019 16:20:10'!
at: index putX: aNumber
	"Store the argument aPoint at the given index"
	super at: index * 2 - 1 put: aNumber asInteger! !

!PointArray methodsFor: 'accessing' stamp: 'jmv 5/16/2019 16:19:55'!
at: index putY: aNumber
	"Store the argument aPoint at the given index"
	super at: index * 2 put: aNumber asInteger! !

!PointArray methodsFor: 'accessing' stamp: 'ar 11/10/1998 19:41'!
bounds
	| min max |
	min _ max _ self at: 1.
	self do:[:pt|
		min _ min min: pt.
		max _ max max: pt].
	^min corner: max
		! !

!PointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:04:33'!
bytesPerElement

	^ 8! !

!PointArray methodsFor: 'accessing' stamp: 'ar 11/2/1998 12:19'!
defaultElement
	"Return the default element of the receiver"
	^0@0! !

!PointArray methodsFor: 'accessing' stamp: 'ar 11/2/1998 12:21'!
size
	"Return the number of elements in the receiver"
	^super size // 2! !

!PointArray class methodsFor: 'instance creation' stamp: 'ar 10/16/1998 00:04'!
new: n
	^super new: n*2! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:21'!
at: index
	"Return the 16-bit signed integer value at the given index of the receiver."

	^self signedAt: index! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:31:26'!
at: index put: value
	"Store the given 16-bit signed integer at the given index in the receiver."

	^self signedAt: index put: value! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 15:41'!
defaultElement
	^0! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:21'!
signedAt: index
	"Return the 16-bit signed integer value at the given index of the receiver."

	<primitive: 143>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber ifTrue: [^ self at: index truncated].
	self errorNonIntegerIndex.
! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:30:51'!
signedAt: index put: value
	"Store the given 16-bit signed integer at the given index in the receiver."

	<primitive: 144>
	index isInteger
		ifTrue: [
			(index >= 1 and: [index <= self size])
				ifTrue: [self errorImproperStore]
				ifFalse: [self errorSubscriptBounds: index]].
	index isNumber ifTrue: [^ self at: index truncated put: value].
	self errorNonIntegerIndex.
! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 15:41'!
size
	^super size * 2! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2013 16:52'!
unsignedAt: index
	"Return the 16-bit unsigned integer value at the given index of the receiver."

	| primValue |
	primValue _ self signedAt: index.
	^primValue < 0
		ifTrue: [
			primValue + 16r10000.		"Faster. No need for LargeInteger arithmetic"
			"(primValue + 1) negated bitInvert16" ]
		ifFalse: [ primValue ]! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:33:29'!
unsignedAt: index put: value
	"Store the given 16-bit unsigned integer at the given index in the receiver."

	| primValue |
	value < 0 ifTrue: [ self errorImproperStore ].
	value > 16rFFFF ifTrue: [ self errorImproperStore ].
	primValue _ value > 16r7FFF
			ifTrue: [
				value - 16r10000.		"Faster. No need for LargeInteger arithmetic"
				"(value bitInvert16 + 1) negated" ]
			ifFalse: [ value ].
	self signedAt: index put: primValue.
	^value! !

!ShortIntegerArray methodsFor: 'objects from disk' stamp: 'nk 3/7/2004 13:54'!
bytesPerElement
	^2! !

!ShortIntegerArray methodsFor: 'objects from disk' stamp: 'jmv 2/15/2008 00:48'!
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."

	| hack blt |
	Smalltalk isLittleEndian ifTrue: [
		"The implementation is a hack, but fast for large ranges"
		hack _ Form new hackBits: self.
		blt _ (BitBlt toForm: hack) sourceForm: hack.
		blt combinationRule: Form reverse.  "XOR"
		blt sourceY: 0; destY: 0; height: hack height; width: 1.
		blt sourceX: 0; destX: 1; copyBits.  "Exchange bytes 0 and 1"
		blt sourceX: 1; destX: 0; copyBits.
		blt sourceX: 0; destX: 1; copyBits.
		blt sourceX: 2; destX: 3; copyBits.  "Exchange bytes 2 and 3"
		blt sourceX: 3; destX: 2; copyBits.
		blt sourceX: 2; destX: 3; copyBits
	].
! !

!ShortIntegerArray methodsFor: 'objects from disk' stamp: 'jmv 2/26/2016 17:08'!
writeOn: aStream 

	| w |
	aStream nextSignedInt32Put: self basicSize bigEndian: true.

	1 to: self basicSize do: [ :i |
		w _ self basicAt: i.
		Smalltalk isLittleEndian
			ifFalse: [ aStream nextUnsignedInt32Put: w bigEndian: true ]
			ifTrue: [ aStream
				nextPut: (w digitAt: 2);
				nextPut: (w digitAt: 1);
				nextPut: (w digitAt: 4);
				nextPut: (w digitAt: 3) ]].! !

!ShortIntegerArray class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:56'!
initialize
	"
	ShortIntegerArray initialize.
	SystemDictionary initialize.
	"
	LastSaveOrder _ self new: 2.
	LastSaveOrder at: 1 put: 42.
	LastSaveOrder at: 2 put: 13! !

!ShortIntegerArray class methodsFor: 'class initialization' stamp: 'jmv 2/25/2016 10:51'!
swapShortObjects
	self allSubInstancesDo: [ :inst |
		BitBlt swapHalvesIn32BitWords: inst ]! !

!ShortIntegerArray class methodsFor: 'instance creation' stamp: 'jmv 9/4/2015 13:09'!
new: n
	"Instances are always of even size. Only accept those."
	n even ifFalse: [
		self error: 'ShortIntegerArray>>#new: must be called with an even argument' ].
	^ super new: n // 2! !

!ShortPointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:14:47'!
at: index
	"Return the element (e.g., point) at the given index"
	^(self signedAt: index * 2 - 1) @ (self signedAt: index * 2)! !

!ShortPointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:31:56'!
at: index put: aPoint
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 - 1 put: aPoint x asInteger.
	self signedAt: index * 2 put: aPoint y asInteger.
	^aPoint! !

!ShortPointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:32:06'!
at: index putX: aNumber
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 - 1 put: aNumber asInteger! !

!ShortPointArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:32:13'!
at: index putY: aNumber
	"Store the argument aPoint at the given index"
	self signedAt: index * 2 put: aNumber asInteger! !

!ShortPointArray methodsFor: 'accessing' stamp: 'ar 11/10/1998 19:41'!
bounds
	| min max |
	min _ max _ self at: 1.
	self do:[:pt|
		min _ min min: pt.
		max _ max max: pt].
	^min corner: max
		! !

!ShortPointArray methodsFor: 'accessing' stamp: 'yo 3/6/2004 12:56'!
bytesPerElement

	^ 4.
	! !

!ShortPointArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 15:43'!
defaultElement
	"Return the default element of the receiver"
	^0@0! !

!ShortPointArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 15:43'!
size
	^self basicSize! !

!ShortPointArray class methodsFor: 'instance creation' stamp: 'ar 1/15/1999 17:40'!
new: n
	^super new: n * 2! !

!ShortWordArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:23'!
at: index
	"Return the 16-bit unsigned integer value at the given index of the receiver."

	^self unsignedAt: index! !

!ShortWordArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:33:43'!
at: index put: value
	"Store the given 16-bit unsigned integer at the given index in the receiver."

	^self unsignedAt: index put: value! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:26'!
at: index
	"Return the short value at the given index"
	| rlIndex |
	index < 1 ifTrue:[^self errorSubscriptBounds: index].
	rlIndex _ index.
	self lengthsAndValuesDo:[:runLength :runValue|
		rlIndex <= runLength ifTrue:[^runValue].
		rlIndex _ rlIndex - runLength].
	"Not found. Must be out of range"
	^self errorSubscriptBounds: index! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:18'!
at: index put: value
	"ShortRunArrays are read-only"
	^self shouldNotImplement.! !

!ShortRunArray methodsFor: 'accessing' stamp: 'yo 3/6/2004 14:19'!
bytesPerElement

	^ 4
! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:39'!
compressionRatio
	"Return the compression ratio.
	The compression ratio is computed based
	on how much space would be needed to
	store the receiver in a ShortIntegerArray"
	^(self size asFloat * 0.5) "Would need only half of the amount in ShortIntegerArray"
		/ (self runSize max: 1)! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:21'!
lengthAtRun: index
	"Return the length of the run starting at the given index"
	^(self basicAt: index) bitShift: -16! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:28'!
runSize
	"Return the number of runs in the receiver"
	^self basicSize! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:28'!
size
	"Return the number of elements stored in the receiver"
	| n |
	n _ 0.
	"Note: The following loop is open-coded for speed"
	1 to: self basicSize do:[:i|
		n _ n + ((self basicAt: i) bitShift: -16).
	].
	^n! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:29'!
species
	"Answer the preferred class for reconstructing the receiver."
	^ShortIntegerArray! !

!ShortRunArray methodsFor: 'accessing' stamp: 'ar 11/3/1998 17:22'!
valueAtRun: index
	"Return the value of the run starting at the given index"
	| uShort |
	uShort _ (self basicAt: index) bitAnd: 16rFFFF.
	^(uShort bitAnd: 16r7FFF) - (uShort bitAnd: 16r8000)! !

!ShortRunArray methodsFor: 'enumerating' stamp: 'ar 11/3/1998 17:31'!
do: aBlock
	"Evaluate aBlock with all elements of the receiver"
	self lengthsAndValuesDo:[:runLength :runValue|
		"Use to:do: instead of timesRepeat: for compiler optimization"
		1 to: runLength do:[:i|
			aBlock value: runValue.
		].
	].! !

!ShortRunArray methodsFor: 'enumerating' stamp: 'ar 12/27/1999 13:44'!
lengthsAndValuesDo: aBlock
	"Evaluate aBlock with the length and value of each run in the receiver"
	^self runsAndValuesDo: aBlock! !

!ShortRunArray methodsFor: 'enumerating' stamp: 'ar 12/27/1999 13:44'!
runsAndValuesDo: aBlock
	"Evaluate aBlock with the length and value of each run in the receiver"
	| basicValue length value |
	1 to: self basicSize do:[:i|
		basicValue _ self basicAt: i.
		length _ basicValue bitShift: -16.
		value _ basicValue bitAnd: 16rFFFF.
		value _ (value bitAnd: 16r7FFF) - (value bitAnd: 16r8000).
		aBlock value: length value: value.
	].! !

!ShortRunArray methodsFor: 'enumerating' stamp: 'ar 11/3/1998 21:05'!
valuesCollect: aBlock
	"Evaluate aBlock with each of the receiver's values as the argument. 
	Collect the resulting values into a collection like the receiver. Answer 
	the new collection."
	| newArray newValue |
	newArray _ self class basicNew: self basicSize.
	1 to: self runSize do:[:i|
		newValue _ aBlock value: (self valueAtRun: i).
		newArray setRunAt: i toLength: (self lengthAtRun: i) value: newValue.
	].
	^newArray! !

!ShortRunArray methodsFor: 'enumerating' stamp: 'ar 11/15/1998 17:22'!
valuesDo: aBlock
	self lengthsAndValuesDo:[:runLength :runValue| aBlock value: runValue]! !

!ShortRunArray methodsFor: 'objects from disk' stamp: 'jmv 2/15/2008 00:49'!
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."

	| w b1 b2 b3 b4 |
	Smalltalk isLittleEndian ifTrue: [
		1 to: self basicSize do: [:i |
			w _ self basicAt: i.
			b1 _ w digitAt: 1.
			b2 _ w digitAt: 2.
			b3 _ w digitAt: 3.
			b4 _ w digitAt: 4.
			w _ (b1 << 24) + (b2 << 16) + (b3 << 8) + b4.
			self basicAt: i put: w.
		]
	].

! !

!ShortRunArray methodsFor: 'private' stamp: 'ar 11/3/1998 21:02'!
setRunAt: i toLength: runLength value: value
	(value < -16r7FFF or:[value > 16r8000]) ifTrue:[^self errorImproperStore].
	(runLength < 0 or:[runLength > 16rFFFF]) ifTrue:[^self errorImproperStore].
	self basicAt: i put: (runLength bitShift: 16) + 
		((value bitAnd: 16r7FFF) - (value bitAnd: -16r8000)).! !

!ShortRunArray methodsFor: 'private' stamp: 'ar 11/3/1998 21:00'!
setRuns: runArray values: valueArray
	| runLength value |
	1 to: runArray size do:[:i|
		runLength _ runArray at: i.
		value _ valueArray at: i.
		self setRunAt: i toLength: runLength value: value.
	].! !

!ShortRunArray class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:56'!
initialize
	"
	ShortRunArray initialize
	SystemDictionary initialize.
	"
	LastSaveOrder _ #(42 42 42) as: self! !

!ShortRunArray class methodsFor: 'class initialization' stamp: 'jmv 2/25/2016 10:52'!
swapRuns
	self allSubInstancesDo: [ :inst |
		BitBlt swapHalvesIn32BitWords: inst ]! !

!ShortRunArray class methodsFor: 'instance creation' stamp: 'ar 11/3/1998 17:12'!
new: n
	"ShortRunArrays must be created with either
		someCollection as: ShortRunArray
	or by using
		ShortRunArray runs: runCollection values: valueCollection.
	"
	^self shouldNotImplement! !

!ShortRunArray class methodsFor: 'instance creation' stamp: 'jmv 12/12/2014 17:12'!
newFrom: aCollection
	"Compress aCollection into a ShortRunArray"
	| lastValue lastRun runs values valueCount |
	aCollection isEmpty ifTrue: [ ^self runs:#() values: #() ].
	runs _ WriteStream on: (WordArray new: 100).
	values _ WriteStream on: (ShortIntegerArray new: 100).
	lastValue _ aCollection first.
	lastRun _ 0.
	valueCount _ 0.
	aCollection do: [ :item |
		(item = lastValue and:[lastRun < 16r8000]) ifTrue:[
			lastRun _ lastRun + 1.
		] ifFalse:[
			runs nextPut: lastRun.
			values nextPut: lastValue.
			lastRun _ 1.
			lastValue _ item.
			valueCount _ valueCount + 1.
		].
	].
	runs nextPut: lastRun.
	values nextPut: lastValue.
	valueCount _ valueCount + 1.
	valueCount odd ifTrue: [
		values nextPut: 0 ].
	^ self runs: runs contents values: values contents! !

!ShortRunArray class methodsFor: 'instance creation' stamp: 'ar 11/3/1998 17:12'!
runs: runCollection values: valueCollection
	^(self basicNew: runCollection size) setRuns: runCollection values: valueCollection! !

!Collection methodsFor: '*Collections-CompactArrays' stamp: 'jmv 12/18/2015 15:01'!
asPointArray
	"Answer a PointArray whose elements are the elements of the receiver"

	^self as: PointArray! !
ShortIntegerArray initialize!
ShortRunArray initialize!
