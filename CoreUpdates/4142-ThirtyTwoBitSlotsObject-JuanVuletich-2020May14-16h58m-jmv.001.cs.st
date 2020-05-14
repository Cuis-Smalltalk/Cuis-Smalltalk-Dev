'From Cuis 5.0 [latest update: #4140] on 14 May 2020 at 5:15:37 pm'!
!classDefinition: #ThirtyTwoBitSlotsObject category: #'Kernel-Objects'!
Object variableWordSubclass: #ThirtyTwoBitSlotsObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!ThirtyTwoBitSlotsObject commentStamp: '<historical>' prior: 0!
Abstract superclass for objects whose slots are 32 bit values.
- Each can be Float or Integer, but always 32 bit.
- They have a fixed size, defined by the class.
- They don't have collection protocol.!


!Float32SlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:04:33'!
floatSlotAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!Float32SlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:05:55'!
floatSlotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self floatSlotAt: index put: value asFloat ].
	^value! !


!ThirtyTwoBitSlotsObject methodsFor: 'accessing' stamp: 'jmv 5/14/2020 17:01:35'!
byteSize
	^self size * 4! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:13:27'!
bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	ThirtyTwoBitSlotsObject new:1 :: at: 1 put: 16rFF32791B ; bytesAt: 1 :: hex
	"

	| bytes word |
	bytes _ ByteArray new: 4.
	word _ self basicAt: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:14:38'!
bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	ThirtyTwoBitSlotsObject new: 1 :: bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF]; basicAt: 1 :: hex 
	"

	| word |
	word _ 0.
	4 to: 1 by: -1 do: [ :i | word _ word * 256 + (aByteArray at: i) ].
	self basicAt: index put: word! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:03:42'!
floatSlotAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:05:41'!
floatSlotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self floatSlotAt: index put: value asFloat ].
	^value! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:07:17'!
integerSlotAt: index
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word _ self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:07:30'!
integerSlotAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word _ 16r100000000 + anInteger"
				word _ (anInteger + 1) negated bitInvert32]
		ifFalse:[word _ anInteger].
	self  basicAt: index put: word.
	^anInteger! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:15:15'!
unsignedIntAt: index

	^self basicAt: index! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:15:24'!
unsignedIntAt: index put: anInteger

	^self basicAt: index put: anInteger! !


!ThirtyTwoBitSlotsObject class methodsFor: 'instance creation' stamp: 'jmv 5/14/2020 17:01:35'!
new
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numSlots! !

!ThirtyTwoBitSlotsObject class methodsFor: 'instance creation' stamp: 'jmv 5/14/2020 17:01:35'!
numSlots
	^0! !


!WordArray methodsFor: 'as yet unclassified' stamp: 'jmv 5/14/2020 17:09:03'!
at: x
	^super at: x! !

!WordArray methodsFor: 'as yet unclassified' stamp: 'jmv 5/14/2020 17:09:10'!
at: x put: y
	^super at: x put: y! !

!WordArray methodsFor: 'as yet unclassified' stamp: 'jmv 5/14/2020 17:10:29'!
basicAt: index 
	^super basicAt: index ! !

!WordArray methodsFor: 'as yet unclassified' stamp: 'jmv 5/14/2020 17:10:45'!
basicAt: x put: y
	^super basicAt: x put: y! !


!Color methodsFor: 'access' stamp: 'jmv 5/14/2020 17:04:45'!
blue
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ self floatSlotAt: 3! !

!Color methodsFor: 'access' stamp: 'jmv 5/14/2020 17:04:54'!
green
	"Return the green component of this color, a float in the range [0.0..1.0]."

	^ self floatSlotAt: 2! !

!Color methodsFor: 'access' stamp: 'jmv 5/14/2020 17:05:16'!
red
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ self floatSlotAt: 1! !

!Color methodsFor: 'conversions' stamp: 'jmv 5/14/2020 17:05:10'!
pixelValueForDepth: d
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8 and less than 32 (no Indexed colors, no real alpha), black maps to the darkest possible blue.
	Note that 
		Color transparent class = TranslucentColor
	this special case is handled in TranslucentColor >> #pixelValueForDepth:
	"

	| bitBltFakeBlack val |
	"Most common case"
	"eight bits per component; top 8 bits set to all ones (opaque alpha)"
	d = 32 ifTrue: [
		^ 16rFF000000 bitOr:
			((((self floatSlotAt: 1) * 255.999) truncated bitShift: 16) bitOr:
			((((self floatSlotAt: 2) * 255.999) truncated bitShift: 8) bitOr: 
			(((self floatSlotAt: 3) * 255.999) truncated))) ].

"Faster in 32 bit systems, but slower in 64 bits"
"	d = 32 ifTrue: [
		val _ LargePositiveInteger new: 4.
		val at: 3 put: ((self at: 1) * 255.999) truncated.
		val at: 2 put: ((self at: 2)  * 255.999) truncated.
		val at: 1 put: ((self at: 3)  * 255.999) truncated.
		val at: 4 put: 16rFF.
		^ val normalize]."

	d = 8 ifTrue: [^ self closestPixelValue8].  "common case"
	d < 8 ifTrue: [
		d = 4 ifTrue: [^ self closestPixelValue4].
		d = 2 ifTrue: [^ self closestPixelValue2].
		d = 1 ifTrue: [^ self closestPixelValue1]].

	"For the depth 16, pixelValue = 0 means transparent, black is represented as 16r8000 (rgb=0, pixelvalue !!= 0)."
	(d = 16) | (d = 15) ifTrue: [
		"five bits per component; top bits ignored"
		val _ ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0
			ifTrue: [d = 16 ifTrue: [16r8000] ifFalse: [1]]
			ifFalse: [val]].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 12 ifTrue: [  "for indexing a color map with 4 bits per color component"
		val _ ((self red * 15) rounded bitShift: 8) bitOr:
			(((self green * 15) rounded bitShift: 4) bitOr: 
			((self blue * 15) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	d = 9 ifTrue: [  "for indexing a color map with 3 bits per color component"
		val _ ((self red * 7) rounded bitShift: 6) bitOr:
			(((self green * 7) rounded bitShift: 3) bitOr: 
			((self blue * 7) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	self error: 'unknown pixel depth: ', d printString! !

!Color methodsFor: 'queries' stamp: 'jmv 5/14/2020 17:04:58'!
isBlack
	"Return true if the receiver represents black"
	(self floatSlotAt: 1) = 0.0 ifFalse: [ ^ false ].
	(self floatSlotAt: 2) = 0.0 ifFalse: [ ^ false ].
	(self floatSlotAt: 3) = 0.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'queries' stamp: 'jmv 5/14/2020 17:05:03'!
isWhite
	"Return true if the receiver represents white"
	(self floatSlotAt: 1) = 1.0 ifFalse: [ ^ false ].
	(self floatSlotAt: 2) = 1.0 ifFalse: [ ^ false ].
	(self floatSlotAt: 3) = 1.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'private' stamp: 'jmv 5/14/2020 17:05:35'!
basicSetRed: r green: g blue: b
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	self
		floatSlotAt: 1 put: r;
		floatSlotAt: 2 put: g;
		floatSlotAt: 3 put: b! !

!Color methodsFor: 'private' stamp: 'jmv 5/14/2020 17:05:49'!
clipToValidValues
	| v |
	1 to: self size do: [ :i |
		v _ self floatSlotAt: i.
		v > 1 ifTrue: [self floatSlotAt: i put: 1.0].
		v < 0 ifTrue: [self floatSlotAt: i put: 0.0]]! !


!TranslucentColor methodsFor: 'accessing' stamp: 'jmv 5/14/2020 17:05:20'!
alpha
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ self floatSlotAt: 4! !

!TranslucentColor methodsFor: 'private' stamp: 'jmv 5/14/2020 17:06:02'!
setRed: r green: g blue: b alpha: alphaValue

	self basicSetRed: r green: g blue: b.
	self floatSlotAt: 4 put: alphaValue! !

!methodRemoval: ThirtyTwoBitSlotsObject #integerSloatAt: stamp: 'jmv 5/14/2020 17:07:09'!
ThirtyTwoBitSlotsObject removeSelector: #integerSloatAt:!
!methodRemoval: ThirtyTwoBitSlotsObject #slotAt:put: stamp: 'jmv 5/14/2020 17:03:56'!
ThirtyTwoBitSlotsObject removeSelector: #slotAt:put:!
!methodRemoval: ThirtyTwoBitSlotsObject #slotAt: stamp: 'jmv 5/14/2020 17:03:54'!
ThirtyTwoBitSlotsObject removeSelector: #slotAt:!
!methodRemoval: Float32SlotsObject #slotAt: stamp: 'jmv 5/14/2020 17:05:26'!
Float32SlotsObject removeSelector: #slotAt:!
!methodRemoval: Float32SlotsObject #slotAt:put: stamp: 'jmv 5/14/2020 17:06:09'!
Float32SlotsObject removeSelector: #slotAt:put:!

!ThirtyTwoBitSlotsObject reorganize!
('accessing' byteSize)
('private' bytesAt: bytesAt:put: floatSlotAt: floatSlotAt:put: integerSlotAt: integerSlotAt:put: unsignedIntAt: unsignedIntAt:put:)
!

