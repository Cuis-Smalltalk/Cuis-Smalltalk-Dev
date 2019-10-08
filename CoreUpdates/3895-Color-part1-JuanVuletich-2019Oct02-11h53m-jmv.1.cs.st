'From Cuis 5.0 [latest update: #3894] on 2 October 2019 at 11:53:53 am'!

!Color methodsFor: 'comparing' stamp: 'jmv 10/2/2019 20:37:24'!
primitiveEqual: aColor 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aColor class == self class ifFalse: [^ false].
	length _ self size.
	length = aColor size ifFalse: [^ false].
	1 to: self size do: [ :i |
		(self basicAt: i) = (aColor basicAt: i) ifFalse: [^ false]].
	^ true! !

!Color methodsFor: 'comparing' stamp: 'jmv 10/2/2019 20:31:03'!
= another 
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	^self primitiveEqual: another! !

!Color methodsFor: 'private' stamp: 'jmv 10/2/2019 11:40:28'!
slotAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!Color methodsFor: 'private' stamp: 'jmv 10/2/2019 11:40:37'!
slotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self slotAt: index put: value asFloat ].
	^value! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 10/2/2019 11:19:25'!
numSlots
	^3! !


!TranslucentColor class methodsFor: 'instance creation' stamp: 'jmv 10/2/2019 11:42:58'!
numSlots
	^4! !


!Color methodsFor: 'access' stamp: 'jmv 10/2/2019 11:25:11'!
blue
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ self slotAt: 3! !

!Color methodsFor: 'access' stamp: 'jmv 10/2/2019 11:25:44'!
green
	"Return the green component of this color, a float in the range [0.0..1.0]."

	^ self slotAt: 2! !

!Color methodsFor: 'access' stamp: 'jmv 10/2/2019 11:26:27'!
red
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ self slotAt: 1! !

!Color methodsFor: 'conversions' stamp: 'jmv 10/2/2019 11:26:14'!
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
			((((self slotAt: 1) * 255.999) truncated bitShift: 16) bitOr:
			((((self slotAt: 2) * 255.999) truncated bitShift: 8) bitOr: 
			(((self slotAt: 3) * 255.999) truncated))) ].

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

!Color methodsFor: 'queries' stamp: 'jmv 10/2/2019 11:25:51'!
isBlack
	"Return true if the receiver represents black"
	(self slotAt: 1) = 0.0 ifFalse: [ ^ false ].
	(self slotAt: 2) = 0.0 ifFalse: [ ^ false ].
	(self slotAt: 3) = 0.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'queries' stamp: 'jmv 10/2/2019 11:25:59'!
isWhite
	"Return true if the receiver represents white"
	(self slotAt: 1) = 1.0 ifFalse: [ ^ false ].
	(self slotAt: 2) = 1.0 ifFalse: [ ^ false ].
	(self slotAt: 3) = 1.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'private' stamp: 'jmv 10/2/2019 11:21:43'!
basicSetRed: r green: g blue: b
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	self
		slotAt: 1 put: r;
		slotAt: 2 put: g;
		slotAt: 3 put: b! !

!Color methodsFor: 'private' stamp: 'jmv 10/2/2019 11:25:15'!
clipToValidValues
	| v |
	1 to: self size do: [ :i |
		v _ self slotAt: i.
		v > 1 ifTrue: [self slotAt: i put: 1.0].
		v < 0 ifTrue: [self slotAt: i put: 0.0]]! !


!TranslucentColor methodsFor: 'accessing' stamp: 'jmv 10/2/2019 11:50:06'!
alpha
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ self slotAt: 4! !

!TranslucentColor methodsFor: 'private' stamp: 'jmv 10/2/2019 11:51:07'!
setRed: r green: g blue: b alpha: alphaValue

	self basicSetRed: r green: g blue: b.
	self slotAt: 4 put: alphaValue! !

!methodRemoval: Color #convertToCurrentVersion:refStream:!
Color removeSelector: #convertToCurrentVersion:refStream:!
