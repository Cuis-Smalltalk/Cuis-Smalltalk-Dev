'From Cuis 5.0 of 7 November 2016 [latest update: #3482] on 18 October 2018 at 11:06:39 am'!

!Float methodsFor: 'mathematical functions' stamp: 'RNG 10/12/2018 05:43:21'!
sign: aNumber
	"Return a Number with the same sign as aNumber and same magnitude as self.
	Implementation is different from super to handle the special case of Float negativeZero."

	(self isZero and: [aNumber sign negative]) ifTrue: [^Float negativeZero].
	^aNumber copySignTo: self! !

!Float methodsFor: 'converting' stamp: 'jmv 10/18/2018 10:56:50'!
asIEEE32BitWord
	"Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format.
	Used for conversion in FloatArrays only."
	
	| word1 word2 sign mantissa exponent destWord truncatedBits mask roundToUpper |
	
	"quickly skip positive and negative zero"
	self isZero ifTrue: [^self basicAt: 1].
	
	"retrieve 64 bits of IEEE 754 double"
	word1 := self basicAt: 1.
	word2 := self basicAt: 2.
	
	"prepare sign exponent and mantissa of 32 bits float"
	sign := word1 bitAnd: 16r80000000.
	exponent := ((word1 bitShift: -20) bitAnd: 16r7FF) - 1023 + 127.
	mantissa := (word2 bitShift: -29) + ((word1 bitAnd:  16rFFFFF) bitShift: 3).
	truncatedBits := (word2 bitAnd: 16r1FFFFFFF).

	"We must now honour default IEEE rounding mode (round to nearest even)"
	
	"we are below gradual underflow, even if rounded to upper mantissa"
	exponent < -24 ifTrue: [^sign "this can be negative zero"].
	
	"BEWARE: rounding occurs on less than 23bits when gradual underflow"
	exponent <= 0
		ifTrue:
			[mask := 1 bitShift: exponent negated.
			mantissa := mantissa bitOr: 16r800000.
			roundToUpper := (mantissa bitAnd: mask) isZero not
				and: [truncatedBits isZero not
					or: [(mantissa bitAnd: mask - 1) isZero not
						or: [(mantissa bitAnd: mask*2) isZero not]]].
			mantissa := mantissa bitShift: exponent - 1.
			"exponent := exponent + 1"]
		ifFalse:
			[roundToUpper := (truncatedBits bitAnd: 16r10000000) isZero not
				and: [(mantissa bitAnd: 16r1) isZero not
					or: [(truncatedBits bitAnd: 16r0FFFFFFF) isZero not]]
			].
		
	"adjust mantissa and exponent due to IEEE rounding mode"
	roundToUpper
		ifTrue:
			[mantissa := mantissa + 1.
			mantissa > 16r7FFFFF
				ifTrue:
					[mantissa := 0.
					exponent := exponent+1]].

	exponent > 254 ifTrue: ["Overflow"
		exponent := 255.
		self isNaN
			ifTrue: [mantissa isZero
				ifTrue: ["BEWARE: do not convert a NaN to infinity due to truncatedBits"
					mantissa := 1]]
			ifFalse: [mantissa := 0]].
		
	"Encode the word"
	destWord := (sign bitOr: ((exponent max: 0) bitShift: 23)) bitOr: mantissa.
	^ destWord! !

!Float methodsFor: 'converting' stamp: 'RNG 10/12/2018 05:44:04'!
withNegativeSign
	"Same as super, but handle the subtle case of Float negativeZero"
	
	self isZero ifTrue: [^self class negativeZero].  
	^super withNegativeSign! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 10/18/2018 10:59:41'!
predecessor
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	ulp := self ulp.
	^self - (0.5 * ulp) = self
		ifTrue: [self - ulp]
		ifFalse: [self - (0.5 * ulp)]! !

!Float methodsFor: 'truncation and round off' stamp: 'RNG 10/12/2018 05:43:53'!
ulp
	"Answer the unit of least precision of self (the power of two corresponding to last bit of mantissa)"
	
	| exponent |
	self isFinite ifFalse: [^self abs].
	self isZero ifTrue: [^Float fmin].
	exponent := self exponent.
	^exponent < self class emin
		ifTrue: [Float fminDenormalized]
 		ifFalse: [Float epsilon timesTwoPower: exponent]! !

!Float methodsFor: 'testing' stamp: 'RNG 10/12/2018 05:46:10'!
isFinite
	"simple, byte-order independent test for rejecting Not-a-Number and (Negative)Infinity"

	^(self - self) isZero! !

!Float methodsFor: 'printing' stamp: 'RNG 10/12/2018 05:43:01'!
printAsIEEE32BitPrecisionFloatOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:mantissaSignificantBits:
	Print as a 32 bit Float" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base mantissaSignificantBits: 24]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self isZero
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base mantissaSignificantBits: 24]]! !

!Float methodsFor: 'printing' stamp: 'RNG 10/12/2018 05:43:12'!
printOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self isZero
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base]]! !

!Float methodsFor: 'printing' stamp: 'RNG 10/12/2018 05:43:39'!
storeOn: aStream base: base 
	"Print the Number exactly so it can be interpreted back unchanged"
	self isFinite
		ifTrue: [self sign = -1 ifTrue: [aStream nextPutAll: '-'].
			base = 10 ifFalse: [aStream print: base; nextPut: $r].
			self isZero
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self abs absPrintExactlyOn: aStream base: base]]
		ifFalse: [self isNaN
				ifTrue: [aStream nextPutAll: 'Float nan']
				ifFalse: [self > 0.0
						ifTrue: [aStream nextPutAll: 'Float infinity']
						ifFalse: [aStream nextPutAll: 'Float infinity negated']]]! !


!Color methodsFor: 'access' stamp: 'RNG 10/12/2018 05:45:04'!
hue
	"Return the hue of this color, an angle in the range [0.0..360.0].
	Color orange hue
	"

	| r g b max min span h |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	span _ (max - min) asFloat.
	span isZero ifTrue: [ ^ 0.0 ].

	r = max ifTrue: [
		h _ ((g - b) asFloat / span) * 60.0.
	] ifFalse: [
		g = max
			ifTrue: [ h _ 120.0 + (((b - r) asFloat / span) * 60.0). ]
			ifFalse: [ h _ 240.0 + (((r - g) asFloat / span) * 60.0). ].
	].

	^h mod: 360.0! !

!Color methodsFor: 'access' stamp: 'RNG 10/12/2018 05:45:46'!
saturation
	"Return the saturation of this color, a value between 0.0 and 1.0.
	Color red saturation
	Color gray saturation
	"

	| r g b max min |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	max isZero ifTrue: [ ^0.0 ].
	^max - min / max! !


!Color class methodsFor: 'class initialization' stamp: 'RNG 10/12/2018 05:45:57'!
initializeGrayToIndexMap
	"Build an array of gray values available in the 8-bit colormap. This array is indexed by a gray level between black (1) and white (256) and returns the pixel value for the corresponding gray level."
	"Note: This method must be called after initializeIndexedColors, since it uses IndexedColors."
	"
	Color initializeGrayToIndexMap
	"

	| grayLevels grayIndices c distToClosest dist indexOfClosest indexedColors |
	"record the level and index of each gray in the 8-bit color table"
	grayLevels _ OrderedCollection new.
	grayIndices _ OrderedCollection new.
	indexedColors _ self indexedColors.
	"Note: skip the first entry, which is reserved for transparent"
	2 to: indexedColors size do: [:i |
		c _ indexedColors at: i.
		c saturation isZero ifTrue: [  "c is a gray"
			grayLevels add: (c green * 255) rounded.  "0 to 255; R, G, and B are the same"
			grayIndices add: i - 1]].  "pixel values are zero-based"
	grayLevels _ grayLevels asArray.
	grayIndices _ grayIndices asArray.

	"for each gray level in [0..255], select the closest match"
	GrayToIndexMap _ ByteArray new: 256.
	0 to: 255 do: [ :level |
		distToClosest _ 10000.  "greater than distance to any real gray"
		1 to: grayLevels size do: [:i |
			dist _ (level - (grayLevels at: i)) abs.
			dist < distToClosest ifTrue: [
				distToClosest _ dist.
				indexOfClosest _ grayIndices at: i]].
		GrayToIndexMap at: (level + 1) put: indexOfClosest]! !


!TranslucentColor methodsFor: 'queries' stamp: 'RNG 10/12/2018 05:46:53'!
isTransparent
	^ self alpha isZero! !

