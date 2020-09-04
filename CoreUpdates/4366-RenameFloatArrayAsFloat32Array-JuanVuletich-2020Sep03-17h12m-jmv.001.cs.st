'From Cuis 5.0 [latest update: #4365] on 3 September 2020 at 5:18:06 pm'!
Smalltalk renameClassNamed: #FloatArray as: #Float32Array!

!Float32Array commentStamp: '<historical>' prior: 0!
Float32Arrays store 32bit IEEE floating point numbers.!


!Float64Array commentStamp: '<historical>' prior: 0!
Float64Arrays store 64bit IEEE floating point numbers, i.e. instances of the Float class.
Some support is included for subclasses in the style of Balloon3D-Math.

Uses the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits. See #floatAt: and #floatAt:put:!


!Float methodsFor: 'converting' stamp: 'jmv 9/3/2020 17:15:41'!
asIEEE32BitWord
	"Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format.
	Used for conversion in Float32Arrays only."
	
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


!Float class methodsFor: 'instance creation' stamp: 'jmv 9/3/2020 17:15:47'!
fromIEEE32Bit: word
  	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from
	a 32bit IEEE floating point representation into an actual Float object (being
	64bit wide). Should only be used for conversion in Float32Arrays or likewise objects."
  	
 	| sign exponent mantissa exponentBits fractionBits answerFractionBits delta signBit answerExponent |
  	word negative ifTrue: [ ^ self error: 'Cannot deal with negative numbers' ].
  	word = 0 ifTrue: [ ^ Float zero ].
 	word = 16r80000000 ifTrue: [ ^Float negativeZero ].
  	
	signBit _ word bitAnd: 16r80000000.
	sign _ (word bitShift: -31) = 0 ifTrue: [1] ifFalse: [-1].
  	exponentBits _ (word bitShift: -23) bitAnd: 16rFF.
  	fractionBits _ word bitAnd: 16r7FFFFF.
  
	" Special cases: infinites and NaN"
	exponentBits = 16rFF ifTrue: [
  		fractionBits = 0 ifFalse: [ ^ Float nan ].
  		^ sign positive
  			ifTrue: [ Float infinity ]
  			ifFalse: [ Float negativeInfinity ]].

	" Unbias exponent: 16r3FF is bias"
	exponent _ exponentBits - 16r7F.

"Older version."
false ifTrue: [
	" Replace omitted leading 1 in fraction if appropriate"
	"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"
 	exponentBits = 0
 		ifTrue: [
 			"gradual underflow (denormalized number)
 			There is no implied one, but the exponent is -126"
			mantissa _ fractionBits.
			answerExponent _ exponent + 1 ]
 		ifFalse: [
			mantissa _ fractionBits + 16r800000.
			answerExponent _ exponent ].
		^ (sign * mantissa) asFloat timesTwoPower: answerExponent - 23 ].

	"Newer version"
 	exponentBits = 0
		ifTrue: [
			"gradual underflow (denormalized number)
			 Remove first bit of mantissa and adjust exponent"
			delta := fractionBits highBit.
			answerFractionBits := (fractionBits bitAnd: (1 bitShift: delta - 1) - 1) bitShift: 24 - delta.
			answerExponent := exponent + delta - 23]
		ifFalse: [
			answerFractionBits _ fractionBits.
			answerExponent _ exponent ].
  
  	"Create new float"
	^ (self basicNew: 2)
		basicAt: 1 put: ((signBit bitOr: (1023 + answerExponent bitShift: 20)) bitOr: (answerFractionBits bitShift: -3));
		basicAt: 2 put: ((answerFractionBits bitAnd: 7) bitShift: 29);
		* 1.0. "reduce to SmallFloat64 if possible"

"
Float fromIEEE32Bit: Float pi asIEEE32BitWord 
(Float fromIEEE32Bit: Float pi asIEEE32BitWord ) = Float pi
(Float fromIEEE32Bit: Float pi asIEEE32BitWord ) - Float pi

Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord
(Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord)  = (Float pi / 1e40)
(Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord)  - (Float pi / 1e40)
"! !


!Collection methodsFor: 'converting' stamp: 'jmv 9/3/2020 17:12:45'!
asFloatArray
	"Answer a FloatArray whose elements are the elements of the receiver"

	^self as: Float32Array! !


!Float32Array methodsFor: 'testing' stamp: 'jmv 9/3/2020 17:12:44'!
is: aSymbol
	^ aSymbol == #Float32Array or: [ super is: aSymbol ]! !


!Float64Array methodsFor: 'converting' stamp: 'jmv 9/3/2020 17:12:45'!
asIEEE32BitPrecisionFloat
	| answer s |
	self class == Float64Array ifFalse: [
		self error: 'please implement' ].
	s _ self size.
	answer _ Float32Array new: s.
	1 to: s do: [ :i | answer at: i put: (self at: i) ].
	^answer! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 9/3/2020 17:12:45'!
initCachedState
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"
	DataStream initCachedState
	"

	| refTypes t |
	refTypes _ OrderedCollection new.
	t _ TypeMap _ Dictionary new: 80. "sparse for fast hashing"

	t at: UndefinedObject put: 1.   refTypes add: 0.
	t at: True put: 2.   refTypes add: 0.
	t at: False put: 3.   refTypes add: 0.
	t at: SmallInteger put: 4.	 refTypes add: 0.
	t at: String put: 5.   refTypes add: 1.
	t at: Symbol put: 6.   refTypes add: 1.
	t at: ByteArray put: 7.   refTypes add: 1.
	t at: Array put: 8.   refTypes add: 1.
	"(type ID 9 is for arbitrary instances of any class, cf. typeIDFor:)"
		refTypes add: 1.
	"(type ID 10 is for references, cf. ReferenceStream>>tryToPutReference:)"
		refTypes add: 0.
	t at: Bitmap put: 11.   refTypes add: 1.

	t at: Metaclass put: 12.   refTypes add: 0.
	"Type ID 13 is used for HyperSqueak User classes that must be reconstructed."
		refTypes add: 1.

	t at: Float put: 14.  refTypes add: 1.
	t at: BoxedFloat64 put: 14.
	t at: SmallFloat64 put: 14.

	"15: Deprecated compact Rects."
		refTypes add: 1.

	"type ID 16 is an instance with short header.  See beginInstance:size:"
		refTypes add: 1.

	t at: String put: 17.   refTypes add: 1.	"new String format, 1 or 4 bytes of length"
	t at: WordArray put: 18.  refTypes add: 1.	"bitmap-like"
	"t at: WordArrayForSegment put: 19."  refTypes add: 1.		"bitmap-like"
	t at: Float32Array put: 20.  refTypes add: 1.	"And all other word arrays, both 16-bit and 32-bit.  See methods in ArrayedCollection.  Overridden in SoundBuffer."
	"For all classes that are like WordArrays, store them the way ColorArray is stored.  As bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isInMemory ifTrue: [
			cls isBehavior ifTrue: [
				cls isPointers not & cls isVariable & cls isWords ifTrue: [
					(t includesKey: cls) ifFalse: [t at: cls put: 20]]]]].

	t at: CompiledMethod put: 21.  refTypes add: 1.	"special creation method"

	t at: Character put: 22. refTypes add: 0.

	"t at:  put: 23.  refTypes add: 0."
	ReferenceTypes _ refTypes.		"save it"! !


!Collection reorganize!
('accessing' anyOne atRandom size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCharacterSet asCommaSeparated:on: asCommaStringAnd asDictionary asFloat64Array asFloatArray asIdentitySet asIntegerArray asNewArray asOrderedCollection asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
!


!Float32Array reorganize!
('accessing' at: at:put: defaultElement floatAt: floatAt:put: length squaredLength)
('arithmetic' * *= + += - -= / /= \\= adaptToNumber:andSend: divideBy:ifDivisorZero:ifBothZero: dot: negated normalize sum)
('comparing' = hash hashFull primitiveEqual:)
('primitives-plugin' divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('private' replaceFrom:to:with:startingAt:)
('inspecting' inspectorClass)
('testing' is:)
('interpolating' interpolateValues:at: interpolatedValueAt:)
('printing' printElementsOn:)
!


!Float64Array reorganize!
('accessing' at: at:put: defaultElement floatAt: floatAt:put: length size squaredLength)
('initialization' loadFrom:)
('private' privateLoadFrom: replaceWordsFrom:to:with:startingAt: swapWords)
('objects from disk' bytesPerElement writeOn:)
('inspecting' inspectorClass)
('arithmetic' * *= + += - -= / /= divideBy:ifDivisorZero:ifBothZero: dot: negated normalize)
('primitives-plugin' divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: primAddArray: primAddScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('converting' asIEEE32BitPrecisionFloat)
('testing' isLiteral)
('printing' printOn: storeOn:)
('interpolating' interpolateValues:at: interpolatedValueAt:)
!

