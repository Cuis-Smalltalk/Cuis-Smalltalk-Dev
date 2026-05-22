"
I provide a test suite for Float values. Examine my tests to see how Floats should behave, and see how to use them.
"
Class {
	#name : 'FloatTest',
	#superclass : 'AbstractNumberTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'helpers' }
FloatTest >> assert: aFloatingPointNumber isExactly: otherFloatingPointNumber  [

    "Both aFloatingPointNumber and otherFloatingPointNumber must be instances
    of our #classTested. Check that and fail if they aren't."

    self assert: (aFloatingPointNumber class inheritsFrom: Float).
    self assert: (otherFloatingPointNumber class inheritsFrom: Float).
    self assert: aFloatingPointNumber = otherFloatingPointNumber.
    self assert: otherFloatingPointNumber = aFloatingPointNumber.
    self assert: aFloatingPointNumber hex = otherFloatingPointNumber hex
]

{ #category : 'helpers' }
FloatTest >> assertIsNaNorFail: aFloatOrSymbol [
	"Assert that aFloat is NaN or #fail.
	For some BoxedFloat64, primitive 55 (sqrt) faile, unless jitter, where it answers NaN. We consider both values valid."
	self assert: (aFloatOrSymbol = #fail or: [ aFloatOrSymbol isNaN ])
]

{ #category : 'helpers' }
FloatTest >> assertIsNegativeZero: aFloat [
	"Assert that aFloat is Float negativeZero"
	self assert: aFloat hex = Float negativeZero hex
]

{ #category : 'helpers' }
FloatTest >> assertIsPositiveZero: aFloat [
	"Assert that aFloat is Float zero (the positive one)"
	self assert: aFloat hex = 0.0 hex
]

{ #category : 'helpers' }
FloatTest >> expectedFailures [
	Smalltalk platformName = 'Mac OS' ifTrue: [
		^#(testE testExp testPrimExpAndLn) ].
	^#().
]

{ #category : 'tests - 32 bit Single Precision' }
FloatTest >> test32bitConversion [
	"Except for NaN, we can convert a 32bits float to a 64bits float exactly.
	Thus we can convert the 64bits float to the original 32bits float pattern."
 
	#(
		16r0 					"zero"
		16r80000000 		"negative zero"
		16r1 					"min denormal"
		16r12345 			"a denormal"
		16r801FEDCB 		"a negative denormal"
		16r7FFFFF 			"largest denormal"
		16r800000 			"smallest normal"
		16r468ACDEF 		"a normal float"
		16rCABD1234 		"a negative normal float"
		16r7F7FFFFF 		"largest finite float"
		16r7F800000 		"positive infinity"
		16rFF800000 		"negative infinity"
		16r803FFFFC
		16r803FFFFD
	)
		do: [ :originalWord | self assert: (Float fromIEEE32Bit: originalWord) asIEEE32BitWord = originalWord ]
]

{ #category : 'tests - 32 bit Single Precision' }
FloatTest >> test32bitConversion2 [
	"This tests the rounding done when going from 64bit Float to 32bit Float
	self new test32bitConversion2
	"

	| pi32 pi32Predecessor roundingTo32Border roundingBorderPrev roundingBorderSuc aux roundedByConversion |
	"Pick two neighbors in the 32-bit Float world"
	pi32 _ Float fromIEEE32Bit: Float pi asIEEE32BitWord.
	pi32Predecessor _ Float fromIEEE32Bit: Float pi asIEEE32BitWord-1.
	"Find the middle value in the 64-bit Float world, and both its neighbors"
	roundingTo32Border _ pi32 + pi32Predecessor / 2.0.
	roundingBorderPrev _ roundingTo32Border predecessor.
	roundingBorderSuc _ roundingTo32Border successor.

	"64 bit middle value is not like any of the 32 bit values. Same for neighbors."
	self deny: roundingTo32Border mantissaPart hex = pi32 mantissaPart hex.
	self deny: roundingTo32Border mantissaPart hex = pi32Predecessor mantissaPart hex.
	self deny: roundingBorderPrev mantissaPart hex = pi32Predecessor mantissaPart hex.
	self deny: roundingBorderPrev mantissaPart hex = pi32 mantissaPart hex.
	self deny: roundingBorderSuc mantissaPart hex = pi32Predecessor mantissaPart hex.
	self deny: roundingBorderSuc mantissaPart hex = pi32 mantissaPart hex.

	"Conversion to 32 bit gives appropriate 32 bit value, done #asIEEE32BitWord or done by FloatArray."
	roundedByConversion _ Float fromIEEE32Bit: roundingTo32Border asIEEE32BitWord.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.
	aux _ Float32Array new: 1. aux at: 1 put: roundingTo32Border. roundedByConversion _ aux at: 1.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.

	"Conversion to 32 bit gives appropriate 32 bit value, done #asIEEE32BitWord or done by FloatArray."
	roundedByConversion _ Float fromIEEE32Bit: roundingBorderPrev asIEEE32BitWord.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.
	aux _ Float32Array new: 1. aux at: 1 put: roundingBorderPrev. roundedByConversion _ aux at: 1.
	self assert: roundedByConversion mantissaPart hex = pi32Predecessor mantissaPart hex.

	"Conversion to 32 bit gives appropriate 32 bit value, done #asIEEE32BitWord or done by FloatArray."
	roundedByConversion _ Float fromIEEE32Bit: roundingBorderSuc asIEEE32BitWord.
	self assert: roundedByConversion mantissaPart hex = pi32 mantissaPart hex.
	aux _ Float32Array new: 1. aux at: 1 put: roundingBorderSuc. roundedByConversion _ aux at: 1.
	self assert: roundedByConversion mantissaPart hex = pi32 mantissaPart hex
]

{ #category : 'tests - 32 bit Single Precision' }
FloatTest >> test32bitGradualUnderflow [
	"method asIEEE32BitWord did not respect IEEE gradual underflow"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-126) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"case 1: This example is the first gradual underflow case"
	conv := 2r0.11111111111111111111111e-126 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1 or 0)"
	exponentPart := 0.
	expected := exponentPart bitOr: 2r11111111111111111111111.
	self assert: expected = conv.
	
	"case 2: smallest number"
	conv := 2r0.00000000000000000000001e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.
	
	"case 3: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.000000000000000000000011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 4: round to nearest even also in underflow cases... here round to lower"
	conv := 2r0.000000000000000000000101e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 5: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.0000000000000000000001011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r11.
	self assert: expected = conv.
	
]

{ #category : 'tests - 32 bit Single Precision' }
FloatTest >> test32bitInfinities [
	| infinityBits negativeInfinityBits |
	infinityBits _ '01111111100000000000000000000000'.
	self assert: (Float infinity asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = infinityBits.
	self assert: (Float fromIEEE32Bit: (Integer readFrom: infinityBits readStream base: 2)) hex = Float infinity hex.
	negativeInfinityBits _ '11111111100000000000000000000000'.
	self assert: (Float negativeInfinity asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = negativeInfinityBits.
	self assert: (Float fromIEEE32Bit: (Integer readFrom: negativeInfinityBits readStream base: 2)) hex = Float negativeInfinity hex.
]

{ #category : 'tests - 32 bit Single Precision' }
FloatTest >> test32bitNaN [
	| nanstr |
	
	"check the NaN string representation conforms to IEEE 754"
	nanstr := Float nan asIEEE32BitWord printStringBase: 2 length: 32 padded: true.
	self
		assert: (#($0 $1) includes: (nanstr at: 1));
		assert: (nanstr copyFrom: 2 to: 9) = '11111111';
		assert: (#($0 $1) includes: (nanstr at: 10)); "accept both quiet and signalled NaNs"
		assert: ((nanstr copyFrom: 11 to: 32) reject: [ :c | #($0 $1) includes: c ]) isEmpty.
	
	"check a correct quiet NaN is created from a string"
	self assert: (Float fromIEEE32Bit:
		(Integer readFrom: '01111111110000000000000000000000' readStream base: 2)) isNaN
]

{ #category : 'tests - 32 bit Single Precision' }
FloatTest >> test32bitRoundingMode [
	"method asIEEE32BitWord did not respect IEEE default rounding mode"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-127) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"This example has two extra bits in mantissa for testing rounding mode
	case 1: should obviously round to upper"
	conv := 2r1.0000000000000000000000111e25 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1)"
	exponentPart := 25+127 bitShift: 23. "127 is 2r01111111 or 16r7F"
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 2: exactly in the mid point of two 32 bit float: round toward nearest even (to upper)"
	conv := 2r1.0000000000000000000000110e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 3: exactly in the mid point of two 32 bit float: round toward nearest even (to lower)"
	conv := 2r1.0000000000000000000000010e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r0.
	self assert: expected = conv.
	
	"case 4: obviously round to upper"
	conv := 2r1.0000000000000000000000011e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.

]

{ #category : 'tests - 32 bit Single Precision' }
FloatTest >> test32bitZeros [
	| negativeZeroBits zeroBits |
	zeroBits _ '00000000000000000000000000000000'.
	self assert: (Float zero asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = zeroBits.
	self assertIsPositiveZero: (Float fromIEEE32Bit: (Integer readFrom: zeroBits readStream base: 2)).
	negativeZeroBits _ '10000000000000000000000000000000'.
	self assert: (Float negativeZero asIEEE32BitWord printStringBase: 2 length: 32 padded: true) = negativeZeroBits.
	self assertIsNegativeZero: (Float fromIEEE32Bit: (Integer readFrom: negativeZeroBits readStream base: 2)).
]

{ #category : 'tests - arithmetic' }
FloatTest >> testAdd [

	self assert: 1.0 + 1.0 = 2.0.
	self assert: Float fminNormalized + Float fminDenormalized > Float fminNormalized.
	self assert: Float fminNormalized + Float fminDenormalized - Float fminNormalized = Float fminDenormalized.
	self assert: 1.0 + Float zero = 1.0.
	self assert: Float zero + 1.0 = 1.0.
	self assert: 1.0 + Float negativeZero = 1.0.
	self assert: Float negativeZero + 1.0 = 1.0.
	self assert: 1.0 + Float infinity = Float infinity.
	self assert: Float infinity + 1.0 = Float infinity.
	self assert: 1.0 + Float negativeInfinity = Float negativeInfinity.
	self assert: Float negativeInfinity + 1.0 = Float negativeInfinity.
	self assert: (1.0 + Float nan) isNaN.
	self assert: (Float nan + 1.0) isNaN.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testArCosh [
	self assert: 1.0 arCosh = 0.0.
	self deny: Float infinity arCosh isFinite.
	self verify: 2.5 arCosh cosh isHyperbolicallyEqualTo: 2.5
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testArSinh [
	self assertIsPositiveZero: 0.0 arSinh.
	self assertIsNegativeZero: Float negativeZero arSinh.
	self deny: Float infinity arSinh isFinite.
	self verify: 0.5 arSinh negated isHyperbolicallyEqualTo: 0.5 negated arSinh.
	self verify: 0.5 arSinh sinh isHyperbolicallyEqualTo: 0.5.
	self verify: -2.5 arSinh sinh isHyperbolicallyEqualTo: -2.5
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testArTanh [
	self assertIsPositiveZero: 0.0 arTanh.
	self assertIsNegativeZero: Float negativeZero arTanh.
	self deny: 1 arTanh isFinite.
	self verify: 0.5 arTanh negated isHyperbolicallyEqualTo: 0.5 negated arTanh.
	self verify: 0.5 arTanh tanh isHyperbolicallyEqualTo: 0.5.
	self verify: -0.5 arTanh tanh isHyperbolicallyEqualTo: -0.5
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testArcTan [

	self verify: (100 arcTan: 100) isTrigonometricallyEqualTo: Float pi / 4.
	self verify: (-100 arcTan: 100) isTrigonometricallyEqualTo: Float pi / -4.
	self verify: (100 arcTan: -100) isTrigonometricallyEqualTo: Float pi * 3 / 4.
	self verify: (-100 arcTan: -100) isTrigonometricallyEqualTo: Float pi * -3 / 4.
	self verify: (0 arcTan: 100) isTrigonometricallyEqualTo: 0.
	self verify: (0 arcTan: -100) isTrigonometricallyEqualTo: Float pi.
	self verify: (100 arcTan: 0) isTrigonometricallyEqualTo: Float pi / 2.
	self verify: (-100 arcTan: 0) isTrigonometricallyEqualTo: Float pi / -2.
	
	self verify: (Float negativeZero arcTan: 100) isTrigonometricallyEqualTo: 0.
	self verify: (Float negativeZero arcTan: -100) isTrigonometricallyEqualTo: Float pi * -1.
	
	self assert: (0 arcTan: 0) = 0.
	self assert: (Float negativeZero arcTan: 0) = 0.
	self verify: (0 arcTan: Float negativeZero) isTrigonometricallyEqualTo: Float pi.
	self verify: (Float negativeZero arcTan: Float negativeZero) isTrigonometricallyEqualTo: Float pi negated.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testArcTanNonPrimitive [

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.0 3.0 5.0 20.0] do: [ :x | 
		self assert: (x predecessor arcTanNonPrimitive - x predecessor arcTan) abs < 1.0e-15.
		self assert: (x arcTanNonPrimitive - x arcTan) abs < 1.0e-15.
		self assert: (x successor arcTanNonPrimitive - x successor arcTan) abs < 1.0e-15.
		]
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testAsTrueFraction [
	"
	self new testAsTrueFraction
	"
	| x |
	x _ Float pi.
	self assert: x asTrueFraction asFloat = x.
	x _ 1.0 / 3.0.
	self assert: x asTrueFraction asFloat = x.
	x _ Float fminNormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ 0.0 - Float fminNormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ Float fminDenormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ 0.0 - Float fminDenormalized.
	self assert: x asTrueFraction asFloat = x.
	x _ 1.023399999997e-312.
	self assert: x asTrueFraction asFloat = x.
	x _ Float fmax.
	self assert: x asTrueFraction asFloat = x.
	x _ 0.0 - Float fmax.
	self assert: x asTrueFraction asFloat = x.
	x _ 1.0234e308.
	self assert: x asTrueFraction asFloat = x.
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testCeiling [
	self assert: 1.0 ceiling = 1.
	self assert: 1.0 successor ceiling = 2.
	self assert: 1.0 predecessor ceiling = 1.
	self assert: 1.1 ceiling = 2.
	self assert: -2.0 ceiling = -2.
	self assert: -2.1 ceiling = -2.
]

{ #category : 'tests - precision and extreme values' }
FloatTest >> testCharacterization [

	"Test the largest finite representable floating point value"
	self assert: Float fmax successor = Float infinity.
	self assert: Float infinity predecessor = Float fmax.
	self assert: Float fmax negated predecessor = Float infinity negated.
	self assert: Float infinity negated successor = Float fmax negated.
	
	"Test the smallest positive representable floating point value"
	self assertIsPositiveZero: Float fmin predecessor.
	self assert: 0.0 successor = Float fmin.
	self assert: Float fmin negated successor hex = -0.0 hex.
	self assert: -0.0 predecessor = Float fmin negated.
	
	"Test the relative precision"
	self assert: Float one + Float epsilon > Float one.
	self assert: Float one + Float epsilon = Float one successor.
	self assert: Float one + (Float epsilon / Float radix) = Float one.
	
	"Test maximum and minimum exponent"
	self assert: Float fmax exponent = Float emax.
	self assert: Float fminNormalized exponent = Float emin.
	Float denormalized ifTrue: [
		self assert: Float fminDenormalized exponent = (Float emin + 1 - Float precision)].
	
	"Alternative tests for maximum and minimum exponents and normalized and denormal values"
	self assert: (Float radix raisedTo: Float emax) *  (Float radix - (Float epsilon)) = Float fmax.
	self assert: (Float radix raisedTo: Float emin) = Float fminNormalized.
	self assert: (Float radix raisedTo: Float emin) * Float epsilon = Float fmin.
	
	"Test sucessors and predecessors"
	self assert: Float one predecessor successor = Float one.
	self assert: Float one successor predecessor = Float one.
	self assert: Float one negated predecessor successor = Float one negated.
	self assert: Float one negated successor predecessor = Float one negated.
	self assert: Float infinity successor = Float infinity.
	self assert: Float negativeInfinity predecessor = Float negativeInfinity.
	self assertIsNegativeZero: Float fmin negated successor.
	self assertIsPositiveZero: Float fmin predecessor.
	self assert: Float nan predecessor isNaN.
	self assert: Float nan successor isNaN.
	
	"SPECIFIC FOR IEEE 754 double precision - 64 bits"
	self assert: Float fmax hex = '7FEFFFFFFFFFFFFF'.
	self assert: Float fminDenormalized hex = '0000000000000001'.
	self assert: Float fminNormalized hex = '0010000000000000'.
	self assert: 0.0 hex = '0000000000000000'.
	self assert: Float negativeZero hex = '8000000000000000'.
	self assert: Float one hex = '3FF0000000000000'.
	self assert: Float infinity hex = '7FF0000000000000'.
	self assert: Float negativeInfinity hex = 'FFF0000000000000'.
]

{ #category : 'tests - comparing' }
FloatTest >> testComparisonWhenPrimitiveFails [
	"This is related to http://bugs.squeak.org/view.php?id=7361"

	self deny: 0.5 < (1/4).
	self deny: 0.5 < (1/2).
	self assert: 0.5 < (3/4).
	
	self deny: 0.5 <= (1/4).
	self assert: 0.5 <= (1/2).
	self assert: 0.5 <= (3/4).
	
	self assert: 0.5 > (1/4).
	self deny: 0.5 > (1/2).
	self deny: 0.5 > (3/4).
	
	self assert: 0.5 >= (1/4).
	self assert: 0.5 >= (1/2).
	self deny: 0.5 >= (3/4).
	
	self deny: 0.5 = (1/4).
	self assert: 0.5 = (1/2).
	self deny: 0.5 = (3/4).
	
	self assert: 0.5 ~= (1/4).
	self deny: 0.5 ~= (1/2).
	self assert: 0.5 ~= (3/4).
]

{ #category : 'tests - comparing' }
FloatTest >> testComparisonWithFraction [
	self assert: 0.5 = (1/2).
	self assert: (1/2) = 0.5.
	self assert: 0.1 = (1/10).
	self assert: (1/10) = 0.1
]

{ #category : 'tests - accessing parts' }
FloatTest >> testCopy [
	"Elementary tests"
	self assert: 2.0 copy = 2.0.
	self assert: -0.5 copy = -0.5.
	
	"Are exceptional Floats preserved by the copy ?"
	self assert: Float nan copy isNaN.
	self assert: Float infinity copy = Float infinity.
	self assert: Float infinity negated copy = Float infinity negated.
	
	"Is the sign of zero preserved by the copy ?"
	self assertIsPositiveZero: 0.0 copy.
	self assertIsNegativeZero: Float negativeZero copy.
]

{ #category : 'tests - zero behavior' }
FloatTest >> testCopySign [
	self assert: (0.0 copySignTo: 1) = 1.
	self assert: (Float negativeZero copySignTo: 1) = -1.
	self assertIsNegativeZero: (-1 copySignTo: 0.0).
	self assertIsPositiveZero: (1 copySignTo: Float negativeZero).
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testCos [

	self verify: 0.0 cos isTrigonometricallyEqualTo: 1.0.
	self verify: (Float pi / 3) cos isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) cos isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 6) cos isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) cos isTrigonometricallyEqualTo: 0.0
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testCosh [
	self verify: 0.0 cosh isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 cosh isHyperbolicallyEqualTo: 2.0 negated cosh.
	self deny: Float infinity cosh isFinite	
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testDegreeCos	 [
	"Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self verify: 45 degreeCos squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 45.0 degreeCos squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 60 degreeCos isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 60.0 degreeCos isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 120 degreeCos isDegreeTrigonometricallyEqualTo: -0.5.
	self verify: 120.0 degreeCos isDegreeTrigonometricallyEqualTo: -0.5.
	-360 to: 360 by: 1/3 do:
		[:step |
			| expected actual |
			expected := step degreesToRadians cos.
			actual := step degreeCos.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry].
			expected := step asFloat degreesToRadians cos.
			actual := step asFloat degreeCos.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry]
		].

	"Following tests use strict equality which is a requested property of degreeCos"
	-10.0 to: 10.0 do: [:k |
		self assert: (k*360 + 90) degreeCos = 0.
		self assert: (k*360 - 90) degreeCos = 0.
		self assert: (k*360 + 180) degreeCos + 1 = 0.
		self assert: (k*360) degreeCos - 1 = 0.].
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testDegreeCosForExceptionalValues [
	self assert: Float nan degreeCos isNaN.
	self assert: Float infinity degreeCos isNaN.
	self assert: Float negativeInfinity degreeCos isNaN.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testDegreeSin	 [
	"Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self verify: 45 degreeSin squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 45.0 degreeSin squared isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 30 degreeSin isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: 30.0 degreeSin isDegreeTrigonometricallyEqualTo: 0.5.
	self verify: -30 degreeSin isDegreeTrigonometricallyEqualTo: -0.5.
	self verify: -30.0 degreeSin isDegreeTrigonometricallyEqualTo: -0.5.
	-360 to: 360 by: 1/3 do:
		[:step |
			| expected actual |
			expected := step degreesToRadians sin.
			actual := step degreeSin.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry].
			expected := step asFloat degreesToRadians sin.
			actual := step asFloat degreeSin.
			expected = actual ifFalse: [self assert: (expected - actual) exponent <= self effectiveZeroExponentForTrigonometry].
		].

	"Following tests use strict equality which is a requested property of degreeSin"
	-10.0 to: 10.0 do: [:k |
		self assert: (k*360 + 90) degreeSin - 1 = 0.
		self assert: (k*360 - 90) degreeSin + 1= 0.
		self assert: (k*360 + 180) degreeSin = 0.
		self assert: (k*360) degreeSin = 0.].
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testDegreeSinForExceptionalValues [
	self assert: Float nan degreeSin isNaN.
	self assert: Float infinity degreeSin isNaN.
	self assert: Float negativeInfinity degreeSin isNaN.
]

{ #category : 'tests - arithmetic' }
FloatTest >> testDivide [

	self assert: 1.5 / 2.0 = 0.75.
	
	self assert: 2.0 / 1 = 2.0.
	
	self should: [ 2.0 / 0 ] raise: ZeroDivide.
	self assert: 2.0 / 0 isExactly: Float infinity.
	self should: [ 2.0 / 0.0 ] raise: ZeroDivide.
	self assert: 2.0 / 0.0 isExactly: Float infinity.
	self should: [ 1.2 / Float negativeZero ] raise: ZeroDivide.
	self assert: 1.2 / Float negativeZero isExactly: Float negativeInfinity.
	self should: [ 1.2 / (1.3 - 1.3) ] raise: ZeroDivide.
	self assert: 1.2 / (1.3 - 1.3) isExactly: Float infinity.

	self assert: 2.0 / 2.0 = 1.0.
	self assert: Float fminNormalized / 2.0 * 2.0 = Float fminNormalized.
	self assert: Float fminDenormalized * 2.0 / 2.0 = Float fminDenormalized.
	self assertIsPositiveZero: Float zero / 1.0.
	self assertIsNegativeZero: Float negativeZero / 1.0.
	self assertIsPositiveZero: 1.0 / Float infinity.
	self assert: Float infinity / 1.0 = Float infinity.
	self assertIsNegativeZero: 1.0 / Float negativeInfinity.
	self assert: Float negativeInfinity / 1.0 = Float negativeInfinity.
	self assert: (1.0 / Float nan) isNaN.
	self assert: (Float nan / 1.0) isNaN.
]

{ #category : 'tests - constants' }
FloatTest >> testE [
	"Just in case..."

	self assert: Float e = 2.718281828459045.
	self assert: Float e ln = 1.0.
	self assert: Float e = 1.0 exp.
	self assert: Float e hex = '4005BF0A8B145769'.
]

{ #category : 'tests - comparing' }
FloatTest >> testEqualityComparison [
	"In Cuis, it was decided to coerce operands to Float for comparison (if any of them was float),
	in the classic ST-80 way"
	| a b c |
	
	"Test equality when Float conversion exact"
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat.
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat asInteger.
	
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) = 1.0.			"Coerce to Float"
	
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) = 1.0.			"Coerce to Float"
	
	"Test exact vs inexact arithmetic"
	(1 to: 100) do: [:i |
		self assert: (1/i) = (1/i asFloat)].
	
	"Test overflow (compare to infinity)"
	a := (11 raisedTo: 400) / 2.
	b := (13 raisedTo: 400) / 2.
	c := a asFloat.
	self assert: ((a = c) & (b = c)).			"Coerce to Float"
	self deny:  (a = b).
	
	"every integer is smaller than infinity"
	self assert: a = Float infinity.			"Coerce to Float"
	self assert: a > Float infinity negated.
	
	"Test underflow"
	self deny: 1 / (11 raisedTo: 400) = 0.
	self assert: 1 / (11 raisedTo: 400) = 0.0.		"Coerce to Float"
	
	"Test hash code"
	self assert:
		((Set newWithRoomForMoreThan: 3) add: 3; add: 3.0; size) =
		((Set newWithRoomForMoreThan: 4) add: 3; add: 3.0; size).


	self assert: 9007199254740996 = 9007199254740996.0.
	self assert: 9007199254740996001 / 1000 = 9007199254740996.0.
	self assert: 90071992547409963 / 10 = 9007199254740996.0.
	self assert: 90071992547409967 / 10 = 9007199254740996.0.
	self assert: 9007199254740995 = 9007199254740996.0.
	self deny: 9007199254740995 = 9007199254740994.0.

	self assert: 9007199254740996.0 = 9007199254740996.
	self assert: 9007199254740996.0 = (9007199254740996001 / 1000).
	self assert: 9007199254740996.0 = (90071992547409963 / 10).
	self assert: 9007199254740996.0 = (90071992547409967 / 10).
	self assert: 9007199254740996.0 = 9007199254740995.
	self deny: 9007199254740994.0 = 9007199254740995.
]

{ #category : 'tests - comparing' }
FloatTest >> testEquals [
	self assert: 1.0 = 1.0.
	self deny: 1.0 = 2.0.
	self assert: 0.0 = 0.0.
	self deny: 0.0 = 1.0.
	self deny: 1.0 = 0.0.
	self assert: 1.0 = 1.
	self deny: 1.0 = 2.
	self assert: Float zero = Float negativeZero.
	self assert: Float negativeZero = Float zero.
	self assert: Float fmax = Float fmax.
	self assert: Float fminNormalized = Float fminNormalized.
	self assert: Float fminDenormalized = Float fminDenormalized.
	self deny: Float fmax = Float fminNormalized.
	self deny: Float fminNormalized = Float fmax.
	self deny: Float fminDenormalized = Float fminNormalized.
	self deny: Float fminNormalized = Float fminDenormalized.
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testExactAsString [
	"Ensures round-trip string conversion when using #asString.
	Use some relevant examples."

	| examples |
	examples _ {
		1.0. 
		Float fminNormalized / 2. 
		Float fminNormalized / 10. 
		Float fminNormalized / 13. 
		2.0. 
		2 sqrt. 
		Float pi. 
		Float fminDenormalized. 
		Float fminDenormalized * 2. 
		Float fminDenormalized * 3. 
		Float fminDenormalized * 13 },
	{
		Float zero.
		Float negativeZero.
		Float negativeInfinity.
		Float infinity.
		Float fmax predecessor }.

	examples do: [ :float |
		self assert: float predecessor asString asNumber = float predecessor.
		self assert: float asString asNumber = float.
		self assert: float successor asString asNumber = float successor ].

	"NaN are special, in that they are not even equal to themselves"
	self assert: Float nan asString asNumber isNaN
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testExactAsString2 [
	"Ensures round-trip string conversion when using #asString.
	Use some relevant examples.
	Note: there is no guarantee to restore the bit pattern of NaN though"

	self assert: Float halfPi asString asNumber = Float halfPi.
	self assert: Float halfPi negated asString asNumber = Float halfPi negated.
	self assert: Float pi asString asNumber = Float pi.
	self assert: Float pi negated asString asNumber = Float pi negated.
	self assert: 2.0 sqrt asString asNumber = 2.0 sqrt.
	self assert: 2.0 sqrt negated asString asNumber = 2.0 sqrt negated.
	self assert: Float infinity asString asNumber = Float infinity.
	self assert: Float negativeInfinity asString asNumber = Float negativeInfinity.
	self assert: Float nan asString asNumber isNaN.
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testExactStoreString [
	"Tests that conversion to / from Strings is exact (same Float is re-created) when using #storeString
	(whose output is meant to be compilable smalltalk code).
	Use some relevant examples."

	| examples |
	examples _ {
		1.0. 
		Float fminNormalized / 2. 
		Float fminNormalized / 10. 
		Float fminNormalized / 13. 
		2.0. 
		2 sqrt. 
		Float pi. 
		Float fminDenormalized. 
		Float fminDenormalized * 2. 
		Float fminDenormalized * 3. 
		Float fminDenormalized * 13 },
	{
		Float zero.
		Float negativeZero.
		Float negativeInfinity.
		Float infinity }.

	examples do: [ :float |
		self assert: (Compiler evaluate: float predecessor storeString) = float predecessor.
		self assert: (Compiler evaluate: float storeString) = float.
		self assert: (Compiler evaluate: float successor storeString) = float successor ].

	"NaN are special, in that they are not even equal to themselves"
	self assert: (Compiler evaluate: Float nan storeString) isNaN
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testExactStoreString2 [
	"If storeOn: prints exactly and the parser avoids cumulating rounding errors,
	then the Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (Compiler evaluate: Float halfPi storeString) = Float halfPi.
	self assert: (Compiler evaluate: Float halfPi negated storeString) = Float halfPi negated.
	self assert: (Compiler evaluate: Float infinity storeString) = Float infinity.
	self assert: (Compiler evaluate: Float negativeInfinity storeString) = Float negativeInfinity.
	self assert: (Compiler evaluate: Float nan storeString) isNaN.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testExp [
	self assert: 0.0 exp = 1.0.
	self assert: 1.0 exp = Float e.
	self assert: 2.0 exp sqrt = Float e
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testExpNonPrimitive [

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.0 2.0 5.0 ] do: [ :x | | computed expected |
		computed _ x expNonPrimitive.
		expected _ x exp.
		self assert: (computed - expected) abs <= (expected ulp*2)
		]
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testFloatRounded [
	"5000000000000001 asFloat has an exact representation (no round off error).
	It should round to nearest integer without loosing bits.
	This is a no regression test on http://bugs.squeak.org/view.php?id=7134"
	
	| x y int r |
	
	"This is a preamble asserting exactness of representation
	and quality of various conversions"
	int := 5000000000000001.
	x := int asFloat.
	y := (5 asFloat squared squared squared squared timesTwoPower: 15) + 1.
	self assert: x = y.
	self assert: x asTrueFraction = int.
	
	"this one should be true for any float
	in order to conform to ISO/IEC 10967-2"
	self assert: x rounded = x asTrueFraction rounded.
	self assert: x negated rounded = x negated asTrueFraction rounded.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x rounded = x asTrueFraction rounded.
		self assert: x negated rounded = x negated asTrueFraction rounded]
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testFloatTruncated [
	"(10 raisedTo: 16) asFloat has an exact representation (no round off error).
	It should convert back to integer without loosing bits.
	This is a no regression test on http://bugs.impara.de/view.php?id=3504"
	
	| x y int r |
	int := 10 raisedTo: 16.
	x := int asFloat.
	y := (5 raisedTo: 16) asFloat timesTwoPower: 16.
	self assert: x = y.
	
	self assert: x asInteger = int.
	
	"this one should be true for any float"
	self assert: x asInteger = x asTrueFraction asInteger.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x truncated = x asTrueFraction truncated]
]

{ #category : 'tests - comparing' }
FloatTest >> testFloatsAwayFrom [

	self verify: 1.0 is: 0 floatsAwayFrom: 1.0.
	self verify: 1.0 is: -1 floatsAwayFrom: 1.0 successor.
	self verify: 1.0 is: 1 floatsAwayFrom: 1.0 predecessor.
	self verify: 0.0 is: 0 floatsAwayFrom: Float negativeZero.
	self verify: Float infinity is: 1 floatsAwayFrom: Float fmax.
	self verify: Float fmax is: -1 floatsAwayFrom: Float infinity.
	self verify: Float negativeInfinity is: -1 floatsAwayFrom: Float fmax negated.
	self verify: Float fmax negated is: 1 floatsAwayFrom: Float negativeInfinity.
	self verify: 0.0 is: -1 floatsAwayFrom: Float fminDenormalized.
	self verify: Float negativeZero is: -1 floatsAwayFrom: Float fminDenormalized.
	self verify: Float fmax is: (1 bitShift: Float precision - 1) - 1 floatsAwayFrom: (1.0 timesTwoPower: Float emax)
]

{ #category : 'tests - comparing' }
FloatTest >> testFloatsAwayFromWithPowersOfTwo [

	| previous current soMany |
	previous := 0.0.
	current := Float fminDenormalized.
	soMany := 1.
	[current isFinite] whileTrue:
		[
			self verify: current is: soMany floatsAwayFrom: previous.
			soMany < 4503599627370496 :: and: [previous > 0.0] :: ifTrue: [soMany := soMany * 2].
			previous := current.
			current := current * 2.0
		]
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testFloor [
	self assert: 1.0 floor = 1.
	self assert: 1.0 successor floor = 1.
	self assert: 1.0 predecessor floor = 0.
	self assert: 1.1 floor = 1.
	self assert: -2.0 floor = -2.
	self assert: -2.1 floor = -3.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testFloorLog2 [
	"Float internal representation of Float being in base 2, we expect (aFloat floorLog: 2) to be exact."
	
	| aBitLess aBitMore |
	aBitMore := 1.0 successor.
	aBitLess := 1.0 predecessor.
	Float emin + 1 to: Float emax - 1 do: [:exp |
		| exactPowerOfTwo |
		exactPowerOfTwo := 1.0 timesTwoPower: exp.
		self assert: (exactPowerOfTwo floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitMore floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitLess floorLog: 2) equals: exp-1 ].
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testFractionAsFloat [
	"use a random test"
	
	| r m frac err collec |
	r := Random new seed: 1234567.
	m := (2 raisedTo: 54) - 1.
	200 timesRepeat: [
		frac := ((r nextInteger: m) * (r nextInteger: m) + 1) / ((r nextInteger: m) * (r nextInteger: m) + 1).
		err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
		self assert: err < (1/2)].
	
	collec := #(16r10000000000000 16r1FFFFFFFFFFFFF 1 2 16r20000000000000 16r20000000000001 16r3FFFFFFFFFFFFF 16r3FFFFFFFFFFFFE 16r3FFFFFFFFFFFFD).
	collec do: [:num |
		collec do: [:den |
			frac := Fraction numerator: num denominator: den.
			err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
			self assert: err <= (1/2)]].
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testFractionAsFloat2 [
	"test rounding to nearest even"
		
	self assert: ((1<<52)+0+(1/4)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(1/2)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(3/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/2)) asFloat asTrueFraction = ((1<<52)+2).
	self assert: ((1<<52)+1+(3/4)) asFloat asTrueFraction = ((1<<52)+2).
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testFractionAsFloatRoundsHalfToEven [
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Test that rounding Fraction to Float picks even mantissa if tie."

	| floatWithEvenMantissa floatWithEvenMantissa2 floatWithOddMantissa |
	floatWithEvenMantissa _ Float maxExactInteger * 1.23.
	self assert: floatWithEvenMantissa mantissaPart even description: 'precondition'.

	floatWithOddMantissa _ floatWithEvenMantissa successor.
	self assert: floatWithOddMantissa mantissaPart even not description: 'precondition'.

	floatWithEvenMantissa2 _ floatWithOddMantissa successor.
	self assert: floatWithEvenMantissa2 mantissaPart even description: 'precondition'.

	self assert: ((floatWithEvenMantissa asTrueFraction *4) + (floatWithOddMantissa asTrueFraction *0) / 4) = floatWithEvenMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *3) + (floatWithOddMantissa asTrueFraction *1) / 4) = floatWithEvenMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *100) + (floatWithOddMantissa asTrueFraction *1) / 101) = floatWithEvenMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *1) + (floatWithOddMantissa asTrueFraction *100) / 101) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *1) + (floatWithOddMantissa asTrueFraction *3) / 4) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithEvenMantissa asTrueFraction *0) + (floatWithOddMantissa asTrueFraction *4) / 4) = floatWithOddMantissa description: 'precondition'.

	self assert: ((floatWithEvenMantissa asTrueFraction *2) + (floatWithOddMantissa asTrueFraction *2) / 4) = floatWithEvenMantissa description: 'Must round to closest even mantissa'.

	self assert: ((floatWithOddMantissa asTrueFraction *4) + (floatWithEvenMantissa2 asTrueFraction *0)/4) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *3) + (floatWithEvenMantissa2 asTrueFraction *1)/4) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *100) + (floatWithEvenMantissa2 asTrueFraction *1)/101) = floatWithOddMantissa description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *1) + (floatWithEvenMantissa2 asTrueFraction *100)/101) = floatWithEvenMantissa2 description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *1) + (floatWithEvenMantissa2 asTrueFraction *3)/4) = floatWithEvenMantissa2 description: 'precondition'.
	self assert: ((floatWithOddMantissa asTrueFraction *0) + (floatWithEvenMantissa2 asTrueFraction *4)/4) = floatWithEvenMantissa2 description: 'precondition'.

	self assert: ((floatWithOddMantissa asTrueFraction *2) + (floatWithEvenMantissa2 asTrueFraction *2)/4) = floatWithEvenMantissa2 description: 'Must round to closest even mantissa'.
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testFractionAsFloatWithUnderflow [
	"test rounding to nearest even"

	| underflowPower |
	underflowPower := Float emin - Float precision.
	self assertIsPositiveZero: (2 raisedTo: underflowPower) asFloat.	
	self assertIsNegativeZero: (2 raisedTo: underflowPower) negated asFloat
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testFromStringDoesNotUnderflow [
	self assert: '12345678901234567890.0e-330' asNumber isZero not.
	self assert: (Compiler evaluate: '12345678901234567890.0e-330')isZero not.
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithBigNegativeSmallInteger [
	"Not in the Float range"
	| a float smallInteger |
	smallInteger _ SmallInteger minVal+1.
	self deny: smallInteger isLarge description: 'precondition'.
	float _ smallInteger asFloat.
	self assert: smallInteger = float description: 'precondition'.
	self assert: float = smallInteger description: 'precondition'.
	a _ Set new.
	a add: smallInteger; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithBigSmallInteger [
	"Not in the Float range"
	| a float smallInteger |
	smallInteger _ SmallInteger maxVal.
	self deny: smallInteger isLarge description: 'precondition'.
	float _ smallInteger asFloat.
	self assert: smallInteger = float description: 'precondition'.
	self assert: float = smallInteger description: 'precondition'.
	a _ Set new.
	a add: smallInteger; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithFraction [

	| a float fraction |
	fraction _ 1/3.
	float _ (1/3) asFloat.
	self assert: fraction = float description: 'precondition'.
	self assert: float = fraction description: 'precondition'.
	a _ Set new.
	a add: fraction; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithLargeNegativeInteger [
	"Not in the Float range"
	| a float largeInteger |
	largeInteger _ -1e400.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithLargePositiveInteger [
	"Not in the Float range"
	| a float largeInteger |
	largeInteger _ 1e400.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithSmallishLargeNegativeInteger [
	"In the Float range"
	| a float largeInteger |
	largeInteger _ SmallInteger minVal -1.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithSmallishLargeNegativeInteger2 [
	"In the Float range"
	| a float integer |
	integer _ Float maxExactInteger negated -1.
	float _ integer asFloat.
	self assert: integer = float description: 'precondition'.
	self assert: float = integer description: 'precondition'.
	a _ Set new.
	a add: integer; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithSmallishLargePositiveInteger [
	"In the Float range"
	| a float largeInteger |
	largeInteger _ SmallInteger maxVal +1.
	self assert: largeInteger isLarge description: 'precondition'.
	float _ largeInteger asFloat.
	self assert: largeInteger = float description: 'precondition'.
	self assert: float = largeInteger description: 'precondition'.
	a _ Set new.
	a add: largeInteger; add: float.
	self assert: a size = 1
]

{ #category : 'tests - hash' }
FloatTest >> testHashWithSmallishLargePositiveInteger2 [
	"In the Float range"
	| a float integer |
	integer _ Float maxExactInteger +1.
	float _ integer asFloat.
	self assert: integer = float description: 'precondition'.
	self assert: float = integer description: 'precondition'.
	a _ Set new.
	a add: integer; add: float.
	self assert: a size = 1
]

{ #category : 'tests - comparing' }
FloatTest >> testHugeValues [

	self assert: 1e400 = Float infinity.
	self deny: 1e400 < Float infinity.
	self deny: 1e400 > Float infinity.

	self assert: 1e400 = 1.0e309.
	self assert: 1e400 > 1.0e308.


	self assert: -1e400 = Float negativeInfinity.
	self deny: -1e400 < Float negativeInfinity.
	self deny: -1e400 > Float negativeInfinity.

	self assert: -1e400 = -1.0e309.
	self assert: -1e400 < -1.0e308.
]

{ #category : 'tests - infinity behavior' }
FloatTest >> testInfinity1 [
   "FloatTest new testInfinity1"

	| i1  i2 |

	i1 := 10000 exp.
	i2 := 1000000000 exp.
	self assert: i1 isInfinite & i2 isInfinite & (i1 = i2).
	"All infinities are equal. (This is a very substantial difference to NaN's, which are never equal."

]

{ #category : 'tests - infinity behavior' }
FloatTest >> testInfinity2 [
   "FloatTest new testInfinity2"

	| i1  i2 |
	i1 := 10000 exp.
	i2 := 1000000000 exp.
	i2 := 0 - i2. " this is entirely ok. You can compute with infinite values."

	self assert: i1 isInfinite & i2 isInfinite & i1 positive & i2 negative.
	self deny: i1 = i2.
  	"All infinities are signed. Negative infinity is not equal to Infinity"

]

{ #category : 'tests - infinity behavior' }
FloatTest >> testInfinityTruncated [
	self assert: Float infinity truncated isInfinite.
	self assert: Float negativeInfinity truncated isInfinite.
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testIntegerAsFloat [
	"assert IEEE 754 round to nearest even mode is honoured"
	
	self deny: 16r1FFFFFFFFFFFF0801 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 65 bits"
	self deny: 16r1FFFFFFFFFFFF0802 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 64 bits"
	self assert: 16r1FFFFFFFFFFF1F800 asFloat = 16r1FFFFFFFFFFF20000 asFloat. "nearest even is upper"
	self assert: 16r1FFFFFFFFFFFF0800 asFloat = 16r1FFFFFFFFFFFF0000 asFloat. "nearest even is lower"

]

{ #category : 'tests - accessing parts' }
FloatTest >> testIsDenormalized [
	self deny: Float pi isDenormalized.
	self deny: 0.0 isDenormalized.
	self assert: 0.0 successor isDenormalized.
	self assert: Float fmin isDenormalized.
	self assert: Float fminNormalized predecessor isDenormalized.
	self deny: Float fminNormalized isDenormalized.
	self deny: Float pi negated isDenormalized.
	self deny: -0.0 isDenormalized.
	self assert: -0.0 predecessor isDenormalized.
	self assert: Float fmin negated isDenormalized.
	self assert: Float fminNormalized negated successor isDenormalized.
	self deny: Float fminNormalized negated isDenormalized.
]

{ #category : 'tests - characterization' }
FloatTest >> testIsFinite [
	self assert: Float fminDenormalized isFinite.
	self assert: 0.0 isFinite.
	self assert: Float pi negated isFinite.
	self assert: Float fmax isFinite.
	
	self deny: Float infinity isFinite.
	self deny: Float negativeInfinity isFinite.
	self deny: Float nan isFinite.
]

{ #category : 'tests - zero behavior' }
FloatTest >> testIsZero [
	self assert: 0.0 isZero.
	self assert: -0.0 isZero.
	self deny:  0.1 isZero.
]

{ #category : 'tests - comparing' }
FloatTest >> testLiteralEqualityOfNan [
	| nan |
	nan := Float nan.
	self assert: (nan literalEqual: nan)
		description: 'Float nan is not equal to itself, though it is literally equal'.
]

{ #category : 'tests - comparing' }
FloatTest >> testLiteralEqualityOfZeroAndNegativeZero [
	self assert: Float zero = Float negativeZero.
	self deny: (Float zero literalEqual: Float negativeZero).
	self
		assert: (Compiler evaluate: '1>2 ifTrue: [0.0] ifFalse: [-0.0]') hex = Float negativeZero hex
		description: 'Float zero and negativeZero are not literally substituable'.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testLn [
	self assert: 0.0 exp ln = 0.0.
	self assert: 1.0 exp ln = 1.0.
	self assert: 2.0 exp ln = 2.0.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testLnForExceptionalValues [

	self assert: -1.0 ln isNaN.
	self assert: Float infinity ln = Float infinity.
	self assert: Float negativeInfinity ln isNaN
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testLnNonPrimitive [

	#[0.0 0.01 0.1 0.3 0.5 0.7 0.8 1.01 2.01 5.0 ] do: [ :x | | computed expected |
		computed _ x exp lnNonPrimitive.
		expected _ x exp ln.
		self assert: (computed - expected) abs <= (expected ulp*2)
		]
]

{ #category : 'tests - constants' }
FloatTest >> testMaxExactInteger [
	"
	FloatTest new testMaxExactInteger
	"

	self assert: Float maxExactInteger asFloat truncated = Float maxExactInteger.
	0 to: 10000 do: [ :j |
		self assert: (Float maxExactInteger-j) asFloat truncated = (Float maxExactInteger-j) ].
	self deny: (Float maxExactInteger+1) asFloat truncated = (Float maxExactInteger+1)
	
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testMinimalAsString [
	"Tests that the conversion from String / to String ends in a minimal String representation.
	This means that there are no extra unneded digits at the end.
	For example, 0.1 (actually the closest Float to 1/10, i.e. ((1/10) asFloat), prints as 0.1, because 
	it can be recovered from that String"

	"These are just to start on solid ground. If these fail, what follows is meaningless."
	self assert: 0.1 = ((1/10) asFloat).		"0.1 really is what it should be. Compiler works ok."
	self assert: '0.1' asNumber = 0.1.			"#asNumber works ok. The string '0.1' is what we want."
	"Now the real test."
	self assert: 0.1 printString = '0.1'.		"String '0.1' is what we get. No unneded extra digits."

	"Whatever Float these Strings represent, print them back the same (no extra digits)"
	self assert: '0.2' asNumber printString = '0.2'.
	#(
		'0.1' '0.2' '0.3' '0.4' '0.5' '0.6' '0.7' '0.8' '0.9' 
		'1.0' '1.1' '1.2' '1.3' '1.4' '1.5' '1.6' '1.7' '1.8' '1.9' '2.0'
		'1.01' '1.001' '1.0001' '1.00001' '1.000001' '1.0000001' '1.00000001' '1.000000001' '1.0000000001' '1.00000000001' '1.000000000001'
		'1.03' '1.003' '1.0003' '1.00003' '1.000003' '1.0000003' '1.00000003' '1.000000003' '1.0000000003' '1.00000000003' '1.000000000003' 
	) do: [ :string |
		self assert: string asNumber printString = string ].
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testMixedTypeArithmetic [

	| samples doubleOne |

	samples := Array with: 1 with: 1 / 3 with: Float pi with: SmallInteger maxVal * 2 + 1 with: SmallInteger minVal * 2 - 1.
	doubleOne := 1.0.
	samples do: [ :sample |
		self assert: sample + doubleOne isExactly: sample asFloat + doubleOne.
		self assert: doubleOne + sample isExactly: doubleOne + sample asFloat.
		self assert: sample - doubleOne isExactly: sample asFloat - doubleOne.
		self assert: doubleOne - sample isExactly: doubleOne - sample asFloat.
		self assert: sample * doubleOne isExactly: sample asFloat * doubleOne.
		self assert: doubleOne * sample isExactly: doubleOne * sample asFloat.
		self assert: sample / doubleOne isExactly: sample asFloat / doubleOne.
		self assert: doubleOne / sample isExactly: doubleOne / sample asFloat.
		self assert: sample \\ doubleOne isExactly: sample asFloat \\ doubleOne.
		self assert: doubleOne \\ sample isExactly: doubleOne \\ sample asFloat.
		self assert: (sample // doubleOne) = (sample asFloat // doubleOne).
		self assert: (doubleOne // sample) = (doubleOne // sample asFloat).
		].
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testMixedTypeComparison [

	| samples doubleOne |

	samples := Array with: 1 with: 1 / 3 with: Float pi with: SmallInteger maxVal * 2 + 1 with: SmallInteger minVal * 2 - 1.
	doubleOne := 1.0.
	samples do: [ :sample |
		self assert: (sample = doubleOne) = (sample asFloat = doubleOne).
		self assert: (doubleOne = sample) = (doubleOne = sample asFloat).
		self assert: (sample ~= doubleOne) = (sample asFloat ~= doubleOne).
		self assert: (doubleOne ~= sample) = (doubleOne ~= sample asFloat).
		self assert: (sample < doubleOne) = (sample asFloat < doubleOne).
		self assert: (doubleOne < sample) = (doubleOne < sample asFloat).
		self assert: (sample <= doubleOne) = (sample asFloat <= doubleOne).
		self assert: (doubleOne <= sample) = (doubleOne <= sample asFloat).
		self assert: (sample > doubleOne) = (sample asFloat > doubleOne).
		self assert: (doubleOne > sample) = (doubleOne > sample asFloat).
		self assert: (sample >= doubleOne) = (sample asFloat >= doubleOne).
		self assert: (doubleOne >= sample) = (doubleOne >= sample asFloat).
		].
]

{ #category : 'tests - arithmetic' }
FloatTest >> testMultiply [

	self assert: 2.0 * 2.0 = 4.0.
	self assert: Float fminNormalized * 2.0 = (Float fminNormalized + Float fminNormalized).
	self assert: Float fminDenormalized * 2.0 = (Float fminDenormalized + Float fminDenormalized).
	self assertIsPositiveZero: 1.0 * Float zero.
	self assertIsPositiveZero: Float zero * 1.0.
	self assertIsNegativeZero: 1.0 * Float negativeZero.
	self assertIsNegativeZero: Float negativeZero * 1.0.
	self assert: 1.0 * Float infinity = Float infinity.
	self assert: Float infinity * 1.0 = Float infinity.
	self assert: 1.0 * Float negativeInfinity = Float negativeInfinity.
	self assert: Float negativeInfinity * 1.0 = Float negativeInfinity.
	self assert: (1.0 * Float nan) isNaN.
	self assert: (Float nan * 1.0) isNaN.
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaN1 [
   	"FloatTest new testNaN1"

	self assert: Float nan == Float nan.
	self deny: Float nan = Float nan.
	"a NaN is not equal to itself."

]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaN2 [
	"Two NaN values are always considered to be different.
	On an little-endian machine (32 bit Intel), Float nan is 16rFFF80000 16r00000000.
	On a big-endian machine (PowerPC), Float nan is 16r7FF80000 16r00000000. Changing
	the bit pattern of the first word of a NaN produces another value that is still
	considered equal to NaN. This test should work on both little endian and big
	endian machines."

	"FloatTest new testNaN2"

	| nan1 nan2 |
	nan1 := Float nan copy.
	nan2 := Float nan copy.

	"test two instances of NaN with the same bit pattern"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1.

	"change the bit pattern of nan1"
	self assert: nan1 size = 2.
	self assert: (nan1 at: 2) = 0.
	nan1 at: 1 put: (nan1 at: 1) + 999.
	self assert: nan1 isNaN.
	self assert: nan2 isNaN.
	self deny: (nan1 at: 1) = (nan2 at: 1).

	"test two instances of NaN with different bit patterns"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaN3 [
	"FloatTest new testNaN3"

	| set item identitySet |
	set _ Set new.
	set add: (item _ Float nan).
	self deny: (set includes: item).
	identitySet _ IdentitySet new.
	identitySet add: (item _ Float nan).
	self assert: (identitySet includes: item).
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaN4 [
	"FloatTest new testNaN4"

	| dict |
	dict _ Dictionary new.
	dict
		at: Float nan
		put: #NaN.
	self deny: (dict includesKey: Float nan).
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaNCompare [
	"IEEE 754 states that NaN cannot be ordered.
	As a consequence, every arithmetic comparison involving a NaN SHOULD return false.
	Except the is different test (~=).
	This test does verify this rule"
	
	| compareSelectors theNaN anotherNaN comparand brokenMethods warningMessage |
	compareSelectors := #(#< #<= #> #>= #=).
	theNaN := Float nan.
	anotherNaN := Float infinity - Float infinity.
	comparand := {1. 2.3. Float infinity. 2/3. 1.25. 2 raisedTo: 50}.
	comparand := comparand , (comparand collect: [:e | e negated]).
	comparand := comparand , {theNaN. anotherNaN}.

"do a first pass to collect all broken methods"
	brokenMethods := Set new.
	comparand do: [:comp |
		compareSelectors do: [:op |
			(theNaN perform: op with: comp) ifTrue: [brokenMethods add: (theNaN class lookupSelector: op)].
			(comp perform: op with: theNaN) ifTrue: [brokenMethods add: (comp class lookupSelector: op)]].
		(theNaN ~= comp) ifFalse: [brokenMethods add: (theNaN class lookupSelector: #~=)].
		(comp ~= theNaN) ifFalse: [brokenMethods add: (comp class lookupSelector: #~=)]].
	
"build a warning message to tell about all broken methods at once"
	warningMessage := String streamContents: [:s |
			s nextPutAll: 'According to IEEE 754 comparing with a NaN should always return false, except ~= that should return true.'; newLine.
			s nextPutAll: 'All these methods failed to do so. They are either broken or call a broken one'.
			brokenMethods do: [:e | s newLine; print: e methodClass; nextPutAll: '>>'; print: e selector]].
		
"Redo the tests so as to eventually open a debugger on one of the failures"
	brokenMethods := Set new.
	comparand do: [:comp2 |
		compareSelectors do: [:op2 |
			self deny: (theNaN perform: op2 with: comp2) description: warningMessage.
			self deny: (comp2 perform: op2 with: theNaN) description: warningMessage].
		self assert: (theNaN ~= comp2) description: warningMessage.
		self assert: (comp2 ~= theNaN) description: warningMessage].
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaNPropagationBinary [
	"Any operation should propagate NaN"
	| s a c |
	s := Set new.
	s addAll: Number selectors.
	s addAll: Float selectors.
	s addAll: BoxedFloat64 selectors.
	s addAll: SmallFloat64 selectors.
	s := s select: [ :sel | sel numArgs = 1 ].
	s removeAll: #(#at: #basicAt: #ifNotZero: smoothIsAbsBelow:).
	a := s asArray sort.
	a do: [ :sel |
		{ 0. 0.0. 1/3. 1. 1.0. 2. 2.0. Float nan } do: [ :op1 |
			{ 0. 0.0. 1/3. 1. 1.0. 2. 2.0. Float nan } do: [ :op2 |
				op1 isNaN | op2 isNaN ifTrue: [
					c := [ op1 perform: sel with: op2 ] on: UnhandledError do: [ #error ].
					self assert: (c isNumber not or: [c isNaN]) ]]]].
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaNPropagationUnary [
	"Any operation should propagate NaN"
	| s a c |
	s := Set new.
	s addAll: Number selectors.
	s addAll: Float selectors.
	s addAll: BoxedFloat64 selectors.
	s addAll: SmallFloat64 selectors.
	s := s select: [ :sel | sel numArgs = 0 ].
	s removeAll: #(#asIEEE32BitWord #asIEEE64BitWord #byteSize #hash #identityHash #mantissaBits #signBit #exponentBits #exponent primTestExponent #fractionPart).
	a := s asArray sort.
	a do: [ :sel |
		c := [ Float nan perform: sel ] on: UnhandledError do: [ #error ].
		self assert: (c isNumber not or: [c isNaN]) ].
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaNTruncated [
	self assert: Float nan truncated isNaN.
]

{ #category : 'tests - NaN behavior' }
FloatTest >> testNaNisLiteral [
	self deny: Float nan isLiteral description: 'there is no literal representation of NaN'
]

{ #category : 'tests - zero behavior' }
FloatTest >> testNegativeZeroAbs [
	self assert: Float negativeZero abs signBit = 0 description: 'the absolute value of a negative zero is zero'
]

{ #category : 'tests - precision and extreme values' }
FloatTest >> testNextAwayFromZero [

	self assert: Float pi nextAwayFromZero = Float pi successor.
	self assert: Float pi negated nextAwayFromZero = Float pi negated predecessor.
	self assert: 1.0 nextAwayFromZero = 1.0 successor.
	self should: [Float infinity nextAwayFromZero] raise: Error.
	self assert: Float infinity predecessor nextAwayFromZero = Float infinity.
	self assert: Float infinity predecessor predecessor nextAwayFromZero = Float infinity predecessor.
	self should: [Float negativeInfinity nextAwayFromZero] raise: Error.
	self assert: Float negativeInfinity successor nextAwayFromZero = Float negativeInfinity.
	self assert: Float negativeInfinity successor successor nextAwayFromZero = Float negativeInfinity successor.
	self assert: 0.0 nextAwayFromZero = Float fminDenormalized.
	self assert: -0.0 nextAwayFromZero = Float fminDenormalized negated.
]

{ #category : 'tests - precision and extreme values' }
FloatTest >> testNextTowardsZero [

	self assert: Float pi nextTowardsZero = Float pi predecessor.
	self assert: Float pi negated nextTowardsZero = Float pi negated successor.
	self assert: 1.0 nextTowardsZero = 1.0 predecessor.
	self should: [Float infinity nextTowardsZero] raise: Error.
	self assert: Float infinity predecessor nextTowardsZero = Float infinity predecessor predecessor.
	self should: [Float negativeInfinity nextTowardsZero] raise: Error.
	self assert: Float negativeInfinity successor nextTowardsZero = Float negativeInfinity successor successor.
	self should: [0.0 nextTowardsZero] raise: Error.
	self assert: 0.0 successor nextTowardsZero = 0.0.
	self should: [-0.0 nextTowardsZero] raise: Error.
	self assert: -0.0 predecessor nextTowardsZero = -0.0
]

{ #category : 'tests - hash' }
FloatTest >> testNoIntegerHash [

	self deny: Float pi hash = 3 hash.
	self deny: Float pi hash = (Float pi *1.00000000000001) hash.
	self deny: Float pi hash = (Float pi /1.00000000000001) hash.
]

{ #category : 'tests - accessing parts' }
FloatTest >> testPartBits [
	| denormals exceptionals normals |
	
	normals := {Float pi. Float pi * 100.0. Float pi/ -100.0. Float fmax. Float fminNormalized}.
	denormals := {0.0. Float negativeZero. Float fminNormalized predecessor. Float fmin negated}.
	exceptionals := {Float nan. Float infinity. Float negativeInfinity.}.
	
	normals , denormals , exceptionals do: [ :aFloat |
		self assert: 
			(Float
				signBit: aFloat signBit
				mantissaBits: aFloat mantissaBits
				exponentBits: aFloat exponentBits) hex
			= aFloat hex ]
]

{ #category : 'tests - constants' }
FloatTest >> testPi [
	"Just in case..."

	self assert: Float pi = 3.141592653589793.
	self assert: Float pi = (1.0 arcTan * 4).
	self assert: Float pi hex = '400921FB54442D18' 
]

{ #category : 'tests - zero behavior' }
FloatTest >> testPositiveAndNegativeZero [
	self assert: Float negativeZero = Float zero description: 'Positive and negative zero are defined to be equal'.
	self deny: -0.0 < 0.0 description: 'Positive and negative zero are defined to be equal'.
	self deny: -0.0 > 0.0 description: 'Positive and negative zero are defined to be equal'.
	self deny: Float negativeZero hex = Float zero hex description: 'Positive and negative zero are not the same!'.
	
	self assertIsPositiveZero: 1.0 * 0.0.
	self assertIsNegativeZero: 1.0 * -0.0.
	self assertIsNegativeZero: -1.0 * 0.0.
	self assertIsPositiveZero: -1.0 * -0.0.

	self assertIsPositiveZero: Float fmin / 4.0.
	self assertIsNegativeZero: Float fmin / -4.0.
	self assertIsNegativeZero: Float fmin negated / 4.0.
	self assertIsPositiveZero: Float fmin negated / -4.0.
]

{ #category : 'tests - precision and extreme values' }
FloatTest >> testPredecessorSuccessor [
	self assert: 0.0 successor = Float fminDenormalized.
	self assertIsNegativeZero: 0.0 predecessor.
	self assertIsPositiveZero: -0.0 successor.
	self assert: -0.0 predecessor = Float fminDenormalized negated.
	self assert: 1.0 successor > 1.0.
	self assert: 1.0 successor predecessor = 1.0.
	self assert: 1.0 predecessor < 1.0.
	self assert: 1.0 predecessor successor = 1.0.
	self assert: -1.0 successor > -1.0.
	self assert: -1.0 successor predecessor = -1.0.
	self assert: -1.0 predecessor < -1.0.
	self assert: -1.0 predecessor successor = -1.0.
]

{ #category : 'tests - precision and extreme values' }
FloatTest >> testPredecessorSuccessor2 [
	| mustBeOneOfThem |
	mustBeOneOfThem _ Float fminNormalized + Float fminNormalized successor / 2.
	self assert: (mustBeOneOfThem = Float fminNormalized or: [
		mustBeOneOfThem = Float fminNormalized successor ]).
	mustBeOneOfThem _ 1.0 + 1.0 successor / 2.
	self assert: (mustBeOneOfThem = 1.0 or: [
		mustBeOneOfThem = 1.0 successor ]).
	mustBeOneOfThem _ 1.0 + 1.0 predecessor / 2.
	self assert: (mustBeOneOfThem = 1.0 or: [
		mustBeOneOfThem = 1.0 successor ]).
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimAdd [

	self assert: (1.0 primTestAdd: 1.0) = 2.0.
	self assert: (Float fminNormalized primTestAdd: Float fminDenormalized) > Float fminNormalized.
	self assert: (Float fminNormalized primTestAdd: Float fminDenormalized) - Float fminNormalized = Float fminDenormalized.
	self assert: (1.0 primTestAdd: Float zero) = 1.0.
	self assert: (Float zero primTestAdd: 1.0) = 1.0.
	self assert: (1.0 primTestAdd: Float negativeZero) = 1.0.
	self assert: (Float negativeZero primTestAdd: 1.0) = 1.0.
	self assert: (1.0 primTestAdd: Float infinity) = Float infinity.
	self assert: (Float infinity primTestAdd: 1.0) = Float infinity.
	self assert: (1.0 primTestAdd: Float negativeInfinity) = Float negativeInfinity.
	self assert: (Float negativeInfinity primTestAdd: 1.0) = Float negativeInfinity.
	self assert: (1.0 primTestAdd: Float nan) isNaN.
	self assert: (Float nan primTestAdd: 1.0) isNaN.
	self assert: (Float nan primTestAdd: Float nan) isNaN.
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimArcTan [

	self verify: 1.0 primTestArcTan isTrigonometricallyEqualTo: Float pi / 4.
	self verify: -1.0 primTestArcTan isTrigonometricallyEqualTo: Float pi / -4.
	self verify: 0.0 primTestArcTan isTrigonometricallyEqualTo: 0.
	self verify: Float infinity primTestArcTan isTrigonometricallyEqualTo: Float pi / 2.
	self verify: Float negativeInfinity primTestArcTan isTrigonometricallyEqualTo: Float pi / -2.
	self assert: Float nan primTestArcTan isNaN
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimDivideBy [

	self assert: (1.5 primTestDivideBy: 2.0) = 0.75.

	Smalltalk numericPrimsMixArithmetic
		ifTrue: [ self assert: (2.0 primTestDivideBy: 1) = 2.0 ]
		ifFalse: [ self assert: (2.0 primTestDivideBy: 1) = #fail ].

	self assert: (2.0 primTestDivideBy: 0) = #fail.
	self assert: (2.0 primTestDivideBy: 0.0) = #fail.
	self assert: (1.2 primTestDivideBy: Float negativeZero) = #fail.
	self assert: (1.2 primTestDivideBy: (1.3 - 1.3)) = #fail.
	self assert: (0.0 primTestDivideBy: 0.0) = #fail.
	self assert: (0.0 primTestDivideBy: Float negativeZero) = #fail.
	self assert: (Float negativeZero primTestDivideBy: 0.0) = #fail.
	self assert: (Float negativeZero primTestDivideBy: Float negativeZero) = #fail.
	self assert: (Float nan primTestDivideBy: 0.0) = #fail.
	self assert: (Float nan primTestDivideBy: Float negativeZero) = #fail.
	self assert: (Float infinity primTestDivideBy: 0.0) = #fail.
	self assert: (Float negativeInfinity primTestDivideBy: Float negativeZero) = #fail.

	self assert: (2.0 primTestDivideBy: 2.0) = 1.0.
	self assert: (Float fminNormalized primTestDivideBy: 2.0) * 2.0 = Float fminNormalized.
	self assert: (Float fminDenormalized * 2.0 primTestDivideBy: 2.0) = Float fminDenormalized.

	self assertIsPositiveZero: (Float zero primTestDivideBy: 1.0).
	self assertIsNegativeZero: (Float negativeZero primTestDivideBy: 1.0).

	self assertIsPositiveZero: (1.0 primTestDivideBy: Float infinity).
	self assert: (Float infinity primTestDivideBy: 1.0) = Float infinity.
	self assertIsNegativeZero: (-1.0 primTestDivideBy: Float infinity).
	self assert: (Float infinity primTestDivideBy: -1.0) = Float negativeInfinity.

	self assertIsNegativeZero: (1.0 primTestDivideBy: Float negativeInfinity).
	self assert: (Float negativeInfinity primTestDivideBy: 1.0) = Float negativeInfinity.
	self assertIsPositiveZero: (-1.0 primTestDivideBy: Float negativeInfinity).
	self assert: (Float negativeInfinity primTestDivideBy: -1.0) = Float infinity.

	self assert: (1.0 primTestDivideBy: Float nan) isNaN.
	self assert: (Float nan primTestDivideBy: 1.0) isNaN.
	self assert: (Float nan primTestDivideBy: Float nan) isNaN.
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimEqual [

	| samples |
	samples _ { Float negativeInfinity. -1.0. Float fminNormalized negated. Float fminDenormalized negated. Float fminDenormalized. Float fminNormalized. 1.0. Float infinity }.

	1 to: samples size do: [ :i |
		1 to: samples size do: [ :j |
			self assert: ((samples at: i) primTestEqual: (samples at: j)) = (i = j ) ]].

	self assert: (Float zero primTestEqual: Float negativeZero).
	self assert: (Float negativeZero primTestEqual: Float zero).
	self deny: (Float zero primTestEqual: Float nan).
	self deny: (Float nan primTestEqual: Float zero).
	self deny: (Float negativeZero primTestEqual: Float nan).
	self deny: (Float nan primTestEqual: Float negativeZero).
	self deny: (Float nan primTestEqual: Float nan).

	1 to: samples size do: [ :i |
		self deny: ((samples at: i) primTestEqual: Float zero).
		self deny: ((samples at: i) primTestEqual: Float negativeZero).
		self deny: ((samples at: i) primTestEqual: Float nan).
		self deny: (Float zero primTestEqual: (samples at: i)).
		self deny: (Float negativeZero primTestEqual: (samples at: i)).
		self deny: (Float nan primTestEqual: (samples at: i)) ].
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimExpAndLn [
	| e |
	e := Float e.
	self assert: 1.0 primTestExp = e.
	self assert: e primTestLn = 1.0.
	self verify: 2.0 primTestExp isWithin: 1 floatsAwayFrom: e squared.
	self assert: 2.0 primTestExp sqrt = e.
	self assert: 2.0 primTestExp primTestLn = 2.0.
	self assert: 0.0 primTestExp = 1.0.
	self assert: 0.0 primTestLn = Float negativeInfinity.
	self assert: 0.0 primTestLn primTestExp = 0.0.
	self assert: 0.0 successor primTestLn ~= Float negativeInfinity.
	self assert: 0.0 successor primTestLn primTestExp = 0.0 successor.
	self assert: 0.0 successor primTestLn ceiling asFloat primTestExp > 0.0 successor.
	self assertIsNaNorFail: -1.0 primTestLn.
	self assert: Float nan primTestLn isNaN.
	self assert: Float nan primTestExp isNaN
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimExponent [
	self assert: 1.0 primTestExponent = 0.
	self assert: 2.0 predecessor primTestExponent = 0.
	self assert: 2.0 primTestExponent =  1.
	self assert: 4.0 predecessor primTestExponent = 1.
	self assert: 4.0 primTestExponent = 2.
	self assert: Float fminNormalized primTestExponent = -1022.
	self assert: Float fminDenormalized primTestExponent = (-1022-52).
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimGreater [
	self deny: (1.0 primTestGreater: 1.0 successor).
	self deny: (1.0 primTestGreater: 1.0).
	self assert: (1.0 primTestGreater: 1.0 predecessor).
	self assert: (0.0 primTestGreater: Float fminNormalized negated).
	self assert: (0.0 primTestGreater: Float fminDenormalized negated).
	self assert: (0.0 primTestGreater: -1.0).
	self assert: (Float fminNormalized primTestGreater: 0.0).
	self assert: (Float fminDenormalized primTestGreater: 0.0).
	self deny: (-1.0 primTestGreater: 0.0).
	self deny: (1.0 primTestGreater: Float infinity).
	self deny: (Float infinity primTestGreater: Float infinity).
	self deny: (Float negativeInfinity primTestGreater: Float infinity).
	self deny: (Float negativeInfinity primTestGreater: -1.0).
	self deny: (Float negativeInfinity primTestGreater: Float negativeZero).
	self deny: (Float negativeZero primTestGreater: Float zero).
	self deny: (Float zero primTestGreater: Float fminDenormalized).
	self deny: (Float fminDenormalized primTestGreater: Float fminNormalized).
	self deny: (Float fminNormalized primTestGreater: 1.0).
	self deny: (Float nan primTestGreater: Float negativeInfinity).
	self deny: (Float nan primTestGreater: Float negativeZero).
	self deny: (Float nan primTestGreater: Float zero).
	self deny: (Float nan primTestGreater: 1.0).
	self deny: (Float nan primTestGreater: Float infinity).
	self deny: (Float nan primTestGreater: Float nan).
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimGreaterEqual [
	self deny: (1.0 primTestGreaterEqual: 1.0 successor).
	self assert: (1.0 primTestGreaterEqual: 1.0).
	self assert: (1.0 primTestGreaterEqual: 1.0 predecessor).
	self assert: (0.0 primTestGreaterEqual: Float fminNormalized negated).
	self assert: (0.0 primTestGreaterEqual: Float fminDenormalized negated).
	self assert: (0.0 primTestGreaterEqual: -1.0).
	self assert: (Float fminNormalized primTestGreaterEqual: 0.0).
	self assert: (Float fminDenormalized primTestGreaterEqual: 0.0).
	self deny: (-1.0 primTestGreaterEqual: 0.0).
	self deny: (1.0 primTestGreaterEqual: Float infinity).
	self assert: (Float infinity primTestGreaterEqual: Float infinity).
	self deny: (Float negativeInfinity primTestGreaterEqual: Float infinity).
	self deny: (Float negativeInfinity primTestGreaterEqual: -1.0).
	self deny: (Float negativeInfinity primTestGreaterEqual: Float negativeZero).
	self assert: (Float negativeZero primTestGreaterEqual: Float zero).
	self deny: (Float zero primTestGreaterEqual: Float fminDenormalized).
	self deny: (Float fminDenormalized primTestGreaterEqual: Float fminNormalized).
	self deny: (Float fminNormalized primTestGreaterEqual: 1.0).
	self deny: (Float nan primTestGreaterEqual: Float negativeInfinity).
	self deny: (Float nan primTestGreaterEqual: Float negativeZero).
	self deny: (Float nan primTestGreaterEqual: Float zero).
	self deny: (Float nan primTestGreaterEqual: 1.0).
	self deny: (Float nan primTestGreaterEqual: Float infinity).
	self deny: (Float nan primTestGreaterEqual: Float nan).
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimLess [
	self assert: (1.0 primTestLess: 1.0 successor).
	self deny: (1.0 primTestLess: 1.0).
	self deny: (1.0 primTestLess: 1.0 predecessor).
	self assert: (0.0 primTestLess: Float fminNormalized).
	self assert: (0.0 primTestLess: Float fminDenormalized).
	self assert: (0.0 primTestLess: 1.0).
	self assert: (Float fminNormalized negated primTestLess: 0.0).
	self assert: (Float fminDenormalized negated primTestLess: 0.0).
	self assert: (-1.0 primTestLess: 0.0).
	self assert: (1.0 primTestLess: Float infinity).
	self deny: (Float infinity primTestLess: Float infinity).
	self assert: (Float negativeInfinity primTestLess: Float infinity).
	self assert: (Float negativeInfinity primTestLess: -1.0).
	self assert: (Float negativeInfinity primTestLess: Float negativeZero).
	self deny: (Float negativeZero primTestLess: Float zero).
	self assert: (Float zero primTestLess: Float fminDenormalized).
	self assert: (Float fminDenormalized primTestLess: Float fminNormalized).
	self assert: (Float fminNormalized primTestLess: 1.0).
	self deny: (Float nan primTestLess: Float negativeInfinity).
	self deny: (Float nan primTestLess: Float negativeZero).
	self deny: (Float nan primTestLess: Float zero).
	self deny: (Float nan primTestLess: 1.0).
	self deny: (Float nan primTestLess: Float infinity).
	self deny: (Float nan primTestLess: Float nan).
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimLessEqual [
	self assert: (1.0 primTestLessEqual: 1.0 successor).
	self assert: (1.0 primTestLessEqual: 1.0).
	self deny: (1.0 primTestLessEqual: 1.0 predecessor).
	self assert: (0.0 primTestLessEqual: Float fminNormalized).
	self assert: (0.0 primTestLessEqual: Float fminDenormalized).
	self assert: (0.0 primTestLessEqual: 1.0).
	self assert: (Float fminNormalized negated primTestLessEqual: 0.0).
	self assert: (Float fminDenormalized negated primTestLessEqual: 0.0).
	self assert: (-1.0 primTestLessEqual: 0.0).
	self assert: (1.0 primTestLessEqual: Float infinity).
	self assert: (Float infinity primTestLessEqual: Float infinity).
	self assert: (Float negativeInfinity primTestLessEqual: Float infinity).
	self assert: (Float negativeInfinity primTestLessEqual: -1.0).
	self assert: (Float negativeInfinity primTestLessEqual: Float negativeZero).
	self assert: (Float negativeZero primTestLessEqual: Float zero).
	self assert: (Float zero primTestLessEqual: Float fminDenormalized).
	self assert: (Float fminDenormalized primTestLessEqual: Float fminNormalized).
	self assert: (Float fminNormalized primTestLessEqual: 1.0).
	self deny: (Float nan primTestLessEqual: Float negativeInfinity).
	self deny: (Float nan primTestLessEqual: Float negativeZero).
	self deny: (Float nan primTestLessEqual: Float zero).
	self deny: (Float nan primTestLessEqual: 1.0).
	self deny: (Float nan primTestLessEqual: Float infinity).
	self deny: (Float nan primTestLessEqual: Float nan).
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimMultiplyBy [

	self assert: (2.0 primTestMultiplyBy: 2.0) = 4.0.
	self assert: (Float fminNormalized primTestMultiplyBy: 2.0) = (Float fminNormalized + Float fminNormalized).
	self assert: (Float fminDenormalized primTestMultiplyBy: 2.0) = (Float fminDenormalized + Float fminDenormalized).
	self assertIsPositiveZero: (1.0 primTestMultiplyBy: Float zero).
	self assertIsPositiveZero: (Float zero primTestMultiplyBy: 1.0).
	self assertIsNegativeZero: (1.0 primTestMultiplyBy: Float negativeZero).
	self assertIsNegativeZero: (Float negativeZero primTestMultiplyBy: 1.0).
	self assert: (1.0 primTestMultiplyBy: Float infinity) = Float infinity.
	self assert: (Float infinity primTestMultiplyBy: 1.0) = Float infinity.
	self assert: (1.0 primTestMultiplyBy: Float negativeInfinity) = Float negativeInfinity.
	self assert: (Float negativeInfinity primTestMultiplyBy: 1.0) = Float negativeInfinity.
	self assert: (1.0 primTestMultiplyBy: Float nan) isNaN.
	self assert: (Float nan primTestMultiplyBy: 1.0) isNaN.
	self assert: (Float nan primTestMultiplyBy: Float nan) isNaN.
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimNotEqual [

	| samples |
	samples _ { Float negativeInfinity. -1.0. Float fminNormalized negated. Float fminDenormalized negated. Float fminDenormalized. Float fminNormalized. 1.0. Float infinity }.

	1 to: samples size do: [ :i |
		1 to: samples size do: [ :j |
			self assert: ((samples at: i) primTestNotEqual: (samples at: j)) ~= (i = j ) ]].

	self deny: (Float zero primTestNotEqual: Float negativeZero).
	self deny: (Float negativeZero primTestNotEqual: Float zero).
	self assert: (Float zero primTestNotEqual: Float nan).
	self assert: (Float nan primTestNotEqual: Float zero).
	self assert: (Float negativeZero primTestNotEqual: Float nan).
	self assert: (Float nan primTestNotEqual: Float negativeZero).
	self assert: (Float nan primTestNotEqual: Float nan).

	1 to: samples size do: [ :i |
		self assert: ((samples at: i) primTestNotEqual: Float zero).
		self assert: ((samples at: i) primTestNotEqual: Float negativeZero).
		self assert: ((samples at: i) primTestNotEqual: Float nan).
		self assert: (Float zero primTestNotEqual: (samples at: i)).
		self assert: (Float negativeZero primTestNotEqual: (samples at: i)).
		self assert: (Float nan primTestNotEqual: (samples at: i)) ].
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimSin [

	self verify: 0.0 primTestSin isTrigonometricallyEqualTo: 0.0.
	self verify: (Float pi / 6) primTestSin isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) primTestSin isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 3) primTestSin isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) primTestSin isTrigonometricallyEqualTo: 1.0
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimSmallIntegerAsFloat [
	self assert: 7 asFloat = 7.0.
	self assert: 0 asFloat = 0.0.
	self assert: 1 asFloat = 1.0.
	self assert: -1 asFloat = -1.0.
	self assert: Float maxExactInteger asFloat = (Float maxExactInteger printString, '.0') asNumber.
	self assert: Float maxExactInteger negated asFloat = (Float maxExactInteger negated printString, '.0') asNumber.
	self assert: SmallInteger maxVal asFloat = (SmallInteger maxVal printString, '.0') asNumber.
	self assert: SmallInteger minVal asFloat = (SmallInteger minVal printString, '.0') asNumber.
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimSqrt [

	self assert: 4.0 primTestSqrt = 2.0.
	self assert: 0.0 primTestSqrt = 0.0.
	self assert: Float negativeZero primTestSqrt = 0.0.
	self assert: Float fminNormalized primTestSqrt squared = Float fminNormalized.
	self assert: Float fminDenormalized primTestSqrt squared = Float fminDenormalized.
	self assert: Float infinity primTestSqrt = Float infinity.
	
	"Seems to be #fail in workspaces, but for some BoxedFloat64, Cog seems to answer NaN.
	Never mind. Both are valid. #primSqrt answers NaN in case of primitive failure."
	self assertIsNaNorFail: -4.0 primTestSqrt.
	self assertIsNaNorFail: Float fminNormalized negated primTestSqrt.
	self assertIsNaNorFail: Float fminDenormalized negated primTestSqrt.
	self assertIsNaNorFail: Float negativeInfinity primTestSqrt.
	self assertIsNaNorFail: Float nan primTestSqrt.
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimSubtract [

	self assert: (3.0 primTestSubtract: 1.0) = 2.0.
	self assert: (Float fminNormalized primTestSubtract: Float fminDenormalized) < Float fminNormalized.
	self assert: (Float fminNormalized primTestSubtract: Float fminDenormalized) - Float fminNormalized = Float fminDenormalized negated.
	self assert: (1.0 primTestSubtract: Float zero) = 1.0.
	self assert: (Float zero primTestSubtract: 1.0) = -1.0.
	self assert: (1.0 primTestSubtract: Float negativeZero) = 1.0.
	self assert: (Float negativeZero primTestSubtract: 1.0) = -1.0.
	self assert: (1.0 primTestSubtract: Float infinity) = Float negativeInfinity.
	self assert: (Float infinity primTestSubtract: 1.0) = Float infinity.
	self assert: (1.0 primTestSubtract: Float negativeInfinity) = Float infinity.
	self assert: (Float negativeInfinity primTestSubtract: 1.0) = Float negativeInfinity.
	self assert: (1.0 primTestSubtract: Float nan) isNaN.
	self assert: (Float nan primTestSubtract: 1.0) isNaN.
	self assert: (Float nan primTestSubtract: Float nan) isNaN.
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimTimesTwoPower [

	self assert: (Float fminNormalized timesTwoPower: -52) = Float fminDenormalized.
	self assert: (Float fminDenormalized timesTwoPower: 52) = Float fminNormalized.
	self assert: (Float fminNormalized timesTwoPower: -53) = 0.0.
	self assert: (0.0 timesTwoPower: 53) = 0.0.

	self assert: (Float fminNormalized timesTwoPower: 1022) = 1.0.
	self assert: (Float fminDenormalized timesTwoPower: 1022+52) = 1.0 .
	self assert: (1.0 timesTwoPower: -1022) = Float fminNormalized.
	self assert:  (2.0 predecessor timesTwoPower: 1023) = Float fmax.

	self assert: (Float pi timesTwoPower: 13) mantissaPart = Float pi mantissaPart.
	
	self assert: (Float infinity timesTwoPower: -1023) = Float infinity.
	self assert: (Float infinity timesTwoPower: -1024) = Float infinity.
	self assert: (Float negativeInfinity timesTwoPower: -1023) = Float negativeInfinity.
	self assert: (Float negativeInfinity timesTwoPower: -1024) = Float negativeInfinity.
	self assert: (Float nan timesTwoPower: 2) isNaN.
]

{ #category : 'tests - primitives' }
FloatTest >> testPrimTruncated [

	self assert: 1.0 primTestTruncated == 1.
	self assert: 1.1 primTestTruncated == 1.
	self assert: 1.9 primTestTruncated == 1.
	self assert: Float pi primTestTruncated == 3.
	self assert: Float zero primTestTruncated = 0.
	self assert: Float fminNormalized primTestTruncated = 0.
	self assert: Float fminDenormalized primTestTruncated = 0.
	self assert: Float infinity primTestTruncated = #fail.

	self assert: -1.0 primTestTruncated == -1.
	self assert: -1.1 primTestTruncated == -1.
	self assert: -1.9 primTestTruncated == -1.
	self assert: Float pi negated primTestTruncated == -3.
	self assert: Float negativeZero primTestTruncated = 0.
	self assert: Float fminNormalized negated primTestTruncated = 0.
	self assert: Float fminDenormalized negated primTestTruncated = 0.
	self assert: Float negativeInfinity primTestTruncated = #fail.

	self assert: Float nan primTestTruncated = #fail.

	"Only if SmallInteger primitive result is enough to hold the result"
	SmallInteger maxVal >= Float maxExactInteger ifTrue: [
		self assert: (Float maxExactInteger + 0.0) primTestTruncated == Float maxExactInteger.
		self assert: (Float maxExactInteger + 0.9) primTestTruncated == Float maxExactInteger.
		self assert: (Float maxExactInteger - 0.1) primTestTruncated == Float maxExactInteger.
		self assert: (Float maxExactInteger negated + 0.0) primTestTruncated == Float maxExactInteger negated.
		self assert: (Float maxExactInteger negated -  0.9) primTestTruncated == Float maxExactInteger negated.
		self assert: (Float maxExactInteger negated + 0.1) primTestTruncated == Float maxExactInteger negated ].

	self assert: SmallInteger maxVal asFloat predecessor asTrueFraction truncated class = SmallInteger.
	self assert: SmallInteger maxVal asFloat predecessor primTestTruncated = SmallInteger maxVal asFloat predecessor asTrueFraction truncated.	
	self assert: (SmallInteger maxVal+1) asFloat successor asTrueFraction truncated class = LargePositiveInteger.
	self assert: (SmallInteger maxVal+1) asFloat successor primTestTruncated = # fail.
	"Depends on rounding to nearest Float. This could change depending on SmallInteger maxVal.
	Currently false in 64 bits images, but true in 32 bits images."
	SmallInteger maxVal asFloat asTrueFraction truncated class = SmallInteger
		ifTrue: [ self assert: SmallInteger maxVal asFloat primTestTruncated = SmallInteger maxVal asFloat asTrueFraction truncated ]
		ifFalse: [ self assert: SmallInteger maxVal asFloat primTestTruncated = #fail ].

	self assert: SmallInteger minVal asFloat successor asTrueFraction truncated class = SmallInteger.
	self assert: SmallInteger minVal asFloat successor primTestTruncated = SmallInteger minVal asFloat successor asTrueFraction truncated.	
	self assert: (SmallInteger minVal-1) asFloat predecessor asTrueFraction truncated class = LargeNegativeInteger.
	self assert: (SmallInteger minVal-1) asFloat predecessor primTestTruncated = # fail.
	"Depends on rounding to nearestFloat. This could change depending on SmallInteger maxVal.
	Currently true in 64 bits images, and in 32 bits images."
	SmallInteger minVal asFloat asTrueFraction truncated class = SmallInteger
		ifTrue: [ self assert: SmallInteger minVal asFloat primTestTruncated = SmallInteger minVal asFloat asTrueFraction truncated ]
		ifFalse: [ self assert: SmallInteger minVal asFloat primTestTruncated = #fail ].

]

{ #category : 'tests - conversion from to String' }
FloatTest >> testPrintShowingDecimalPlaces4 [
	| requiredDigits |
	requiredDigits _ Float pi printString size - 2.
	self assert: (Float pi printStringFractionDigits: requiredDigits) = Float pi printString.
	0 to: 100 do: [ :extra |
		self assert: (Float pi printStringFractionDigits: requiredDigits + extra) asNumber = Float pi ]
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testPrintStringStress [
	| eWidth mWidth ePrimeScale mPrimeScale |
	eWidth := Float emax - Float emin.
	"211 for a more exhaustive but slower test. 41 for a quick run."
	ePrimeScale := 211.
	ePrimeScale := 41.
	mWidth := 1 bitShift: Float precision.
	"9931 for a more exhaustive but slower test. 37 for a quick run."
	mPrimeScale := 9931.
	mPrimeScale := 37.
	0
		to: mWidth - 1 * mPrimeScale
		by: mWidth
		do: [ :mScaled | | m eAdjustment |
			m := mScaled // mPrimeScale.
			eAdjustment := m highBit - 1.
			"{mScaled. m. m hex. eAdjustment } print."
			Float emin * ePrimeScale
				to: Float emax * ePrimeScale
				by: eWidth
				do: [ :eFraction | | e fp |
					e := eFraction // ePrimeScale - eAdjustment.
					fp := m asFloat timesTwoPower: e.
					self assert: fp printString asNumber = fp.
					self assert: fp negated printString asNumber = fp negated ]]
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testReadFromManyDigits [
	"A naive algorithm may interpret these representations as Infinity or NaN.
	This is http://bugs.squeak.org/view.php?id=6982"
	
	| s1 s2 |
	s1 := '1' , (String new: 321 withAll: $0) , '.0e-321'.
	s2 := '0.' , (String new: 320 withAll: $0) , '1e321'.
	self assert: (Number readFrom: s1) = 1.
	self assert: (Number readFrom: s2) = 1.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testReciprocal [

	self 
		assert: 1.0 reciprocal = 1.0;
		assert: 2.0 reciprocal = 0.5;
		assert: -1.0 reciprocal = -1.0;
		assert: -2.0 reciprocal = -0.5.
		
	self should: [ 0.0 reciprocal ] raise: ZeroDivide.
	self assert:  0.0 reciprocal isExactly: Float infinity
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testRoundHalfAwayFromZero [
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero"
	self assert: 0.5 roundedHAFZ = 1.
	self assert: 1.5 roundedHAFZ = 2.
	self assert: -0.5 roundedHAFZ = -1.
	self assert: -1.5 roundedHAFZ = -2.
	self assert: (((0.0 to: 5.0 by: 0.25) collect: [ :f | f roundedHAFZ ]) = #(0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5) ).
	self assert: (((-0.0 to: -5.0 by: -0.25) collect: [ :f | f roundedHAFZ ]) = #(0 0 -1 -1 -1 -1 -2 -2 -2 -2 -3 -3 -3 -3 -4 -4 -4 -4 -5 -5 -5) )
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testRoundHalfToEven [
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"
	self assert: 0.5 rounded = 0.
	self assert: 1.5 rounded = 2.
	self assert: -0.5 rounded = -0.
	self assert: -1.5 rounded = -2.
	self assert: (((0.0 to: 5.0 by: 0.25) collect: [ :f | f rounded ]) = #(0 0 0 1 1 1 2 2 2 2 2 3 3 3 4 4 4 4 4 5 5) ).
	self assert: (((-0.0 to: -5.0 by: -0.25) collect: [ :f | f rounded ]) = #(0 0 0 -1 -1 -1 -2 -2 -2 -2 -2 -3 -3 -3 -4 -4 -4 -4 -4 -5 -5) )
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testRounded [
	self assert: 0.9 rounded = 1.
	self assert: 1.0 rounded = 1.
	self assert: 1.1 rounded = 1.
	self assert: -1.9 rounded = -2.
	self assert: -2.0 rounded = -2.
	self assert: -2.1 rounded = -2.
	
	"In case of tie, round to upper magnitude"
	self assert: 1.5 rounded = 2.
	self assert: -1.5 rounded = -2.
]

{ #category : 'tests - hash' }
FloatTest >> testSetOfFloat [
	"Classical disagreement between hash and = did lead to a bug.
	This is a non regression test from http://bugs.squeak.org/view.php?id=3360"

	| size3 size4 |
	size3 := (Set newWithRoomForMoreThan: 3) add: 3; add: 3.0; size.
	size4 := (Set newWithRoomForMoreThan: 4) add: 3; add: 3.0; size.
	self assert: size3 = size4 description: 'The size of a Set should not depend on its capacity.'
]

{ #category : 'tests - accessing parts' }
FloatTest >> testSignificandAndExponent [
	| denormals exceptionals normals |
	
	normals := {Float pi. Float pi * 100.0. Float pi/ -100.0. Float fmax. Float fminNormalized}.
	denormals := {0.0. Float negativeZero. Float fminNormalized predecessor. Float fmin negated}.
	exceptionals := {Float nan. Float infinity. Float negativeInfinity.}.
	
	normals, denormals, exceptionals do: [ :aFloat |
		"Any Float can be decomposed into its significand and exponent, and the significand holds the sign"
		aFloat isNaN
			ifTrue: [self assert: (aFloat significand timesTwoPower: aFloat exponent) isNaN]
			ifFalse: [self
				assert: (aFloat significand timesTwoPower: aFloat exponent)
				equals: aFloat]].
	
	normals , denormals do: [:aFloat |
		"The significand magnitude is in interval [1.0, 2.0) "
		aFloat = 0.0
			ifTrue: [self assert: aFloat significand equals: 0]
			ifFalse: [self
				assert: aFloat significand abs >= 1.0;
				assert: aFloat significand abs < 2.0]]
]

{ #category : 'tests - accessing parts' }
FloatTest >> testSignificandAsInteger [
	| mantissaBits denormalPowersOfTwo denormals exceptionals normalPowersOfTwo normals |
	"There are 52 bits used for representing the mantissa (plus an eventual leading 1, see below)"
	mantissaBits := Float precision - 1.
	
	normals := {Float pi. Float pi * 100.0. Float pi/ -100.0. Float fmax. Float fminNormalized}.
	denormals := {0.0. Float negativeZero. Float fminNormalized predecessor. Float fmin negated}.
	exceptionals := {Float nan. Float infinity. Float negativeInfinity.}.
	normalPowersOfTwo := (-10 to: 10) collect: [:i | 1.0 timesTwoPower: i].
	denormalPowersOfTwo := (Float emin - mantissaBits to: Float emin - 1) collect: [:i | 1.0 timesTwoPower: i].
	
	normals do: [:aNormalFloat |
		"Assume the mantissa is written in least 52 bits of hex format, with an implied 1 on position 53"
		self
			assert: (((Integer readFrom: aNormalFloat hex readStream base: 16) bitAnd: 1<<mantissaBits-1) bitOr: 1<<mantissaBits)
			equals: aNormalFloat significandAsInteger].
	
	denormals do: [:aDenormalFloat |
		"For zero and denormal, no implied leading one"
		self
			assert: ((Integer readFrom: aDenormalFloat hex readStream base: 16) bitAnd: 1<<mantissaBits-1)
			equals: aDenormalFloat significandAsInteger].

"This currently is not allowed in Cuis"
false ifTrue: [
	exceptionals do: [:aDenormalOrExceptionalFloat |
		"For every other Float, zero, denormal or exceptional, no implied leading one"
		self
			assert: ((Integer readFrom: aDenormalOrExceptionalFloat hex readStream base: 16) bitAnd: 1<<mantissaBits-1)
			equals: aDenormalOrExceptionalFloat significandAsInteger].
].

	normalPowersOfTwo do: [:aNormalPowerOfTwoFloat |
		"The significand of a power of two is a power of two, with high bit of expected precision"
		self assert: aNormalPowerOfTwoFloat significandAsInteger isPowerOfTwo.
		self assert: aNormalPowerOfTwoFloat significandAsInteger highBit equals: Float precision.
		self assert: aNormalPowerOfTwoFloat successor significandAsInteger equals: aNormalPowerOfTwoFloat significandAsInteger + 1.
		"The last one is not true for fminNormalized"
		aNormalPowerOfTwoFloat = Float fminNormalized or: [
			self assert: aNormalPowerOfTwoFloat predecessor significandAsInteger equals: aNormalPowerOfTwoFloat significandAsInteger * 2 - 1]].
	
	denormalPowersOfTwo do: [:aDenormalPowerOfTwoFloat |
		"The significand of a denormal power of two is a power of two, just with less bits"
		self assert: aDenormalPowerOfTwoFloat significandAsInteger isPowerOfTwo.
		self assert: aDenormalPowerOfTwoFloat significandAsInteger highBit equals: Float precision + aDenormalPowerOfTwoFloat exponent - Float emin.
		aDenormalPowerOfTwoFloat successor = Float fminNormalized or: [
			self assert: aDenormalPowerOfTwoFloat successor significandAsInteger equals: aDenormalPowerOfTwoFloat significandAsInteger + 1].
		self assert: aDenormalPowerOfTwoFloat predecessor significandAsInteger equals: aDenormalPowerOfTwoFloat significandAsInteger - 1.].
	
	"Well known value for a few extremal cases"
	self assert: Float fmax significandAsInteger equals: 1 << Float precision - 1.
	self assert: Float fmin significandAsInteger equals: 1.
	self assert: 0.0 significandAsInteger equals: 0.
"This currently is not allowed in Cuis"
false ifTrue: [
	self assert: Float infinity significandAsInteger equals: 0.
	self assert: Float nan significandAsInteger > 0
].
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testSin [

	self verify: 0.0 sin isTrigonometricallyEqualTo: 0.0.
	self verify: (Float pi / 6) sin isTrigonometricallyEqualTo: 1.0 sqrt / 2.0.
	self verify: (Float pi / 4) sin isTrigonometricallyEqualTo: 2.0 sqrt / 2.0.
	self verify: (Float pi / 3) sin isTrigonometricallyEqualTo: 3.0 sqrt / 2.0.
	self verify: (Float pi / 2) sin isTrigonometricallyEqualTo: 1.0
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testSinNonPrimitive [
	self assert: 0.0 sinNonPrimitive = 0.0.
	self assert: (45 degreesToRadians sinNonPrimitive - 0.5 sqrt) abs <= (0.5 ulp * 2).
	self assert: (90 degreesToRadians sinNonPrimitive - 1.0) abs <= (1.0 ulp * 2).
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testSinh [
	self assertIsPositiveZero: 0.0 sinh.
	self assertIsNegativeZero: Float negativeZero sinh.
	self deny: Float infinity sinh isFinite.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 sinh negated isHyperbolicallyEqualTo: 2.0 negated sinh
]

{ #category : 'tests - arithmetic' }
FloatTest >> testSmallIntegerDivide [
	"Mixed integer and float operation should not introduce loss of precision"

	self should: [Compiler evaluate: '(3 / 5.0) = (3.0 / 5.0)']
	
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testSomeNines [
	| twelveNines |
	twelveNines _ Compiler evaluate: '999999 + 0.999999'.
	self assert: twelveNines printString = '999999.999999'.
	self assert: twelveNines printString asNumber = twelveNines.
	self assert: twelveNines printString asNumber - twelveNines = 0.0.
	self assert: twelveNines + 1e-6 = 1e6
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testSqrt [
	self assert: Float nan sqrt isNaN.
	self assert: Float infinity sqrt = Float infinity.
	self assertIsPositiveZero: Float zero sqrt.
	self assertIsNegativeZero: Float negativeZero sqrt.
	self assert: Float fminNormalized sqrt squared = Float fminNormalized .
	self assert: Float fminDenormalized sqrt squared = Float fminDenormalized .
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testSqrtNonPrimitive [
	self assert: Float nan sqrtNonPrimitive isNaN.
	self assert: Float infinity sqrtNonPrimitive = Float infinity.
	self assertIsPositiveZero: Float zero sqrtNonPrimitive.
	self assertIsNegativeZero: Float negativeZero sqrtNonPrimitive.
	
	self assert: 1.0 sqrt = 1.0 sqrtNonPrimitive.
	self assert: 2.0 sqrt = 2.0 sqrtNonPrimitive.
	self assert: 4.0 sqrt = 4.0 sqrtNonPrimitive.
	self assert: 8.0 sqrt = 8.0 sqrtNonPrimitive.
	self assert: 9.0 sqrt = 9.0 sqrtNonPrimitive.
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testStoreBase16 [
	"This bug was reported in mantis http://bugs.squeak.org/view.php?id=6695"

	self
		assert: (20.0 storeStringBase: 16) = '16r14.0'
		description: 'the radix prefix should not be omitted, except in base 10'
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testStoreOn [
	"If storeOn: prints exactly and the parser avoids cumulating rounding errors,
	then the Float should be read back exactly.
	Note: there is no guarantee to restore the bit pattern of NaN though"
	
	self assert: (Compiler evaluate: Float halfPi storeString) = Float halfPi.
	self assert: (Compiler evaluate: Float halfPi negated storeString) = Float halfPi negated.
	self assert: (Compiler evaluate: Float infinity storeString) = Float infinity.
	self assert: (Compiler evaluate: Float negativeInfinity storeString) = Float negativeInfinity.
	self assert: (Compiler evaluate: Float nan storeString) isNaN.
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testStringAsFloatRoundsHalfToEven [
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Test that rounding Fraction to Float picks even mantissa if tie."

	| floatWithEvenMantissa floatWithEvenMantissa2 floatWithOddMantissa halfway1 halfway2 |
	floatWithEvenMantissa _ Float maxExactInteger * 1.23.
	self assert: floatWithEvenMantissa mantissaPart even description: 'precondition'.

	floatWithOddMantissa _ floatWithEvenMantissa successor.
	self assert: floatWithOddMantissa mantissaPart even not description: 'precondition'.

	floatWithEvenMantissa2 _ floatWithOddMantissa successor.
	self assert: floatWithEvenMantissa2 mantissaPart even description: 'precondition'.

	halfway1 _ floatWithEvenMantissa asTrueFraction + floatWithOddMantissa asTrueFraction / 2.
	self assert: halfway1 = floatWithEvenMantissa description: 'precondition-Must round to closest even mantissa'.
	self assert: halfway1 printString = '11078855083331421' description: 'precondition'.
	self assert: '11078855083331421.0' asNumber = floatWithEvenMantissa description: 'when converting string to Float, if tie, round to closest even mantissa'.

	halfway2 _ floatWithOddMantissa asTrueFraction + floatWithEvenMantissa2 asTrueFraction / 2.
	self assert: halfway2 = floatWithEvenMantissa2 description: 'precondition-Must round to closest even mantissa'.
	self assert: halfway2 printString = '11078855083331423' description: 'precondition'.
	self assert: '11078855083331423.0' asNumber = floatWithEvenMantissa2 description: 'when converting string to Float, if tie, round to closest even mantissa'.
]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testStringAsFloatRoundsHalfToEven2 [
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Test that rounding Fraction to Float picks even mantissa if tie."

	| evenMantissa oddMantissa scaledEven scaledOdd evenMantissa2 scaledEven2 |
	evenMantissa _ 1.0.
	self assert: evenMantissa mantissaPart even description: 'precondition'.
	oddMantissa _ 1.0 successor.
	self assert: oddMantissa mantissaPart even not description: 'precondition'.
	evenMantissa2 _ oddMantissa successor.
	self assert: evenMantissa2 mantissaPart even description: 'precondition'.

	scaledEven _ evenMantissa asTrueFraction * 1e53.
	scaledOdd _ oddMantissa asTrueFraction * 1e53.
	scaledEven2 _ evenMantissa2 asTrueFraction * 1e53.

	self assert: (scaledEven + scaledOdd / 2) printString = '100000000000000011102230246251565404236316680908203125' description: 'precondition'.
	self assert: '1.00000000000000011102230246251565404236316680908203124999' asNumber = evenMantissa description: 'precondition'.
	self assert: '1.00000000000000011102230246251565404236316680908203125001' asNumber = oddMantissa description: 'precondition'.
	self assert: '1.00000000000000011102230246251565404236316680908203125' asNumber = evenMantissa description: 'when converting string to Float, if tie, round to closest even mantissa'.

	self assert: (scaledEven2 + scaledOdd / 2) printString = '100000000000000033306690738754696212708950042724609375' description: 'precondition'.
	self assert: '1.00000000000000033306690738754696212708950042724609374999' asNumber = oddMantissa description: 'precondition'.
	self assert: '1.00000000000000033306690738754696212708950042724609375001' asNumber = evenMantissa2 description: 'precondition'.
	self assert: '1.00000000000000033306690738754696212708950042724609375' asNumber = evenMantissa2 description: 'when converting string to Float, if tie, round to closest even mantissa'.
]

{ #category : 'tests - conversion from to String' }
FloatTest >> testStringAsNumber [
	"This covers parsing in Number>>readFrom:"

	| aFloat |
	aFloat := '10r-12.3456' asNumber.
	self assert: -12.3456 = aFloat.
	aFloat := '10r-12.3456e2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456d2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '12.3456q2' asNumber.
	self assert: 1234.56 = aFloat.

]

{ #category : 'tests - conversion from to String' }
FloatTest >> testStringAsNumber2 [
	"Check that small but normal Floats can be created from Strings"

	| aFloat |
	aFloat _ '12345678901234567890.0e-326' asNumber.
	self deny: aFloat = 0.0.
	self assert: aFloat > 0.0.
	self assert: aFloat - 1.2345678901234568e-307 = 0.
	self assert: aFloat = 1.2345678901234568e-307.
]

{ #category : 'tests - arithmetic' }
FloatTest >> testSubtract [

	self assert: 3.0 - 1.0 = 2.0.
	self assert: Float fminNormalized - Float fminDenormalized < Float fminNormalized.
	self assert: Float fminNormalized - Float fminDenormalized - Float fminNormalized = Float fminDenormalized negated.
	self assert: 1.0 - Float zero = 1.0.
	self assert: Float zero - 1.0 = -1.0.
	self assert: 1.0 - Float negativeZero = 1.0.
	self assert: Float negativeZero - 1.0 = -1.0.
	self assert: 1.0 - Float infinity = Float negativeInfinity.
	self assert: Float infinity - 1.0 = Float infinity.
	self assert: 1.0 - Float negativeInfinity = Float infinity.
	self assert: Float negativeInfinity - 1.0 = Float negativeInfinity.
	self assert: (1.0 - Float nan) isNaN.
	self assert: (Float nan - 1.0) isNaN.
]

{ #category : 'tests - mathematical functions' }
FloatTest >> testTanh [
	self assertIsPositiveZero: 0.0 tanh.
	self assertIsNegativeZero: Float negativeZero tanh.
	self assert: Float infinity tanh = 1.0.
	self verify: 2.0 cosh squared - 2.0 sinh squared isHyperbolicallyEqualTo: 1.0.
	self verify: 2.0 tanh negated isHyperbolicallyEqualTo: 2.0 negated tanh
]

{ #category : 'tests - arithmetic' }
FloatTest >> testTimesTwoPowerGradualUnderflow [
	"Here is a vicious case where timesTwoPower is inexact because it underflows.
	And two consecutive inexact operations lead to a different result than a single one.
	Typically expressed as multiple of Float fmin in base 2,
	2r1011*Float fmin shifted by -3 with round to nearest, tie to even mode:
	-> round(1.011) -> 1.0 = fmin
	But if first shifted by -2 then by -1:
	-> round(10.11) -> 11.0 = 3*fmin
	-> round(1.1) -> 10.0 = 2*fmin
	Or first shifted by -1 then by -2:
	-> round(101.1) -> 110.0 = 6*fmin
	-> round(1.1) -> 10.0 = 2*fmin
	A naive implementation that split the shift uncarefully might fail to handle such case correctly."

	| f |
	f := 2r1011 asFloat.
	"scan the whole range of possible exponents for this significand"
	Float fmin exponent + f exponent to: Float fmax exponent - f exponent
		do:
			[ :exp |
				| g |
				g := f timesTwoPower: exp.
				(g timesTwoPower: Float fmin exponent - g exponent) = Float fmin ifFalse: [ exp print ].
				"self assert: (g timesTwoPower: Float fmin exponent - g exponent) = Float fmin"
				]

"
testTimesTwoPowerGradualUnderflow
https://github.com/OpenSmalltalk/opensmalltalk-vm/issues/383
"
]

{ #category : 'tests - arithmetic' }
FloatTest >> testTimesTwoPowerOverflow [
	self assert: (Float fminNormalized timesTwoPower: Float emax - Float emin) equals: (2.0 raisedTo: Float emax).
	self assert: (Float zero timesTwoPower: SmallInteger maxVal) equals: Float zero.
	self assert: (Float zero timesTwoPower: SmallInteger maxVal squared) equals: Float zero.
	self assert: (1.0  timesTwoPower: SmallInteger maxVal) equals: Float infinity.
	self assert: (1.0  timesTwoPower: SmallInteger maxVal squared) equals: Float infinity.
	self assert: (-1.0  timesTwoPower: SmallInteger maxVal) equals: Float negativeInfinity.
	self assert: (-1.0  timesTwoPower: SmallInteger maxVal squared) equals: Float negativeInfinity.
]

{ #category : 'tests - arithmetic' }
FloatTest >> testTimesTwoPowerUnderflow [
	self assert: ((2.0 raisedTo: Float emax) timesTwoPower: Float emin - Float emax) equals: Float fminNormalized.
	self assert: (Float infinity timesTwoPower: SmallInteger minVal * SmallInteger maxVal) equals: Float infinity.
	self assertIsPositiveZero: (1.0  timesTwoPower: SmallInteger maxVal negated).
	self assertIsPositiveZero: (1.0  timesTwoPower: SmallInteger maxVal squared negated).
	self assertIsNegativeZero: (-1.0  timesTwoPower: SmallInteger maxVal negated).
	self assertIsNegativeZero: (-1.0  timesTwoPower: SmallInteger maxVal squared negated).

]

{ #category : 'tests - conversion and rounding' }
FloatTest >> testTruncated [
	self assert: 1.0 truncated = 1.
	self assert: 1.0 successor truncated = 1.
	self assert: 1.0 predecessor truncated = 0.
	self assert: 1.1 truncated = 1.
	self assert: -2.0 truncated = -2.
	self assert: -2.1 truncated = -2.
]

{ #category : 'tests - precision and extreme values' }
FloatTest >> testUlp [

	{1.0 predecessor. 1.0 successor. Float pi predecessor. Float pi. Float pi successor} do:
		[:f |
		self assert: (f * 2) ulp = (f ulp * 2).
		self assert: (f / 2) ulp = (f ulp / 2).
		self deny: f + f ulp = f.
		self deny: f - f ulp = f.
		"Tests below are valid as long as default rounding mode (to nearest even) is used"
		self assert: f significandAsInteger odd not | (f ulp / 2.0 + f = f successor).
		self assert: f significandAsInteger even not | (f ulp / 2.0 + f = f)].
	
	self assert: 1.0 ulp = 0.9 ulp.
	self deny: 1.0 ulp = 1.1 ulp.
	
	self assert: 0.0 ulp = Float fmin.
	self assert: 1.01  ulp = Float epsilon.
	self assert: Float nan ulp isNaN.
	self assert: Float infinity ulp = Float infinity.
	self assert: Float negativeInfinity ulp = Float infinity.

	self assert: ((0 to: Float precision - 1) allSatisfy: [:each | (Float fmin timesTwoPower: each) ulp = Float fmin]).	
]

{ #category : 'tests - zero behavior' }
FloatTest >> testZero1 [
	"FloatTest new testZero1"

	self assert: Float negativeZero = 0 asFloat.
	self assert: (Float negativeZero at: 1) ~= (0 asFloat at: 1).

	"The negative zero has a bit representation that is different from the bit representation of the positive zero. Nevertheless, both values are defined to be equal."

]

{ #category : 'tests - zero behavior' }
FloatTest >> testZeroAbs [
	self assertIsPositiveZero: Float zero abs. 'the absolute value of a positive zero is zero'.
	self assertIsPositiveZero: Float negativeZero abs. 'the absolute value of a negative zero is zero'.
]

{ #category : 'tests - arithmetic' }
FloatTest >> testZeroDividedByZero [

	self assert: (0.0 / 0.0) isNaN.
	self assert: (0.0 / -0.0) isNaN.
	self assert: (-0.0 / 0.0) isNaN.
	self assert: (-0.0 / -0.0) isNaN.
]

{ #category : 'tests - zero behavior' }
FloatTest >> testZeroRaisedToNegativePower [
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"

	self should: [0.0 raisedToInteger: -1] raise: ZeroDivide.
	self assert: (0.0 raisedToInteger: -1) isExactly: Float infinity.
	self should: [0.0 raisedTo: -1] raise: ZeroDivide.
	self assert: (0.0 raisedTo: -1) isExactly: Float infinity.
	self should: [0.0 raisedTo: -1.0] raise: ZeroDivide.
	self assert: (0.0 raisedTo: -1.0) isExactly: Float infinity.

	self should: [-0.0 raisedToInteger: -1] raise: ZeroDivide.
	self assert: (-0.0 raisedToInteger: -1) isExactly: Float negativeInfinity.
	self should: [-0.0 raisedTo: -1] raise: ZeroDivide.
	self assert: (-0.0 raisedTo: -1) isExactly: Float negativeInfinity.
	self should: [-0.0 raisedTo: -1.0] raise: ZeroDivide.
	self assert: (-0.0 raisedTo: -1.0) isExactly: Float negativeInfinity.
]

{ #category : 'tests - zero behavior' }
FloatTest >> testZeroSign [
	self assert: Float zero sign = 0.
	self assert: Float negativeZero sign = -1 description: 'negative zero is a zero with non zero sign!'
]

{ #category : 'tests - zero behavior' }
FloatTest >> testZeroSignificandAsInteger [
	"This is about http://bugs.squeak.org/view.php?id=6990"

	self assert: 0.0 significandAsInteger = 0.
	self assert: -0.0 significandAsInteger = 0
]
