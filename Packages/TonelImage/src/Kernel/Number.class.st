"
Class Number holds the most general methods for dealing with numbers. Subclasses Float, Fraction, and Integer, and their subclasses, provide concrete representations of a numeric quantity.

All of Number's subclasses participate in a simple type coercion mechanism that supports mixed-mode arithmetic and comparisons.  It works as follows:  If
	self<typeA> op: arg<typeB>
fails because of incompatible types, then it is retried in the following guise:
	(arg adaptTypeA: self) op: arg adaptToTypeA.
This gives the arg of typeB an opportunity to resolve the incompatibility, knowing exactly what two types are involved.  If self is more general, then arg will be converted, and viceVersa.  This mechanism is extensible to any new number classes that one might wish to add to Squeak.  The only requirement is that every subclass of Number must support a pair of conversion methods specific to each of the other subclasses of Number.


Note:
To write an hex number, in Cuis you can do 16rFE and not 16rfe. WRT some proposal to change this, Dan Ingalls said: at http://lists.squeakfoundation.org/pipermail/squeak-dev/2000-March/013368.html :

I will have to admit to a bit of bigotry here.  I was the originator of the numeric format, and I object to a couple of the ANSI decisions regarding the syntax of numeric constants.  I have three reasons for this...

1.  I much prefer 2r1e31 to 2r10000000000000000000000000000000 for the sign bit and 16rFe8 for the high nibble of a 32-bit integer and 10e6 for a million.  Come on. (jmv note: it is actually 16rFe7 hex and 10e5)

2.  I just feel that the current (Squeak) def, which is the original one, is a micro world of simplicity and generality.  Radix, digits and exponent defined analogously over ints, largeInts, and Floats.  That's all.

3.  I have spent entire hours with inquisitive kids, just exploring this micro world.  If they get bored with 2r1e10 then it's time to step out and explore the meaning of 3r11.2.

If I were going to "fix" anything, I think it would be to allow 1r111111111111 = 12.

This may be the issue that finally leads to a parting of the ways in the Squeak Community.

Just Kidding

	- Dan

Note: Cuis does support the Unary numeric system. So
		1r111111111111 = 12
		1r1 = 1
		1r11 = 2
		1r-11 = -2
		1r111 = 3
		1r = 0
See See http://en.wikipedia.org/wiki/Unary_numeral_system and #readBaseOneFrom:
"
Class {
	#name : 'Number',
	#superclass : 'Magnitude',
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'constants' }
Number class >> one [

	^1
]

{ #category : 'instance creation' }
Number class >> readFrom: stringOrStream  [
	"Answer a number as described on aStream.  The number may
	include a leading radix specification, as in 16rFADE"
	| value base aStream sign |
	aStream := stringOrStream isString
		ifTrue: [stringOrStream readStream]
		ifFalse: [stringOrStream].
	(aStream nextMatchAll: 'NaN') ifTrue: [^ Float nan].
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [aStream peekFor: $+. 1].
	(aStream nextMatchAll: 'Infinity') ifTrue: [^ Float infinity * sign].
	base := 10.
	aStream peek == $.
		ifTrue: [ value := 0 ]
		ifFalse: [
			value := Integer readFrom: aStream base: base.
			(aStream peekFor: $r)
				ifTrue: [
					"<base>r<integer>"
					(base := value) < 2 ifTrue: [
						base = 1 ifTrue: [ ^Integer readBaseOneFrom: aStream ].
						^self error: 'Invalid radix'].
					(aStream peekFor: $-) ifTrue: [sign := sign negated].
					value := Integer readFrom: aStream base: base].
			(aStream peekFor: $x)
				ifTrue: [
					"0x<integer>" "Hexadecimal"
					base := 16.
					(aStream peekFor: $-) ifTrue: [sign := sign negated].
					value := Integer readFrom: aStream base: base]].
	^ self readRemainderOf: value from: aStream base: base withSign: sign.
]

{ #category : 'instance creation' }
Number class >> readRemainderOf: integerPart from: aStream base: base withSign: sign [
	"Read optional fractional part and exponent, and return the final result"
	| value fraction fracpos peekChar exp scale convertToFloat |

	convertToFloat := false.
	value := integerPart.
	(aStream peekFor: $.)
		ifTrue: [		 "<integer>.<fraction>"
			(aStream atEnd not and: [ aStream peek digitValue between: 0 and: base - 1 ])
				ifTrue:  [
					fracpos := aStream position.
					fraction := Integer readFrom: aStream base: base.
					fraction := fraction / (base raisedToInteger: aStream position - fracpos).
					value := value + fraction.
					convertToFloat := true ]
				ifFalse: [
					"oops - just <integer>."
					aStream skipBack.
					"un-gobble the period"
					^ value * sign"Number readFrom: '3r-22.2'"]].
	peekChar := aStream peek.
	('deqp' includes: peekChar)
		ifTrue: [			"<number>(e|d|q)<exponent>>" "<number>(p)<binaryExponent>>"
			aStream next.
			(aStream atEnd not and: [ (aStream peek digitValue between: 0 and: 9) or: [ '+-' includes: aStream peek ]])
				ifTrue:  [
					exp := Integer readFrom: aStream.
					scale := (peekChar == $p ifTrue: [2] ifFalse: [base]) raisedToInteger: exp.
					value := value * scale ]
				ifFalse: [
					"oops - just <number>."
					aStream skipBack.]].
	^convertToFloat
		ifTrue: [
			(value = 0.0 and: [ sign = -1 ])
				ifTrue: [ Float negativeZero ]
				ifFalse: [ (value * sign) asFloat ]]
		ifFalse: [ value * sign ]
]

{ #category : 'constants' }
Number class >> zero [

	^0
]

{ #category : 'comparing' }
Number >> % another [
	"Answer the relative distance between two number"
	^ (self - another) abs / (self abs + another abs / 2)
]

{ #category : 'arithmetic' }
Number >> * aNumber  [
	"Answer the result of multiplying the receiver by aNumber."
	self subclassResponsibility
]

{ #category : 'arithmetic' }
Number >> + aNumber  [
	"Answer the sum of the receiver and aNumber."
	self subclassResponsibility
]

{ #category : 'arithmetic' }
Number >> - aNumber  [
	"Answer the difference between the receiver and aNumber."
	self subclassResponsibility
]

{ #category : 'arithmetic' }
Number >> / aNumber  [
	"Answer the result of dividing receiver by aNumber."
	self subclassResponsibility
]

{ #category : 'arithmetic' }
Number >> // aNumber [
	"Integer division with truncation toward negative infinity. 
	9//4 = 2
	-9//4 = -3
	-0.9//0.4 = -3
	#\\ answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) floor
]

{ #category : 'comparing' }
Number >> = aNumber [
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility
]

{ #category : 'converting' }
Number >> @ y  [
	"Answer a new Point whose x value is the receiver and whose y value is the 
	argument.  Optional.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 18>
	^Point x: self y: y
]

{ #category : 'arithmetic' }
Number >> \\ divisor [
	"Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infinity, a.k.a Knuth's division)
	Answer a Number with the same sign as divisor. 
	9\\4 = 1
	-9\\4 = 3
	9\\ -4 = -3
	0.9\\0.4 = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - (self // divisor * divisor)

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d := 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d := -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"
]

{ #category : 'arithmetic' }
Number >> ^ another [
	"Covenient, usual idiom.
	2 ^ 8
	"
	^ self raisedTo: another
]

{ #category : 'arithmetic' }
Number >> abs [
	"Answer a Number that is the absolute value (positive magnitude) of the receiver."

	self < 0
		ifTrue: [^self negated]
		ifFalse: [^self]
]

{ #category : 'converting' }
Number >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]
]

{ #category : 'converting' }
Number >> adaptToFloat: rcvr andSend: selector  [
	"If I am involved in arithmetic with a Float, convert me to a Float."
	^ rcvr perform: selector with: self asFloat
]

{ #category : 'converting' }
Number >> adaptToFraction: rcvr andSend: selector [
	"If I am involved in arithmetic with a Fraction, convert us and evaluate exprBlock."
	^ self subclassResponsibility
]

{ #category : 'converting' }
Number >> adaptToInteger: rcvr andSend: selector [
	"If I am involved in arithmetic with a Integer, convert us and evaluate exprBlock."
	^ self subclassResponsibility
]

{ #category : 'converting' }
Number >> adaptToPoint: rcvr andSend: selector [
	"If I am involved in arithmetic with a Point, convert me to a Point."
	^ rcvr perform: selector with: self@self
]

{ #category : 'mathematical functions' }
Number >> arCosh [
	"Answer receiver's area hyperbolic cosine.
	That is the inverse function of cosh."

	^self asFloat arCosh
]

{ #category : 'mathematical functions' }
Number >> arSinh [
	"Answer receiver's area hyperbolic sine.
	That is the inverse function of sinh."

	^self asFloat arSinh
]

{ #category : 'mathematical functions' }
Number >> arTanh [
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	^self asFloat arTanh
]

{ #category : 'mathematical functions' }
Number >> arcCos  [
	"The receiver is the cosine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcCos
]

{ #category : 'mathematical functions' }
Number >> arcSin [
	"The receiver is the sine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcSin
]

{ #category : 'mathematical functions' }
Number >> arcTan [
	"The receiver is the tangent of an angle. Answer the angle measured in  radians.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	^self asFloat arcTan
]

{ #category : 'mathematical functions' }
Number >> arcTan: denominator [
	"The receiver is the tangent of an angle. Answer the angle measured in 
	radians."

	^(self asFloat) arcTan: denominator.
]

{ #category : 'converting' }
Number >> asFloat [
	^ self subclassResponsibility.
]

{ #category : 'converting' }
Number >> asInteger [
	"Answer an integer nearest the receiver toward zero."
	^self truncated
]

{ #category : 'converting' }
Number >> asIntegerOrFloat [
	"Convert to integer if integer, or to Float otherwhise."

	^self asFloat
]

{ #category : 'converting' }
Number >> asNumber [
	^ self
]

{ #category : 'converting' }
Number >> asPoint [
	"Answer a new Point with the receiver as both coordinates; 
	often used to supply the same value in two dimensions, as with
	symmetrical gridding or scaling."

	^self @ self
]

{ #category : 'converting' }
Number >> asSmallAngleDegrees [
	"Return the receiver normalized to lie within the range (-180, 180)"

	| pos |
	pos := self \\ 360.
	pos > 180 ifTrue: [pos := pos - 360].
	^ pos

"#(-500 -300 -150 -5 0 5 150 300 500 1200) collect: [:n | n asSmallAngleDegrees]"
]

{ #category : 'converting' }
Number >> asSmallPositiveDegrees [
	"Return the receiver normalized to lie within the range (0, 360)"

	^self \\ 360
]

{ #category : 'truncation and round off' }
Number >> ceiling [
	"Answer the integer nearest the receiver toward  infinity."

	| truncation |
	truncation := self truncated.
	self <= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation + 1]
]

{ #category : 'mathematical functions' }
Number >> copySignTo: aNumber [
	"Return a number with same magnitude as aNumber and same sign as self."

	^ self positive
		ifTrue: [aNumber abs]
		ifFalse: [aNumber withNegativeSign].
]

{ #category : 'mathematical functions' }
Number >> cos [
	"The receiver represents an angle measured in radians. Answer its cosine.
	See comment at Fraction >> #moduloTwoPiAsFloat"

	^self moduloTwoPiAsFloat cos
]

{ #category : 'mathematical functions' }
Number >> cosh [
	"Answer receivers hyperbolic cosine."
	
	^self asFloat cosh
]

{ #category : 'mathematical functions' }
Number >> cubed [
	"Answer the receiver multipled by itself."

	^self * self * self
]

{ #category : 'converting' }
Number >> days [

	^ Duration days: self
]

{ #category : 'mathematical functions' }
Number >> degreeCos [
	"Answer the cosine of the receiver taken as an angle in degrees."
	
	^ (90 - (180 + self \\ 360 - 180) abs) degreesToRadians sin
]

{ #category : 'mathematical functions' }
Number >> degreeSin [
	"Answer the sine of the receiver taken as an angle in degrees."
	
	^(90 - self) degreeCos
]

{ #category : 'converting' }
Number >> degreesToRadians [
	"The receiver is assumed to represent degrees.  Answer the
	conversion to radians."
	^self asFloat degreesToRadians
]

{ #category : 'converting' }
Number >> degreesToRadiansMinutes: minutes seconds: seconds [
	" deg degreesToRadiansWithMinutes: min seconds: secs
	Take receiver and arguments as degrees, minutes and seconds. Answer radians"

	^(seconds / 60.0 + minutes / 60.0 + self) degreesToRadians
]

{ #category : 'truncation and round off' }
Number >> detentBy: detent atMultiplesOf: grid snap: snap [
	"Map all values that are within detent/2 of any multiple of grid to that multiple.  Otherwise, if snap is true, return self, meaning that the values in the dead zone will never be returned.  If snap is false, then expand the range between dead zones so that it covers the range between multiples of the grid, and scale the value by that factor."
	| r1 r2 |
	r1 := self roundTo: grid.  "Nearest multiple of grid"
	(self - r1) abs <= detent ifTrue: [^ r1].  "Snap to that multiple..."
	snap ifTrue: [^ self].  "...or return self"

	r2 := self < r1  "Nearest end of dead zone"
		ifTrue: [r1 - (detent asFloat/2)]
		ifFalse: [r1 + (detent asFloat/2)].
	"Scale values between dead zones to fill range between multiples"
	^ r1 + ((self - r2) * grid asFloat / (grid - detent))
"
	(170 to: 190 by: 2) collect: [:a | a detentBy: 10 atMultiplesOf: 90 snap: true] 	(170 to: 190 by: 2) collect: [:a | a detentBy: 10 atMultiplesOf: 90 snap: false]
	(3.9 to: 4.1 by: 0.02) collect: [:a | a detentBy: 0.1 atMultiplesOf: 1.0 snap: true] 	(-3.9 to: -4.1 by: -0.02) collect: [:a | a detentBy: 0.1 atMultiplesOf: 1.0 snap: false]
"
]

{ #category : 'arithmetic' }
Number >> div: aNumber [
	"Integer division with non-negative remainder.
	(9 div:4) = 2
	(-9 div: 4) = -3
	(-0.9 div: 0.4) = -3
	#mod: answers the remainder from this division. See comments and examples there.
	See #//, #quo:, #div:"
	"Answer an integer q such that: 
		for some r, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	aNumber positive ifTrue: [ ^self // aNumber ].
	^ (self // aNumber abs) negated
]

{ #category : 'testing' }
Number >> even [
	"Answer whether the receiver is an even number."

	^self \\ 2 = 0
]

{ #category : 'mathematical functions' }
Number >> exp [
	"Answer the exponential of the receiver as a floating point number."
	^self asFloat exp
]

{ #category : 'truncation and round off' }
Number >> floor [
	"Answer the integer nearest the receiver toward negative infinity."

	| truncation |
	truncation := self truncated.
	self >= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation - 1]
]

{ #category : 'mathematical functions' }
Number >> floorLog: radix [
	"Answer the floor of the log base radix of the receiver."

	^(self log: radix) floor
]

{ #category : 'truncation and round off' }
Number >> fractionPart [
	
	"Added for ANSI compatibility"
	
	^self - self integerPart
]

{ #category : 'comparing' }
Number >> hash [
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility
]

{ #category : 'converting' }
Number >> hours [

	^ Duration hours: self
]

{ #category : 'testing' }
Number >> ifNotZero: aBlock [
	"
	Useful for workarounding division by zero
	#(1.0 2.0 0.0) collect: [ :k | k ifNotZero: [100.0/k]]
	"
	^ self isZero ifFalse: aBlock ifTrue: [self]
]

{ #category : 'converting' }
Number >> inMinusPiToPiRange [
	"Avoid conversion to Float if already ok"
	(self > Float pi negated and: [self <= Float pi]) ifTrue: [
		^ self ].
	^ self moduloTwoPiAsFloat inMinusPiToPiRange
]

{ #category : 'truncation and round off' }
Number >> integerPart [
	"Added for ANSI compatibility"
	^self truncated
]

{ #category : 'mathematical functions' }
Number >> interpolateTo: aNumber at: param [
	^self + (aNumber - self * param)
]

{ #category : 'testing' }
Number >> is: aSymbol [
	"Note: Senders might prefer #isNumber for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Number = aSymbol or: [ super is: aSymbol]
]

{ #category : 'testing' }
Number >> isDivisibleBy: aNumber [
	aNumber = 0 ifTrue: [^ false].
	aNumber isInteger ifFalse: [^ false].
	^ (self \\ aNumber) = 0
]

{ #category : 'testing' }
Number >> isFinite [
	^ true
]

{ #category : 'testing' }
Number >> isInfinite [

	^ false
]

{ #category : 'testing' }
Number >> isNaN [
	^ false
]

{ #category : 'testing' }
Number >> isNumber [
	^ true
]

{ #category : 'printing' }
Number >> isOrAreStringWith: aNoun [
	| result |
	result := self = 1
		ifTrue:
			[' is one ']
		ifFalse:
			[self = 0
				ifTrue:
					[' are no ']
				ifFalse:
					[' are ', self printString, ' ']].
	result := result, aNoun.
	self = 1 ifFalse: [result := result, 's'].
	^ result

"
#(0 1 2 98.6) do:
	[:num | Transcript newLine; show: 'There', (num isOrAreStringWith: 'way'), ' to skin a cat'].
"
]

{ #category : 'testing' }
Number >> isZero [
	^self = 0
]

{ #category : 'mathematical functions' }
Number >> lg [
	"Answer the base 2 log of the receiver.
	Alternative notation. See https://en.wikipedia.org/wiki/Binary_logarithm"

	^self log2
]

{ #category : 'mathematical functions' }
Number >> ln [
	"Answer the natural log of the receiver.
	See comment at #log:"

	self <= 0 ifTrue: [DomainError signal: 'ln is only defined for x > 0'].
	^self asFloat ln
]

{ #category : 'mathematical functions' }
Number >> log [
	"Answer the base-10 log of the receiver.
	See comment at #log:"

	^self asFloat log
]

{ #category : 'mathematical functions' }
Number >> log: aNumber [
	"Answer the log base aNumber of the receiver.
	Note 1:
		Answer an Integer number if receiver and base are both integer (receiver might also be a fraction of the form 1/Integer), and the correct answer is integer too.
	Note2:
		There are several implementors of #log:, #log, #log2 and #ln in the Number hierarchy. Behavior is not consistent when receiver is a negative value. Some implementors answer Float nan, others raise a DomainError. Another possibility would be to answer the correct Complex number.
		It would be good to pick one and make it consistently followed.
	"

	(self <= 0 and: [ aNumber isFloat not])
		ifTrue: [DomainError signal: 'logarithm is only defined for x > 0'].
	aNumber = 2 ifTrue: [^self log2].
	aNumber isPowerOfTwo ifTrue: [^self log2 / aNumber log2].
	^self asFloat log: aNumber
]

{ #category : 'mathematical functions' }
Number >> log2 [
	"Answer the base-2 logarithm of the receiver.
	See comment at #log:"

	self <= 0 ifTrue: [DomainError signal: 'log2 is only defined for x > 0'].
	^self asFloat log2
]

{ #category : 'mathematical functions' }
Number >> magnitude [
	"Compatibility with Point and Complex numbers"
	^self abs
]

{ #category : 'converting' }
Number >> milliSeconds [

	^ Duration milliSeconds: self

]

{ #category : 'converting' }
Number >> minutes [

	^ Duration minutes: self
]

{ #category : 'arithmetic' }
Number >> mod: divisor [
	"Modulo operation. Remainder of the integer division #div: (Euclidean division)
	Answer a Number that is never negative (it is positive or zero). 
	(9 mod: 4) = 1
	(-9 mod: 4) = 3
	(9 mod: -4) = 1
	(0.9 mod: 0.4) = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	And 'The Euclidean Definition of the Functions div and mod' by Raymond T. Boute, https://core.ac.uk/download/pdf/55698442.pdf
	"
	"Answer r such that: 
		for some integer q, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	^self \\ divisor abs

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d := 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: d ] color: Color green.
	g addFunction: [ :x | x div: d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d := -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: d ] color: Color green.
	g addFunction: [ :x | x div: d ] color: Color red.
	g openInWorld'
"

]

{ #category : 'converting' }
Number >> moduloTwoPiAsFloat [
	"Answer the remainder of the division by 2 * pi, as a Floating Point number.
	Provides full Float precision of the remainder result, even for huge arguments.
	See comment at Fraction.
	See senders.
	Note: Not to be implemented in the Float hierarchy. Only for exact number representations.
	If receiver is ever a Float, sender should be using a Float specific operation."

	^self subclassResponsibility
]

{ #category : 'converting' }
Number >> nanoSeconds [

	^ Duration nanoSeconds: self.
]

{ #category : 'arithmetic' }
Number >> negated [
	"Answer a Number that is the negation of the receiver."
	^0 - self
]

{ #category : 'testing' }
Number >> negative [
	"Answer whether the receiver is mathematically negative."

	^ self < 0
]

{ #category : 'mathematical functions' }
Number >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."

	self subclassResponsibility
]

{ #category : 'testing' }
Number >> odd [
	"Answer whether the receiver is an odd number."
	^self even == false
]

{ #category : 'testing' }
Number >> positive [
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ self >= 0
]

{ #category : 'printing' }
Number >> printOn: aStream [
	self printOn: aStream base: 10
]

{ #category : 'printing' }
Number >> printOn: aStream base: base [
	"This method should print a representation of the number for the given base,
	excluding the base prefix (and the letter r for radix)"
	
	^self subclassResponsibility
]

{ #category : 'printing' }
Number >> printOn: aStream fractionDigits: placesDesired [
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator."

	| rounder rounded roundedFractionPart |
	placesDesired > 0 ifFalse: [ ^ self rounded printOn: aStream ].
	rounder := 10 raisedToInteger: placesDesired.
	rounded := self roundTo: rounder reciprocal.
	rounded negative ifTrue: [ aStream nextPut: $- ].
	rounded := rounded abs.
	rounded integerPart truncated printOn: aStream.
	aStream nextPut: $..
	roundedFractionPart := (rounded fractionPart * rounder) truncated.
	roundedFractionPart
		printOn: aStream
		base: 10
		length: placesDesired
		padded: true
]

{ #category : 'printing' }
Number >> printOn: aStream integerDigits: placesLeftOfFractionPoint fractionDigits: placesRightOfFractionPoint [
	"placesLeftOfFractionPoint is the minimum to be used (use more if required)
	placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate."
	"
	String streamContents: [ :strm | 23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | 123456.23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float pi printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float nan printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float infinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float negativeInfinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float zero printOn: strm integerDigits: 3 fractionDigits: 5 ]
	"

	^self printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: nil fractionDigits: placesRightOfFractionPoint positiveIndicator: nil
]

{ #category : 'printing' }
Number >> printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: leftPaddingCharOrNil fractionDigits: placesRightOfFractionPoint positiveIndicator: aCharacterOrNil [
	"placesLeftOfFractionPoint is the minimum to be used (use more if required)
	placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate."
	"
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+ ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: nil ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: $  fractionDigits: 5 positiveIndicator: Character space ]
	String streamContents: [ :strm | Float nan printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+ ]
	String streamContents: [ :strm | Float infinity printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+  ]
	String streamContents: [ :strm | Float negativeInfinity printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: nil ]
	String streamContents: [ :strm | Float zero printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+  ]
	"

	| withoutSignAndPad zeroPad |
	withoutSignAndPad := String streamContents: [ :strm |
		self abs printOn: strm fractionDigits: placesRightOfFractionPoint ].
	self < 0
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ aCharacterOrNil ifNotNil: [ aStream nextPut: aCharacterOrNil ]].
	zeroPad := placesLeftOfFractionPoint - ((withoutSignAndPad indexOf: $. startingAt: 1 ifAbsent: [withoutSignAndPad size + 1]) - 1).
	zeroPad > 0 ifTrue: [
		zeroPad timesRepeat: [ aStream nextPut: (leftPaddingCharOrNil ifNil: [$0]) ]].
	aStream nextPutAll: withoutSignAndPad
]

{ #category : 'printing' }
Number >> printString [

	^String streamContents: [:str | self printOn: str base: 10]
]

{ #category : 'printing' }
Number >> printStringBase: base [
	^ String streamContents:
		[:strm | self printOn: strm base: base]
]

{ #category : 'printing' }
Number >> printStringFractionDigits: placesDesired [
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator.
	Float pi printStringFractionDigits: 4
	"
	^ String streamContents: [ :strm | self printOn: strm fractionDigits: placesDesired ]
]

{ #category : 'arithmetic' }
Number >> quo: aNumber [
	"Integer division with truncation towards zero.
	(-9 quo: 4) = -2
	(-0.9 quo: 0.4) = -2
	#rem: answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) truncated
]

{ #category : 'converting' }
Number >> radiansToDegrees [
	"The receiver is assumed to represent radians.  Answer the
	conversion to degrees."
	^self asFloat radiansToDegrees
]

{ #category : 'mathematical functions' }
Number >> raisedTo: exponent [
	"Answer the receiver raised to aNumber."

	exponent isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: exponent].
	exponent isFraction ifTrue: [
		"Special case for fraction power by Nicolas Cellier:
		If aNumber is a fraction, but result must be a Float, learn it as quickly as possible, and give quick Float answer
		Allows evaluating:
		(2009/2000) raisedTo: (3958333/100000)
		"
		^ self raisedToFraction: exponent].
	self < 0 ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedTo: argument: exponent].
	0.0 = exponent ifTrue: [^ self class one]. 	"Special case of exponent=0"
	1.0 = exponent ifTrue: [^ self].					"Special case of exponent=1"
	0 = self ifTrue: [									"Special case of self = 0"
		exponent isNaN ifTrue: [ ^exponent ].
		^exponent < 0
			ifTrue: [(self raisedTo: exponent negated) reciprocal]
			ifFalse: [self]].
	^ (exponent * self ln) exp							"Otherwise use logarithms"
]

{ #category : 'mathematical functions' }
Number >> raisedToFraction: aFraction [
	"Raise an exception if argument is not a Fraction"
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	self isZero ifTrue: [
		aFraction negative ifTrue: [^ (self raisedToFraction: aFraction negated) reciprocal].
		^ self ].
	self negative ifFalse: [
		^ (self ln * aFraction) exp ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	^ (self negated ln * aFraction) exp negated
]

{ #category : 'mathematical functions' }
Number >> raisedToInteger: exponent [
	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	Maybe further discussion is required on this topic."
	
	| bitProbe result |

	exponent negative ifTrue: [^self raisedToNegativeInteger: exponent ].
	exponent = 0 ifTrue: [^ self class one].
	exponent = 1 ifTrue: [^ self].

	bitProbe := 1 bitShift: exponent highBit - 1.
 	result := self class one.
 	[
		(exponent bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
		bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
			whileTrue: [
				result := result * result].
	^result
]

{ #category : 'mathematical functions' }
Number >> raisedToNegativeInteger: negativeExponent [
	
	| firstTry positiveExponent exponent1 exponent2 |

	positiveExponent := negativeExponent negated.
	firstTry := self raisedToInteger: positiveExponent.
	^firstTry isInfinite
		ifFalse: [firstTry reciprocal]
		ifTrue: [
			exponent1 := positiveExponent // 2.
			exponent2 := positiveExponent - exponent1.
			(self raisedToInteger: exponent1) reciprocal * (self raisedToInteger: exponent2) reciprocal ]
]

{ #category : 'arithmetic' }
Number >> reciprocal [
	"Returns the reciprocal of self.
	In case self is 0 the / signals ZeroDivide"
	
	^1 / self
]

{ #category : 'arithmetic' }
Number >> rem:  divisor [
	"Modulo operation. Remainder of the integer division #quo: (division with truncation towards zero).
	Answer a Number with the same  sign as dividend (i.e.,  self).
	(9 rem: 4) = 1.
	(-9 rem: 4) = -1.
	(0.9 rem: 0.4) = 0.1.
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - ((self quo: divisor) * divisor)

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d := 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x rem: d ] color: Color green.
	g addFunction: [ :x | x quo: d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d := -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x rem: d ] color: Color green.
	g addFunction: [ :x | x quo: d ] color: Color red.
	g openInWorld'
"
]

{ #category : 'truncation and round off' }
Number >> round4perMagnitudeOrder [
	"Round receiver to 1 or two significant digits.
	Answer is 1, 2, 2.5, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, etc.
	better name?"

	| excess firstDigitPosition |
	firstDigitPosition := self log floor.
	excess := self log - firstDigitPosition.
	excess < 2 log ifTrue: [ ^10 raisedTo: firstDigitPosition ].
	excess < 2.5 log ifTrue: [ ^(10 raisedTo: firstDigitPosition) * 2 ].
	excess < 5 log ifTrue: [ ^(10 raisedTo: firstDigitPosition-1) * 25 ].
	^(10 raisedTo: firstDigitPosition) * 5
]

{ #category : 'truncation and round off' }
Number >> roundDownTo: aNumber  [
	"Answer the next multiple of aNumber toward negative infinity that is nearest the 
	receiver."
 	"Examples:
		3.1479 roundDownTo: 0.01 -> 3.14
		3.1479 roundDownTo: 0.1 -> 3.1
		1923 roundDownTo: 10 -> 1920
		3.1479 roundDownTo: 0.005 -> 3.145
		-3.1479 roundDownTo: 0.01 -> -3.15"
		
	^(self/aNumber) floor * aNumber
]

{ #category : 'truncation and round off' }
Number >> roundTo: quantum [
	"Answer the nearest number that is a multiple of quantum.

	Note: Please use this method when you actually want a numeric result.
	If what you need is a string representation with certain precision,
	consider using #printOn:fractionDigits: or some other method in the 'printing' category.

	Note: You may get surprising results if the desired quantum is not representable as a
	Floating Point number. Instead of getting the closest Float to the correct answer, you'll likely get
	an answer that is a few Floats away. 	For instance, compare:
		4.808 roundTo: 1/100
		(4.808 roundTo: 1/100) asFloat
		4.808 roundTo: 0.01
	"

	^ (self / quantum) rounded * quantum
]

{ #category : 'truncation and round off' }
Number >> roundUpTo: aNumber  [
	"Answer the next multiple of aNumber toward infinity that is nearest the 
	receiver."
 	"Examples:
		3.1479 roundUpTo: 0.01 -> 3.15
		3.1479 roundUpTo: 0.1 -> 3.2
		1923 roundUpTo: 10 -> 1930
		3.1479 roundUpTo: 0.005 -> 3.15
		-3.1479 roundUpTo: 0.01 -> -3.14"

	^(self/aNumber) ceiling * aNumber
]

{ #category : 'truncation and round off' }
Number >> rounded [
	"Answer the integer nearest the receiver.
	There are two rounding strategies available in Cuis:
	
	#roundedHAFZ: What most people assume as the standard way of rounding
	#roundedHTE: A more careful strategy. Default rounding used in IEEE 754 binary Floating Point
	
	Note: Feel free to redefine this method to use a different rounding mode.
	"

	^self roundedHTE
]

{ #category : 'truncation and round off' }
Number >> roundedHAFZ [
	"Answer the integer nearest the receiver.
	Use the rounding rule commonly taught in school and most familiar to most people.
		(-3/2) roundedHAFZ. -2 .
		(-5/2) roundedHAFZ. -3 .
		(3/2) roundedHAFZ. 2 .
		(5/2) roundedHAFZ. 3 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero
	See #rounded"

	^(self + (self sign / 2)) truncated.
]

{ #category : 'truncation and round off' }
Number >> roundedHTE [
	"Answer the integer nearest the receiver.
		(-3/2) roundedHTE. -2 .
		(-5/2) roundedHTE. -2 .
		(3/2) roundedHTE. 2 .
		(5/2) roundedHTE. 2 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	
	See more detailed comment at Float >> #roundedHTE"

	| truncated fractionPartAbs |
	truncated := self truncated.
	fractionPartAbs := (self-truncated) abs.
	fractionPartAbs = `1/2`
		ifTrue: [ truncated even ifTrue: [^truncated] ifFalse: [^truncated + self sign]].
	fractionPartAbs < `1/2`
		ifTrue: [^ truncated]
		ifFalse: [^ truncated + self sign]
]

{ #category : 'truncation and round off' }
Number >> roundedToEven [
	"Convenience"
	^self roundTo: 2
]

{ #category : 'converting' }
Number >> seconds [

	^ Duration seconds: self
]

{ #category : 'testing' }
Number >> sign [
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0
]

{ #category : 'mathematical functions' }
Number >> sign: aNumber [
	"Return a Number with the same sign as aNumber and same magnitude as self."

	^ aNumber copySignTo: self
]

{ #category : 'mathematical functions' }
Number >> sin [
	"The receiver represents an angle measured in radians. Answer its sine.
	See comment at Fraction >> #moduloTwoPiAsFloat"

	^self moduloTwoPiAsFloat sin
]

{ #category : 'mathematical functions' }
Number >> sinh [
	"Answer receivers hyperbolic sine"
	
	^self asFloat sinh
]

{ #category : 'mathematical functions' }
Number >> sqrt [
	"Answer the square root of the receiver."

	self subclassResponsibility
]

{ #category : 'mathematical functions' }
Number >> squared [
	"Answer the receiver multipled by itself."
	^self * self
]

{ #category : 'printing' }
Number >> storeOn: aStream [

	self storeOn: aStream base: 10
]

{ #category : 'printing' }
Number >> storeOn: aStream base: base [
	"This method should print a representation of the number for the given base,
	including the base prefix (with letter r for radix)"
	
	^self subclassResponsibility
]

{ #category : 'printing' }
Number >> storeStringBase: base [
	^ String streamContents: [:strm | self storeOn: strm base: base]
]

{ #category : 'testing' }
Number >> strictlyPositive [
	"Answer whether the receiver is mathematically positive."

	^ self > 0
]

{ #category : 'mathematical functions' }
Number >> tan [
	"The receiver represents an angle measured in radians. Answer its 
	tangent."

	^self moduloTwoPiAsFloat tan
]

{ #category : 'mathematical functions' }
Number >> tanh [
	"Answer receivers hyperbolic tangent"
	
	^self asFloat tanh
]

{ #category : 'intervals' }
Number >> to: stop [
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by 1."

	^Interval from: self to: stop by: 1
]

{ #category : 'intervals' }
Number >> to: stop by: step [
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by step."

	^Interval from: self to: stop by: step
]

{ #category : 'intervals' }
Number >> to: stop by: step do: aBlock  [
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: step).

	Warning: Might fail with Float numbers. For instance:
		0 to: 2.4 by: 0.1 do: [ :each | each print ]
	is wrong, while
		(0 to: 2.4 by: 0.1) do: [ :each | each print ]
	is right. Prefer explicit Interval creation if using Floats!
	"
	| nextValue |
	nextValue := self.
	step = 0 ifTrue: [self error: 'step must be non-zero'].
	step < 0
		ifTrue: [
			[stop <= nextValue]
				whileTrue: [
					aBlock value: nextValue.
					nextValue := nextValue + step]]
		ifFalse: [
			[stop >= nextValue]
				whileTrue: [
					aBlock value: nextValue.
					nextValue := nextValue + step]]
]

{ #category : 'intervals' }
Number >> to: stop count: n [
	"Answer an Interval from the receiver up to the argument, stop, 
	with n elements."

	^Interval from: self to: stop count: n
]

{ #category : 'intervals' }
Number >> to: stop do: aBlock  [
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: 1)."
	| nextValue |
	nextValue := self.
	[nextValue <= stop]
		whileTrue: 
			[aBlock value: nextValue.
			nextValue := nextValue + 1]
]

{ #category : 'intervals' }
Number >> to: stop do: elementBlock separatedBy: separatorBlock [
	"
	String streamContents: [ :strm |
		1 to: 10 do: [ :i | i printOn: strm ] separatedBy: [ strm nextPutAll: ' -- ' ]]
	"
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst := true.
	self to: stop do: [ :element |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]
]

{ #category : 'intervals' }
Number >> toSelfPlus: aDelta [

	^self to: self + aDelta
]

{ #category : 'truncation and round off' }
Number >> truncateTo: aNumber  [
	"Answer the next multiple of aNumber toward zero that is nearest the 
	receiver."
	"Please use this method when you actually want a numeric result.
	If what you need is a string representation with certain precision,
	consider using #printOn:fractionDigits: or some other method in the 'printing' category."
	"Examples:
		3.1479 truncateTo: 0.01 -> 3.14
		3.1479 truncateTo: 0.1 -> 3.1
		1923 truncateTo: 10 -> 1920
		3.1479 truncateTo: 0.005 -> 3.145
		-3.1479 truncateTo: 0.01 -> -3.14"

	^ (self quo: aNumber) * aNumber
]

{ #category : 'truncation and round off' }
Number >> truncated [
	"Answer an integer nearest the receiver toward zero."
	^self quo: 1
]

{ #category : 'converting' }
Number >> weeks [

	^ Duration weeks: self
]

{ #category : 'printing' }
Number >> withBinaryUnitPrefixAndValue: aBlock [
	"
	As in https://en.wikipedia.org/wiki/Binary_prefix
	{ 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withBinaryUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]
	"
	| prefixIndex factor nameAndSymbol |
	prefixIndex := self log floor // 3.
	prefixIndex := prefixIndex min: 8 max: 0.
	factor := 1024 raisedToInteger: prefixIndex.
	nameAndSymbol := {
		{''. 		''}.
		{'kibi'.	'Ki'}.
		{'mebi'.	'Mi'}.
		{'gibi'.	'Gi'}.
		{'tebi'.	'Ti'}.
		{'pebi'.	'Pi'}.
		{'exbi'.	'Ei'}.
		{'zebi'.	'Zi'}.
		{'yobi'.	'Yi'}
	} at: prefixIndex+1.
	aBlock value: (self / factor) asIntegerOrFloat value: nameAndSymbol second value: nameAndSymbol first
]

{ #category : 'printing' }
Number >> withDecimalUnitPrefixAndValue: aBlock [
	"
	As in https://en.wikipedia.org/wiki/Metric_prefix
	{ 0.00000123456. 0.0000123456. 0.000123456. 0.00123456. 0.0123456. 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]
	"
	| prefixIndex factor nameAndSymbol |
	prefixIndex := self log floor // 3.
	prefixIndex := prefixIndex min: 6 max: -6.
	factor := 1000 raisedToInteger: prefixIndex.
	nameAndSymbol := {
		{'atto'.		'a'}.
		{'femto'.	'f'}.
		{'pico'.		'p'}.
		{'nano'.		'n'}.
		{'micro'.		'µ'}.
		{'milli'.		'm'}.
		{''. 			''}.
		{'kilo'.		'k'}.
		{'mega'.		'M'}.
		{'giga'.		'G'}.
		{'tera'.		'T'}.
		{'peta'.		'P'}.
		{'exa'.		'E'}
	} at: prefixIndex+7.
	aBlock value: self asFloat / factor value: nameAndSymbol second value: nameAndSymbol first
]

{ #category : 'converting' }
Number >> withNegativeSign [
	"Answer a number with same magnitude than receiver and negative sign."
	^self abs negated
]
