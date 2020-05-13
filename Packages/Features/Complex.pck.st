'From Cuis 5.0 [latest update: #4140] on 8 May 2020 at 3:32:04 pm'!
'Description '!
!provides: 'Complex' 1 7!
SystemOrganization addCategory: #Complex!


!classDefinition: #Complex category: #Complex!
Object subclass: #Complex
	instanceVariableNames: 'real imaginary'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Complex'!
!classDefinition: 'Complex class' category: #Complex!
Complex class
	instanceVariableNames: ''!


!Complex commentStamp: 'mk 10/31/2003 22:19' prior: 0!
I represent a complex number.

real			--	real part of the complex number
imaginary	--	imaginary part of the complex number

Complex number constructors:

	5 i
	6 + 7 i.
	5.6 - 8 i.
	Complex real: 10 imaginary: 5.
	Complex abs: 5 arg: (Float pi / 4)

Arithmetic operation with other complex or non-complex numbers work.

	(5 - 6 i) + (-5 + 8 i).			"Arithmetic between two complex numbers."
	5 * (5 - 6 i).				"Arithmetic between a non-complex and a complex number."
					
It is also possible to perform arithmetic operations between a complex number
and a array of (complex) numbers:

	2 * {1 + 2i.
	     3 + 4i.
	     5 + 6i}

	5 + 5i * {1 + 2i.
	          3.
	          5 + 6i}

It behaves analogously as it is with normal numbers and an array.

NOTE: Although Complex something similiar to the Smalltalk's Number class, it would
not be a good idea to make a Complex to be a subclass of a Number because:
- Number is subclass of Magnitude and Complex is certainly not a magnitude.
  Complex does not behave very well as a Magnitude. Operations such as
	<
	>
	<=
	>=
  do not have sense in case of complex numbers.
- Methods in the following Number methods' categories do not have sense for a Complex numbers
	trucation and round off
	testing
	intervals
	comparing
- However the following Number methods' categories do have sense for a Complex number
	arithmetic (with the exception of operation
		//
		\\
		quo:
		rem:	
	mathematical functions

Thus Complex is somewhat similar to a Number but it is not a subclass of it. Some operations
we would like to inherit (e.g. #abs, #negated, #reciprocal) but some of the Number operation
do not have sens to inherit or to overload. Classes are not always neat mechanism.

!!!!!! We had to COPY the implementation of the
		abs
		negated
		reciprocal
		log:
		isZero
		reciprocal
		...
	methods from the Number class to the Complex class. Awful solution. Now I begin to
	appreciate the Self.

Missing methods
	String | converting | asComplex
	Complex | mathematical functions | arcSin
	Complex | mathematical functions | arcCos
	Complex | mathematical functions | arcTan!

!Complex methodsFor: 'printing' stamp: 'jmv 7/2/2019 08:43:23'!
printOn: aStream
	real printOn: aStream.
	aStream nextPut: Character space.
	imaginary < 0
		ifTrue: [aStream nextPut: $-]
		ifFalse: [aStream nextPut: $+].
	aStream nextPut: Character space.
	imaginary abs printOn: aStream.
	aStream nextPut: Character space.
	aStream nextPut: $i
! !

!Complex methodsFor: 'accessing' stamp: 'jmv 6/25/2018 12:01:46'!
argument
	"Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]"

	self isZero ifTrue: [ ^0.0 ].
	^imaginary arcTan: real! !

!Complex methodsFor: 'accessing' stamp: 'mk 10/27/2003 17:39'!
imaginary
	^ imaginary! !

!Complex methodsFor: 'accessing' stamp: 'jmv 6/25/2018 11:52:36'!
magnitude
	"Answer the distance of the receiver from zero (0 + 0 i)."

	^ self abs! !

!Complex methodsFor: 'accessing' stamp: 'jmv 6/25/2018 12:03:15'!
phase
	"Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^ self argument! !

!Complex methodsFor: 'accessing' stamp: 'mk 10/27/2003 17:39'!
real
	^ real! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 8/2/2019 12:13:14'!
* aNumber
	"Answer the result of multiplying the receiver by aNumber."
	| c d newReal newImaginary |
	aNumber isComplex
		ifTrue: [
			c _ aNumber real.
			d _ aNumber imaginary.
			imaginary = 0 "For example, 1 * 2i, or more importantly 1 * Float infinity i"
				ifTrue: [
					newReal _ real * c.
					newImaginary _ real * d ]
				ifFalse: [
					newReal _ (real * c) - (imaginary * d).
					newImaginary _ (real * d) + (imaginary * c) ]]
		ifFalse: [
			newReal _ real * aNumber.
			newImaginary _ imaginary * aNumber ].
	^ Complex real: newReal imaginary: newImaginary! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:48:36'!
+ aNumber
	"Answer the sum of the receiver and aNumber."
	^ Complex
		real: real + aNumber real
		imaginary: imaginary + aNumber imaginary.! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:48:52'!
- aNumber
	"Answer the difference between the receiver and aNumber."
	^ Complex
		real: real - aNumber real
		imaginary: imaginary - aNumber imaginary.! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 7/2/2019 09:17:45'!
/ aNumber
	"Answer the result of dividing receiver by aNumber"
	| c d newReal newImaginary s e f |
	aNumber isZero ifTrue: [^ ZeroDivide new signalReceiver: self selector: #/ argument: aNumber].
	aNumber isComplex
		ifTrue: [
			c _ aNumber real.
			d _ aNumber imaginary.
			e _ (real * c) + (imaginary * d).
			e isFinite ifFalse: [ ^ self safeDivideBy: aNumber ].
			f _ (imaginary * c) - (real * d).
			s _ (c * c) + (d * d).
			(e isFloat and: [ s  = 0.0 ]) ifTrue: [ ^ self safeDivideBy: aNumber ].
			newReal _ e / s.
			newImaginary _ f / s ]
		ifFalse: [
			newReal _ real / aNumber.
			newImaginary _ imaginary / aNumber ].
	^ Complex real: newReal imaginary: newImaginary! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 19:56:25'!
abs
	"Answer the distance of the receiver from zero (0 + 0 i)."

	| absSquared |
	absSquared _ (real * real) + (imaginary * imaginary).
	absSquared isFloat ifTrue: [
		absSquared < Float fminNormalized ifTrue: [
			^ self safeAbs ].
		absSquared isFinite ifFalse: [
			^ self safeAbs ]].
	^absSquared sqrt! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 6/25/2018 12:02:03'!
arg
	"Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]
	Note: #argument and #phase assume the convention of 0+0i having argument=0"

	self isZero ifTrue: [self error: 'zero has no argument.'].
	^imaginary arcTan: real! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:09:44'!
complexConjugate
	"Return the complex conjugate of this complex number."

	^self class real: real imaginary: imaginary negated! !

!Complex methodsFor: 'arithmetic' stamp: 'nice 2/5/2006 03:25'!
i
	"Answer the result of multiplying the receiver with pure imaginary.
		^self * 1 i
	This is an obvious extension of method i implemented in Number."

	^self class real: imaginary negated imaginary: real! !

!Complex methodsFor: 'arithmetic' stamp: 'nice 4/20/2011 22:06'!
negated
	"Answer a Number that is the negation of the receiver."

	^self class real: real negated imaginary: imaginary negated! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 7/2/2019 09:05:11'!
reciprocal
	"Returns the reciprocal of self.
	In case self is 0 the / signals ZeroDivide"
	
	^1 / self! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 19:37:44'!
safeAbs
	"Answer the distance of the receiver from zero (0 + 0 i).
	Try avoiding overflow and/or underflow"

	| scale a b |
	scale _ real abs max: imaginary abs.
	scale = 0.0 
		ifTrue: [^0.0].
	a _ real / scale.
	b _ imaginary / scale.
	^((a * a) + (b * b)) sqrt * scale! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/8/2019 09:44:54'!
safeDivideBy: aComplex
	"Answer the result of dividing receiver by aNumber"
	" Both operands are scaled to avoid arithmetic overflow. 
	  This algorithm works for a wide range of values, and it needs only three divisions."
	| r d newReal newImaginary |
	aComplex real abs > aComplex imaginary abs
		ifTrue: [
			r _ aComplex imaginary / aComplex real.
			d _ r * aComplex imaginary + aComplex real.
			newReal _ r * imaginary + real / d.
			newImaginary _ r negated * real + imaginary / d ]
		ifFalse: [
			r _ aComplex real / aComplex imaginary.
			d _ r * aComplex real + aComplex imaginary.
			newReal _ r * real + imaginary / d.
			newImaginary _ r * imaginary - real / d ].
	^ Complex
		real: newReal
		imaginary: newImaginary.! !

!Complex methodsFor: 'arithmetic' stamp: 'nice 2/5/2006 03:27'!
squaredNorm
	"Answer the square of receiver norm."

	^real * real + (imaginary * imaginary)! !

!Complex methodsFor: 'comparing' stamp: 'jmv 4/5/2019 08:37:31'!
= anObject
	self == anObject ifTrue: [ ^ true ].
	anObject isNumber ifFalse: [^false].
	^real = anObject real and: [ imaginary = anObject imaginary ]! !

!Complex methodsFor: 'comparing' stamp: 'mk 10/27/2003 20:35'!
hash
	"Hash is reimplemented because = is implemented."
	
	^ real hash bitXor: imaginary hash.! !

!Complex methodsFor: 'converting' stamp: 'mk 10/27/2003 21:51'!
adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]! !

!Complex methodsFor: 'converting' stamp: 'mk 10/27/2003 18:32'!
adaptToFloat: rcvr andSend: selector
	"If I am involved in arithmetic with a Float, convert it to a Complex number."
	^ rcvr asComplex perform: selector with: self! !

!Complex methodsFor: 'converting' stamp: 'mk 10/27/2003 18:32'!
adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert it to a Complex number."
	^ rcvr asComplex perform: selector with: self! !

!Complex methodsFor: 'converting' stamp: 'mk 10/27/2003 18:31'!
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Complex number."
	^ rcvr asComplex perform: selector with: self! !

!Complex methodsFor: 'converting' stamp: 'nice 10/16/2011 16:34'!
asComplex
	^self! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/24/2011 22:44'!
arCosh
	"Answer receiver's area hyperbolic cosine.
	That is the inverse function of cosh.
	Some possible implementations:
	^imaginary > 0 
		ifTrue: [(self + (self * self - 1) sqrt) ln]
		ifFalse: [(self + (self * self - 1) sqrt) ln negated]
	^self arcCos i
	This implementation provides an answer with a positive real part.
	It also avoids creating intermediate Complex."
	
	| x y tmp sh2x shx delta ch2x chx |
	imaginary = 0 ifTrue: [real abs > 1
			ifTrue: 
				[y := real < 0
					ifTrue: [Float pi]
					ifFalse: [0].
				x := real abs arCosh.
				^self class real: x imaginary: y]
			ifFalse: [^self class real: 0 imaginary: real arcCos]].
	tmp := self squaredNorm - 1 / 2.
	delta := tmp squared + imaginary squared.
	sh2x := tmp + delta sqrt.
	shx := sh2x sqrt.
	ch2x := 1 + sh2x.
	chx := ch2x sqrt.
	x := shx arSinh.
	y := imaginary copySignTo: (real / chx) arcCos.
	^self class real: x imaginary: y! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 01:31'!
arSinh
	"Answer receiver's area hyperbolic sine.
	That is the inverse function of sinh."

	"Some possible implementation:

	^imaginary * real < 0 
		ifTrue: [(self + (self * self + 1) sqrt) ln]
		ifFalse: [(self - (self * self + 1) sqrt) ln]"

	^self i arcSin i negated! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 01:31'!
arTanh
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	"Some other possible implementation:

	^((1 + self) / (1 - self)) ln / 2"

	^self i arcTan i negated! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/24/2011 21:31'!
arcCos
	"Answer the arc cosine of the receiver.
	This is the inverse function of cos."

	| x y tmp sh2y shy delta ch2y chy |
	imaginary = 0 ifTrue: [real abs > 1
			ifTrue: 
				[x := real < 0
					ifTrue: [Float pi]
					ifFalse: [0].
				y := real copySignTo: real abs arCosh.
				^self class real: x imaginary: y]
			ifFalse: [^self class real: real arcCos imaginary: 0]].
	tmp := self squaredNorm - 1 / 2.
	delta := tmp squared + imaginary squared.
	sh2y := tmp + delta sqrt.
	shy := sh2y sqrt.
	ch2y := 1 + sh2y.
	chy := ch2y sqrt.
	y := imaginary copySignTo: shy arSinh.
	x := (real / chy) arcCos.
	^self class real: x imaginary: y negated! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/24/2011 22:46'!
arcSin
	"Answer the arc sine of the receiver.
	This is the inverse function of sin."

	| x y tmp delta sh2y shy ch2y chy |
	imaginary = 0 
		ifTrue: 
			[real abs > 1 
				ifTrue: 
					[x := Float pi / 2 * real sign.
					y := (real copySignTo: real abs arCosh) negated.
					^self class real: x imaginary: y]
				ifFalse: [^self class real: real arcSin imaginary: 0]].
	tmp := (self squaredNorm - 1) / 2.
	delta := tmp squared + imaginary squared.
	sh2y := tmp + delta sqrt.
	shy := sh2y sqrt.
	ch2y := 1 + sh2y.
	chy := ch2y sqrt.
	y := imaginary copySignTo: shy arSinh.
	x := (real / chy) arcSin.
	^self class real: x imaginary: y! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/21/2011 21:45'!
arcTan
	"Answer the arc tangent of the receiver.
	This is the inverse function of tan."

	| r2 |
	r2 := self squaredNorm.
	^self class
		real: (real * 2 arcTan: 1 - r2) / 2
		imaginary: ((r2 + (imaginary * 2) + 1) / (r2 - (imaginary * 2) + 1)) ln / 4! !

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 5/6/2020 16:42:19'!
arcTan: denominator 
	"Answer the  four quadrants arc tangent of receiver over denominator."

	self isNaN ifTrue: [ ^Complex nan ].
	denominator isNaN ifTrue: [ ^Complex nan ].
	^ denominator isZero 
		ifTrue: [
			self isZero 
				ifTrue: [
					"shouldn't it be an error ? ^DomainError signal: '0 arcTan: 0'"
					self class real: 0 imaginary: 0]
				ifFalse: [
					self class
						real: Float pi / (real copySignTo: 2)
						imaginary: 0]]
		ifFalse: [
			| res |
			res := (self / denominator) arcTan.
			denominator real < 0 ifTrue: [res := res + Float pi].
			res real > Float pi 
				ifTrue: [res := res - (Float pi * 2)].
			res]! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 00:54'!
cos
	"Answer receiver's cosine."

	^self i cosh! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 21:30'!
cosh
	"Answer receiver's hyperbolic cosine.
	Hyperbolic cosine is defined by same power serie expansion as for real numbers, that is in term of exponential:
	^ (self exp + self negated exp) / 2.
	This implementation avoids creating intermediate objects."
	
	^self class
		real: real cosh * imaginary cos
		imaginary: real sinh * imaginary sin! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/22/2011 00:38'!
exp
	"Answer the exponential of the receiver."

	^ real exp * (imaginary cos + imaginary sin i)! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/22/2011 00:39'!
ln
	"Answer the natural log of the receiver."

	^ self abs ln + self arg i! !

!Complex methodsFor: 'mathematical functions' stamp: 'mk 10/27/2003 22:05'!
log: aNumber 
	"Answer the log base aNumber of the receiver."

	^self ln / aNumber ln! !

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:57:33'!
nthRoot: aPositiveInteger

	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].

	^ Complex abs: (self abs nthRoot: aPositiveInteger) arg: self arg / aPositiveInteger! !

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 10:44:13'!
raisedTo: exponent 
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^Complex nan ].
	exponent isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: exponent].
	exponent isFraction ifTrue: [
		^ self raisedToFraction: exponent].
	
	0.0 = exponent ifTrue: [^ self class one].	"Special case of exponent=0"
	1.0 = exponent ifTrue: [^ self].					"Special case of exponent=1"
	0 = self ifTrue: [									"Special case of self = 0"
		exponent isNaN ifTrue: [ ^Complex nan ].
		^exponent < 0
			ifTrue: [(self raisedTo: exponent negated) reciprocal]
			ifFalse: [self]].
	^ (exponent * self ln) exp							"Otherwise use logarithms"! !

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 09:47:10'!
raisedToFraction: aFraction
	"Raise an exception if argument is not a Fraction"
	^ (self nthRoot: aFraction denominator) raisedToInteger: aFraction numerator! !

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 11:47:34'!
raisedToInteger: exponent 
	"Answer the receiver raised to the power operand, an Integer."
	
	| bitProbe result |

	self isNaN ifTrue: [ ^Complex nan ].
	exponent = 0 ifTrue: [^ self class one].
	exponent = 1 ifTrue: [^ self].
	exponent negative ifTrue: [^ (self raisedToInteger: exponent negated) reciprocal].

	bitProbe := 1 bitShift: exponent highBit - 1.
 	result := self class one.
 	[
		(exponent bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
		bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
			whileTrue: [
				result := result * result].
	^result! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 00:54'!
sin
	"Answer receiver's sine."

	^self i sinh i negated! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 21:30'!
sinh
	"Answer receiver's hyperbolic sine.
	Hyperbolic sine is defined by same power serie expansion as for real numbers, that is in term of exponential:
	^ (self exp - self negated exp) / 2.
	This implementation avoids creating intermediate objects."
	
	^self class
		real: real sinh * imaginary cos
		imaginary: real cosh * imaginary sin! !

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 5/6/2020 16:51:58'!
sqrt
	"Return the square root of the receiver with a positive imaginary part."

	| u v |
	imaginary = 0 ifTrue: [
		^real >= 0
			ifTrue: [self class real: real sqrt imaginary: 0 ]
			ifFalse: [self class real: 0 imaginary: real negated sqrt]].
	v := (self abs - real / 2) sqrt.
	u := imaginary / 2 / v.
	^self class real: u imaginary: v! !

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:52:15'!
sqrtNonPrimitive 
	^ self raisedTo: 1/2! !

!Complex methodsFor: 'mathematical functions' stamp: 'md 7/20/2004 12:02'!
squared
	"Answer the receiver multipled by itself."

	^self * self! !

!Complex methodsFor: 'mathematical functions' stamp: 'mk 10/27/2003 22:04'!
tan
	"Answer receivers tangent."

	^ self sin / self cos! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 00:55'!
tanh
	"Answer receiver's hyperbolic tangent."

	"Some possible implementation are:

	^self sinh / self cosh

	| tr ti |
	tr := real tanh.
	ti := imaginary tan i.
	^(tr + ti) / (tr * ti + 1)"

	^self i tan i negated! !

!Complex methodsFor: 'testing' stamp: 'mk 10/27/2003 17:33'!
isComplex
	^ true! !

!Complex methodsFor: 'testing' stamp: 'jmv 7/2/2019 12:11:25'!
isComplexNaN
	^real isNaN or: [ imaginary isNaN ]! !

!Complex methodsFor: 'testing' stamp: 'jmv 5/6/2020 16:44:07'!
isFloatOrFloatComplex
	"If imaginary part is Float, real will also be."

	^ real isFloat! !

!Complex methodsFor: 'testing' stamp: 'jmv 5/20/2018 21:10:51'!
isInfinite
	^real isInfinite or: [ imaginary isInfinite ]! !

!Complex methodsFor: 'testing' stamp: 'jmv 5/26/2018 23:00:13'!
isNaN
	^real isNaN or: [ imaginary isNaN ]! !

!Complex methodsFor: 'testing' stamp: 'hmm 11/1/2006 23:34'!
isNumber
	^ true! !

!Complex methodsFor: 'testing' stamp: 'nice 4/20/2011 22:06'!
isZero
	^ real isZero and: [imaginary isZero]! !

!Complex methodsFor: 'private' stamp: 'jmv 5/6/2020 16:25:56'!
setReal: realPart imaginary: imaginaryPart
	"Private - initialize the real and imaginary parts of a Complex.
	If either the real part or the imaginary part is a Float, then both will be."

	realPart isFloat | imaginaryPart isFloat
		ifTrue: [
			real _ realPart asFloat.
			imaginary _ imaginaryPart asFloat ]
		ifFalse: [
			real _ realPart.
			imaginary _ imaginaryPart ]! !

!Complex class methodsFor: 'instance creation' stamp: 'jmv 5/20/2018 20:27:23'!
abs: rho arg: theta
	| theta1 |
	"So that if theta is any integer multiple of twoPi, answer is real"
	theta1 _ theta \\ Float twoPi.
	^ self
		real: rho * theta1 cos
		imaginary: rho * theta1 sin! !

!Complex class methodsFor: 'instance creation' stamp: 'jmv 5/6/2020 16:51:21'!
basicReal: realPart imaginary: imaginaryPart
	"Answer a Complex even if imaginary part is zero.
	Usually you might don't want to enforce this and just call #real:imaginary:"
	^self basicNew setReal: realPart imaginary: imaginaryPart! !

!Complex class methodsFor: 'instance creation' stamp: 'jmv 7/4/2018 17:03:13'!
magnitude: rho phase: theta
	^ self abs: rho arg: theta! !

!Complex class methodsFor: 'instance creation' stamp: 'mk 10/27/2003 17:28'!
new
	^ self real: 0 imaginary: 0! !

!Complex class methodsFor: 'instance creation' stamp: 'nice 9/18/2008 22:50'!
one
	^ self real: 1 imaginary: 0! !

!Complex class methodsFor: 'instance creation' stamp: 'jmv 5/6/2020 16:32:23'!
real: realPart imaginary: imaginaryPart

	"This implementation allows reducing Complex to Integer or Fraction, avoiding instances of Complex where the imaginary part is zero. It is currently disabled."
	"
	^ (imaginaryPart isFloat not and: [ imaginaryPart isZero ])
		ifTrue: [ realPart ]
		ifFalse: [ self basicReal: realPart imaginary: imaginaryPart ]
	"

	"We allow for instances of Complex that actually just real numbers"
	^ self basicReal: realPart imaginary: imaginaryPart! !

!Complex class methodsFor: 'instance creation' stamp: 'nice 9/18/2008 22:50'!
zero
	^ self real: 0 imaginary: 0! !

!Complex class methodsFor: 'constants' stamp: 'jmv 7/2/2019 10:13:02'!
infinity
	^ (1 + 1 i) * Float infinity! !

!Complex class methodsFor: 'constants' stamp: 'jmv 7/2/2019 08:30:04'!
nan
	^ (1 + 1 i) * Float nan! !

!Number methodsFor: '*Complex' stamp: 'jmv 5/20/2018 20:33:20'!
asComplex
	"Answer a Complex number that represents value of the the receiver."

	^ Complex basicReal: self imaginary: 0! !

!Number methodsFor: '*Complex' stamp: 'mk 10/27/2003 18:17'!
i
	^ Complex real: 0 imaginary: self! !

!NegativePowerError methodsFor: '*Complex' stamp: 'jmv 6/25/2019 18:53:10'!
defaultAction

	^(Complex basicReal: receiver imaginary: 0) perform: selector withArguments: arguments! !
