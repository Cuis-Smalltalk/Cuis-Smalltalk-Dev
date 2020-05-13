'From Cuis 5.0 [latest update: #4140] on 8 May 2020 at 3:32:18 pm'!
'Description '!
!provides: 'Tests-Complex' 1 4!
!requires: 'BaseImageTests' 1 0 nil!
!requires: 'Complex' 1 0 nil!
SystemOrganization addCategory: #'Tests-Complex'!


!classDefinition: #ComplexTest category: #'Tests-Complex'!
AbstractNumberTest subclass: #ComplexTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Complex'!
!classDefinition: 'ComplexTest class' category: #'Tests-Complex'!
ComplexTest class
	instanceVariableNames: ''!


!ComplexTest methodsFor: 'tests - creation' stamp: 'jmv 5/6/2020 16:30:15'!
testCreation
	| c c1 c2 c3 |
	c _ 1 + 2i.
	self assert: c real = 1.
	self assert: c imaginary = 2.


	c1 _ 1/3 + (10/11)i.
	self assert: c1 real = (1/3).
	self assert: c1 imaginary = (10/11).
	self assert: c1 - (10/11)i = (1/3).
	self assert: 1 + c1 = (4/3 + (10/11)i).
	self assert: c1+1 = (4/3 + (10/11)i).

	c1 _ 0.5 + (10/11)i.
	self assert: 1.0 + c1 = (1.5 + (10/11)i).
	self assert: c1+1.0 = (1.5 + (10/11)i).

	c2 _ Complex abs: 5 arg: 0.
	self assert: c2 = 5.
	self assert: c2 isComplex.
	c3 _ Complex abs: 5 arg: Float twoPi.
	self assert: c3 = 5.
	self assert: c2 isComplex.

	c2 _ Complex abs: 5.1 arg: 0.
	self assert: c2 = 5.1.
	c3 _ Complex abs: 5.1 arg: Float twoPi.
	self assert: c3 = 5.1.! !

!ComplexTest methodsFor: 'tests - creation' stamp: 'jmv 4/5/2019 09:14:05'!
testCreation2
	
	| c |
	c := 5 i.
	self assert: (c real = 0).
	self assert: (c imaginary = 5).
	
	c := 6 + 7 i.
	self assert: (c real = 6).
	self assert: ( c imaginary = 7).
	
	c := 5.6 - 8 i.
	self assert: (c real = 5.6).
	self assert: (c imaginary = -8).
	
	c := Complex real: 10 imaginary: 5.
	self assert: (c real = 10).
	self assert: (c imaginary = 5).
	
	c := Complex abs: 5 arg: (Float pi/2).
	self assert: (c real rounded = 0).
	self assert: (c imaginary = 5).
	! !

!ComplexTest methodsFor: 'tests - accessing' stamp: 'jmv 4/5/2019 19:56:05'!
testAbs
	
	| c |
	c _ (6 - 6 i).
	self assert: c abs  = 72 sqrt.
	c _ 2.0e306 + 3.0e305 i.
	self assert: c abs = c safeAbs.
	c _ 2.0e-155 + 3.0e-155 i.
	self assert: c abs = c safeAbs.! !

!ComplexTest methodsFor: 'tests - accessing' stamp: 'jmv 4/5/2019 09:13:12'!
testArg
	
	| c |
	c := (0 + 5 i) .
	self assert: c arg  = (Float pi/ 2).
	! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 09:12:49'!
testAdd
	
	| c |
	c := (5 - 6 i) + (-5 + 8 i).     "Complex with Complex"
	self assert: (c =  (0 + 2 i)).! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 09:10:03'!
testComplexConjugate
	
	| c cc |
	c := (5 - 6 i).
	cc := c complexConjugate.
	self assert: cc real = c real.
	self assert: cc imaginary = c imaginary negated.! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 7/2/2019 12:07:10'!
testDivideComplexByZero

	self assert: 1 i / 0.0 = Complex infinity.
	self assert: 1 i + 1 / 0.0 = Complex infinity.
	self assert: 1 i / -0.0 = Complex infinity.
	self assert: 1 i + 1 / -0.0 = Complex infinity.
	self assert: 1 i / (Complex basicReal: 0.0 imaginary: 0.0) = Complex infinity.
	self assert: 1 i / (Complex basicReal: 0.0 imaginary: -0.0) = Complex infinity.
	self assert: 1 i / (Complex basicReal: -0.0 imaginary: 0.0) = Complex infinity.
	self assert: 1 i / (Complex basicReal: -0.0 imaginary: -0.0) = Complex infinity.
	self assert: 1 i + 1 / (Complex basicReal: 0.0 imaginary: 0.0) = Complex infinity.
	self assert: 1 i + 1 / (Complex basicReal: 0.0 imaginary: -0.0) = Complex infinity.
	self assert: 1 i + 1 / (Complex basicReal: -0.0 imaginary: 0.0) = Complex infinity.
	self assert: 1 i + 1 / (Complex basicReal: -0.0 imaginary: -0.0) = Complex infinity.

	self assert: -1  / (Complex basicReal: 0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1  / (Complex basicReal: 0.0 imaginary: -0.0) = Float negativeInfinity.
	self assert: -1  / (Complex basicReal: -0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1  / (Complex basicReal: -0.0 imaginary: -0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: 0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: 0.0 imaginary: -0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: -0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: -0.0 imaginary: -0.0) = Float negativeInfinity.! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 7/2/2019 12:12:20'!
testDivideComplexZeroByZero

	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / 0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / 0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / 0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / 0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / -0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / -0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / -0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / -0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / 0.0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / 0.0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / 0.0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / 0.0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / -0.0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / -0.0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / -0.0) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / -0.0) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / (Complex basicReal: 0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / (Complex basicReal: 0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / (Complex basicReal: 0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / (Complex basicReal: 0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / (Complex basicReal: 0.0 imaginary: -0.0)) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / (Complex basicReal: 0.0 imaginary: -0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / (Complex basicReal: 0.0 imaginary: -0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / (Complex basicReal: 0.0 imaginary: -0.0)) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / (Complex basicReal: -0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / (Complex basicReal: -0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / (Complex basicReal: -0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / (Complex basicReal: -0.0 imaginary: 0.0)) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) / (Complex basicReal: -0.0 imaginary: -0.0)) isComplexNaN.
	self assert: ((Complex basicReal: 0.0 imaginary: -0.0) / (Complex basicReal: -0.0 imaginary: -0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: 0.0) / (Complex basicReal: -0.0 imaginary: -0.0)) isComplexNaN.
	self assert: ((Complex basicReal: -0.0 imaginary: -0.0) / (Complex basicReal: -0.0 imaginary: -0.0)) isComplexNaN.! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 7/2/2019 12:05:24'!
testDivideFloatByZero

	self assert: 1  / (Complex basicReal: 0.0 imaginary: 0.0) = Float infinity.
	self assert: 1  / (Complex basicReal: 0.0 imaginary: -0.0) = Float infinity.
	self assert: 1  / (Complex basicReal: -0.0 imaginary: 0.0) = Float infinity.
	self assert: 1  / (Complex basicReal: -0.0 imaginary: -0.0) = Float infinity.
	self assert: 1.0  / (Complex basicReal: 0.0 imaginary: 0.0) = Float infinity.
	self assert: 1.0  / (Complex basicReal: 0.0 imaginary: -0.0) = Float infinity.
	self assert: 1.0  / (Complex basicReal: -0.0 imaginary: 0.0) = Float infinity.
	self assert: 1.0  / (Complex basicReal: -0.0 imaginary: -0.0) = Float infinity.

	self assert: -1  / (Complex basicReal: 0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1  / (Complex basicReal: 0.0 imaginary: -0.0) = Float negativeInfinity.
	self assert: -1  / (Complex basicReal: -0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1  / (Complex basicReal: -0.0 imaginary: -0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: 0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: 0.0 imaginary: -0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: -0.0 imaginary: 0.0) = Float negativeInfinity.
	self assert: -1.0  / (Complex basicReal: -0.0 imaginary: -0.0) = Float negativeInfinity.! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 7/2/2019 12:07:43'!
testDivideFloatZeroByZero

	self assert: (0 / (Complex basicReal: 0.0 imaginary: 0.0)) isNaN.
	self assert: (0 / (Complex basicReal: 0.0 imaginary: -0.0)) isNaN.
	self assert: (0 / (Complex basicReal: -0.0 imaginary: 0.0)) isNaN.
	self assert: (0 / (Complex basicReal: -0.0 imaginary: -0.0)) isNaN.
	self assert: (0.0 / (Complex basicReal: 0.0 imaginary: 0.0)) isNaN.
	self assert: (0.0 / (Complex basicReal: 0.0 imaginary: -0.0)) isNaN.
	self assert: (0.0 / (Complex basicReal: -0.0 imaginary: 0.0)) isNaN.
	self assert: (0.0 / (Complex basicReal: -0.0 imaginary: -0.0)) isNaN.! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 10:28:50'!
testDivideLarge

	| c1 c2 quotient |
	c1 := 2e252 + 3e70 i.
	c2 := 2e252 + 3e70 i.
	quotient := c1 / c2.
	self assert: quotient = 1.

	c1 := 2e252 + 3e70 i.
	c2 := 2.0e252 + 3.0e70 i.
	quotient := c1 / c2.
	self assert: quotient = 1.

	c1 := 2.0e252 + 3.0e70 i.
	c2 := 2e252 + 3e70 i.
	quotient := c1 / c2.
	self assert: quotient abs = 1.

	c1 := 2.0e252 + 3.0e70 i.
	c2 := 2.0e252 + 3.0e70 i.
	quotient := c1 / c2.
	self assert: quotient = 1.! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 10:29:27'!
testDivideSmall

	| c1 c2 quotient |
	c1 := 2e-252 + 3e-270 i.
	c2 := 2e-252 + 3e-270 i.
	quotient := c1 / c2.
	self assert: quotient = 1.

	c1 := 2e-252 + 3e-270 i.
	c2 := 2.0e-252 + 3.0e-270 i.
	quotient := c1 / c2.
	self assert: quotient = 1.

	c1 := 2.0e-252 + 3.0e-270 i.
	c2 := 2e-252 + 3e-270 i.
	quotient := c1 / c2.
	self assert: quotient abs = 1.

	c1 := 2.0e-252 + 3.0e-270 i.
	c2 := 2.0e-252 + 3.0e-270 i.
	quotient := c1 / c2.
	self assert: quotient = 1.! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 09:14:41'!
testMultiply
	
	| c |
	c := (5 - 6 i).
	self assert: (c * 1i = c i)! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 09:13:36'!
testNegated

	| c |
	c := (2 + 5 i) .
	self assert: c negated  = (-2 - 5i).
	! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 09:13:40'!
testReciprocal
	
	| c |
	c := (2 + 5 i).
	self assert: c reciprocal  = ((2/29) - (5/29)i).
	! !

!ComplexTest methodsFor: 'tests - arithmetic' stamp: 'jmv 4/5/2019 09:13:44'!
testReciprocalError
	
	| c |
	c := (0 i).
	self should: [c reciprocal] raise: ZeroDivide
	! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 5/6/2020 16:36:42'!
testArcTan
	| c |
	c := (0.5 + 0 i).
	self assert: (c arcTan real isWithin: 2 floatsFrom: c real arcTan).
	#(-0.5 -2 -3 0 0.5 2 3) do: [ :real |
		#(-0.5 -2 -3 0 0.5 2 3) do: [ :imag |
			c := real + imag i.
			self verify: c arcTan tan isTrigonometricallyEqualTo: c ]]! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/17/2018 19:17:39'!
testCos
	| c c2 |
	c := (2 + 0 i).
	self assert: c cos = c real cos.
	c := (2 + 3 i).
	c2 := c i exp + c i negated exp / 2.
	self verify: c cos isTrigonometricallyEqualTo: c2.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/17/2018 19:19:33'!
testCosh
	| c c2 |
	c := (2 + 0 i).
	self assert: c cosh = c real cosh.
	c := (2 + 3 i).
	c2 := c exp + c negated exp / 2.
	self verify: c cosh isHyperbolicallyEqualTo: c2.
	c2 := c i cos.
	self verify: c cosh isHyperbolicallyEqualTo: c2.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 16:51:47'!
testFourthRoot

	"SmallInteger"
	self verify: (-1 nthRoot: 4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1.0 / 2.0 sqrt + (1.0 / 2.0 sqrt)i.

	"LargeNegativeInteger"
	self assert: (100 factorial negated nthRoot: 4) abs = (100 factorial nthRoot: 4).
	self assert: (100 factorial negated nthRoot: 4) arg = (Float pi / 4).

	"Fraction"
	self verify: (-1/4 nthRoot: 4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1/2 + (1/2) i.

	"Float"
	self verify: (-1.0 nthRoot: 4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1.0 / 2.0 sqrt + (1.0 / 2.0 sqrt)i.
	self verify: (-0.25 nthRoot: 4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 0.5 + 0.5 i.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'laza 9/26/2005 10:24'!
testLn
	self assert: (Float e + 0 i) ln = Float e ln "See Bug 1815 on Mantis"! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 16:53:09'!
testRaisedTo
	
	| c c3 |
	c := (5 - 6 i).
	c3 := (c raisedTo: 0.2) raisedTo: 5.
	self verify: c3 isMagnitudeAndArgumentWithin: 7 floatsAwayFrom: c.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'nice 4/23/2011 03:05'!
testRaisedToInteger
	
	| c c3 |
	c := (5 - 6 i).
	c3 := (c * c * c).
	self assert: (c3 = (c raisedToInteger: 3)).
	self assert: (c3 reciprocal = (c raisedToInteger: -3)).! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 7/2/2019 12:00:13'!
testRaisedToMinusOne
	
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) raisedTo: -1) = Float infinity.
	self assert: ((Complex basicReal: 0.0 imaginary: 0.0) raisedTo: -1.0) = Float infinity.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 16:54:06'!
testRaisedToOneFourth

	"SmallInteger"
	self verify: (-1 raisedTo: 1/4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1.0 / 2.0 sqrt + (1.0 / 2.0 sqrt)i.

	"LargeNegativeInteger"
	self assert: (100 factorial negated raisedTo: 1/4) abs = (100 factorial nthRoot: 4).
	self assert: (100 factorial negated raisedTo: 1/4) arg = (Float pi / 4).

	"Fraction"
	self verify: (-1/4 raisedTo: 1/4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1/2 + (1/2) i.

	"Float"
	self verify: (-1.0 raisedTo: 1/4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1.0 / 2.0 sqrt + (1.0 / 2.0 sqrt)i.
	self verify: (-0.25 raisedTo: 1/4) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 0.5 + 0.5 i.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 6/22/2019 15:55:41'!
testRaisedToOneHalf

	"SmallInteger"
	self assert: (-1 raisedTo: 1/2) equals: 1i.
	self assert: (-1 raisedTo: 1/2) imaginary isInteger.
	self assert: (-3 raisedTo: 1/2) imaginary isFloat.
	self assert: (-4 raisedTo: 3/2) equals: -8i.
	self assert: (-4 raisedTo: 3/2) imaginary isInteger.
	self assert: (-4 raisedTo: 5/2) equals: 32i.
	self assert: (-4 raisedTo: 5/2) imaginary isInteger.

	"LargeNegativeInteger"
	self assert: (100 factorial negated raisedTo: 1/2) imaginary = 100 factorial sqrt.

	"Fraction"
	self assert: ((-1/4) raisedTo: 1/2) = (1/2)i.
	self assert: ((-1/4) raisedTo: 3/2) = (-1/8)i.
	self assert: ((-1/4) raisedTo: 5/2) = (1/32)i.
	self assert: ((-100/101) raisedTo: 1/2) imaginary = (100/101) sqrt.

	"Float"
	self assert: (-1.0 raisedTo: 1/2) equals: 0 + 1.0i.
	self assert: (-1.0 raisedTo: 1/2) real isFloat.
	self assert: (-1.0 raisedTo: 1/2) imaginary isFloat.
	self assert: (-4.0 raisedTo: 1/2) equals: 0 + 2.0i.
	self assert: (-4.0 raisedTo: 1/2) real isFloat.
	self assert: (-4.0 raisedTo: 1/2) imaginary isFloat.
	self assert: (-0.25 raisedTo: 1/2) equals: 0 + 0.5i.
	self assert: (-0.25 raisedTo: 1/2) real isFloat.
	self assert: (-0.25 raisedTo: 1/2) imaginary isFloat.

	"Complex"
	self assert: ((Complex basicReal: -1 imaginary: 0) raisedTo: 1/2) equals: 1i.
	self assert: ((Complex basicReal: -1 imaginary: 0) raisedTo: 1/2) imaginary isInteger.
	self assert: ((Complex basicReal: -3 imaginary: 0) raisedTo: 1/2) imaginary isFloat.
	self verify: (1+1i raisedTo: 1/2 :: raisedTo: 2) isMagnitudeAndArgumentWithin: 3 floatsAwayFrom: 1.0 + 1.0 i.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/18/2018 17:01:09'!
testRaisedToPointFive

	"SmallInteger"
	self verify: (-1 raisedTo: 0.5) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1 i.

	"LargeNegativeInteger"
	self verify: (100 factorial negated raisedTo: 0.5) isMagnitudeAndArgumentWithin: 60 floatsAwayFrom: 100 factorial sqrt i.

	"Fraction"
	self verify: ((-1/4) raisedTo: 0.5) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 0.5 i.
	self assert: ((-100/101) raisedTo: 0.5) imaginary = (100/101) sqrt.

	"Float"
	self verify: (-1.0 raisedTo: 0.5) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1 i.
	self verify: (-0.25 raisedTo: 0.5) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 0.5 i.

	"Complex"
	self verify: ((Complex basicReal: -1.0 imaginary: 0.0) raisedTo: 0.5) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 1 i.
	self verify: ((Complex basicReal: -0.25 imaginary: 0.0) raisedTo: 0.5) isMagnitudeAndArgumentWithin: 1 floatsAwayFrom: 0.5 i.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 5/26/2018 20:26:06'!
testSecondRoot

	"SmallInteger"
	self assert: (-1 nthRoot: 2) equals: 1i.
	self assert: (-1 nthRoot: 2) imaginary isInteger.
	self assert: (-3 nthRoot: 2) imaginary isFloat.

	"LargeNegativeInteger"
	self assert: (100 factorial negated nthRoot: 2) imaginary = 100 factorial sqrt.

	"Fraction"
	self assert: ((-1/4) nthRoot: 2) = (1/2)i.
	self assert: ((-100/101) nthRoot: 2) imaginary = (100/101) sqrt.

	"Float"
	self assert: (-1.0 nthRoot: 2) equals: 0.0 + 1.0i.
	self assert: (-1.0 nthRoot: 2) imaginary isFloat.
	self assert: (-0.25 nthRoot: 2) = (1/2)i.
	self assert: (-0.25 nthRoot: 2) imaginary isFloat.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/17/2018 19:26:13'!
testSin
	| c c2 |
	c := (2 + 0 i).
	self assert: c sin = c real sin. 
	c := 2 + 3 i.
	c2 := c i exp - c i negated exp / 2 i.
	self verify: c sin isTrigonometricallyEqualTo: c2.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/17/2018 19:27:05'!
testSinh
	| c c2 |
	c := (2 + 0 i).
	self assert: c sinh = c real sinh.
	c := 2 + 3 i.
	c := c cosh squared - c sinh squared.
	self verify: c isHyperbolicallyEqualTo: 1.0.
	c2 := c exp - c negated exp / 2.
	self verify: c sinh isHyperbolicallyEqualTo: c2.
	c2 := c i sin i negated.
	self verify: c sinh isHyperbolicallyEqualTo: c2.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 11/27/2018 10:49:57'!
testSqrt

	"SmallInteger"
	self assert: -1 sqrt equals: 1i.
	self assert: -1 sqrt imaginary isInteger.

	"LargeNegativeInteger"
	self assert: 100 factorial negated sqrt imaginary = 100 factorial sqrt.

	"Fraction"
	self assert: (-1/4) sqrt = (1/2)i.
	self assert: (-100/101) sqrt imaginary = (100/101) sqrt.
	self assert: ((1 << 1024 + 1) / (1 << 1024 + 3)) negated sqrt = (0 + 1.0i).

	"Float"
	self assert: -1.0 sqrt equals: 0.0 + 1.0i.
	self assert: -1.0 sqrt imaginary isFloat.
	self assert: -0.25 sqrt = (1/2)i.
	self assert: -0.25 sqrt imaginary isFloat.
	self assert: Float negativeInfinity sqrt = Float infinity i.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 4/5/2019 09:13:56'!
testSquared
	
	| c c2 |
	c := (6 - 6 i).
	c2 := (c squared).
	self assert: c2 imaginary = -72.
	self assert: c2 real = 0.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 12/17/2018 19:27:24'!
testTan
	| c c2 |
	c := (2 + 0 i).
	self assert: c tan = c real tan.
	c := 2 + 3 i.
	c2 := c sin / c cos.
	self verify: c2 isTrigonometricallyEqualTo: c tan.! !

!ComplexTest methodsFor: 'tests - mathematical functions' stamp: 'jmv 5/6/2020 16:37:53'!
testTanh
	| c c2 |
	c := (2 + 0 i).
	self assert: (c tanh real isWithin: 1 floatsFrom: c real tanh).
	c := 2 + 3 i.
	c2 := c sinh / c cosh.
	self verify: c2 isHyperbolicallyEqualTo: c tanh.! !

!ComplexTest methodsFor: 'tests - function properties' stamp: 'jmv 12/17/2018 19:18:13'!
testCos2PlusSin2
	| c |
	#(-0.5 -2 -3 0 0.5 2 3) do:  [:real |
	#(-0.5 -2 -3 0 0.5 2 3) do: [ :imag |
		c := real + imag i.
		self verify: (c cos squared + c sin squared) isTrigonometricallyEqualTo: 1.0 ]]! !

!ComplexTest methodsFor: 'tests - function properties' stamp: 'jmv 12/17/2018 19:19:49'!
testCosh2MinusSinh2
	| c |
	#(-0.5 -2 -3 0 0.5 2 3) do: [:real |
	#(-0.5 -2 -3 0 0.5 2 3) do: [:imag |
		c := real + imag i.
		self verify: (c cosh squared - c sinh squared) isHyperbolicallyEqualTo: 1.0 ]]! !

!ComplexTest methodsFor: 'tests - comparing' stamp: 'jmv 4/5/2019 09:13:29'!
testEquality
	
	self assert: 0i = 0.
	self assert: (2 - 5i) = ((1 -4 i) + (1 - 1i)).
	self assert: 0i isZero.
	self deny: (1 + 3 i) = 1.
	self deny: (1 + 3 i) = (1 + 2i).

"Some more stuff"
	self deny: (1 i) = nil.
	self deny: nil = (1 i).

	self deny: (1 i) = #(1 2 3).
	self deny: #(1 2 3) = (1 i).

	self deny: (1 i) = 0.
	self deny: 0 = (1 i).

	self assert:  (1 + 0 i) = 1.
	self assert:  1 = (1+ 0 i).

	self assert:  (1 + 0 i) = 1.0.
	self assert:  1.0 = (1+ 0 i).

	self assert:  (1/2 + 0 i) = (1/2).
	self assert:  (1/2) = (1/2+ 0 i).! !

!ComplexTest methodsFor: 'tests - conversion and rounding' stamp: 'jmv 4/5/2019 09:13:19'!
testConversion
	
	self assert: ((1 + 2i) + 1) =  (2 + 2 i).
	self assert: (1 + (1 + 2i)) =  (2 + 2 i).
	self assert: ((1 + 2i) + 1.0) =  (2.0 + 2 i).
	self assert: (1.0 + (1 + 2i)) =  (2.0 + 2 i).
	self assert: ((1 + 2i) + (2/3)) = ((5/3) + 2 i ).
	self assert: ((2/3) + (1 + 2i)) = ((5/3) + 2 i )! !

!ComplexTest methodsFor: 'tests - misc' stamp: 'jmv 4/5/2019 09:13:16'!
testComplexCollection
	
	| array array2 |
	array := Array with: 1 + 2i with:  3 + 4i with: 5 + 6i.
	array2 := 2 * array.
	array with:  array2 do: [:one :two | self assert: (2 * one) = two ] ! !

!ComplexTest methodsFor: 'tests - bugs' stamp: 'md 2/18/2006 16:53'!
testBug1

	self assert: (0.5 * (2+0i) ln) exp = (0.5 * 2 ln) exp.! !

!ComplexTest methodsFor: 'helpers' stamp: 'sqr 12/18/2018 16:27:27'!
effectiveZeroExponentForTrigonometry

	^-46! !

!ComplexTest methodsFor: 'helpers' stamp: 'sqr 12/15/2018 01:19:43'!
hyperbolicPrecision

	^384! !

!ComplexTest methodsFor: 'helpers' stamp: 'sqr 12/15/2018 00:55:13'!
trigonometricPrecision

	^384! !

!ComplexTest methodsFor: 'helpers' stamp: 'jmv 12/17/2018 19:13:11'!
verify: aComplex isHyperbolicallyEqualTo: anotherComplex
	super verify: aComplex real asFloat isHyperbolicallyEqualTo: anotherComplex real asFloat.
	super verify: aComplex imaginary asFloat isHyperbolicallyEqualTo: anotherComplex imaginary asFloat.! !

!ComplexTest methodsFor: 'helpers' stamp: 'jmv 12/18/2018 16:47:43'!
verify: aComplex isMagnitudeAndArgumentWithin: anInteger floatsAwayFrom: anotherComplex
	self verify: aComplex magnitude asFloat isWithin: anInteger floatsAwayFrom: anotherComplex magnitude asFloat.
	self verify: aComplex argument asFloat isWithin: anInteger floatsAwayFrom: anotherComplex argument asFloat.! !

!ComplexTest methodsFor: 'helpers' stamp: 'jmv 12/17/2018 19:13:18'!
verify: aComplex isTrigonometricallyEqualTo: anotherComplex
	super verify: aComplex real asFloat isTrigonometricallyEqualTo: anotherComplex real asFloat.
	super verify: aComplex imaginary asFloat isTrigonometricallyEqualTo: anotherComplex imaginary asFloat.! !
