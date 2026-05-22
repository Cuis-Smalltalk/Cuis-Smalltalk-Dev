Class {
	#name : 'FractionTest',
	#superclass : 'AbstractNumberTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'private' }
FractionTest >> assert: a classAndValueEquals: b [
	self assert: a class = b class.
	self assert: a = b
]

{ #category : 'tests - conversions' }
FractionTest >> testCeiling [
	self assert: (3 / 2) ceiling = 2.
	self assert: (-3 / 2) ceiling = -1.
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testDegreeCos2 [
	"self run: #testDegreeCos"
	
	self shouldnt: [ (4/3) degreeCos] raise: Error.
	self assert: (1/3) degreeCos printString =  '0.9999830768577442'
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testDegreeSin2 [
	"self run: #testDegreeSin"
	
	self shouldnt: [ (4/3) degreeSin] raise: Error.
	self assert: (1/3) degreeSin printString =  '0.005817731354993834'.
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testExactRaisedTo [
	"
	FractionTest new testExactRaisedTo
	"
	| f |
	self assert: (4/9 raisedTo: 1/2) classAndValueEquals: 2/3.
	self assert: (9/4 raisedTo: 1/2) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f.
		f _ b / a.
		self assert: (f squared raisedTo: 1/2) classAndValueEquals: f.
		self assert: (f negated squared raisedTo: 1/2) classAndValueEquals: f ].

	self assert: (8/27 raisedTo: 1/3) classAndValueEquals: 2/3.
	self assert: (27/8 raisedTo: 1/3) classAndValueEquals: 3/2.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated.
		f _ b / a.
		self assert: ((f raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f.
		self assert: ((f negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: f negated ].

	self assert: (4/9 raisedTo: 3/2) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 2/3) classAndValueEquals: 4/9.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		f _ b / a.
		self assert: ((f raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f.
		self assert: ((f negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: f*f.
		self assert: ((f negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: f*f*f ].

	self assert: (32/243 raisedTo: 3/5) classAndValueEquals: 8/27.
	self assert: (8/27 raisedTo: 5/3) classAndValueEquals: 32/243.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :a :b |
		f _ a / b.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		self assert: ((f raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(f*f*f*f*f).
		self assert: ((f raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(f*f*f).
		self assert: ((f raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(f*f*f*f*f).
		self assert: ((f negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(f*f*f).
		self assert: ((f negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(f*f*f*f*f).

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat.

		f _ b / a.
		self assert: ((f raisedTo: 5) raisedTo: 3/5) classAndValueEquals: f*f*f.
		self assert: ((f raisedTo: 3) raisedTo: 5/3) classAndValueEquals: f*f*f*f*f.
		self assert: ((f negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (f*f*f) negated.
		self assert: ((f negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (f*f*f*f*f) negated.

		"No exact result => Float result"
		self assert: ((f raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((f negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testExactSqrt [
	"
	FractionTest new testExactSqrt
	"
	| f |
	self assert: (4/9) sqrt classAndValueEquals: 2/3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) pairsDo: [ :i :j |
		f _ i / j.
		self assert: f squared sqrt classAndValueEquals: f.
		f _ j / i.
		self assert: f squared sqrt classAndValueEquals: f ]
]

{ #category : 'tests - conversions' }
FractionTest >> testFloor [
	self assert: (3 / 2) floor = 1.
	self assert: (-3 / 2) floor = -2.
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testFloorLog [
	self assert: (1/100 floorLog: 10) = -2.
	self assert: (((2 raisedTo: Float emax + 11)/3) floorLog: 10)
		= ((Float emax + 11)*2 log - 3 log) floor description: 'Fraction>>log should not overflow'.
	self assert: ((3/(2 raisedTo: Float precision - Float emin)) floorLog: 10)
		= ((Float emin - Float precision)*2 log + 3 log) floor description: 'Fraction>>log should not underflow'
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testFloorLogExactness [

	1 + (Float fminDenormalized floorLog: 10) to: -1 do: [:n |
		self assert: ((10 raisedTo: n) floorLog: 10) = n].

	"Float version is not exact for at least 2 reasons:
	1/(10 raisedTo: n) asFloat is not exact
	(aFloat log: radix) is not exact

	(1 + (Float fminDenormalized floorLog: 10) to: -1) count: [:n |
		((10 raisedTo: n) asFloat floorLog: 10) ~= n]."
	
]

{ #category : 'tests - printing' }
FractionTest >> testFractionPrinting [

	self assert: (353/359) printString = '(353/359)'.
	self assert: ((2/3) printStringBase: 2) = '(10/11)'.
	self assert: ((2/3) storeStringBase: 2) = '(2r10/2r11)'.
	self assert: ((5/7) printStringBase: 3) = '(12/21)'.
	self assert: ((5/7) storeStringBase: 3) = '(3r12/3r21)'.
	self assert: ((11/13) printStringBase: 4) = '(23/31)'.
	self assert: ((11/13) storeStringBase: 4) = '(4r23/4r31)'.
	self assert: ((17/19) printStringBase: 5) = '(32/34)'.
	self assert: ((17/19) storeStringBase: 5) = '(5r32/5r34)'.
	self assert: ((23/29) printStringBase: 6) = '(35/45)'.
	self assert: ((23/29) storeStringBase: 6) = '(6r35/6r45)'.
	self assert: ((31/37) printStringBase: 7) = '(43/52)'.
	self assert: ((31/37) storeStringBase: 7) = '(7r43/7r52)'.
	self assert: ((41/43) printStringBase: 8) = '(51/53)'.
	self assert: ((41/43) storeStringBase: 8) = '(8r51/8r53)'.
	self assert: ((47/53) printStringBase: 9) = '(52/58)'.
	self assert: ((47/53) storeStringBase: 9) = '(9r52/9r58)'.
	self assert: ((59/61) printStringBase: 10) = '(59/61)'.
	self assert: ((59/61) storeStringBase: 10) = '(59/61)'.
	self assert: ((67/71) printStringBase: 11) = '(61/65)'.
	self assert: ((67/71) storeStringBase: 11) = '(11r61/11r65)'.
	self assert: ((73/79) printStringBase: 12) = '(61/67)'.
	self assert: ((73/79) storeStringBase: 12) = '(12r61/12r67)'.
	self assert: ((83/89) printStringBase: 13) = '(65/6B)'.
	self assert: ((83/89) storeStringBase: 13) = '(13r65/13r6B)'.
	self assert: ((97/101) printStringBase: 14) = '(6D/73)'.
	self assert: ((97/101) storeStringBase: 14) = '(14r6D/14r73)'.
	self assert: ((103/107) printStringBase: 15) = '(6D/72)'.
	self assert: ((103/107) storeStringBase: 15) = '(15r6D/15r72)'.
	self assert: ((109/113) printStringBase: 16) = '(6D/71)'.
	self assert: ((109/113) storeStringBase: 16) = '(16r6D/16r71)'.
	self assert: ((127/131) printStringBase: 17) = '(78/7C)'.
	self assert: ((127/131) storeStringBase: 17) = '(17r78/17r7C)'.
	self assert: ((137/139) printStringBase: 18) = '(7B/7D)'.
	self assert: ((137/139) storeStringBase: 18) = '(18r7B/18r7D)'.
	self assert: ((149/151) printStringBase: 19) = '(7G/7I)'.
	self assert: ((149/151) storeStringBase: 19) = '(19r7G/19r7I)'.
	self assert: ((157/163) printStringBase: 20) = '(7H/83)'.
	self assert: ((157/163) storeStringBase: 20) = '(20r7H/20r83)'.
	self assert: ((167/173) printStringBase: 21) = '(7K/85)'.
	self assert: ((167/173) storeStringBase: 21) = '(21r7K/21r85)'.
	self assert: ((179/181) printStringBase: 22) = '(83/85)'.
	self assert: ((179/181) storeStringBase: 22) = '(22r83/22r85)'.
	self assert: ((191/193) printStringBase: 23) = '(87/89)'.
	self assert: ((191/193) storeStringBase: 23) = '(23r87/23r89)'.
	self assert: ((197/199) printStringBase: 24) = '(85/87)'.
	self assert: ((197/199) storeStringBase: 24) = '(24r85/24r87)'.
	self assert: ((211/223) printStringBase: 25) = '(8B/8N)'.
	self assert: ((211/223) storeStringBase: 25) = '(25r8B/25r8N)'.
	self assert: ((227/229) printStringBase: 26) = '(8J/8L)'.
	self assert: ((227/229) storeStringBase: 26) = '(26r8J/26r8L)'.
	self assert: ((233/239) printStringBase: 27) = '(8H/8N)'.
	self assert: ((233/239) storeStringBase: 27) = '(27r8H/27r8N)'.
	self assert: ((241/251) printStringBase: 28) = '(8H/8R)'.
	self assert: ((241/251) storeStringBase: 28) = '(28r8H/28r8R)'.
	self assert: ((257/263) printStringBase: 29) = '(8P/92)'.
	self assert: ((257/263) storeStringBase: 29) = '(29r8P/29r92)'.
	self assert: ((269/271) printStringBase: 30) = '(8T/91)'.
	self assert: ((269/271) storeStringBase: 30) = '(30r8T/30r91)'.
	self assert: ((277/281) printStringBase: 31) = '(8T/92)'.
	self assert: ((277/281) storeStringBase: 31) = '(31r8T/31r92)'.
	self assert: ((283/293) printStringBase: 32) = '(8R/95)'.
	self assert: ((283/293) storeStringBase: 32) = '(32r8R/32r95)'.
	self assert: ((307/311) printStringBase: 33) = '(9A/9E)'.
	self assert: ((307/311) storeStringBase: 33) = '(33r9A/33r9E)'.
	self assert: ((313/317) printStringBase: 34) = '(97/9B)'.
	self assert: ((313/317) storeStringBase: 34) = '(34r97/34r9B)'.
	self assert: ((331/337) printStringBase: 35) = '(9G/9M)'.
	self assert: ((331/337) storeStringBase: 35) = '(35r9G/35r9M)'.
	self assert: ((347/349) printStringBase: 36) = '(9N/9P)'.
	self assert: ((347/349) storeStringBase: 36) = '(36r9N/36r9P)'.

	self assert: ((-2/3) printStringBase: 2) = '(-10/11)'.
	self assert: ((-2/3) storeStringBase: 2) = '(-2r10/2r11)'.
	self assert: ((5 / -7) printStringBase: 3) = '(-12/21)'.
	self assert: ((5 / -7) storeStringBase: 3) = '(-3r12/3r21)'.

]

{ #category : 'testing' }
FractionTest >> testHash [

	| a fraction1 fraction2 |
	fraction1 _ 1/3.
	fraction2 _ (1/3) + (1e-1000).
	self deny: fraction1 = fraction2 description: 'precondition'.
	self assert: fraction1 asFloat = fraction2 asFloat description: 'precondition'.
	a _ Set new.
	a add: fraction1; add: fraction2.
	self assert: a size = 2
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testInexactRaisedTo [
	"
	FractionTest new testInexactRaisedTo
	"
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) raisedTo: 1/3) = 1.0.
	self assert: (((1 << 1024 + 1) / (1 << 1024 + 3)) negated raisedTo: 1/3) = -1.0
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testInexactSqrt [
	"
	FractionTest new testInexactSqrt
	"
	self assert: ((1 << 1024 + 1) / (1 << 1024 + 3)) sqrt = 1.0
]

{ #category : 'tests - arithmetic' }
FractionTest >> testIntegerWholeDivision [
	
	self assert: 4 / (2/3) classAndValueEquals: 6.
	
	self assert: 4 / (-2/3) classAndValueEquals: -6.
	
	self assert: -4 / (-2/3) classAndValueEquals: 6.
	
	self assert: -4 / (2/3) classAndValueEquals: -6.
]

{ #category : 'tests - arithmetic' }
FractionTest >> testIntegerWholeMultiplication [
	
	self assert: 4 * (3/2) classAndValueEquals: 6.
	
	self assert: 4 * (-3/2) classAndValueEquals: -6.
	
	self assert: -4 * (-3/2) classAndValueEquals: 6.
	
	self assert: -4 * (3/2) classAndValueEquals: -6.
]

{ #category : 'testing' }
FractionTest >> testIsType [
	self assert: ((1 / 2100) is: #Fraction).
	self assert: ((1 / 2100) is: #Number).
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testLn [
	self verify: (1/100) ln isWithinOneFloatAwayFrom: -2 * 10 ln.
	self verify: ((2 raisedTo: Float emax + 11) / 3) ln isWithinOneFloatAwayFrom: (Float emax + 11) * 2 ln - 3 ln.
	self verify: (3 / (2 raisedTo: Float precision - Float emin)) ln isWithinOneFloatAwayFrom: (Float emin - Float precision) * 2 ln + 3 ln
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testLog [
	self assert: (1/100) log = -2.
	self verify: ((2 raisedTo: Float emax + 11) / 3) log isWithinOneFloatAwayFrom: (Float emax + 11) * 2 log - 3 log.
	self verify: (3 / (2 raisedTo: Float precision - Float emin)) log isWithinOneFloatAwayFrom: (Float emin - Float precision) * 2 log + 3 log.
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testNthRoot [
	self assert: ((-2 raisedTo: 35) / (3 raisedTo: 20) raisedTo: 1/5) equals: (-2 raisedTo: 7) / (3 raisedTo: 4).
	self assert: (1 / (1 << 2000) raisedTo: 1/100) equals: 1 / (1 << 20)
]

{ #category : 'tests - arithmetic' }
FractionTest >> testReciprocal [

	self 
		assert: (1/2) reciprocal classAndValueEquals: 2;
		assert: (3/4) reciprocal equals: (4/3);
		assert: (-1/3) reciprocal classAndValueEquals: -3;
		assert: (-3/5) reciprocal equals: (-5/3)
]

{ #category : 'tests - rounding' }
FractionTest >> testRoundHalfAwayFromZero [
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero"
	self assert: (1/2) roundedHAFZ = 1.
	self assert: (3/2) roundedHAFZ = 2.
	self assert: (-1/2) roundedHAFZ = -1.
	self assert: (-3/2) roundedHAFZ = -2.
	self assert: (((0 to: 5 by: 1/4) collect: [ :f | f roundedHAFZ ]) = #(0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5) ).
	self assert: (((0 to: -5 by: -1/4) collect: [ :f | f roundedHAFZ ]) = #(0 0 -1 -1 -1 -1 -2 -2 -2 -2 -3 -3 -3 -3 -4 -4 -4 -4 -5 -5 -5) ).
]

{ #category : 'tests - rounding' }
FractionTest >> testRoundHalfToEven [
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"
	self assert: (1/2) rounded = 0.
	self assert: (3/2) rounded = 2.
	self assert: (-1/2) rounded = 0.
	self assert: (-3/2) rounded = -2.
	self assert: (((0 to: 5 by: 1/4) collect: [ :f | f rounded ]) = #(0 0 0 1 1 1 2 2 2 2 2 3 3 3 4 4 4 4 4 5 5) ).
	self assert: (((0 to: -5 by: -1/4) collect: [ :f | f rounded ]) = #(0 0 0 -1 -1 -1 -2 -2 -2 -2 -2 -3 -3 -3 -4 -4 -4 -4 -4 -5 -5) ).
]

{ #category : 'tests - conversions' }
FractionTest >> testRounded [
	self assert: (4 / 5) rounded = 1.
	self assert: (6 / 5) rounded = 1.
	self assert: (-4 / 5) rounded = -1.
	self assert: (-6 / 5) rounded = -1.
	
	"In case of tie, round to upper magnitude"
	self assert: (3 / 2) rounded = 2.
	self assert: (-3 / 2) rounded = -2.
]

{ #category : 'tests - mathematical functions' }
FractionTest >> testSinCosForHugeValues1 [
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e22 + (10202/10000).
	equivalentFloatInTwoPiRange := 0.0.
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e22 + (10202/10000) + i.
		equivalentFloatInTwoPiRange := 0.0 + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].

]

{ #category : 'tests - mathematical functions' }
FractionTest >> testSinCosForHugeValues2 [
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e500 - (282532 / 100000).
	equivalentFloatInTwoPiRange := 0.0.
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e500 - (282532 / 100000) + i.
		equivalentFloatInTwoPiRange := 0.0 + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].

]

{ #category : 'tests - invariants' }
FractionTest >> testThatFractionDenominatorIsPositive [
	self assert: (-3 / 2) numerator negative description: 'a Fraction sign is allways carried by its numerator'.
	self assert: (-3 / 2) denominator positive description: 'a Fraction denominator is allways positive'.
	
	self assert: (3 / -2) numerator negative description: 'a Fraction sign is allways carried by its numerator'.
	self assert: (3 / -2) denominator positive description: 'a Fraction denominator is allways positive'.
	
	self assert: (-3 / -2) numerator positive description: 'two negative signs are simplified'.
	self assert: (-3 / -2) denominator positive description: 'a Fraction denominator is allways positive'.
]

{ #category : 'tests - invariants' }
FractionTest >> testThatFractionIsReduced [
	self assert: (4 / 6) numerator equals: 2.
	self assert: (4 / 6) denominator equals: 3.
	
	self assert: (4 / 2) classAndValueEquals: 2.
	
	"Fraction class>>#numerator:denominator: does not automatically reduce the Fraction.
	Since it does not guaranty above invariant, it must be used with care."
	self assert: (Fraction numerator: 4 denominator: 6) numerator equals: 4.
	self assert: (Fraction numerator: 4 denominator: 6) denominator equals: 6.
	self assert: (Fraction numerator: 4 denominator: 6) reduced numerator equals: 2.
	self assert: (Fraction numerator: 4 denominator: 6) reduced denominator equals: 3.
]

{ #category : 'tests - conversions' }
FractionTest >> testTruncated [
	self assert: (3 / 2) truncated = 1.
	self assert: (-3 / 2) truncated = -1.
]

{ #category : 'tests - arithmetic' }
FractionTest >> testWholeDifference [
	
	self assert: (2/3) - (5/3) classAndValueEquals: -1.
]

{ #category : 'tests - arithmetic' }
FractionTest >> testWholeDivision [
	
	self assert: (3/2) / (3/4) classAndValueEquals: 2.
	
	self assert: (3/2) / (-3/4) classAndValueEquals: -2.
	
	self assert: (-3/2) / (-3/4) classAndValueEquals: 2.
	
	self assert: (-3/2) / (3/4) classAndValueEquals: -2.
]

{ #category : 'tests - arithmetic' }
FractionTest >> testWholeMultiplication [
	
	self assert: (3/2) * (4/3) classAndValueEquals: 2.
	
	self assert: (3/2) * (-4/3) classAndValueEquals: -2.
	
	self assert: (-3/2) * (-4/3) classAndValueEquals: 2.
	
	self assert: (-3/2) * (4/3) classAndValueEquals: -2.
]

{ #category : 'tests - arithmetic' }
FractionTest >> testWholeSum [
	
	self assert: (5/3) + (1/3) classAndValueEquals: 2.
]
