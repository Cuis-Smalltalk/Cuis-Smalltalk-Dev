'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:35:07 pm'!
'Description '!
!provides: 'Tests-Cryptography-DigitalSignatures' 1 0!
!requires: 'Cryptography-DigitalSignatures' 1 14 nil!
SystemOrganization addCategory: 'Tests-Cryptography-DigitalSignatures'!


!classDefinition: #SecureHashAlgorithm256Test category: 'Tests-Cryptography-DigitalSignatures'!
TestCase subclass: #SecureHashAlgorithm256Test
	instanceVariableNames: 'hash usedClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Cryptography-DigitalSignatures'!
!classDefinition: 'SecureHashAlgorithm256Test class' category: 'Tests-Cryptography-DigitalSignatures'!
SecureHashAlgorithm256Test class
	instanceVariableNames: ''!

!classDefinition: #SecureHashAlgorithmTest category: 'Tests-Cryptography-DigitalSignatures'!
TestCase subclass: #SecureHashAlgorithmTest
	instanceVariableNames: 'hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Cryptography-DigitalSignatures'!
!classDefinition: 'SecureHashAlgorithmTest class' category: 'Tests-Cryptography-DigitalSignatures'!
SecureHashAlgorithmTest class
	instanceVariableNames: ''!

!classDefinition: #ThirtyTwoBitRegisterTest category: 'Tests-Cryptography-DigitalSignatures'!
TestCase subclass: #ThirtyTwoBitRegisterTest
	instanceVariableNames: 'random'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Cryptography-DigitalSignatures'!
!classDefinition: 'ThirtyTwoBitRegisterTest class' category: 'Tests-Cryptography-DigitalSignatures'!
ThirtyTwoBitRegisterTest class
	instanceVariableNames: ''!


!SecureHashAlgorithmTest commentStamp: '<historical>' prior: 0!
This is the unit test for the class SecureHashAlgorithm. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- http://minnow.cc.gatech.edu/squeak/1547
	- the sunit class category!

!SecureHashAlgorithm256Test methodsFor: 'as yet unclassified' stamp: 'jpb 4/25/2020 19:18:23'!
setUp
	usedClass _ SecureHashAlgorithm256 ! !

!SecureHashAlgorithm256Test methodsFor: 'as yet unclassified' stamp: 'jpb 4/25/2020 18:25:31'!
testEmptyInput
	self assert: 'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855'
		equals: ((usedClass new hashMessage: '') printStringBase: 16).! !

!SecureHashAlgorithm256Test methodsFor: 'as yet unclassified' stamp: 'jpb 4/30/2020 17:26:02'!
testFoobar2Input
	| message |
	message _ #[16r66 16r6F 16r6F 16r62 16r61 16r72]. "foobar"
	
	self assert: 'C3AB8FF13720E8AD9047DD39466B3C8974E592C2FA383D4A3960714CAEF0C4F2' 
		equals: ((usedClass new hashMessage: message) printStringBase: 16).! !

!SecureHashAlgorithm256Test methodsFor: 'as yet unclassified' stamp: 'jpb 4/30/2020 17:24:22'!
testFoobarInput
	| message |
	message _ #[16r66 16r6F 16r6F 16r62 16r61 16r72 16r0A]. "foobar\n"
	
	self assert:  'AEC070645FE53EE3B3763059376134F058CC337247C978ADD178B6CCDFB0019F' 
		equals: ((usedClass new hashMessage: message) printStringBase: 16).! !

!SecureHashAlgorithm256Test methodsFor: 'as yet unclassified' stamp: 'jpb 5/3/2020 22:25:32'!
testUsingTestVector1
	"Source : https://www.di-mgt.com.au/sha_testvectors.html"
	| message |
	message _ 'abc'.
	
	self assert: 'BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD'
		equals: ((usedClass new hashMessage: message) printStringBase: 16).! !

!SecureHashAlgorithm256Test methodsFor: 'as yet unclassified' stamp: 'jpb 4/30/2020 17:33:05'!
testUsingTestVector3
	"Source : https://www.di-mgt.com.au/sha_testvectors.html"
	| message |
	message _ 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'.
	
	self assert: '248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1'
		equals: ((usedClass new hashMessage: message) printStringBase: 16).! !

!SecureHashAlgorithm256Test methodsFor: 'as yet unclassified' stamp: 'jpb 4/30/2020 17:36:04'!
testUsingTestVector4
	"Source : https://www.di-mgt.com.au/sha_testvectors.html"
	| message |
	message _ 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'.
	
	self assert: 'CF5B16A778AF8380036CE59E7B0492370B249B11E8F07A51AFAC45037AFEE9D1'
		equals: ((usedClass new hashMessage: message) printStringBase: 16).! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'DSG 3/28/2015 15:02'!
testEmptyInput
	self assert: ((SecureHashAlgorithm new hashMessage: '') printStringBase: 16)
			= 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709'! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:23'!
testExample1

	"This is the first example from the specification document (FIPS PUB 180-1)"

	hash := SecureHashAlgorithm new hashMessage: 'abc'.
	self assert: (hash = 16rA9993E364706816ABA3E25717850C26C9CD0D89D).
		! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:23'!
testExample2

	"This is the second example from the specification document (FIPS PUB 180-1)"

	hash := SecureHashAlgorithm new hashMessage:
		'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'.
	self assert: (hash = 16r84983E441C3BD26EBAAE4AA1F95129E5E54670F1).! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:25'!
testExample3

	"This is the third example from the specification document (FIPS PUB 180-1). 
	This example may take several minutes."

	hash := SecureHashAlgorithm new hashMessage: (String new: 1000000 withAll: $a).
	self assert: (hash = 16r34AA973CD4C4DAA4F61EEB2BDBAD27316534016F).! !

!ThirtyTwoBitRegisterTest methodsFor: 'helpers' stamp: 'ul 4/11/2015 04:21'!
assertBinaryRegisterOperation: registerOperationBlock gives: integerOperationBlock times: n

	| rx ry |
	rx := ThirtyTwoBitRegister new.
	ry := rx copy.
	n timesRepeat: [
		| x y expectedResult |
		x := self nextRandom.
		y := self nextRandom.
		expectedResult := integerOperationBlock value: x value: y .
		rx load: x.
		ry load: y.
		registerOperationBlock value: rx value: ry.
		self assert: expectedResult equals: rx asInteger ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'helpers' stamp: 'ul 4/11/2015 04:48'!
assertComparisonRegisterOperation: registerOperationBlock gives: integerOperationBlock times: n

	| rx ry |
	rx := ThirtyTwoBitRegister new.
	ry := rx copy.
	n timesRepeat: [
		| x y expectedResult actualResult |
		x := self nextRandom.
		y := self nextRandom.
		expectedResult := integerOperationBlock value: x value: y .
		rx load: x.
		ry load: y.
		actualResult := registerOperationBlock value: rx value: ry.
		self assert: expectedResult equals: actualResult ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'helpers' stamp: 'ul 4/11/2015 04:22'!
assertUnaryRegisterOperation: registerOperationBlock gives: integerOperationBlock times: n

	| rx |
	rx := ThirtyTwoBitRegister new.
	n timesRepeat: [
		| x expectedResult |
		x := self nextRandom.
		expectedResult := integerOperationBlock value: x.
		rx load: x.
		registerOperationBlock value: rx.
		self assert: expectedResult equals: rx asInteger ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'helpers' stamp: 'jpb 4/30/2020 18:37:57'!
nextRandom
	"Return the next random 32-bit unsigned integer value."

	^(random nextInteger: 4294967296) - 1! !

!ThirtyTwoBitRegisterTest methodsFor: 'running' stamp: 'jpb 4/30/2020 18:37:42'!
setUp

	random := Random seed: 36rSQUEAK! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:19'!
testAddition

	self
		assertBinaryRegisterOperation: [ :rx :ry | rx += ry ]
		gives: [ :x :y | x + y bitAnd: 16rFFFFFFFF ]
		times: 10000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:19'!
testBitAnd

	self
		assertBinaryRegisterOperation: [ :rx :ry | rx bitAnd: ry ]
		gives: [ :x :y | x bitAnd: y ]
		times: 10000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:26'!
testBitInvert

	self
		assertUnaryRegisterOperation: [ :rx | rx bitInvert ]
		gives: [ :x | x bitInvert32 ]
		times: 10000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:20'!
testBitOr

	self
		assertBinaryRegisterOperation: [ :rx :ry | rx bitOr: ry ]
		gives: [ :x :y | x bitOr: y ]
		times: 10000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:20'!
testBitXor

	self
		assertBinaryRegisterOperation: [ :rx :ry | rx bitXor: ry ]
		gives: [ :x :y | x bitXor: y ]
		times: 10000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:36'!
testLeftRotateBy

	0 to: 33 do: [ :shift |
		self
			assertUnaryRegisterOperation: [ :rx | rx leftRotateBy: shift ]
			gives: [ :x | 
				| actualShift |
				actualShift := shift \\ 32.
				(x << actualShift bitOr: x >> (32 - actualShift)) bitAnd: 16rFFFFFFFF ]
			times: 1000 ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:25'!
testLeftShift

	0 to: 33 do: [ :shift |
		self
			assertUnaryRegisterOperation: [ :rx | rx << shift ]
			gives: [ :x | x << shift bitAnd: 16rFFFFFFFF ]
			times: 1000 ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:20'!
testMultiplication

	self
		assertBinaryRegisterOperation: [ :rx :ry | rx *= ry ]
		gives: [ :x :y | x * y bitAnd: 16rFFFFFFFF ]
		times: 10000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'jpb 5/1/2020 14:03:07'!
testRightRotateBy

	0 to: 33 do: [ :shift |
		self
			assertUnaryRegisterOperation: [ :rx | rx rightRotateBy: shift ]
			gives: [ :x | 
				| actualShift |
				actualShift _ shift \\ 32.
				(x >> actualShift bitOr: x << (32 - actualShift))
					bitAnd: 16rFFFFFFFF ]
			times: 1000 ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - accumulator ops' stamp: 'ul 4/11/2015 04:25'!
testRightShift

	0 to: 33 do: [ :shift |
		self
			assertUnaryRegisterOperation: [ :rx | rx >> shift ]
			gives: [ :x | x >> shift bitAnd: 16rFFFFFFFF ]
			times: 1000 ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - comparison' stamp: 'ul 4/11/2015 04:50'!
testEquals

	self
		assertComparisonRegisterOperation: [ :rx :ry | rx = ry ]
		gives: [ :x :y | x = y ]
		times: 1000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - comparison' stamp: 'ul 4/11/2015 04:51'!
testGreater

	self
		assertComparisonRegisterOperation: [ :rx :ry | rx > ry ]
		gives: [ :x :y | x > y ]
		times: 1000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - comparison' stamp: 'ul 4/11/2015 04:51'!
testGreaterOrEqual

	self
		assertComparisonRegisterOperation: [ :rx :ry | rx >= ry ]
		gives: [ :x :y | x >= y ]
		times: 1000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - comparison' stamp: 'ul 4/11/2015 04:49'!
testLess

	self
		assertComparisonRegisterOperation: [ :rx :ry | rx < ry ]
		gives: [ :x :y | x < y ]
		times: 1000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests - comparison' stamp: 'ul 4/11/2015 04:50'!
testLessOrEqual

	self
		assertComparisonRegisterOperation: [ :rx :ry | rx <= ry ]
		gives: [ :x :y | x <= y ]
		times: 1000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests' stamp: 'ul 4/11/2015 04:32'!
testHi

	self
		assertUnaryRegisterOperation: [ :rx | rx load: rx hi ]
		gives: [ :x | x bitShift: -16 ]
		times: 1000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests' stamp: 'ul 4/11/2015 04:29'!
testLoad

	1000 timesRepeat: [
		| value |
		value := self nextRandom.
		self
			assertUnaryRegisterOperation: [ :rx | rx load: value ]
			gives: [ :x | value ]
			times: 1 ]! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests' stamp: 'ul 4/11/2015 04:29'!
testLoadFrom

	self
		assertBinaryRegisterOperation: [ :rx :ry | rx loadFrom: ry ]
		gives: [ :x :y | y ]
		times: 10000! !

!ThirtyTwoBitRegisterTest methodsFor: 'tests' stamp: 'ul 4/11/2015 04:32'!
testLow

	self
		assertUnaryRegisterOperation: [ :rx | rx load: rx low ]
		gives: [ :x | x bitAnd: 16rFFFF ]
		times: 1000! !
