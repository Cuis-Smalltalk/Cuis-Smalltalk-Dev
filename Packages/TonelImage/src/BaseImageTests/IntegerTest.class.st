Class {
	#name : 'IntegerTest',
	#superclass : 'AbstractNumberTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'private' }
IntegerTest >> assert: a classAndValueEquals: b [
	self assert: a class = b class.
	self assert: a = b
]

{ #category : 'as yet unclassified' }
IntegerTest >> test01readFromWithEmptyStringRaisesAnError [
	|input|
	
	input := ReadStream on: ''.

	self should: [ Integer readFrom: input ] raise: Error description: 'At least one digit expected here'
]

{ #category : 'as yet unclassified' }
IntegerTest >> test02readFromWithInvalidStringRaisesAnError [
	| input |
	
	input := ReadStream on: 'hello'.

	self should: [ Integer readFrom: input ] raise: Error description: 'At least one digit expected here'
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLnLn [

	self assert: -42391158275216203514294433200 \\ -14130386091738734504764811067 = -14130386091738734504764811066.
	self assert: -42391158275216203514294433201 \\ -14130386091738734504764811067 = 0.
	self assert: -42391158275216203514294433202 \\ -14130386091738734504764811067 = -1.
	self assert: -8727963568087712425891397479476727340041448 \\ -79766443076872509863361 = -79766443076872509863360.
	self assert: -8727963568087712425891397479476727340041449 \\ -79766443076872509863361 = 0.
	self assert: -8727963568087712425891397479476727340041450 \\ -79766443076872509863361 = -1
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLnLp [

	self assert: -42391158275216203514294433200 \\ 14130386091738734504764811067 = 1.
	self assert: -42391158275216203514294433201 \\ 14130386091738734504764811067 = 0.
	self assert: -42391158275216203514294433202 \\ 14130386091738734504764811067 = 14130386091738734504764811066.
	self assert: -8727963568087712425891397479476727340041448 \\ 79766443076872509863361 = 1.
	self assert: -8727963568087712425891397479476727340041449 \\ 79766443076872509863361 = 0.
	self assert: -8727963568087712425891397479476727340041450 \\ 79766443076872509863361 = 79766443076872509863360
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLnSn [

	self assert: -42391158275216203514294433201 \\ -3 = 0.
	self assert: -42391158275216203514294433202 \\ -3 = -1.
	self assert: -42391158275216203514294433203 \\ -3 = -2.
	self assert: -42391158275216203514294433204 \\ -3 = 0
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLnSp [

	self assert: -42391158275216203514294433201 \\ 3 = 0.
	self assert: -42391158275216203514294433202 \\ 3 = 2.
	self assert: -42391158275216203514294433203 \\ 3 = 1.
	self assert: -42391158275216203514294433204 \\ 3 = 0
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLpLn [

	self assert: 42391158275216203514294433200 \\ -14130386091738734504764811067 = -1.
	self assert: 42391158275216203514294433201 \\ -14130386091738734504764811067 = 0.
	self assert: 42391158275216203514294433202 \\ -14130386091738734504764811067 = -14130386091738734504764811066.
	self assert: 8727963568087712425891397479476727340041448 \\ -79766443076872509863361 = -1.
	self assert: 8727963568087712425891397479476727340041449 \\ -79766443076872509863361 = 0.
	self assert: 8727963568087712425891397479476727340041450 \\ -79766443076872509863361 = -79766443076872509863360
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLpLp [

	self assert: 42391158275216203514294433200 \\ 14130386091738734504764811067 = 14130386091738734504764811066.
	self assert: 42391158275216203514294433201 \\ 14130386091738734504764811067 = 0.
	self assert: 42391158275216203514294433202 \\ 14130386091738734504764811067 = 1.
	self assert: 8727963568087712425891397479476727340041448 \\ 79766443076872509863361 = 79766443076872509863360.
	self assert: 8727963568087712425891397479476727340041449 \\ 79766443076872509863361 = 0.
	self assert: 8727963568087712425891397479476727340041450 \\ 79766443076872509863361 = 1
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLpSn [

	self assert: 42391158275216203514294433201 \\ -3 = 0.
	self assert: 42391158275216203514294433202 \\ -3 = -2.
	self assert: 42391158275216203514294433203 \\ -3 = -1.
	self assert: 42391158275216203514294433204 \\ -3 = 0
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashLpSp [

	self assert: 42391158275216203514294433201 \\ 3 = 0.
	self assert: 42391158275216203514294433202 \\ 3 = 1.
	self assert: 42391158275216203514294433203 \\ 3 = 2.
	self assert: 42391158275216203514294433204 \\ 3 = 0
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSnLn [

	self assert: 0 \\ -42391158275216203514294433201 = 0.
	self assert: -1 \\ -42391158275216203514294433201 = -1.
	self assert: -14348907 \\ -42391158275216203514294433201 = -14348907
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSnLp [

	self assert: 0 \\ 42391158275216203514294433201 = 0.
	self assert: -1 \\ 42391158275216203514294433201 =  42391158275216203514294433200.
	self assert: -14348907 \\ 42391158275216203514294433201 = 42391158275216203514280084294
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSnSn [

	self assert: 0 \\ -3 = 0.
	self assert: -1 \\ -3 = -1.
	self assert: -9 \\ -3 = 0.
	self assert: -10 \\ -3 = -1.
	self assert: -11 \\ -3 = -2.
	self assert: -12 \\ -3 = 0.
	self assert: -13 \\ -3 = -1
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSnSp [

	self assert: 0 \\ 3 = 0.
	self assert: -1 \\ 3 = 2.
	self assert: -9 \\ 3 = 0.
	self assert: -10 \\ 3 = 2.
	self assert: -11 \\ 3 = 1.
	self assert: -12 \\ 3 = 0.
	self assert: -13 \\ 3 = 2
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSpLn [

	self assert: 0 \\ -42391158275216203514294433201 = 0.
	self assert: 1 \\ -42391158275216203514294433201 = -42391158275216203514294433200.
	self assert: 14348907 \\ -42391158275216203514294433201 = -42391158275216203514280084294
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSpLp [

	self assert: 0 \\ 42391158275216203514294433201 = 0.
	self assert: 1 \\ 42391158275216203514294433201 = 1.
	self assert: 14348907 \\ 42391158275216203514294433201 = 14348907
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSpSn [

	self assert: 0 \\ -3 = 0.
	self assert: 1 \\ -3 = -2.
	self assert: 9 \\ -3 = 0.
	self assert: 10 \\ -3 = -2.
	self assert: 11 \\ -3 = -1.
	self assert: 12 \\ -3 = 0.
	self assert: 13 \\ -3 = -2
]

{ #category : 'tests - division - \\' }
IntegerTest >> testBackslashBackslashSpSp [

	self assert: 0 \\ 3 = 0.
	self assert: 1 \\ 3 = 1.
	self assert: 9 \\ 3 = 0.
	self assert: 10 \\ 3 = 1.
	self assert: 11 \\ 3 = 2.
	self assert: 12 \\ 3 = 0.
	self assert: 13 \\ 3 = 1
]

{ #category : 'tests - benchmarks' }
IntegerTest >> testBenchFib [

	self assert: (0 benchFib = 1).
	self assert: (1 benchFib = 1).
	self assert: (2 benchFib = 3).
	
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testBigReceiverInexactNthRoot [
	"
	IntegerTest new testBigReceiverInexactNthRoot
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."
	| bigNum result |
	bigNum _ (100 factorial raisedTo: 3) + 1.		"Add 1 so it is not a whole cube"
	self assert: bigNum asFloat isInfinite.			"Otherwise, we chose a bad sample"
	result _ bigNum nthRoot: 3.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result = 100 factorial asFloat.		"No other float is closer. See following line"
	self assert: 100 factorial asFloat = (100 factorial+1) asFloat
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testBigReceiverInexactSqrt [
	"
	IntegerTest new testBigReceiverInexactSqrt
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."
	| bigNum result |
	bigNum _ 100 factorial squared + 1.		"Add 1 so it is not a whole square"
	self assert: bigNum asFloat isInfinite.			"Otherwise, we chose a bad sample"
	result _ bigNum sqrt.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result = 100 factorial asFloat.		"No other float is closer. See following lines"
	self assert: (result successor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs.
	self assert: (result predecessor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs.
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testBitAt [
	| trials bitSequence2 |

	self
		assert: ((1 to: 100) allSatisfy: [:i | (0 bitAt: i) = 0])
		description: 'all bits of zero are set to zero'.
	
	self
		assert: ((1 to: 100) allSatisfy: [:i | (-1 bitAt: i) = 1])
		description: 'In two complements, all bits of -1 are set to 1'.
		
	
	trials := #(
		'2r10010011'
		'2r11100100'
		'2r10000000'
		'2r0000101011011001'
		'2r1000101011011001'
		'2r0101010101011000'
		'2r0010011110110010'
		'2r0010011000000000'
		'2r00100111101100101000101011011001'
		'2r01110010011110110010100110101101'
		'2r10101011101011001010000010110110'
		'2r10101000000000000000000000000000'
		'2r0010101110101001110010100000101101100010011110110010100010101100'
		'2r1010101110101100101000001011011000100111101100101000101011011001'
		'2r1010101110101000000000000000000000000000000000000000000000000000').
	trials do: [:bitSequence | | aNumber |
		aNumber := Number readFrom: bitSequence.
		bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [:string :i | string copyWith: (Character digitValue: (aNumber bitAt: i))].
		self assert: bitSequence2 = bitSequence].
	
	trials do: [:bitSequence | | bitInvert |
		bitInvert := -1 - (Number readFrom: bitSequence).
		bitSequence2 := (bitSequence size - 2 to: 1 by: -1) inject: '2r' into: [:string :i | string copyWith: (Character digitValue: 1 - (bitInvert bitAt: i))].
		self assert: bitSequence2 = bitSequence description: '-1-x is similar to a bitInvert operation in two complement']
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testBitLogic   [
	"This little suite of tests is designed to verify correct operation of most
	of Squeak's bit manipulation code, including two's complement
	representation of negative values.  It was written in a hurry and
	is probably lacking several important checks."

	"Shift 1 bit left then right and test for 1"
	"self run: #testBitLogic"
	| n |
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitShift: i negated) = 1].

	"Shift -1 left then right and test for 1"
	1 to: 100 do: [:i | self assert: ((-1 bitShift: i) bitShift: i negated) = -1].

	"And a single bit with -1 and test for same value"
	1 to: 100 do: [:i | self assert: ((1 bitShift: i) bitAnd: -1) = (1 bitShift: i)].

	"Verify that (n bitAnd: n negated) = n for single bits"
	1 to: 100 do: [:i |  n := 1 bitShift: i. self assert: (n bitAnd: n negated) = n].

	"Verify that n negated = (n complemented + 1) for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i. 
					self assert: n negated = ((n bitXor: -1) + 1)].

	"Verify that (n + n complemented) = -1 for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i.
					self assert: (n + (n bitXor: -1)) = -1].

	"Verify that n negated = (n complemented +1) for single bits"
	1 to: 100 do: [:i | 
					n := 1 bitShift: i.
					self assert: n negated = ((n bitXor: -1) + 1)].

	self assert: (-2 bitAnd: 16rFFFFFFFF) = 16rFFFFFFFE.
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testBitReversal [
	{ 0. 1. SmallInteger maxVal-1. SmallInteger maxVal. SmallInteger maxVal+1. (2 raisedTo: 64)-1091. (2 raisedTo: 64)-1090. (2 raisedTo: 64)-1 } do: 
		[ : fixture |
		| printedThenReversed reversedThenPrinted |
		printedThenReversed := (fixture printStringBase: 2 length: 64 padded: true) reversed.
		reversedThenPrinted := (fixture bitReverse: 64) printStringBase: 2 length: 64 padded: true.
		self assert: printedThenReversed = reversedThenPrinted ]
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testBitReversalCorrectClass [
	"This test was added due to a bug!
	The problem only happened in 64 bit systems running with jitted Cog VM."

	| a b |
	a _ 16r944F245FDBB2B06.
	10 timesRepeat: [
		b _ (a bitReverse: 64) bitReverse: 64.
		self assert: a class = b class ]
]

{ #category : 'testing - arithmetic' }
IntegerTest >> testCrossSumBase [
	"self run: #testCrossSumBase"

	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 10]) asArray = 
		#(2 10 9 8 7 6 5 4 3 2 1 9 8 7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 10 2)).
	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 2]) asArray = 
		#(2 3 2 2 1 4 3 3 2 3 2 2 1 3 2 2 1 2 1 1 0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2)).
	self should: [10 crossSumBase: 1] raise: AssertionFailure
]

{ #category : 'tests - instance creation' }
IntegerTest >> testDifferentBases [
	"self run: #testDifferentBases"
	"| value |
	2 to: 36 do: [:each|
		value := 0.
		1 to: each-1 do: [:n| value := value + (n * (each raisedToInteger: n))].
		value := value negated.
		Transcript tab; show: 'self assert: (', value printString, ' printStringBase: ', each printString, ') = ''', (value printStringBase: each), '''.'; cr.
		Transcript tab; show: 'self assert: (', value printString, ' radix: ', each printString, ') = ''', (value radix: each), '''.'; cr.
		Transcript tab; show: 'self assert: ', value printString, ' printStringHex = ''', (value printStringBase: 16), '''.'; cr.
		Transcript tab; show: 'self assert: (', value printString, ' storeStringBase: ', each printString, ') = ''', (value storeStringBase: each), '''.'; cr.
		Transcript tab; show: 'self assert: ', value printString, ' storeStringHex = ''', (value storeStringBase: 16), '''.'; cr.


].
	"

	self assert: 2r10 = 2.
	self assert: 3r210 = 21.
	self assert: 4r3210 = 228.
	self assert: 5r43210 = 2930.
	self assert: 6r543210 = 44790.
	self assert: 7r6543210 = 800667.
	self assert: 8r76543210 = 16434824.
	self assert: 9r876543210 = 381367044.
	self assert: 10r9876543210 = 9876543210.
	self assert: 11rA9876543210 = 282458553905.
	self assert: 12rBA9876543210 = 8842413667692.
	self assert: 13rCBA9876543210 = 300771807240918.
	self assert: 14rDCBA9876543210 = 11046255305880158.
	self assert: 15rEDCBA9876543210 = 435659737878916215.
	self assert: 16rFEDCBA9876543210 = 18364758544493064720.
	self assert: 17rGFEDCBA9876543210 = 824008854613343261192.
	self assert: 18rHGFEDCBA9876543210 = 39210261334551566857170.
	self assert: 19rIHGFEDCBA9876543210 = 1972313422155189164466189.
	self assert: 20rJIHGFEDCBA9876543210 = 104567135734072022160664820.
	self assert: 21rKJIHGFEDCBA9876543210 = 5827980550840017565077671610.
	self assert: 22rLKJIHGFEDCBA9876543210 = 340653664490377789692799452102.
	self assert: 23rMLKJIHGFEDCBA9876543210 = 20837326537038308910317109288851.
	self assert: 24rNMLKJIHGFEDCBA9876543210 = 1331214537196502869015340298036888.
	self assert: 25rONMLKJIHGFEDCBA9876543210 = 88663644327703473714387251271141900.
	self assert: 26rPONMLKJIHGFEDCBA9876543210 = 6146269788878825859099399609538763450.
	self assert: 27rQPONMLKJIHGFEDCBA9876543210 = 442770531899482980347734468443677777577.
	self assert: 28rRQPONMLKJIHGFEDCBA9876543210 = 33100056003358651440264672384704297711484.
	self assert: 29rSRQPONMLKJIHGFEDCBA9876543210 = 2564411043271974895869785066497940850811934.
	self assert: 30rTSRQPONMLKJIHGFEDCBA9876543210 = 205646315052919334126040428061831153388822830.
	self assert: 31rUTSRQPONMLKJIHGFEDCBA9876543210 = 17050208381689099029767742314582582184093573615.
	self assert: 32rVUTSRQPONMLKJIHGFEDCBA9876543210 = 1459980823972598128486511383358617792788444579872.
	self assert: 33rWVUTSRQPONMLKJIHGFEDCBA9876543210 = 128983956064237823710866404905431464703849549412368.
	self assert: 34rXWVUTSRQPONMLKJIHGFEDCBA9876543210 = 11745843093701610854378775891116314824081102660800418.
	self assert: 35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = 1101553773143634726491620528194292510495517905608180485.
	self assert: 36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = 106300512100105327644605138221229898724869759421181854980.

	self assert: -2r10 = -2.
	self assert: -3r210 = -21.
	self assert: -4r3210 = -228.
	self assert: -5r43210 = -2930.
	self assert: -6r543210 = -44790.
	self assert: -7r6543210 = -800667.
	self assert: -8r76543210 = -16434824.
	self assert: -9r876543210 = -381367044.
	self assert: -10r9876543210 = -9876543210.
	self assert: -11rA9876543210 = -282458553905.
	self assert: -12rBA9876543210 = -8842413667692.
	self assert: -13rCBA9876543210 = -300771807240918.
	self assert: -14rDCBA9876543210 = -11046255305880158.
	self assert: -15rEDCBA9876543210 = -435659737878916215.
	self assert: -16rFEDCBA9876543210 = -18364758544493064720.
	self assert: -17rGFEDCBA9876543210 = -824008854613343261192.
	self assert: -18rHGFEDCBA9876543210 = -39210261334551566857170.
	self assert: -19rIHGFEDCBA9876543210 = -1972313422155189164466189.
	self assert: -20rJIHGFEDCBA9876543210 = -104567135734072022160664820.
	self assert: -21rKJIHGFEDCBA9876543210 = -5827980550840017565077671610.
	self assert: -22rLKJIHGFEDCBA9876543210 = -340653664490377789692799452102.
	self assert: -23rMLKJIHGFEDCBA9876543210 = -20837326537038308910317109288851.
	self assert: -24rNMLKJIHGFEDCBA9876543210 = -1331214537196502869015340298036888.
	self assert: -25rONMLKJIHGFEDCBA9876543210 = -88663644327703473714387251271141900.
	self assert: -26rPONMLKJIHGFEDCBA9876543210 = -6146269788878825859099399609538763450.
	self assert: -27rQPONMLKJIHGFEDCBA9876543210 = -442770531899482980347734468443677777577.
	self assert: -28rRQPONMLKJIHGFEDCBA9876543210 = -33100056003358651440264672384704297711484.
	self assert: -29rSRQPONMLKJIHGFEDCBA9876543210 = -2564411043271974895869785066497940850811934.
	self assert: -30rTSRQPONMLKJIHGFEDCBA9876543210 = -205646315052919334126040428061831153388822830.
	self assert: -31rUTSRQPONMLKJIHGFEDCBA9876543210 = -17050208381689099029767742314582582184093573615.
	self assert: -32rVUTSRQPONMLKJIHGFEDCBA9876543210 = -1459980823972598128486511383358617792788444579872.
	self assert: -33rWVUTSRQPONMLKJIHGFEDCBA9876543210 = -128983956064237823710866404905431464703849549412368.
	self assert: -34rXWVUTSRQPONMLKJIHGFEDCBA9876543210 = -11745843093701610854378775891116314824081102660800418.
	self assert: -35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = -1101553773143634726491620528194292510495517905608180485.
	self assert: -36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210 = -106300512100105327644605138221229898724869759421181854980.
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLnLn [

	self assert: (-42391158275216203514294433200 div: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433201 div: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433202 div: -14130386091738734504764811067) = 4.
	self assert: (-8727963568087712425891397479476727340041448 div: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041449 div: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 div: -79766443076872509863361) = 109418989131512359210
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLnLp [

	self assert: (-42391158275216203514294433200 div: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433201 div: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433202 div: 14130386091738734504764811067) = -4.
	self assert: (-8727963568087712425891397479476727340041448 div: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041449 div: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 div: 79766443076872509863361) = -109418989131512359210
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLnSn [

	self assert: (-42391158275216203514294433201 div: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 div: -3) = 14130386091738734504764811068.
	self assert: (-42391158275216203514294433203 div: -3) = 14130386091738734504764811068.
	self assert: (-42391158275216203514294433204 div: -3) = 14130386091738734504764811068
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLnSp [

	self assert: (-42391158275216203514294433201 div: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 div: 3) = -14130386091738734504764811068.
	self assert: (-42391158275216203514294433203 div: 3) = -14130386091738734504764811068.
	self assert: (-42391158275216203514294433204 div: 3) = -14130386091738734504764811068
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLpLn [

	self assert: (42391158275216203514294433200 div: -14130386091738734504764811067) = -2.
	self assert: (42391158275216203514294433201 div: -14130386091738734504764811067) = -3.
	self assert: (42391158275216203514294433202 div: -14130386091738734504764811067) = -3.
	self assert: (8727963568087712425891397479476727340041448 div: -79766443076872509863361) = -109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 div: -79766443076872509863361) = -109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 div: -79766443076872509863361) = -109418989131512359209
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLpLp [

	self assert: (42391158275216203514294433200 div: 14130386091738734504764811067) = 2.
	self assert: (42391158275216203514294433201 div: 14130386091738734504764811067) = 3.
	self assert: (42391158275216203514294433202 div: 14130386091738734504764811067) = 3.
	self assert: (8727963568087712425891397479476727340041448 div: 79766443076872509863361) = 109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 div: 79766443076872509863361) = 109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 div: 79766443076872509863361) = 109418989131512359209
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLpSn [

	self assert: (42391158275216203514294433201 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433202 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433203 div: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433204 div: -3) = -14130386091738734504764811068
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivLpSp [

	self assert: (42391158275216203514294433201 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433202 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433203 div: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433204 div: 3) = 14130386091738734504764811068
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSnLn [

	self assert: (0 div: -42391158275216203514294433201) = 0.
	self assert: (-1 div: -42391158275216203514294433201) = 1.
	self assert: (-14348907 div: -42391158275216203514294433201) = 1
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSnLp [

	self assert: (0 div: 42391158275216203514294433201) = 0.
	self assert: (-1 div: 42391158275216203514294433201) = -1.
	self assert: (-14348907 div: 42391158275216203514294433201) = -1
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSnSn [

	self assert: (0 div: -3) = 0.
	self assert: (-1 div: -3) = 1.
	self assert: (-9 div: -3) = 3.
	self assert: (-10 div: -3) = 4.
	self assert: (-11 div: -3) = 4.
	self assert: (-12 div: -3) = 4.
	self assert: (-13 div: -3) = 5
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSnSp [

	self assert: (0 div: 3) = 0.
	self assert: (-1 div: 3) = -1.
	self assert: (-9 div: 3) = -3.
	self assert: (-10 div: 3) = -4.
	self assert: (-11 div: 3) = -4.
	self assert: (-12 div: 3) = -4.
	self assert: (-13 div: 3) = -5
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSpLn [

	self assert: (0 div: -42391158275216203514294433201) = 0.
	self assert: (1 div: -42391158275216203514294433201) = 0.
	self assert: (14348907 div: -42391158275216203514294433201) = 0
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSpLp [

	self assert: (0 div: 42391158275216203514294433201) = 0.
	self assert: (1 div: 42391158275216203514294433201) = 0.
	self assert: (14348907 div: 42391158275216203514294433201) = 0
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSpSn [

	self assert: (0 div: -3) = 0.
	self assert: (1 div: -3) = 0.
	self assert: (9 div: -3) = -3.
	self assert: (10 div: -3) = -3.
	self assert: (11 div: -3) = -3.
	self assert: (12 div: -3) = -4.
	self assert: (13 div: -3) = -4
]

{ #category : 'tests - division - div:' }
IntegerTest >> testDivSpSp [

	self assert: (0 div: 3) = 0.
	self assert: (1 div: 3) = 0.
	self assert: (9 div: 3) = 3.
	self assert: (10 div: 3) = 3.
	self assert: (11 div: 3) = 3.
	self assert: (12 div: 3) = 4.
	self assert: (13 div: 3) = 4
]

{ #category : 'tests - basic' }
IntegerTest >> testEven [
	
	self deny: (1073741825 even).
	self assert: (1073741824  even).
	
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testExactRaisedTo [
	"
	IntegerTest new testExactRaisedTo
	"
	self assert: (4 raisedTo: 1/2) classAndValueEquals: 2.
	self assert: (9 raisedTo: 1/2) classAndValueEquals: 3.
	self assert: (9 raisedTo: -1/2) classAndValueEquals: 1/3.
	self assert: (-1 raisedTo: 1/3) classAndValueEquals: -1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: (i squared raisedTo: 1/2) classAndValueEquals: i.
		self assert: (i negated squared raisedTo: 1/2) classAndValueEquals: i ].

	self assert: (8 raisedTo: 1/3) classAndValueEquals: 2.
	self assert: (27 raisedTo: 1/3) classAndValueEquals: 3.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i.
		self assert: ((i negated raisedTo: 3) raisedTo: 1/3) classAndValueEquals: i negated ].

	self assert: (4 raisedTo: 3/2) classAndValueEquals: 8.
	self assert: (8 raisedTo: 2/3) classAndValueEquals: 4.
	self assert: (8 raisedTo: -2/3) classAndValueEquals: 1/4.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i.
		self assert: ((i negated raisedTo: 3) raisedTo: 2/3) classAndValueEquals: i*i.
		self assert: ((i negated raisedTo: 2) raisedTo: 3/2) classAndValueEquals: i*i*i ].

	self assert: (32 raisedTo: 3/5) classAndValueEquals: 8.
	self assert: (8 raisedTo: 5/3) classAndValueEquals: 32.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: ((i raisedTo: 5) raisedTo: 3/5) classAndValueEquals: i*i*i.
		self assert: ((i raisedTo: 3) raisedTo: 5/3) classAndValueEquals: i*i*i*i*i.
		self assert: ((i negated raisedTo: 5) raisedTo: 3/5) classAndValueEquals: (i*i*i) negated.
		self assert: ((i negated raisedTo: 3) raisedTo: 5/3) classAndValueEquals: (i*i*i*i*i) negated.

		self assert: ((i raisedTo: -5) raisedTo: 3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: -3) raisedTo: 5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: -5) raisedTo: 3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: -3) raisedTo: 5/3) classAndValueEquals: -1/(i*i*i*i*i).

		self assert: ((i raisedTo: 5) raisedTo: -3/5) classAndValueEquals: 1/(i*i*i).
		self assert: ((i raisedTo: 3) raisedTo: -5/3) classAndValueEquals: 1/(i*i*i*i*i).
		self assert: ((i negated raisedTo: 5) raisedTo: -3/5) classAndValueEquals: -1/(i*i*i).
		self assert: ((i negated raisedTo: 3) raisedTo: -5/3) classAndValueEquals: -1/(i*i*i*i*i).

		"No exact result => Float result"
		self assert: ((i raisedTo: 3) +1 raisedTo: 5/3) isFloat.
		self assert: ((i negated raisedTo: 3) -1 raisedTo: 5/3) isFloat ].
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testExactSqrt [
	"
	IntegerTest new testExactSqrt
	"
	self assert: 4 sqrt classAndValueEquals: 2.
	self assert: 9 sqrt classAndValueEquals: 3.
	self assert: Float maxExactInteger squared sqrt classAndValueEquals: Float maxExactInteger.
	self assert: (Float maxExactInteger+1) squared sqrt classAndValueEquals: Float maxExactInteger+1.
	#( 1 5 29 135 1234 567890 123123123 456456456456 98765432109876543210987654321 987123987123987123987123987123987123987123987123) do: [ :i |
		self assert: i squared sqrt classAndValueEquals: i ]
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testFloorLog [
	self assert: (100 floorLog: 10) = 2.
	self assert: (((2 raisedTo: Float emax + 3) floorLog: 10) = (2 log*(Float emax + 3)) floor) description: 'Integer>>floorLog: should not overflow'
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testFloorLogExactness [

	1 to: (Float fmax floorLog: 10) do: [:n |
		self assert: ((10 raisedTo: n) floorLog: 10) = n].

	"Float version is not exact for at least 2 reasons:
	(10 raisedTo: n) asFloat is not exact for n > 22
	(aFloat log: radix) is not exact

	(1 to: (Float fmax floorLog: 10)) count: [:n |
		((10 raisedTo: n) asFloat floorLog: 10) ~= n]."
]

{ #category : 'gcd and lcm' }
IntegerTest >> testGCDAndLCM [
	| a b c |
	a _ (-20 to: 20) atRandom.
	b _ (-20 to: 20) atRandom.
	c _ (-20 to: 20) atRandom.
	self assert: (a gcd: b) * (a lcm: b) = (a*b) abs.
	self assert: (a gcd: (b lcm: c)) = ((a gcd: b) lcm: (a gcd: c)).
	self assert: (a lcm: (b gcd: c)) = ((a lcm: b) gcd: (a lcm: c))
]

{ #category : 'gcd and lcm' }
IntegerTest >> testGCDExample [
	self assert: (120 gcd: 70) = 10
]

{ #category : 'gcd and lcm' }
IntegerTest >> testGCDProperties [
	| a b c |
	[
		a := (-20 to: 20) atRandom.
		b := (-20 to: 20) atRandom.
		c := (-20 to: 20) atRandom.
		a = 0 and: [ b = 0 ]] whileTrue. "yes, it can happen"
	self assert: a \\ (a gcd: b) = 0. "divides a"
	self assert: b \\ (a gcd: b) = 0. "divides b"
	self assert: (a gcd: b) = (a gcd: b) abs. "it is normalized to be >= 0"
	self assert: (a gcd: b) = (b gcd: a). "commutative"
	self assert: (a gcd: (b gcd: c)) = ((a gcd: b) gcd: c). "associative"
	self assert: ((a gcd: 0) = a abs or: [a = 0]).
	self assert: (0 gcd: 0) = 0.
	self assert: (a + (b*c) gcd: b) = (a gcd: b).
	self assert: (a*c gcd: b*c) = ((a gcd: b) * c abs)
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testHighBit [
	| suite |

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA ) , {SmallInteger maxVal . SmallInteger maxVal+1}.
	suite := suite , (suite collect: [:e | e raisedTo: 20]).
	
	suite do: [:anInteger |
		| highBit shifted |
		highBit := 0.
		shifted := 1.
		[shifted > anInteger] whileFalse: [highBit := highBit+1. shifted := shifted bitShift: 1].
		self assert: anInteger highBit = highBit].
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testHighBitOfMagnitude [
	| suite |

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA ) , {SmallInteger maxVal . SmallInteger maxVal+1}.
	suite := suite , (suite collect: [:e | e raisedTo: 20]).
	
	suite do: [:anInteger |
		| highBit shifted |
		highBit := 0.
		shifted := 1.
		[shifted > anInteger] whileFalse: [highBit := highBit+1. shifted := shifted bitShift: 1].
		self assert: anInteger highBitOfMagnitude = highBit.
		self assert: anInteger negated highBitOfMagnitude = highBit].
]

{ #category : 'testing - arithmetic' }
IntegerTest >> testIntegerDivision [
	| suite |
	suite := #( 1 2 5 1000 123456798  111222333444555 987654321098765432109876 ).
	suite := suite , (suite collect: [:e | e negated]).
	suite do: [:a |
		suite do: [:b |
			| q r |
			q := a // b.
			r := a \\ b.
			self assert: b * q + r = a.
			self assert: r abs < b abs.
			self assert: (r isZero or: [b negative = r negative])]].
]

{ #category : 'tests - printing' }
IntegerTest >> testIntegerPadding [
	"self run: #testIntegerPadding"

	self assert: (1 printStringBase: 10 length: 0 padded: false) = '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: false) = '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: false) = ' 1'.
	self assert: (1024 printStringBase: 10 length: 19 padded: false) = '               1024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: false) = '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: false) =  ' 1024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: false) =   '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: false) =  '              -1024'.

	self assert: (1 printStringBase: 10 length: 0 padded: true) = '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: true) = '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: true) = '01'.
	self assert: (1024 printStringBase: 10 length: 19 padded: true) = '0000000000000001024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: true) = '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: true) =  '01024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: true) =   '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: true) =  '-000000000000001024'.

	self assert: (1 printStringBase: 16 length: 0 padded: false) = '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: false) = '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: false) = ' 1'.
	self assert: (2047 printStringBase: 16 length: 19 padded: false) =  '                7FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: false) =  '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: false) =  ' 7FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: false) = '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: false) =  '               -7FF'.

	self assert: (1 printStringBase: 16 length: 0 padded: true) = '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: true) = '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: true) = '01'.
	self assert: (2047 printStringBase: 16 length: 19 padded: true) =  '00000000000000007FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: true) =  '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: true) =  '07FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: true) = '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: true) =  '-0000000000000007FF'
]

{ #category : 'tests - instance creation' }
IntegerTest >> testIntegerReadFrom [
	self assert: (Integer readFrom: '123' readStream base: 10) = 123.
	self assert: (Integer readFrom: '-123' readStream base: 10) = -123.
	self should: [Integer readFrom: 'abc' readStream base: 10] raise: Error.
	self should: [Integer readFrom: 'D12' readStream base: 10] raise: Error.
	self assert: (Integer readFrom: '1two3' readStream base: 10) = 1.

]

{ #category : 'tests - basic' }
IntegerTest >> testIsInteger [
  self assert: 0 isInteger.
  self assert: (0 is: #Integer).
	self assert: (1 is: #Integer).
	self assert: (1 is: #Number).
	
	self assert: (10000 is: #Integer).
	self assert: (10000 is: #Number).
]

{ #category : 'tests - basic' }
IntegerTest >> testIsPowerOfTwo [

	self deny: (0 isPowerOfTwo).
	self assert: (1 isPowerOfTwo).
	self assert: (2 isPowerOfTwo).
	self deny:  (3 isPowerOfTwo).
	self assert: (4 isPowerOfTwo).
	
]

{ #category : 'tests - basic' }
IntegerTest >> testIsPowerOfTwoM6873 [
	"This is a non regression test for http://bugs.squeak.org/view.php?id=6873"

	self deny: ((1 to: 80) anySatisfy: [:n | (2 raisedTo: n) negated isPowerOfTwo])
		description: 'A negative integer cannot be a power of two'.
]

{ #category : 'tests - basic' }
IntegerTest >> testIsPrime [

	"Not primes:"
	#(-100 -5 -3 -2 -1 0 1) do: [ :each |
		self deny: each isPrime ].

	"The following tests should return 'true'"
	#(17 78901 104729 15485863 2038074743) do: [ :each |
		self assert: each isPrime ].
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	#(561 2821 6601 10585 15841 256 29996224275831) do: [ :each |
		self deny: each isPrime ].
]

{ #category : 'tests - basic' }
IntegerTest >> testIsProbablyPrime [

	"Not primes:"
	#(-100 -5 -3 -2 -1 0 1) do: [ :each |
		self deny: each isProbablyPrime ].

	"The following tests should return 'true'"
	#(17 78901 104729 15485863 2038074743 29996224275833) do: [ :each |
		self assert: each isProbablyPrime ].
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	#(561 2821 6601 10585 15841 256 29996224275831) do: [ :each |
		self deny: each isProbablyPrime ].
]

{ #category : 'gcd and lcm' }
IntegerTest >> testLCMExample [
	self assert: (120 lcm: 70) = 840
]

{ #category : 'gcd and lcm' }
IntegerTest >> testLCMProperties [
	| a b c |
	a _ (-10 to: 10) atRandom.
	b _ (-10 to: 10) atRandom.
	c _ (-10 to: 10) atRandom.
	self assert: (a = 0 or: [(a lcm: b) \\ a = 0]). "a divides it"
	self assert: (b = 0 or: [(a lcm: b) \\ b = 0]). "b divides it"
	"is is the smallest positive integer divisible by both a and b:"
	self deny: ((a ~= 0 and: [b ~= 0]) and: [(1 to: (a lcm: b) abs - 1) anySatisfy: [:any| any \\ a = 0 and: [any \\ b = 0]]]).
	self assert: (a lcm: b) = (a lcm: b) abs. "it is normalized to be >= 0"
	self assert: (a lcm: b) = (b lcm: a). "commutative"
	self assert: (a lcm: (b lcm: c)) = ((a lcm: b) lcm: c). "associative"
	self assert: (a lcm: 0) = 0.
	self assert: (0 lcm: a) = 0.
	self assert: (0 lcm: 0) = 0
]

{ #category : 'tests - basic' }
IntegerTest >> testLargePrimesUpTo [

	| nn | 
	nn := (2 raisedTo: 17) - 1. 
	self deny: (Integer primesUpTo: nn) last = nn.
	self assert: (Integer primesUpTo: nn + 1) last  = nn.
	
	

]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testLn [
	self verify: 100 ln isWithinOneFloatAwayFrom: 10 ln*2.
	self verify: (2 raisedTo: Float emax + 3) ln isWithinOneFloatAwayFrom: 2 ln * (Float emax + 3)
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testLog [
	self assert: 100 log = 2.
	self verify: (2 raisedTo: Float emax + 3) log isWithinOneFloatAwayFrom: 2 log * (Float emax + 3)
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testLowBit [
	| suite |

	suite := (0 to: 1024) asArray , #(16rFDFD 16rFFFF 16r1000 16r1000000 16r1000001 16r70000000 16r7AFAFAFA ) , {SmallInteger maxVal . SmallInteger maxVal+1}.
	suite := suite , (suite collect: [:e | e raisedTo: 20]).
	
	suite do: [:anInteger |
		| lowBit |
		lowBit := (anInteger respondsTo: #bitAt:)
			ifTrue: [(1 to: anInteger highBit) detect: [:bitIndex | (anInteger bitAt: bitIndex) ~= 0] ifNone: [0]]
			ifFalse: [(1 to: anInteger highBit) detect: [:bitIndex | (anInteger bitAnd: (1 bitShift: bitIndex-1)) ~= 0] ifNone: [0]].
		self assert: anInteger lowBit = lowBit.
		self assert: anInteger negated lowBit = lowBit].
    self assert: (LargePositiveInteger new: 0) lowBit = 0.
    self assert: (LargePositiveInteger new: 2) lowBit = 0
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLnLn [

	self assert: (-42391158275216203514294433200 mod: -14130386091738734504764811067) = 1.
	self assert: (-42391158275216203514294433201 mod: -14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 mod: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (-8727963568087712425891397479476727340041448 mod: -79766443076872509863361) = 1.
	self assert: (-8727963568087712425891397479476727340041449 mod: -79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 mod: -79766443076872509863361) = 79766443076872509863360
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLnLp [

	self assert: (-42391158275216203514294433200 mod: 14130386091738734504764811067) = 1.
	self assert: (-42391158275216203514294433201 mod: 14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 mod: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (-8727963568087712425891397479476727340041448 mod: 79766443076872509863361) = 1.
	self assert: (-8727963568087712425891397479476727340041449 mod: 79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 mod: 79766443076872509863361) = 79766443076872509863360
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLnSn [

	self assert: (-42391158275216203514294433201 mod: -3) = 0.
	self assert: (-42391158275216203514294433202 mod: -3) = 2.
	self assert: (-42391158275216203514294433203 mod: -3) = 1.
	self assert: (-42391158275216203514294433204 mod: -3) = 0
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLnSp [

	self assert: (-42391158275216203514294433201 mod: 3) = 0.
	self assert: (-42391158275216203514294433202 mod: 3) = 2.
	self assert: (-42391158275216203514294433203 mod: 3) = 1.
	self assert: (-42391158275216203514294433204 mod: 3) = 0
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLpLn [

	self assert: (42391158275216203514294433200 mod: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 mod: -14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 mod: -14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 mod: -79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 mod: -79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 mod: -79766443076872509863361) = 1
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLpLp [

	self assert: (42391158275216203514294433200 mod: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 mod: 14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 mod: 14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 mod: 79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 mod: 79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 mod: 79766443076872509863361) = 1
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLpSn [

	self assert: (42391158275216203514294433201 mod: -3) = 0.
	self assert: (42391158275216203514294433202 mod: -3) = 1.
	self assert: (42391158275216203514294433203 mod: -3) = 2.
	self assert: (42391158275216203514294433204 mod: -3) = 0
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModLpSp [

	self assert: (42391158275216203514294433201 mod: 3) = 0.
	self assert: (42391158275216203514294433202 mod: 3) = 1.
	self assert: (42391158275216203514294433203 mod: 3) = 2.
	self assert: (42391158275216203514294433204 mod: 3) = 0
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSnLn [

	self assert: (0 mod: -42391158275216203514294433201) = 0.
	self assert: (-1 mod: -42391158275216203514294433201) = 42391158275216203514294433200.
	self assert: (-14348907 mod: -42391158275216203514294433201) = 42391158275216203514280084294
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSnLp [

	self assert: (0 mod: 42391158275216203514294433201) = 0.
	self assert: (-1 mod: 42391158275216203514294433201) =  42391158275216203514294433200.
	self assert: (-14348907 mod: 42391158275216203514294433201) = 42391158275216203514280084294
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSnSn [

	self assert: (0 mod: -3) = 0.
	self assert: (-1 mod: -3) = 2.
	self assert: (-9 mod: -3) = 0.
	self assert: (-10 mod: -3) = 2.
	self assert: (-11 mod: -3) = 1.
	self assert: (-12 mod: -3) = 0.
	self assert: (-13 mod: -3) = 2
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSnSp [

	self assert: (0 mod: 3) = 0.
	self assert: (-1 mod: 3) = 2.
	self assert: (-9 mod: 3) = 0.
	self assert: (-10 mod: 3) = 2.
	self assert: (-11 mod: 3) = 1.
	self assert: (-12 mod: 3) = 0.
	self assert: (-13 mod: 3) = 2
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSpLn [

	self assert: (0 mod: -42391158275216203514294433201) = 0.
	self assert: (1 mod: -42391158275216203514294433201) = 1.
	self assert: (14348907 mod: -42391158275216203514294433201) = 14348907
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSpLp [

	self assert: (0 mod: 42391158275216203514294433201) = 0.
	self assert: (1 mod: 42391158275216203514294433201) = 1.
	self assert: (14348907 mod: 42391158275216203514294433201) = 14348907
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSpSn [

	self assert: (0 mod: -3) = 0.
	self assert: (1 mod: -3) = 1.
	self assert: (9 mod: -3) = 0.
	self assert: (10 mod: -3) = 1.
	self assert: (11 mod: -3) = 2.
	self assert: (12 mod: -3) = 0.
	self assert: (13 mod: -3) = 1
]

{ #category : 'tests - division - mod:' }
IntegerTest >> testModSpSp [

	self assert: (0 mod: 3) = 0.
	self assert: (1 mod: 3) = 1.
	self assert: (9 mod: 3) = 0.
	self assert: (10 mod: 3) = 1.
	self assert: (11 mod: 3) = 2.
	self assert: (12 mod: 3) = 0.
	self assert: (13 mod: 3) = 1
]

{ #category : 'tests - division - \\' }
IntegerTest >> testModuloTwoPiAsFloat [

	| k |
	k := 1e22.
	self assert: k moduloTwoPiAsFloat + 1 = 	(k+1) moduloTwoPiAsFloat.
	k negated moduloTwoPiAsFloat + 1 = 	(k negated+1) moduloTwoPiAsFloat.
	k moduloTwoPiAsFloat + k negated moduloTwoPiAsFloat = Float twoPi.
	(k+1) moduloTwoPiAsFloat + (k negated-1) moduloTwoPiAsFloat = Float twoPi.
	(k-1) moduloTwoPiAsFloat + (k negated+1) moduloTwoPiAsFloat = Float twoPi.

	k := 1e500.
	self assert: k moduloTwoPiAsFloat + 1 = 	(k+1) moduloTwoPiAsFloat.
	k negated moduloTwoPiAsFloat + 1 = 	(k negated+1) moduloTwoPiAsFloat.
	k moduloTwoPiAsFloat + k negated moduloTwoPiAsFloat = Float twoPi.
	(k+1) moduloTwoPiAsFloat + (k negated-1) moduloTwoPiAsFloat = Float twoPi.
	(k-1) moduloTwoPiAsFloat + (k negated+1) moduloTwoPiAsFloat = Float twoPi.

]

{ #category : 'testing - arithmetic' }
IntegerTest >> testMontgomeryMultiplication [
	| a m mInv |
	m := 15485863.
	mInv := m montgomeryDigitBase - ((m bitAnd: m montgomeryDigitMax) reciprocalModulo: m montgomeryDigitBase).
	a := (m montgomeryDigitBase raisedTo: m montgomeryNumberOfDigits) \\ m.
	#(483933 3871465 8951195) do: [:s |
		(s montgomeryTimes: a modulo: m mInvModB: mInv) ifNotNil: [:s1 |
			| s2 sa ssa |
			self assert: s = s1.
			sa := s montgomeryTimes: (a * a \\ m) modulo: m mInvModB: mInv.
			self assert: sa = (s * a \\ m).
			ssa := sa montgomeryTimes: sa modulo: m mInvModB: mInv.
			self assert: ssa = (s * s * a \\ m).
			s2 := ssa montgomeryTimes: 1 modulo: m mInvModB: mInv.
			self assert: s2 = (s * s \\ m)]].
]

{ #category : 'tests - printing' }
IntegerTest >> testNegativeIntegerPrinting [
	"self run: #testnegativeIntegerPrinting"

	self assert: (-2 printStringBase: 2) = '-10'.
	self assert: -2 printStringHex = '-2'.
	self assert: (-2 storeStringBase: 2) = '-2r10'.
	self assert: -2 storeStringHex = '-16r2'.
	self assert: (-21 printStringBase: 3) = '-210'.
	self assert: -21 printStringHex = '-15'.
	self assert: (-21 storeStringBase: 3) = '-3r210'.
	self assert: -21 storeStringHex = '-16r15'.
	self assert: (-228 printStringBase: 4) = '-3210'.
	self assert: -228 printStringHex = '-E4'.
	self assert: (-228 storeStringBase: 4) = '-4r3210'.
	self assert: -228 storeStringHex = '-16rE4'.
	self assert: (-2930 printStringBase: 5) = '-43210'.
	self assert: -2930 printStringHex = '-B72'.
	self assert: (-2930 storeStringBase: 5) = '-5r43210'.
	self assert: -2930 storeStringHex = '-16rB72'.
	self assert: (-44790 printStringBase: 6) = '-543210'.
	self assert: -44790 printStringHex = '-AEF6'.
	self assert: (-44790 storeStringBase: 6) = '-6r543210'.
	self assert: -44790 storeStringHex = '-16rAEF6'.
	self assert: (-800667 printStringBase: 7) = '-6543210'.
	self assert: -800667 printStringHex = '-C379B'.
	self assert: (-800667 storeStringBase: 7) = '-7r6543210'.
	self assert: -800667 storeStringHex = '-16rC379B'.
	self assert: (-16434824 printStringBase: 8) = '-76543210'.
	self assert: -16434824 printStringHex = '-FAC688'.
	self assert: (-16434824 storeStringBase: 8) = '-8r76543210'.
	self assert: -16434824 storeStringHex = '-16rFAC688'.
	self assert: (-381367044 printStringBase: 9) = '-876543210'.
	self assert: -381367044 printStringHex = '-16BB3304'.
	self assert: (-381367044 storeStringBase: 9) = '-9r876543210'.
	self assert: -381367044 storeStringHex = '-16r16BB3304'.
	self assert: (-9876543210 printStringBase: 10) = '-9876543210'.
	self assert: -9876543210 printStringHex = '-24CB016EA'.
	self assert: (-9876543210 storeStringBase: 10) = '-9876543210'.
	self assert: -9876543210 storeStringHex = '-16r24CB016EA'.
	self assert: (-282458553905 printStringBase: 11) = '-A9876543210'.
	self assert: -282458553905 printStringHex = '-41C3D77E31'.
	self assert: (-282458553905 storeStringBase: 11) = '-11rA9876543210'.
	self assert: -282458553905 storeStringHex = '-16r41C3D77E31'.
	self assert: (-8842413667692 printStringBase: 12) = '-BA9876543210'.
	self assert: -8842413667692 printStringHex = '-80AC8ECF56C'.
	self assert: (-8842413667692 storeStringBase: 12) = '-12rBA9876543210'.
	self assert: -8842413667692 storeStringHex = '-16r80AC8ECF56C'.
	self assert: (-300771807240918 printStringBase: 13) = '-CBA9876543210'.
	self assert: -300771807240918 printStringHex = '-1118CE4BAA2D6'.
	self assert: (-300771807240918 storeStringBase: 13) = '-13rCBA9876543210'.
	self assert: -300771807240918 storeStringHex = '-16r1118CE4BAA2D6'.
	self assert: (-11046255305880158 printStringBase: 14) = '-DCBA9876543210'.
	self assert: -11046255305880158 printStringHex = '-273E82BB9AF25E'.
	self assert: (-11046255305880158 storeStringBase: 14) = '-14rDCBA9876543210'.
	self assert: -11046255305880158 storeStringHex = '-16r273E82BB9AF25E'.
	self assert: (-435659737878916215 printStringBase: 15) = '-EDCBA9876543210'.
	self assert: -435659737878916215 printStringHex = '-60BC6392F366C77'.
	self assert: (-435659737878916215 storeStringBase: 15) = '-15rEDCBA9876543210'.
	self assert: -435659737878916215 storeStringHex = '-16r60BC6392F366C77'.
	self assert: (-18364758544493064720 printStringBase: 16) = '-FEDCBA9876543210'.
	self assert: -18364758544493064720 printStringHex = '-FEDCBA9876543210'.
	self assert: (-18364758544493064720 storeStringBase: 16) = '-16rFEDCBA9876543210'.
	self assert: -18364758544493064720 storeStringHex = '-16rFEDCBA9876543210'.
	self assert: (-824008854613343261192 printStringBase: 17) = '-GFEDCBA9876543210'.
	self assert: -824008854613343261192 printStringHex = '-2CAB6B877C1CD2D208'.
	self assert: (-824008854613343261192 storeStringBase: 17) = '-17rGFEDCBA9876543210'.
	self assert: -824008854613343261192 storeStringHex = '-16r2CAB6B877C1CD2D208'.
	self assert: (-39210261334551566857170 printStringBase: 18) = '-HGFEDCBA9876543210'.
	self assert: -39210261334551566857170 printStringHex = '-84D97AFCAE81415B3D2'.
	self assert: (-39210261334551566857170 storeStringBase: 18) = '-18rHGFEDCBA9876543210'.
	self assert: -39210261334551566857170 storeStringHex = '-16r84D97AFCAE81415B3D2'.
	self assert: (-1972313422155189164466189 printStringBase: 19) = '-IHGFEDCBA9876543210'.
	self assert: -1972313422155189164466189 printStringHex = '-1A1A75329C5C6FC00600D'.
	self assert: (-1972313422155189164466189 storeStringBase: 19) = '-19rIHGFEDCBA9876543210'.
	self assert: -1972313422155189164466189 storeStringHex = '-16r1A1A75329C5C6FC00600D'.
	self assert: (-104567135734072022160664820 printStringBase: 20) = '-JIHGFEDCBA9876543210'.
	self assert: -104567135734072022160664820 printStringHex = '-567EF3C9636D242A8C68F4'.
	self assert: (-104567135734072022160664820 storeStringBase: 20) = '-20rJIHGFEDCBA9876543210'.
	self assert: -104567135734072022160664820 storeStringHex = '-16r567EF3C9636D242A8C68F4'.
	self assert: (-5827980550840017565077671610 printStringBase: 21) = '-KJIHGFEDCBA9876543210'.
	self assert: -5827980550840017565077671610 printStringHex = '-12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (-5827980550840017565077671610 storeStringBase: 21) = '-21rKJIHGFEDCBA9876543210'.
	self assert: -5827980550840017565077671610 storeStringHex = '-16r12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (-340653664490377789692799452102 printStringBase: 22) = '-LKJIHGFEDCBA9876543210'.
	self assert: -340653664490377789692799452102 printStringHex = '-44CB61B5B47E1A5D8F88583C6'.
	self assert: (-340653664490377789692799452102 storeStringBase: 22) = '-22rLKJIHGFEDCBA9876543210'.
	self assert: -340653664490377789692799452102 storeStringHex = '-16r44CB61B5B47E1A5D8F88583C6'.
	self assert: (-20837326537038308910317109288851 printStringBase: 23) = '-MLKJIHGFEDCBA9876543210'.
	self assert: -20837326537038308910317109288851 printStringHex = '-1070108876456E0EF115B389F93'.
	self assert: (-20837326537038308910317109288851 storeStringBase: 23) = '-23rMLKJIHGFEDCBA9876543210'.
	self assert: -20837326537038308910317109288851 storeStringHex = '-16r1070108876456E0EF115B389F93'.
	self assert: (-1331214537196502869015340298036888 printStringBase: 24) = '-NMLKJIHGFEDCBA9876543210'.
	self assert: -1331214537196502869015340298036888 printStringHex = '-41A24A285154B026B6ED206C6698'.
	self assert: (-1331214537196502869015340298036888 storeStringBase: 24) = '-24rNMLKJIHGFEDCBA9876543210'.
	self assert: -1331214537196502869015340298036888 storeStringHex = '-16r41A24A285154B026B6ED206C6698'.
	self assert: (-88663644327703473714387251271141900 printStringBase: 25) = '-ONMLKJIHGFEDCBA9876543210'.
	self assert: -88663644327703473714387251271141900 printStringHex = '-111374860A2C6CEBE5999630398A0C'.
	self assert: (-88663644327703473714387251271141900 storeStringBase: 25) = '-25rONMLKJIHGFEDCBA9876543210'.
	self assert: -88663644327703473714387251271141900 storeStringHex = '-16r111374860A2C6CEBE5999630398A0C'.
	self assert: (-6146269788878825859099399609538763450 printStringBase: 26) = '-PONMLKJIHGFEDCBA9876543210'.
	self assert: -6146269788878825859099399609538763450 printStringHex = '-49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (-6146269788878825859099399609538763450 storeStringBase: 26) = '-26rPONMLKJIHGFEDCBA9876543210'.
	self assert: -6146269788878825859099399609538763450 storeStringHex = '-16r49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (-442770531899482980347734468443677777577 printStringBase: 27) = '-QPONMLKJIHGFEDCBA9876543210'.
	self assert: -442770531899482980347734468443677777577 printStringHex = '-14D1A80A997343640C1145A073731DEA9'.
	self assert: (-442770531899482980347734468443677777577 storeStringBase: 27) = '-27rQPONMLKJIHGFEDCBA9876543210'.
	self assert: -442770531899482980347734468443677777577 storeStringHex = '-16r14D1A80A997343640C1145A073731DEA9'.
	self assert: (-33100056003358651440264672384704297711484 printStringBase: 28) = '-RQPONMLKJIHGFEDCBA9876543210'.
	self assert: -33100056003358651440264672384704297711484 printStringHex = '-6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (-33100056003358651440264672384704297711484 storeStringBase: 28) = '-28rRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -33100056003358651440264672384704297711484 storeStringHex = '-16r6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (-2564411043271974895869785066497940850811934 printStringBase: 29) = '-SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -2564411043271974895869785066497940850811934 printStringHex = '-1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (-2564411043271974895869785066497940850811934 storeStringBase: 29) = '-29rSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -2564411043271974895869785066497940850811934 storeStringHex = '-16r1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (-205646315052919334126040428061831153388822830 printStringBase: 30) = '-TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -205646315052919334126040428061831153388822830 printStringHex = '-938B4343B54B550989989D02998718FFB212E'.
	self assert: (-205646315052919334126040428061831153388822830 storeStringBase: 30) = '-30rTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -205646315052919334126040428061831153388822830 storeStringHex = '-16r938B4343B54B550989989D02998718FFB212E'.
	self assert: (-17050208381689099029767742314582582184093573615 printStringBase: 31) = '-UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -17050208381689099029767742314582582184093573615 printStringHex = '-2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (-17050208381689099029767742314582582184093573615 storeStringBase: 31) = '-31rUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -17050208381689099029767742314582582184093573615 storeStringHex = '-16r2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (-1459980823972598128486511383358617792788444579872 printStringBase: 32) = '-VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1459980823972598128486511383358617792788444579872 printStringHex = '-FFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (-1459980823972598128486511383358617792788444579872 storeStringBase: 32) = '-32rVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1459980823972598128486511383358617792788444579872 storeStringHex = '-16rFFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (-128983956064237823710866404905431464703849549412368 printStringBase: 33) = '-WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -128983956064237823710866404905431464703849549412368 printStringHex = '-584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (-128983956064237823710866404905431464703849549412368 storeStringBase: 33) = '-33rWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -128983956064237823710866404905431464703849549412368 storeStringHex = '-16r584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (-11745843093701610854378775891116314824081102660800418 printStringBase: 34) = '-XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -11745843093701610854378775891116314824081102660800418 printStringHex = '-1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (-11745843093701610854378775891116314824081102660800418 storeStringBase: 34) = '-34rXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -11745843093701610854378775891116314824081102660800418 storeStringHex = '-16r1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (-1101553773143634726491620528194292510495517905608180485 printStringBase: 35) = '-YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1101553773143634726491620528194292510495517905608180485 printStringHex = '-B8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (-1101553773143634726491620528194292510495517905608180485 storeStringBase: 35) = '-35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -1101553773143634726491620528194292510495517905608180485 storeStringHex = '-16rB8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (-106300512100105327644605138221229898724869759421181854980 printStringBase: 36) = '-ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -106300512100105327644605138221229898724869759421181854980 printStringHex = '-455D441E55A37239AB4C303189576071AF5578FFCA80504'.
	self assert: (-106300512100105327644605138221229898724869759421181854980 storeStringBase: 36) = '-36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: -106300512100105327644605138221229898724869759421181854980 storeStringHex = '-16r455D441E55A37239AB4C303189576071AF5578FFCA80504'.
]

{ #category : 'tests - instance creation' }
IntegerTest >> testNew [
	self should: [Integer new] raise: TestResult exError. 
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testNthRoot [
	"
	IntegerTest new testNthRoot
	"
	| i |
	i _ 1234987687234509123.
	#(3 5 7 9 11 13 15 17 19 21 23 25 27) do: [ :n |
		self assert: ((i raisedTo: n) nthRoot: n) = i ].

	self shouldnt: [ (1 << 2000 nthRoot: 100) ] raise: ArithmeticError.
	self assert: (1 << 2000 nthRoot: 100) equals: 1 << 20
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testNthRootErrorConditions [
	"
	IntegerTest new testExactRaisedToErrorConditions
	"

	self should: [ -2 nthRoot: 1/4 ] raise: ArithmeticError.
	self should: [ -2 nthRoot: 1.24 ] raise: ArithmeticError.
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testNthRootExactness [
	| inexactRoots largeRaisedTo6 |
	largeRaisedTo6 := (2 to: 100) collect: [:k | (k raisedTo: 11) raisedTo: 6].
	inexactRoots := largeRaisedTo6 reject: [:e | (e nthRoot: 6) isInteger].
	self assert: inexactRoots isEmpty description: 'Failed to find the exact 6th root of these numbers'
]

{ #category : 'mathematical functions' }
IntegerTest >> testNthRootExactnessForHugeValue [
	"
	self new testNthRootExactnessForHugeValue
	"
        self assert: ((10 raisedTo: 600) nthRoot: 300) = 100.
        self assert: ((10 raisedTo: 600) nthRoot: 300) class == SmallInteger.
        self assert: ((10 raisedTo: 600) + 1 nthRoot: 300) = 100.0.
        self assert: (((10 raisedTo: 600) + 1 nthRoot: 300) isKindOf: Float)
]

{ #category : 'mathematical functions' }
IntegerTest >> testNthRootImmuneToDoubleRounding [
"
self new testNthRootImmuneToDoubleRounding
"
          "Use a specially crafted number for causing double rounding.
         Solution is 10...01.1 - verySmallQuantity.
         Where verySmallQuantity is approximately 1/53/(1<<53).
         If the verySmallQuantity is not taken into account, then solution is rounded to 10....010"
         | exponent crafted root highPrecisionRoot |
         exponent := 4.
         crafted := (1 << Float precision + 3 raisedTo: exponent) - 1.
         root := crafted nthRoot: exponent.
         highPrecisionRoot := (crafted << (exponent squared * Float precision * 4) nthRootRounded: exponent) / (1 << (exponent * Float precision * 4)).
         self assert: (root asFraction - highPrecisionRoot) abs < (root predecessor asFraction - highPrecisionRoot) abs.
         
         "Same with the other sign.
         Solution is 10...00.1 + verySmallQuantity."
         crafted := (1 << Float precision + 1 raisedTo: exponent) + 1.
         root := crafted nthRoot: exponent.
         highPrecisionRoot := (crafted << (exponent squared * Float precision * 4) nthRootRounded: exponent) / (1 << (exponent * Float precision * 4)).
         self assert: (root asFraction - highPrecisionRoot) abs < (root successor asFraction - highPrecisionRoot) abs.
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testNthRootRounded [
	<timeout:  5 "seconds">
	| d x |
	d := ((1 + 2 reciprocal - (1<<30) reciprocal) raisedTo: 1000) floor. "close to 1.5 by default"
	x := ((1 + 2 reciprocal + (1<<30) reciprocal) raisedTo: 1000) ceiling. "close to 1.5 by excess"
	self assert: (d nthRootTruncated: 1000) equals: 1.
	self assert: (x nthRootTruncated: 1000) equals: 1.
	self assert: (d nthRootRounded: 1000) equals: 1.
	self assert: (x nthRootRounded: 1000) equals: 2.
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testNthRootTruncated [
	<timeout:  5 "seconds">
	| tooBigToBeAFloat large |
	tooBigToBeAFloat := 1 << 2000.
	self assert: (tooBigToBeAFloat nthRootTruncated: 100) equals: 1 << 20.
	self assert: (tooBigToBeAFloat + 1 nthRootTruncated: 100) equals: 1 << 20.
	self assert: (tooBigToBeAFloat - 1 nthRootTruncated: 100) equals: 1 << 20 - 1.
	
	large := -3 raisedTo: 255.
	self assert: (large nthRootTruncated: 17) equals: (-3 raisedTo: 15).
	self assert: (large + 11 nthRootTruncated: 17) equals: (-3 raisedTo: 15) + 1.
	self assert: (large - 11 nthRootTruncated: 17) equals: (-3 raisedTo: 15).
	
	2 to: 10 do: [:thePower |
		1 to: 10000 do: [:n |
			| theTruncatedRoot |
			theTruncatedRoot := n nthRootTruncated: thePower.
			self assert: (theTruncatedRoot raisedTo: thePower) <= n.
			self assert: (theTruncatedRoot + 1 raisedTo: thePower) > n]]
]

{ #category : 'tests - printing' }
IntegerTest >> testNumberOfDigits [
	
	2 to: 32 do: [:b |
		1 to: 1000//b do: [:n |
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 numberOfDigitsInBase: b) = n.
			self assert: (bRaisedToN numberOfDigitsInBase: b) = (n+1).
			self assert: (bRaisedToN + 1 numberOfDigitsInBase: b) = (n+1).
			
			self assert: (bRaisedToN negated + 1 numberOfDigitsInBase: b) = n.
			self assert: (bRaisedToN negated numberOfDigitsInBase: b) = (n+1).
			self assert: (bRaisedToN negated - 1 numberOfDigitsInBase: b) = (n+1).]].

]

{ #category : 'tests - printing' }
IntegerTest >> testPositiveIntegerPrinting [
	"self run: #testPositiveIntegerPrinting"

	self assert: 0 printString = '0'.
	self assert: 0 printStringHex = '0'.
	self assert: 0 storeStringHex = '16r0'.
	self assert: (2 printStringBase: 2) = '10'.
	self assert: 2 printStringHex = '2'.
	self assert: (2 storeStringBase: 2) = '2r10'.
	self assert: 2 storeStringHex = '16r2'.
	self assert: (21 printStringBase: 3) = '210'.
	self assert: 21 printStringHex = '15'.
	self assert: (21 storeStringBase: 3) = '3r210'.
	self assert: 21 storeStringHex = '16r15'.
	self assert: (228 printStringBase: 4) = '3210'.
	self assert: 228 printStringHex = 'E4'.
	self assert: (228 storeStringBase: 4) = '4r3210'.
	self assert: 228 storeStringHex = '16rE4'.
	self assert: (2930 printStringBase: 5) = '43210'.
	self assert: 2930 printStringHex = 'B72'.
	self assert: (2930 storeStringBase: 5) = '5r43210'.
	self assert: 2930 storeStringHex = '16rB72'.
	self assert: (44790 printStringBase: 6) = '543210'.
	self assert: 44790 printStringHex = 'AEF6'.
	self assert: (44790 storeStringBase: 6) = '6r543210'.
	self assert: 44790 storeStringHex = '16rAEF6'.
	self assert: (800667 printStringBase: 7) = '6543210'.
	self assert: 800667 printStringHex = 'C379B'.
	self assert: (800667 storeStringBase: 7) = '7r6543210'.
	self assert: 800667 storeStringHex = '16rC379B'.
	self assert: (16434824 printStringBase: 8) = '76543210'.
	self assert: 16434824 printStringHex = 'FAC688'.
	self assert: (16434824 storeStringBase: 8) = '8r76543210'.
	self assert: 16434824 storeStringHex = '16rFAC688'.
	self assert: (381367044 printStringBase: 9) = '876543210'.
	self assert: 381367044 printStringHex = '16BB3304'.
	self assert: (381367044 storeStringBase: 9) = '9r876543210'.
	self assert: 381367044 storeStringHex = '16r16BB3304'.
	self assert: (9876543210 printStringBase: 10) = '9876543210'.
	self assert: 9876543210 printStringHex = '24CB016EA'.
	self assert: (9876543210 storeStringBase: 10) = '9876543210'.
	self assert: 9876543210 storeStringHex = '16r24CB016EA'.
	self assert: (282458553905 printStringBase: 11) = 'A9876543210'.
	self assert: 282458553905 printStringHex = '41C3D77E31'.
	self assert: (282458553905 storeStringBase: 11) = '11rA9876543210'.
	self assert: 282458553905 storeStringHex = '16r41C3D77E31'.
	self assert: (8842413667692 printStringBase: 12) = 'BA9876543210'.
	self assert: 8842413667692 printStringHex = '80AC8ECF56C'.
	self assert: (8842413667692 storeStringBase: 12) = '12rBA9876543210'.
	self assert: 8842413667692 storeStringHex = '16r80AC8ECF56C'.
	self assert: (300771807240918 printStringBase: 13) = 'CBA9876543210'.
	self assert: 300771807240918 printStringHex = '1118CE4BAA2D6'.
	self assert: (300771807240918 storeStringBase: 13) = '13rCBA9876543210'.
	self assert: 300771807240918 storeStringHex = '16r1118CE4BAA2D6'.
	self assert: (11046255305880158 printStringBase: 14) = 'DCBA9876543210'.
	self assert: 11046255305880158 printStringHex = '273E82BB9AF25E'.
	self assert: (11046255305880158 storeStringBase: 14) = '14rDCBA9876543210'.
	self assert: 11046255305880158 storeStringHex = '16r273E82BB9AF25E'.
	self assert: (435659737878916215 printStringBase: 15) = 'EDCBA9876543210'.
	self assert: 435659737878916215 printStringHex = '60BC6392F366C77'.
	self assert: (435659737878916215 storeStringBase: 15) = '15rEDCBA9876543210'.
	self assert: 435659737878916215 storeStringHex = '16r60BC6392F366C77'.
	self assert: (18364758544493064720 printStringBase: 16) = 'FEDCBA9876543210'.
	self assert: 18364758544493064720 printStringHex = 'FEDCBA9876543210'.
	self assert: (18364758544493064720 storeStringBase: 16) = '16rFEDCBA9876543210'.
	self assert: 18364758544493064720 storeStringHex = '16rFEDCBA9876543210'.
	self assert: (824008854613343261192 printStringBase: 17) = 'GFEDCBA9876543210'.
	self assert: 824008854613343261192 printStringHex = '2CAB6B877C1CD2D208'.
	self assert: (824008854613343261192 storeStringBase: 17) = '17rGFEDCBA9876543210'.
	self assert: 824008854613343261192 storeStringHex = '16r2CAB6B877C1CD2D208'.
	self assert: (39210261334551566857170 printStringBase: 18) = 'HGFEDCBA9876543210'.
	self assert: 39210261334551566857170 printStringHex = '84D97AFCAE81415B3D2'.
	self assert: (39210261334551566857170 storeStringBase: 18) = '18rHGFEDCBA9876543210'.
	self assert: 39210261334551566857170 storeStringHex = '16r84D97AFCAE81415B3D2'.
	self assert: (1972313422155189164466189 printStringBase: 19) = 'IHGFEDCBA9876543210'.
	self assert: 1972313422155189164466189 printStringHex = '1A1A75329C5C6FC00600D'.
	self assert: (1972313422155189164466189 storeStringBase: 19) = '19rIHGFEDCBA9876543210'.
	self assert: 1972313422155189164466189 storeStringHex = '16r1A1A75329C5C6FC00600D'.
	self assert: (104567135734072022160664820 printStringBase: 20) = 'JIHGFEDCBA9876543210'.
	self assert: 104567135734072022160664820 printStringHex = '567EF3C9636D242A8C68F4'.
	self assert: (104567135734072022160664820 storeStringBase: 20) = '20rJIHGFEDCBA9876543210'.
	self assert: 104567135734072022160664820 storeStringHex = '16r567EF3C9636D242A8C68F4'.
	self assert: (5827980550840017565077671610 printStringBase: 21) = 'KJIHGFEDCBA9876543210'.
	self assert: 5827980550840017565077671610 printStringHex = '12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (5827980550840017565077671610 storeStringBase: 21) = '21rKJIHGFEDCBA9876543210'.
	self assert: 5827980550840017565077671610 storeStringHex = '16r12D4CAE2B8A09BCFDBE30EBA'.
	self assert: (340653664490377789692799452102 printStringBase: 22) = 'LKJIHGFEDCBA9876543210'.
	self assert: 340653664490377789692799452102 printStringHex = '44CB61B5B47E1A5D8F88583C6'.
	self assert: (340653664490377789692799452102 storeStringBase: 22) = '22rLKJIHGFEDCBA9876543210'.
	self assert: 340653664490377789692799452102 storeStringHex = '16r44CB61B5B47E1A5D8F88583C6'.
	self assert: (20837326537038308910317109288851 printStringBase: 23) = 'MLKJIHGFEDCBA9876543210'.
	self assert: 20837326537038308910317109288851 printStringHex = '1070108876456E0EF115B389F93'.
	self assert: (20837326537038308910317109288851 storeStringBase: 23) = '23rMLKJIHGFEDCBA9876543210'.
	self assert: 20837326537038308910317109288851 storeStringHex = '16r1070108876456E0EF115B389F93'.
	self assert: (1331214537196502869015340298036888 printStringBase: 24) = 'NMLKJIHGFEDCBA9876543210'.
	self assert: 1331214537196502869015340298036888 printStringHex = '41A24A285154B026B6ED206C6698'.
	self assert: (1331214537196502869015340298036888 storeStringBase: 24) = '24rNMLKJIHGFEDCBA9876543210'.
	self assert: 1331214537196502869015340298036888 storeStringHex = '16r41A24A285154B026B6ED206C6698'.
	self assert: (88663644327703473714387251271141900 printStringBase: 25) = 'ONMLKJIHGFEDCBA9876543210'.
	self assert: 88663644327703473714387251271141900 printStringHex = '111374860A2C6CEBE5999630398A0C'.
	self assert: (88663644327703473714387251271141900 storeStringBase: 25) = '25rONMLKJIHGFEDCBA9876543210'.
	self assert: 88663644327703473714387251271141900 storeStringHex = '16r111374860A2C6CEBE5999630398A0C'.
	self assert: (6146269788878825859099399609538763450 printStringBase: 26) = 'PONMLKJIHGFEDCBA9876543210'.
	self assert: 6146269788878825859099399609538763450 printStringHex = '49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (6146269788878825859099399609538763450 storeStringBase: 26) = '26rPONMLKJIHGFEDCBA9876543210'.
	self assert: 6146269788878825859099399609538763450 storeStringHex = '16r49FBA7F30B0F48BD14E6A99BD8ADABA'.
	self assert: (442770531899482980347734468443677777577 printStringBase: 27) = 'QPONMLKJIHGFEDCBA9876543210'.
	self assert: 442770531899482980347734468443677777577 printStringHex = '14D1A80A997343640C1145A073731DEA9'.
	self assert: (442770531899482980347734468443677777577 storeStringBase: 27) = '27rQPONMLKJIHGFEDCBA9876543210'.
	self assert: 442770531899482980347734468443677777577 storeStringHex = '16r14D1A80A997343640C1145A073731DEA9'.
	self assert: (33100056003358651440264672384704297711484 printStringBase: 28) = 'RQPONMLKJIHGFEDCBA9876543210'.
	self assert: 33100056003358651440264672384704297711484 printStringHex = '6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (33100056003358651440264672384704297711484 storeStringBase: 28) = '28rRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 33100056003358651440264672384704297711484 storeStringHex = '16r6145B6E6DACFA25D0E936F51D25932377C'.
	self assert: (2564411043271974895869785066497940850811934 printStringBase: 29) = 'SRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 2564411043271974895869785066497940850811934 printStringHex = '1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (2564411043271974895869785066497940850811934 storeStringBase: 29) = '29rSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 2564411043271974895869785066497940850811934 storeStringHex = '16r1D702071CBA4A1597D4DD37E95EFAC79241E'.
	self assert: (205646315052919334126040428061831153388822830 printStringBase: 30) = 'TSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 205646315052919334126040428061831153388822830 printStringHex = '938B4343B54B550989989D02998718FFB212E'.
	self assert: (205646315052919334126040428061831153388822830 storeStringBase: 30) = '30rTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 205646315052919334126040428061831153388822830 storeStringHex = '16r938B4343B54B550989989D02998718FFB212E'.
	self assert: (17050208381689099029767742314582582184093573615 printStringBase: 31) = 'UTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 17050208381689099029767742314582582184093573615 printStringHex = '2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (17050208381689099029767742314582582184093573615 storeStringBase: 31) = '31rUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 17050208381689099029767742314582582184093573615 storeStringHex = '16r2FC8ECB1521BA16D24A69E976D53873E2C661EF'.
	self assert: (1459980823972598128486511383358617792788444579872 printStringBase: 32) = 'VUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1459980823972598128486511383358617792788444579872 printStringHex = 'FFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (1459980823972598128486511383358617792788444579872 storeStringBase: 32) = '32rVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1459980823972598128486511383358617792788444579872 storeStringHex = '16rFFBBCDEB38BDAB49CA307B9AC5A928398A418820'.
	self assert: (128983956064237823710866404905431464703849549412368 printStringBase: 33) = 'WVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 128983956064237823710866404905431464703849549412368 printStringHex = '584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (128983956064237823710866404905431464703849549412368 storeStringBase: 33) = '33rWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 128983956064237823710866404905431464703849549412368 storeStringHex = '16r584120A0328DE272AB055A8AA003CE4A559F223810'.
	self assert: (11745843093701610854378775891116314824081102660800418 printStringBase: 34) = 'XWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 11745843093701610854378775891116314824081102660800418 printStringHex = '1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (11745843093701610854378775891116314824081102660800418 storeStringBase: 34) = '34rXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 11745843093701610854378775891116314824081102660800418 storeStringHex = '16r1F64D4FC76000F7B92CF0CD5D0F350139AB9F25D8FA2'.
	self assert: (1101553773143634726491620528194292510495517905608180485 printStringBase: 35) = 'YXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1101553773143634726491620528194292510495517905608180485 printStringHex = 'B8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (1101553773143634726491620528194292510495517905608180485 storeStringBase: 35) = '35rYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 1101553773143634726491620528194292510495517905608180485 storeStringHex = '16rB8031AD55AD1FAA89E07A271CA1ED2F420415D1570305'.
	self assert: (106300512100105327644605138221229898724869759421181854980 printStringBase: 36) = 'ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 106300512100105327644605138221229898724869759421181854980 printStringHex = '455D441E55A37239AB4C303189576071AF5578FFCA80504'.
	self assert: (106300512100105327644605138221229898724869759421181854980 storeStringBase: 36) = '36rZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'.
	self assert: 106300512100105327644605138221229898724869759421181854980 storeStringHex = '16r455D441E55A37239AB4C303189576071AF5578FFCA80504'.
]

{ #category : 'tests - basic' }
IntegerTest >> testPrimesUpTo [

	| primes nn|
	primes := Integer primesUpTo: 100.
	self assert: primes = #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97).
	
	"upTo: semantics means 'non-inclusive'"
	primes := Integer primesUpTo: 5.
	self assert: primes = #(2 3).
	
	"this test is green for nn>25000, see #testLargePrimesUpTo"
	nn := 5. 
	self deny: (Integer primesUpTo: nn) last = nn.
	self assert: (Integer primesUpTo: nn + 1) last  = nn.
]

{ #category : 'tests - printing' }
IntegerTest >> testPrintOnBaseShowRadix [

	self assert: (123 printStringRadix: 10) = '10r123'.
	self assert: (123 printStringRadix: 8) = '8r173'.
]

{ #category : 'tests - printing' }
IntegerTest >> testPrintStringBase [
	
	2 to: 32 do: [:b |
		1 to: 1000//b do: [:n |
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 printStringBase: b) = (String new: n withAll: (Character digitValue: b-1)).
			self assert: (bRaisedToN printStringBase: b) = ('1' , (String new: n withAll: $0)).
			
			self assert: (bRaisedToN negated + 1 printStringBase: b) = ('-' , (String new: n withAll: (Character digitValue: b-1))).
			self assert: (bRaisedToN negated printStringBase: b) = ('-1' , (String new: n withAll: $0))]].

]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLnLn [

	self assert: (-42391158275216203514294433200 quo: -14130386091738734504764811067) = 2.
	self assert: (-42391158275216203514294433201 quo: -14130386091738734504764811067) = 3.
	self assert: (-42391158275216203514294433202 quo: -14130386091738734504764811067) = 3.
	self assert: (-8727963568087712425891397479476727340041448 quo: -79766443076872509863361) = 109418989131512359208.
	self assert: (-8727963568087712425891397479476727340041449 quo: -79766443076872509863361) = 109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 quo: -79766443076872509863361) = 109418989131512359209
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLnLp [

	self assert: (-42391158275216203514294433200 quo: 14130386091738734504764811067) = -2.
	self assert: (-42391158275216203514294433201 quo: 14130386091738734504764811067) = -3.
	self assert: (-42391158275216203514294433202 quo: 14130386091738734504764811067) = -3.
	self assert: (-8727963568087712425891397479476727340041448 quo: 79766443076872509863361) = -109418989131512359208.
	self assert: (-8727963568087712425891397479476727340041449 quo: 79766443076872509863361) = -109418989131512359209.
	self assert: (-8727963568087712425891397479476727340041450 quo: 79766443076872509863361) = -109418989131512359209
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLnSn [

	self assert: (-42391158275216203514294433201 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433203 quo: -3) = 14130386091738734504764811067.
	self assert: (-42391158275216203514294433204 quo: -3) = 14130386091738734504764811068
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLnSp [

	self assert: (-42391158275216203514294433201 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433202 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433203 quo: 3) = -14130386091738734504764811067.
	self assert: (-42391158275216203514294433204 quo: 3) = -14130386091738734504764811068
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLpLn [

	self assert: (42391158275216203514294433200 quo: -14130386091738734504764811067) = -2.
	self assert: (42391158275216203514294433201 quo: -14130386091738734504764811067) = -3.
	self assert: (42391158275216203514294433202 quo: -14130386091738734504764811067) = -3.
	self assert: (8727963568087712425891397479476727340041448 quo: -79766443076872509863361) = -109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 quo: -79766443076872509863361) = -109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 quo: -79766443076872509863361) = -109418989131512359209
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLpLp [

	self assert: (42391158275216203514294433200 quo: 14130386091738734504764811067) = 2.
	self assert: (42391158275216203514294433201 quo: 14130386091738734504764811067) = 3.
	self assert: (42391158275216203514294433202 quo: 14130386091738734504764811067) = 3.
	self assert: (8727963568087712425891397479476727340041448 quo: 79766443076872509863361) = 109418989131512359208.
	self assert: (8727963568087712425891397479476727340041449 quo: 79766443076872509863361) = 109418989131512359209.
	self assert: (8727963568087712425891397479476727340041450 quo: 79766443076872509863361) = 109418989131512359209
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLpSn [

	self assert: (42391158275216203514294433201 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433202 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433203 quo: -3) = -14130386091738734504764811067.
	self assert: (42391158275216203514294433204 quo: -3) = -14130386091738734504764811068
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoLpSp [

	self assert: (42391158275216203514294433201 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433202 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433203 quo: 3) = 14130386091738734504764811067.
	self assert: (42391158275216203514294433204 quo: 3) = 14130386091738734504764811068
]

{ #category : 'testing - arithmetic' }
IntegerTest >> testQuoRem [
	| suite |
	suite := #( 1 2 5 1000 123456798 111222333444555 987654321098765432109876 ).
	suite := suite , (suite collect: [:e | e negated]).
	suite do: [:a |
		suite do: [:b |
			| q r |
			q := a quo: b.
			r := a rem: b.
			self assert: b * q + r = a.
			self assert: r abs < b abs.
			self assert: (r isZero or: [a negative = r negative])]].
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSnLn [

	self assert: (0 quo: -42391158275216203514294433201) = 0.
	self assert: (-1 quo: -42391158275216203514294433201) = 0.
	self assert: (-14348907 quo: -42391158275216203514294433201) = 0
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSnLp [

	self assert: (0 quo: 42391158275216203514294433201) = 0.
	self assert: (-1 quo: 42391158275216203514294433201) = 0.
	self assert: (-14348907 quo: 42391158275216203514294433201) = 0
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSnSn [

	self assert: (0 quo: -3) = 0.
	self assert: (-1 quo: -3) = 0.
	self assert: (-9 quo: -3) = 3.
	self assert: (-10 quo: -3) = 3.
	self assert: (-11 quo: -3) = 3.
	self assert: (-12 quo: -3) = 4.
	self assert: (-13 quo: -3) = 4
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSnSp [

	self assert: (0 quo: 3) = 0.
	self assert: (-1 quo: 3) = 0.
	self assert: (-9 quo: 3) = -3.
	self assert: (-10 quo: 3) = -3.
	self assert: (-11 quo: 3) = -3.
	self assert: (-12 quo: 3) = -4.
	self assert: (-13 quo: 3) = -4
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSpLn [

	self assert: (0 quo: -42391158275216203514294433201) = 0.
	self assert: (1 quo: -42391158275216203514294433201) = 0.
	self assert: (14348907 quo: -42391158275216203514294433201) = 0
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSpLp [

	self assert: (0 quo: 42391158275216203514294433201) = 0.
	self assert: (1 quo: 42391158275216203514294433201) = 0.
	self assert: (14348907 quo: 42391158275216203514294433201) = 0
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSpSn [

	self assert: (0 quo: -3) = 0.
	self assert: (1 quo: -3) = 0.
	self assert: (9 quo: -3) = -3.
	self assert: (10 quo: -3) = -3.
	self assert: (11 quo: -3) = -3.
	self assert: (12 quo: -3) = -4.
	self assert: (13 quo: -3) = -4
]

{ #category : 'tests - division - quo:' }
IntegerTest >> testQuoSpSp [

	self assert: (0 quo: 3) = 0.
	self assert: (1 quo: 3) = 0.
	self assert: (9 quo: 3) = 3.
	self assert: (10 quo: 3) = 3.
	self assert: (11 quo: 3) = 3.
	self assert: (12 quo: 3) = 4.
	self assert: (13 quo: 3) = 4
]

{ #category : 'testing - arithmetic' }
IntegerTest >> testRaisedToModulo [
	#(301 2047) do: [:m |
		1 to: m - 1 by: (m // 30) do: [:x |
			11 to: m - 1 by: (m // 40) do: [:y |
				self assert: (x raisedTo: y) \\ m = (x raisedTo: y modulo: m)]]].
	self assert: (8951195 raisedTo: 7742931 modulo: 15485863) = 15485862.
]

{ #category : 'tests - basic' }
IntegerTest >> testRange [
	self assert: SmallInteger maxVal class equals: SmallInteger.
	self assert: (SmallInteger maxVal + 1) class equals: LargePositiveInteger.
	self assert: SmallInteger minVal class equals: SmallInteger.
	self assert: (SmallInteger minVal - 1) class equals: LargeNegativeInteger
]

{ #category : 'tests - instance creation' }
IntegerTest >> testReadFrom [
	"Ensure remaining characters in a stream are not lost when parsing an integer."

	| rs i s |
	rs := ReadStream on: '123s could be confused with a ScaledDecimal'.
	i := Number readFrom: rs.
	self assert: i isInteger.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: 's could be confused with a ScaledDecimal' = s.
	
	"Modified for Cuis. Classic number parsing considers this to be an integer."
	rs := ReadStream on: '123.s could be confused with a ScaledDecimal'.
	i := Number readFrom: rs.
	self assert: i isInteger.
	self assert: i = 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' = s

]

{ #category : 'tests - arithmetic' }
IntegerTest >> testReciprocalModulo [
	1 to: 512 do: [:a |
		a + 1 to: 512 do: [:b |
			| c |
			(a gcd: b) = 1
				ifTrue:
					[c := a reciprocalModulo: b.
					self assert: (a * c) \\ b = 1]
				ifFalse: [self assert: ( a reciprocalModulo: b ) isNil]]].
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLnLn [

	self assert: (-42391158275216203514294433200 rem: -14130386091738734504764811067) = -14130386091738734504764811066.
	self assert: (-42391158275216203514294433201 rem: -14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 rem: -14130386091738734504764811067) = -1.
	self assert: (-8727963568087712425891397479476727340041448 rem: -79766443076872509863361) = -79766443076872509863360.
	self assert: (-8727963568087712425891397479476727340041449 rem: -79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 rem: -79766443076872509863361) = -1
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLnLp [

	self assert: (-42391158275216203514294433200 rem: 14130386091738734504764811067) = -14130386091738734504764811066.
	self assert: (-42391158275216203514294433201 rem: 14130386091738734504764811067) = 0.
	self assert: (-42391158275216203514294433202 rem: 14130386091738734504764811067) = -1.
	self assert: (-8727963568087712425891397479476727340041448 rem: 79766443076872509863361) = -79766443076872509863360.
	self assert: (-8727963568087712425891397479476727340041449 rem: 79766443076872509863361) = 0.
	self assert: (-8727963568087712425891397479476727340041450 rem: 79766443076872509863361) = -1
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLnSn [

	self assert: (-42391158275216203514294433201 rem: -3) = 0.
	self assert: (-42391158275216203514294433202 rem: -3) = -1.
	self assert: (-42391158275216203514294433203 rem: -3) = -2.
	self assert: (-42391158275216203514294433204 rem: -3) = 0
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLnSp [

	self assert: (-42391158275216203514294433201 rem: 3) = 0.
	self assert: (-42391158275216203514294433202 rem: 3) = -1.
	self assert: (-42391158275216203514294433203 rem: 3) = -2.
	self assert: (-42391158275216203514294433204 rem: 3) = 0
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLpLn [

	self assert: (42391158275216203514294433200 rem: -14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 rem: -14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 rem: -14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 rem: -79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 rem: -79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 rem: -79766443076872509863361) = 1
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLpLp [

	self assert: (42391158275216203514294433200 rem: 14130386091738734504764811067) = 14130386091738734504764811066.
	self assert: (42391158275216203514294433201 rem: 14130386091738734504764811067) = 0.
	self assert: (42391158275216203514294433202 rem: 14130386091738734504764811067) = 1.
	self assert: (8727963568087712425891397479476727340041448 rem: 79766443076872509863361) = 79766443076872509863360.
	self assert: (8727963568087712425891397479476727340041449 rem: 79766443076872509863361) = 0.
	self assert: (8727963568087712425891397479476727340041450 rem: 79766443076872509863361) = 1
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLpSn [

	self assert: (42391158275216203514294433201 rem: -3) = 0.
	self assert: (42391158275216203514294433202 rem: -3) = 1.
	self assert: (42391158275216203514294433203 rem: -3) = 2.
	self assert: (42391158275216203514294433204 rem: -3) = 0
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemLpSp [

	self assert: (42391158275216203514294433201 rem: 3) = 0.
	self assert: (42391158275216203514294433202 rem: 3) = 1.
	self assert: (42391158275216203514294433203 rem: 3) = 2.
	self assert: (42391158275216203514294433204 rem: 3) = 0
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSnLn [

	self assert: (0 rem: -42391158275216203514294433201) = 0.
	self assert: (-1 rem: -42391158275216203514294433201) = -1.
	self assert: (-14348907 rem: -42391158275216203514294433201) = -14348907
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSnLp [

	self assert: (0 rem: 42391158275216203514294433201) = 0.
	self assert: (-1 rem: 42391158275216203514294433201) = -1.
	self assert: (-14348907 rem: 42391158275216203514294433201) = -14348907
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSnSn [

	self assert: (0 rem: -3) = 0.
	self assert: (-1 rem: -3) = -1.
	self assert: (-9 rem: -3) = 0.
	self assert: (-10 rem: -3) = -1.
	self assert: (-11 rem: -3) = -2.
	self assert: (-12 rem: -3) = 0.
	self assert: (-13 rem: -3) = -1
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSnSp [

	self assert: (0 rem: 3) = 0.
	self assert: (-1 rem: 3) = -1.
	self assert: (-9 rem: 3) = 0.
	self assert: (-10 rem: 3) = -1.
	self assert: (-11 rem: 3) = -2.
	self assert: (-12 rem: 3) = 0.
	self assert: (-13 rem: 3) = -1
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSpLn [

	self assert: (0 rem: -42391158275216203514294433201) = 0.
	self assert: (1 rem: -42391158275216203514294433201) = 1.
	self assert: (14348907 rem: -42391158275216203514294433201) = 14348907
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSpLp [

	self assert: (0 rem: 42391158275216203514294433201) = 0.
	self assert: (1 rem: 42391158275216203514294433201) = 1.
	self assert: (14348907 rem: 42391158275216203514294433201) = 14348907
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSpSn [

	self assert: (0 rem: -3) = 0.
	self assert: (1 rem: -3) = 1.
	self assert: (9 rem: -3) = 0.
	self assert: (10 rem: -3) = 1.
	self assert: (11 rem: -3) = 2.
	self assert: (12 rem: -3) = 0.
	self assert: (13 rem: -3) = 1
]

{ #category : 'tests - division - rem:' }
IntegerTest >> testRemSpSp [

	self assert: (0 rem: 3) = 0.
	self assert: (1 rem: 3) = 1.
	self assert: (9 rem: 3) = 0.
	self assert: (10 rem: 3) = 1.
	self assert: (11 rem: 3) = 2.
	self assert: (12 rem: 3) = 0.
	self assert: (13 rem: 3) = 1
]

{ #category : 'tests - printing' }
IntegerTest >> testRomanPrinting [
	self assert: 0 printStringRoman = ''. "No symbol for zero"
	self assert: 1 printStringRoman = 'I'.
	self assert: 2 printStringRoman = 'II'.
	self assert: 3 printStringRoman = 'III'.
	self assert: 4 printStringRoman = 'IV'.
	self assert: 5 printStringRoman = 'V'.
	self assert: 6 printStringRoman = 'VI'.
	self assert: 7 printStringRoman = 'VII'.
	self assert: 8 printStringRoman = 'VIII'.
	self assert: 9 printStringRoman = 'IX'.
	self assert: 10 printStringRoman = 'X'.
	self assert: 23 printStringRoman = 'XXIII'.
	self assert: 36 printStringRoman = 'XXXVI'.
	self assert: 49 printStringRoman = 'XLIX'.
	self assert: 62 printStringRoman = 'LXII'.
	self assert: 75 printStringRoman = 'LXXV'.
	self assert: 88 printStringRoman = 'LXXXVIII'.
	self assert: 99 printStringRoman = 'XCIX'.
	self assert: 100 printStringRoman = 'C'.
	self assert: 101 printStringRoman = 'CI'.
	self assert: 196 printStringRoman = 'CXCVI'.
	self assert: 197 printStringRoman = 'CXCVII'.
	self assert: 198 printStringRoman = 'CXCVIII'.
	self assert: 293 printStringRoman = 'CCXCIII'.
	self assert: 294 printStringRoman = 'CCXCIV'.
	self assert: 295 printStringRoman = 'CCXCV'.
	self assert: 390 printStringRoman = 'CCCXC'.
	self assert: 391 printStringRoman = 'CCCXCI'.
	self assert: 392 printStringRoman = 'CCCXCII'.
	self assert: 487 printStringRoman = 'CDLXXXVII'.
	self assert: 488 printStringRoman = 'CDLXXXVIII'.
	self assert: 489 printStringRoman = 'CDLXXXIX'.
	self assert: 584 printStringRoman = 'DLXXXIV'.
	self assert: 585 printStringRoman = 'DLXXXV'.
	self assert: 586 printStringRoman = 'DLXXXVI'.
	self assert: 681 printStringRoman = 'DCLXXXI'.
	self assert: 682 printStringRoman = 'DCLXXXII'.
	self assert: 683 printStringRoman = 'DCLXXXIII'.
	self assert: 778 printStringRoman = 'DCCLXXVIII'.
	self assert: 779 printStringRoman = 'DCCLXXIX'.
	self assert: 780 printStringRoman = 'DCCLXXX'.
	self assert: 875 printStringRoman = 'DCCCLXXV'.
	self assert: 876 printStringRoman = 'DCCCLXXVI'.
	self assert: 877 printStringRoman = 'DCCCLXXVII'.
	self assert: 972 printStringRoman = 'CMLXXII'.
	self assert: 973 printStringRoman = 'CMLXXIII'.
	self assert: 974 printStringRoman = 'CMLXXIV'.
	self assert: 1069 printStringRoman = 'MLXIX'.
	self assert: 1070 printStringRoman = 'MLXX'.
	self assert: 1071 printStringRoman = 'MLXXI'.
	self assert: 1166 printStringRoman = 'MCLXVI'.
	self assert: 1167 printStringRoman = 'MCLXVII'.
	self assert: 1168 printStringRoman = 'MCLXVIII'.
	self assert: 1263 printStringRoman = 'MCCLXIII'.
	self assert: 1264 printStringRoman = 'MCCLXIV'.
	self assert: 1265 printStringRoman = 'MCCLXV'.
	self assert: 1360 printStringRoman = 'MCCCLX'.
	self assert: 1361 printStringRoman = 'MCCCLXI'.
	self assert: 1362 printStringRoman = 'MCCCLXII'.
	self assert: 1457 printStringRoman = 'MCDLVII'.
	self assert: 1458 printStringRoman = 'MCDLVIII'.
	self assert: 1459 printStringRoman = 'MCDLIX'.
	self assert: 1554 printStringRoman = 'MDLIV'.
	self assert: 1555 printStringRoman = 'MDLV'.
	self assert: 1556 printStringRoman = 'MDLVI'.
	self assert: 1651 printStringRoman = 'MDCLI'.
	self assert: 1652 printStringRoman = 'MDCLII'.
	self assert: 1653 printStringRoman = 'MDCLIII'.
	self assert: 1748 printStringRoman = 'MDCCXLVIII'.
	self assert: 1749 printStringRoman = 'MDCCXLIX'.
	self assert: 1750 printStringRoman = 'MDCCL'.
	self assert: 1845 printStringRoman = 'MDCCCXLV'.
	self assert: 1846 printStringRoman = 'MDCCCXLVI'.
	self assert: 1847 printStringRoman = 'MDCCCXLVII'.
	self assert: 1942 printStringRoman = 'MCMXLII'.
	self assert: 1943 printStringRoman = 'MCMXLIII'.
	self assert: 1944 printStringRoman = 'MCMXLIV'.
	self assert: 2004 printStringRoman = 'MMIV'.

	self assert: -1 printStringRoman = '-I'.
	self assert: -2 printStringRoman = '-II'.
	self assert: -3 printStringRoman = '-III'.
	self assert: -4 printStringRoman = '-IV'.
	self assert: -5 printStringRoman = '-V'.
	self assert: -6 printStringRoman = '-VI'.
	self assert: -7 printStringRoman = '-VII'.
	self assert: -8 printStringRoman = '-VIII'.
	self assert: -9 printStringRoman = '-IX'.
	self assert: -10 printStringRoman = '-X'.

]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testSinCosConsistency [
	"sin and cos are tricky on very large numbers. Check for correct behavior."

	"Angles within a reasonable range"
	-1234567 to: 123456789 by: 113*100 do: [:angle | | hypotenuse diff |
		hypotenuse := (angle sin squared + angle cos squared) sqrt.
		diff := (1 - hypotenuse) abs.
		self assert: diff < 0.00001].

	"Large magnitude angles. results should be consistent"
	(1 to: 1000) inject: (2 raisedToInteger: 122) into: [ :angle :i | | hypotenuse diff |
		hypotenuse := (angle sin squared + angle cos squared) sqrt.
		diff := (1 - hypotenuse) abs.
		self assert: diff < 0.00001.
		angle asFloat successor asInteger].

	"Large negative angle magnitudes"
	(1 to: 1000) inject: (2 raisedToInteger: 122) negated into: [ :angle :i | | hypotenuse diff |
		hypotenuse := (angle sin squared + angle cos squared) sqrt.
		diff := (1 - hypotenuse) abs.
		self assert: diff < 0.00001.
		angle asFloat predecessor asInteger].
]

{ #category : 'mathematical functions' }
IntegerTest >> testSinCosForHugeValues1 [
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e22.
	equivalentFloatInTwoPiRange := Float twoPi - (10202/10000).
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e22 + i.
		equivalentFloatInTwoPiRange := Float twoPi - (10202/10000) + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].

]

{ #category : 'mathematical functions' }
IntegerTest >> testSinCosForHugeValues2 [
	| bigNum equivalentFloatInTwoPiRange |

	bigNum := 1e500.
	equivalentFloatInTwoPiRange := Float twoPi + (282532 / 100000).
	self assert: (bigNum sin squared + bigNum cos squared isWithin: 2 floatsFrom: 1.0).
	self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
	self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4.

	1 to: 10 do: [	 :i |
		bigNum := 1e500 + i.
		equivalentFloatInTwoPiRange := Float twoPi + (282532 / 100000) + i.
		self assert: (bigNum sin squared + bigNum cos squared isWithin: 4 floatsFrom: 1.0).
		self assert: (bigNum sin - equivalentFloatInTwoPiRange sin) abs < 1e-4.
		self assert: (bigNum cos -equivalentFloatInTwoPiRange cos) abs < 1e-4 ].

]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashLnLn [

	self assert: -42391158275216203514294433200 // -14130386091738734504764811067 = 2.
	self assert: -42391158275216203514294433201 // -14130386091738734504764811067 = 3.
	self assert: -42391158275216203514294433202 // -14130386091738734504764811067 = 3.
	self assert: -8727963568087712425891397479476727340041448 // -79766443076872509863361 = 109418989131512359208.
	self assert: -8727963568087712425891397479476727340041449 // -79766443076872509863361 = 109418989131512359209.
	self assert: -8727963568087712425891397479476727340041450 // -79766443076872509863361 = 109418989131512359209
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashLnLp [

	self assert: -42391158275216203514294433200 // 14130386091738734504764811067 = -3.
	self assert: -42391158275216203514294433201 // 14130386091738734504764811067 = -3.
	self assert: -42391158275216203514294433202 // 14130386091738734504764811067 = -4.
	self assert: -8727963568087712425891397479476727340041448 // 79766443076872509863361 = -109418989131512359209.
	self assert: -8727963568087712425891397479476727340041449 // 79766443076872509863361 = -109418989131512359209.
	self assert: -8727963568087712425891397479476727340041450 // 79766443076872509863361 = -109418989131512359210
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashLnSn [

	self assert: -42391158275216203514294433201 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433202 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433203 // -3 = 14130386091738734504764811067.
	self assert: -42391158275216203514294433204 // -3 = 14130386091738734504764811068
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashLnSp [

	self assert: -42391158275216203514294433201 // 3 = -14130386091738734504764811067.
	self assert: -42391158275216203514294433202 // 3 = -14130386091738734504764811068.
	self assert: -42391158275216203514294433203 // 3 = -14130386091738734504764811068.
	self assert: -42391158275216203514294433204 // 3 = -14130386091738734504764811068
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashLpLn [

	self assert: 42391158275216203514294433200 // -14130386091738734504764811067 = -3.
	self assert: 42391158275216203514294433201 // -14130386091738734504764811067 = -3.
	self assert: 42391158275216203514294433202 // -14130386091738734504764811067 = -4.
	self assert: 8727963568087712425891397479476727340041448 // -79766443076872509863361 = -109418989131512359209.
	self assert: 8727963568087712425891397479476727340041449 // -79766443076872509863361 = -109418989131512359209.
	self assert: 8727963568087712425891397479476727340041450 // -79766443076872509863361 = -109418989131512359210
]

{ #category : 'tests - division' }
IntegerTest >> testSlashSlashLpLp [

	self assert: 42391158275216203514294433200 // 14130386091738734504764811067 = 2.
	self assert: 42391158275216203514294433201 // 14130386091738734504764811067 = 3.
	self assert: 42391158275216203514294433202 // 14130386091738734504764811067 = 3.
	self assert: 8727963568087712425891397479476727340041448 // 79766443076872509863361 = 109418989131512359208.
	self assert: 8727963568087712425891397479476727340041449 // 79766443076872509863361 = 109418989131512359209.
	self assert: 8727963568087712425891397479476727340041450 // 79766443076872509863361 = 109418989131512359209
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashLpSn [

	self assert: 42391158275216203514294433201 // -3 = -14130386091738734504764811067.
	self assert: 42391158275216203514294433202 // -3 = -14130386091738734504764811068.
	self assert: 42391158275216203514294433203 // -3 = -14130386091738734504764811068.
	self assert: 42391158275216203514294433204 // -3 = -14130386091738734504764811068
]

{ #category : 'tests - division' }
IntegerTest >> testSlashSlashLpSp [

	self assert: 42391158275216203514294433201 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433202 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433203 // 3 = 14130386091738734504764811067.
	self assert: 42391158275216203514294433204 // 3 = 14130386091738734504764811068
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSnLn [

	self assert: 0 // -42391158275216203514294433201 = 0.
	self assert: -1 // -42391158275216203514294433201 = 0.
	self assert: -14348907 // -42391158275216203514294433201 = 0
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSnLp [

	self assert: 0 // 42391158275216203514294433201 = 0.
	self assert: -1 // 42391158275216203514294433201 = -1.
	self assert: -14348907 // 42391158275216203514294433201 = -1
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSnSn [

	self assert: 0 // -3 = 0.
	self assert: -1 // -3 = 0.
	self assert: -9 // -3 = 3.
	self assert: -10 // -3 = 3.
	self assert: -11 // -3 = 3.
	self assert: -12 // -3 = 4.
	self assert: -13 // -3 = 4
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSnSp [

	self assert: 0 // 3 = 0.
	self assert: -1 // 3 = -1.
	self assert: -9 // 3 = -3.
	self assert: -10 // 3 = -4.
	self assert: -11 // 3 = -4.
	self assert: -12 // 3 = -4.
	self assert: -13 // 3 = -5
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSpLn [

	self assert: 0 // -42391158275216203514294433201 = 0.
	self assert: 1 // -42391158275216203514294433201 = -1.
	self assert: 14348907 // -42391158275216203514294433201 = -1
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSpLp [

	self assert: 0 // 42391158275216203514294433201 = 0.
	self assert: 1 // 42391158275216203514294433201 = 0.
	self assert: 14348907 // 42391158275216203514294433201 = 0
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSpSn [

	self assert: 0 // -3 = 0.
	self assert: 1 // -3 = -1.
	self assert: 9 // -3 = -3.
	self assert: 10 // -3 = -4.
	self assert: 11 // -3 = -4.
	self assert: 12 // -3 = -4.
	self assert: 13 // -3 = -5
]

{ #category : 'tests - division - //' }
IntegerTest >> testSlashSlashSpSp [

	self assert: 0 // 3 = 0.
	self assert: 1 // 3 = 0.
	self assert: 9 // 3 = 3.
	self assert: 10 // 3 = 3.
	self assert: 11 // 3 = 3.
	self assert: 12 // 3 = 4.
	self assert: 13 // 3 = 4
]

{ #category : 'tests - mathematical functions' }
IntegerTest >> testSqrtFloor [

	#(-1234567890123 -10 -5 -1) do: [ :each |
		self should: [ each sqrtFloor ] raise: Error ].
	#(
		0 1 2 3 4 5 10 16 30 160479924 386234481 501619156 524723498 580855366 766098594 834165249 1020363860 1042083924 1049218924
		1459774772895569 3050005981408238 4856589481837079 5650488387708463 7831037396100244) do: [ :each |
			self assert: each asFloat sqrt floor = each sqrtFloor ]
		
]

{ #category : 'tests - instance creation' }
IntegerTest >> testStringAsNumber [
	"This covers parsing in Number>>readFrom:
	Trailing decimal points should be ignored."

	self assert: ('123' asNumber isInteger).
	self assert: ('123' asNumber = 123).
	self assert: ('-123' asNumber isInteger).
	self assert: ('-123' asNumber = -123).
	self assert: ('123.' asNumber) isInteger.
	self assert: ('123.' asNumber = 123).
	self assert: ('-123.' asNumber) isInteger.
	self assert: ('-123.' asNumber = -123).
	self assert: ('123This is not to be read' asNumber isInteger).
	self assert: ('123This is not to be read' asNumber = 123).
	self assert: ('123s could be confused with a ScaledDecimal' asNumber isInteger).
	self assert: ('123s could be confused with a ScaledDecimal' asNumber = 123).
	self assert: ('123e could be confused with a Float' asNumber isInteger).
	self assert: ('123e could be confused with a Float' asNumber = 123).

]

{ #category : 'tests - bitLogic' }
IntegerTest >> testTwoComplementBitLogicWithCarry [
	"This is non regression test for http://bugs.squeak.org/view.php?id=6874"
	
	"By property of two complement, following operation is:
	...111110000 this is -16
	...111101111 this is -16-1
	...111100000 this is -32, the result of bitAnd: on two complement
	
	This test used to fail with n=31 39 47.... because of bug 6874"
	
	self assert: ((2 to: 80) allSatisfy: [:n | ((2 raisedTo: n) negated bitAnd: (2 raisedTo: n) negated - 1) = (2 raisedTo: n + 1) negated]).
]

{ #category : 'tests - bitLogic' }
IntegerTest >> testTwoComplementRightShift [
	"self run: #testTwoComplementRightShift"

	| large small |
	small := 2 << 16.
	large := 2 << 32.	
	self assert: ((small negated bitShift: -1) ~= ((small + 1) negated bitShift: -1)
		== ((large negated bitShift: -1) ~= ((large + 1) negated bitShift: -1))).
		
     self assert: ((small bitShift: -1) ~= (small + 1 bitShift: -1)
		== ((large bitShift: -1) ~= (large + 1 bitShift: -1))).
]
