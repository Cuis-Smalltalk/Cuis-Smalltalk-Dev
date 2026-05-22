"
In 32-bit images my instances are 31-bit numbers, stored in twos complement form. The allowable range is approximately +- 1 billion.

In 64-bit images my instances are 61-bit numbers, stored in twos complement form. The allowable range is approximately +- 10^18 (+- 1 quintillion)

(See SmallInteger minVal, maxVal).

Of the various classes in the Number hierarchy, SmallInteger gives:
- Maximum performance
- Top precision
- Restricted possible values

LargePositive(Negative)Integer and Fraction give increasing generality (more possible values) at the expense of performance.

Float gives more generality at the expense of precision.

Please see the class comments of the other Number classes.
"
Class {
	#name : 'SmallInteger',
	#superclass : 'Integer',
	#classInstVars : [
		'minVal',
		'maxVal'
	],
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'instance creation' }
SmallInteger class >> basicNew [

	self error: 'SmallIntegers can only be created by performing arithmetic'
]

{ #category : 'constants' }
SmallInteger class >> maxVal [
	"Answer the maximum value for a SmallInteger."
	maxVal ifNil: [
		"Don't create any LargeInteger!"
		maxVal := -1 - self minVal ].
	^maxVal.
]

{ #category : 'constants' }
SmallInteger class >> minVal [
	"Answer the minimum value for a SmallInteger."
	minVal ifNil: [
		"Don't create any LargeInteger!"
		minVal := Smalltalk wordSize = 8
			ifTrue: [((1 bitShift: 59) negated bitShift: 1)]
			ifFalse: [((1 bitShift: 29) negated bitShift: 1)]].
	^minVal.
]

{ #category : 'instance creation' }
SmallInteger class >> new [

	self basicNew	"generates an error"
]

{ #category : 'arithmetic' }
SmallInteger >> * aNumber  [
	"Primitive. Multiply the receiver by the argument and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 9>
	^ super * aNumber
]

{ #category : 'arithmetic' }
SmallInteger >> + aNumber  [
	"Primitive. Add the receiver to the argument and answer with the result
	if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 1>
	^ super + aNumber
]

{ #category : 'arithmetic' }
SmallInteger >> - aNumber  [
	"Primitive. Subtract the argument from the receiver and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation
	whatIsAPrimitive."

	<primitive: 2>
	^super - aNumber
]

{ #category : 'arithmetic' }
SmallInteger >> / aNumber  [
	"Primitive. This primitive (for /) divides the receiver by the argument
	and returns the result if the division is exact. Fail if the result is not a
	whole integer. Fail if the argument is 0 or is not a SmallInteger. Optional.
	No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 10>
	aNumber isZero ifTrue: [^ZeroDivide new signalReceiver: self selector: #/ argument: aNumber ].
	^(aNumber isMemberOf: SmallInteger)
		ifTrue: [(Fraction numerator: self denominator: aNumber) reduced]
		ifFalse: [super / aNumber]
]

{ #category : 'arithmetic' }
SmallInteger >> // aNumber  [
	"Primitive. Divide the receiver by the argument and answer with the
	result. Round the result down towards negative infinity to make it a
	whole integer. Fail if the argument is 0 or is not a SmallInteger.
	Essential. No Lookup. See Object documentation whatIsAPrimitive. "

	<primitive: 12>
	^ super // aNumber 	"Do with quo: if primitive fails"
]

{ #category : 'comparing' }
SmallInteger >> < aNumber  [
	"Compare the receiver with the argument and answer with true if the receiver is less 
	than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 3>
	^super < aNumber
]

{ #category : 'comparing' }
SmallInteger >> <= aNumber  [
	"Compare the receiver with the argument and answer true if the receiver is less 
	than or equal to the argument.  Otherwise answer false.  Fail if the argument is 
	not a SmallInteger.  Optional.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 5>
	^super <= aNumber
]

{ #category : 'comparing' }
SmallInteger >> = aNumber  [
	"Compare the receiver with the argument and answer true if the receiver is 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 7>
	^super = aNumber
]

{ #category : 'comparing' }
SmallInteger >> > aNumber  [
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 4>
	^super > aNumber
]

{ #category : 'comparing' }
SmallInteger >> >= aNumber  [
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than or equal to the argument.  Otherwise answer false.  Fail if the 
	argument is not a SmallInteger.  Optional.  No Lookup.  See Object 
	documentation whatIsAPrimitive."

	<primitive: 6>
	^super >= aNumber
]

{ #category : 'arithmetic' }
SmallInteger >> \\ aNumber  [
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided by
	the argument Fail if the argument is 0 or is not a SmallInteger. Optional.
	No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 11>
	^ super \\ aNumber 	"will use // to compute it if primitive fails"
]

{ #category : 'comparing' }
SmallInteger >> ~= aNumber  [
	"Compare the receiver with the argument and answer true if the receiver is not 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 8>
	^super ~= aNumber
]

{ #category : 'converting' }
SmallInteger >> asFloat [
	"Primitive. Answer a Float that represents the value of the receiver.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 40>
	self primitiveFailed
]

{ #category : 'bit manipulation' }
SmallInteger >> bitAnd: arg  [
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 14>
	self >= 0 ifTrue: [^ arg bitAnd: self].
	^ arg < 0
		ifTrue: [(arg bitInvert bitOr: self bitInvert) bitInvert]
		ifFalse: [arg bitClear: self bitInvert]
]

{ #category : 'bit manipulation' }
SmallInteger >> bitOr: arg  [
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 15>
	self >= 0 ifTrue: [^ arg bitOr: self].
	^ arg < 0
		ifTrue: [(self bitInvert bitAnd: arg bitInvert) bitInvert]
		ifFalse: [(self bitInvert bitClear: arg) bitInvert]
]

{ #category : 'bit manipulation' }
SmallInteger >> bitShift: arg  [
	"Primitive. Answer an Integer whose value is the receiver's value shifted
	left by the number of bits indicated by the argument. Negative arguments
	shift right. The receiver is interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 17>
	self >= 0 ifTrue: [^ super bitShift: arg].
	^ arg >= 0
		ifTrue: [(self negated bitShift: arg) negated]
		ifFalse: [(self bitInvert bitShift: arg) bitInvert].
]

{ #category : 'bit manipulation' }
SmallInteger >> bitXor: arg  [
	"Primitive. Answer an Integer whose bits are the logical XOR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 16>
	self >= 0 ifTrue: [^ arg bitXor: self].
	^ arg < 0
		ifTrue: [self bitInvert bitXor: arg bitInvert]
		ifFalse: [(self bitInvert bitXor: arg) bitInvert].
]

{ #category : 'bit manipulation' }
SmallInteger >> byteReversed [
	"Answer the receiver with bits reversed in a byte.
	The receiver must be between 0 and 255.
	The constant has been obtained by this snippet:
	(0 to: 255) collect: [:e |
		| r |
		r := ((e bitAnd: 2r11110000) bitShift: -4) + ((e bitAnd: 2r00001111) bitShift: 4).
		r := ((r bitAnd: 2r11001100) bitShift: -2) + ((r bitAnd: 2r00110011) bitShift: 2).
		((r bitAnd: 2r10101010) bitShift: -1) + ((r bitAnd: 2r01010101) bitShift: 1).] as: ByteArray"
	
	^#[0 128 64 192 32 160 96 224 16 144 80 208 48 176 112 240 8 136 72 200 40 168 104 232 24 152 88 216 56 184 120 248 4 132 68 196 36 164 100 228 20 148 84 212 52 180 116 244 12 140 76 204 44 172 108 236 28 156 92 220 60 188 124 252 2 130 66 194 34 162 98 226 18 146 82 210 50 178 114 242 10 138 74 202 42 170 106 234 26 154 90 218 58 186 122 250 6 134 70 198 38 166 102 230 22 150 86 214 54 182 118 246 14 142 78 206 46 174 110 238 30 158 94 222 62 190 126 254 1 129 65 193 33 161 97 225 17 145 81 209 49 177 113 241 9 137 73 201 41 169 105 233 25 153 89 217 57 185 121 249 5 133 69 197 37 165 101 229 21 149 85 213 53 181 117 245 13 141 77 205 45 173 109 237 29 157 93 221 61 189 125 253 3 131 67 195 35 163 99 227 19 147 83 211 51 179 115 243 11 139 75 203 43 171 107 235 27 155 91 219 59 187 123 251 7 135 71 199 39 167 103 231 23 151 87 215 55 183 119 247 15 143 79 207 47 175 111 239 31 159 95 223 63 191 127 255] at: 1 + self
]

{ #category : 'printing' }
SmallInteger >> decimalDigitLength [
	"Answer the number of digits printed out in base 10.
	 Note that this only works for positive SmallIntegers up to 63-bits."
	
	self <= 99999999 "8" ifTrue: [
		self <= 9999 "4" ifTrue: [
			self <= 99 "2" ifTrue: [
				self <= 9 "1" ifTrue: [ ^1].
				^2 ].
			self <= 999 "3" ifTrue: [ ^3].
			^4 ].
		self <= 999999 "6" ifTrue: [
			self <= 99999 "5" ifTrue: [ ^5 ].
			^6 ].
		self <= 9999999 "7" ifTrue: [ ^7 ].
		^8 ].
	self <= 1073741823 "10" ifTrue: [ "This is here only to avoid LargeInteger comparisons in 32-bit VMs"
		self <= 999999999 "9" ifTrue: [ ^9 ].
		^10 ].
	self <= 999999999999999 "15" ifTrue: [
		self <= 9999999999999 "13" ifTrue: [
			self <= 99999999999 "11" ifTrue: [
				self <= 9999999999 "10" ifTrue: [ ^10 ].
				^11 ].
			self <= 999999999999 "12" ifTrue: [ ^12 ].
			^13 ].
		self <= 99999999999999 "14" ifTrue: [ ^14 ].
		^15 ].
	self <= 99999999999999999 "17" ifTrue: [
		self <= 9999999999999999 "16" ifTrue: [ ^16 ].
		^17 ].
	self <= 999999999999999999 "18" ifTrue: [ ^18 ].
	^19 "(1 << 60 - 1) asString size"
]

{ #category : 'system primitives' }
SmallInteger >> digitAt: n  [
	"Answer the value of an indexable field in the receiver.  LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds."
	n > Smalltalk wordSize ifTrue: [^ 0].
	self < 0
		ifTrue: 
			[self = SmallInteger minVal ifTrue: [
				"Can't negate minVal -- treat specially"
				^ Smalltalk wordSize = 4
					ifTrue: [ #(0 0 0 64) at: n ]
					ifFalse: [ #(0 0 0 0 0 0 0 16) at: n ]].
			^ ((0-self) bitShift: (1-n)*8) bitAnd: 16rFF]
		ifFalse: [^ (self bitShift: (1-n)*8) bitAnd: 16rFF]
]

{ #category : 'system primitives' }
SmallInteger >> digitAt: n put: value  [
	"Fails. The digits of a small integer can not be modified."

	self error: 'You can''t store in a SmallInteger'
]

{ #category : 'system primitives' }
SmallInteger >> digitLength [
	"Answer the number of indexable fields in the receiver. This value is the 
	 same as the largest legal subscript. Included so that a SmallInteger can 
	 behave like a LargePositiveInteger or LargeNegativeInteger."

	| value length |
	length := 1.
	value := self.
	value >= 0
		ifTrue:
			[[value > 255] whileTrue:
				[value := value bitShift: -8.
				 length := length + 1]]
		ifFalse:
			[[value < -255] whileTrue:
				[value := value bitShift: -8.
				 length := length + 1]].
	^length
]

{ #category : 'testing' }
SmallInteger >> even [
	^(self bitAnd: 1) = 0
]

{ #category : 'arithmetic' }
SmallInteger >> gcd: anInteger  [
	"See SmallInteger (Integer) | gcd:"
	| n m |
	n := self.
	m := anInteger.
	[n = 0]
		whileFalse: 
			[n := m \\ (m := n)].
	^ m abs
]

{ #category : 'comparing' }
SmallInteger >> hashMultiply [
	"Multiply by 1664525, take lower 28 bits, do not use LargeIntegers (not even in 32 bit images)"
	| low |

	low := self bitAnd: 16383.
	^(16r260D * low + ((16r260D * (self bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384))
			bitAnd: 16r0FFFFFFF
]

{ #category : 'comparing' }
SmallInteger >> identityHash [

	^self
]

{ #category : 'testing' }
SmallInteger >> isLarge [
	^false
]

{ #category : 'bit manipulation' }
SmallInteger >> lowBit [
	" Answer the index of the low order one bit.
		2r00101000 lowBit       (Answers: 4)
		2r-00101000 lowBit      (Answers: 4)"

	self = 0 ifTrue: [^0].
	^(self bitXor: self - 1) highBit
]

{ #category : 'system primitives' }
SmallInteger >> nextInstance [
	"SmallIntegers can't be enumerated this way.  There are a finite number of them from from (SmallInteger minVal) to (SmallInteger maxVal), but you'll have to enumerate them yourself with:
	(SmallInteger minVal) to: (SmallInteger maxVal) do: [:integer | <your code here>].
	"

	self shouldNotImplement 
]

{ #category : 'system primitives' }
SmallInteger >> nextObject [
	"SmallIntegers are immediate objects, and, as such, do not have successors in object memory."

	self shouldNotImplement 
]

{ #category : 'printing' }
SmallInteger >> numberOfDigitsInBase: b  [
	"Return how many digits are necessary to print this number in base b.
	Mostly same as super but an optimized version for base 10 case"
	
	b = 10 ifFalse: [^super numberOfDigitsInBase: b].
	self < 0 ifTrue: [^self negated numberOfDigitsInBase: b].
	^self decimalDigitLength
]

{ #category : 'testing' }
SmallInteger >> odd [
	^(self bitAnd: 1) = 1
]

{ #category : 'printing' }
SmallInteger >> printOn: stream base: base  [
	"Append a representation of this number in base b on aStream."

	self printOn: stream base: base length: 0 padded: false
]

{ #category : 'printing' }
SmallInteger >> printOn: stream base: base length: minimumLength padded: padWithZeroes [

	| n numberOfDigits totalLength divisor |
	self < 0
		ifTrue: [
			n := self negated.
			totalLength := 1 ]
		ifFalse: [
			n := self.
			totalLength := 0 ].
	numberOfDigits := n numberOfDigitsInBase: base.
	totalLength := totalLength + numberOfDigits.
	padWithZeroes ifFalse: [
		[ totalLength < minimumLength ] whileTrue: [
			stream space.
			totalLength := totalLength + 1 ] ].
	n = self ifFalse: [ stream nextPut: $- ].
	padWithZeroes ifTrue: [
		[ totalLength < minimumLength ] whileTrue: [
			stream nextPut: $0.
			totalLength := totalLength + 1 ] ].
	divisor := (base raisedToInteger: numberOfDigits - 1).
	[ divisor > 0 ] whileTrue: [
		| digit |
		digit := n // divisor.
		stream nextPut: ('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' at: digit + 1).
		n := n - (digit * divisor).
		divisor := divisor // base ]
]

{ #category : 'printing' }
SmallInteger >> printOn: aStream base: b nDigits: n  [
	"Append a representation of this number in base b on aStream using nDigits.
	self must be positive."

	self printOn: aStream base: b length: n padded: true
]

{ #category : 'printing' }
SmallInteger >> printString [
	"Highly optimized version for base 10
	and that we know it is a SmallInteger."
	
	| integer next result len |
	self = 0 ifTrue: [^'0'].
	self < 0 ifTrue: [^'-', self negated printString].
	len := self decimalDigitLength.
	result := ByteArray new: len.
	integer := self.
	len to: 1 by: -1 do: [:i |
		next := integer // 10.
		result at: i put: 48 + (integer - (next * 10)).
		integer := next ].
	^result asString
]

{ #category : 'printing' }
SmallInteger >> printStringBase: b  [
	"Return a String representation of this number in base b.
	For SmallIntegers, it is more efficient to print directly in a String,
	rather than using a Stream like super."

	self < 0
		ifTrue: [^ '-'
				, (self negated printStringBase: b)].
	self < b
		ifTrue: [^ String
				with: (Character digitValue: self)].
	^ self printStringBase: b nDigits: (self numberOfDigitsInBase: b)
]

{ #category : 'printing' }
SmallInteger >> printStringBase: b nDigits: n [
	"Return a string representation of this number in base b with n digits (left padded with 0).
	Should be invoked with: 0 <= self < (b raisedToInteger: n)."
	
	| integer next result |
	result := String new: n.
	integer := self.
	n to: 1 by: -1 do: [:i |
		next := integer // b.
		result at: i put: (Character digitValue: (integer - (next * b))).
		integer := next].
	^result
]

{ #category : 'arithmetic' }
SmallInteger >> quo: aNumber  [
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards zero to make it a whole integer. 
	Fail if the argument is 0 or is not a SmallInteger. Optional. See Object 
	documentation whatIsAPrimitive."
	<primitive: 13>
	aNumber = 0 ifTrue: [^ZeroDivide new signalReceiver: self selector: #quo: argument: aNumber ].
	(aNumber isMemberOf: SmallInteger)
		ifFalse: [^ super quo: aNumber].
	(aNumber = -1 and: [self = self class minVal])
		ifTrue: ["result is aLargeInteger" ^ self negated].
	self primitiveFailed
]

{ #category : 'copying' }
SmallInteger >> shallowCopy [

	^self
]

{ #category : 'mathematical functions' }
SmallInteger >> sqrt [

	self positive ifTrue: [^super sqrt].
	^NegativePowerError new signalReceiver: self selector: #sqrt arguments: {}
]

{ #category : 'printing' }
SmallInteger >> threeDigitName [

	| units answer |

	self = 0 ifTrue: [^''].
	units := #('one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' 'ten' 
		'eleven' 'twelve' 'thirteen' 'fourteen' 'fifteen' 'sixteen' 'seventeen' 
		'eighteen' 'nineteen').
	self > 99 ifTrue: [
		answer := (units at: self // 100),' hundred'.
		(self \\ 100) = 0 ifFalse: [
			answer := answer,' ',(self \\ 100) threeDigitName
		].
		^answer
	].
	self < 20 ifTrue: [
		^units at: self
	].
	answer := #('twenty' 'thirty' 'forty' 'fifty' 'sixty' 'seventy' 'eighty' 'ninety')
			at: self // 10 - 1.
	(self \\ 10) = 0 ifFalse: [
		answer := answer,'-',(units at: self \\ 10)
	].
	^answer
]
