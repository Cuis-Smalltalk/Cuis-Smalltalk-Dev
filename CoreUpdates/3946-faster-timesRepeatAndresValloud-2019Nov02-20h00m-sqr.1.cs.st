'From Cuis 5.0 [latest update: #3866] on 2 November 2019 at 8:46:40 pm'!

!SmallInteger methodsFor: 'enumerating' stamp: 'sqr 11/2/2019 20:45:34'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	self < 1 ifTrue: [^self].
	1 to: (self bitAnd: 31) do: [:x | aBlock value].
	1 to: (self bitAnd: -32) by: 32 do:
		[:x |
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value.
			aBlock value.  aBlock value.  aBlock value.  aBlock value
		]! !


!Integer methodsFor: 'enumerating' stamp: 'sqr 11/2/2019 20:17:15'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| toGo thisRound |
	toGo := self.
	[toGo > 0] whileTrue:
		[
			thisRound := toGo min: SmallInteger maxVal.
			thisRound timesRepeat: aBlock.
			toGo := toGo - thisRound
		]! !

!methodRemoval: LargePositiveInteger #timesRepeat:!
LargePositiveInteger removeSelector: #timesRepeat:!

!LargePositiveInteger reorganize!
('arithmetic' * + - / // \\ \\\ abs negated quo:)
('bit manipulation' bitAt: bitReverse: bitXor: hashMultiply highBit highBitOfMagnitude)
('comparing' < <= > >= hash)
('converting' asFloat normalize withAtLeastNDigits:)
('mathematical functions' mightBeASquare sqrt)
('printing' printOn:base: printOn:base:nDigits:)
('system primitives' digitAt: digitAt:put: digitLength replaceFrom:to:with:startingAt:)
('testing' isLarge negative positive sign strictlyPositive)
!


!SmallInteger reorganize!
('arithmetic' * + - / // \\ gcd: quo:)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed hashMultiply lowBit)
('testing' even isLarge odd)
('comparing' < <= = > >= ~= identityHash)
('copying' shallowCopy)
('converting' asFloat)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
('enumerating' timesRepeat:)
!

