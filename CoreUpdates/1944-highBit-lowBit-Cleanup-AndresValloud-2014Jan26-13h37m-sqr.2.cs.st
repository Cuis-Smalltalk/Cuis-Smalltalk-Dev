'From Cuis 4.2 of 25 July 2013 [latest update: #1918] on 26 January 2014 at 1:57:14.67259 pm'!
!classDefinition: #Integer category: #'Kernel-Numbers'!
Number subclass: #Integer
	instanceVariableNames: ''
	classVariableNames: 'LowBitPerByteTable '
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!Integer methodsFor: 'bit manipulation' stamp: 'sqr 1/26/2014 13:38'!
highBit
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to 
	get the highest bit of the magnitude."
	self < 0 ifTrue: [^ self error: 'highBit is not defined for negative integers'].
	^ self highBitOfMagnitude! !

!Integer methodsFor: 'bit manipulation' stamp: 'sqr 1/26/2014 13:50'!
highBitOfMagnitude
	"Answer the index of the high order bit of the receiver, or zero if the 
	receiver is zero. Receiver has to be positive!!"

	| shifted bitNo |
	shifted := self < 0 ifTrue: [0 - self] ifFalse: [self].
	bitNo := 0.
	[shifted < 65536]
		whileFalse: 
			[shifted := shifted bitShift: -16.
			bitNo := bitNo + 16].
	shifted < 256
		ifFalse: 
			[shifted := shifted bitShift: -8.
			bitNo := bitNo + 8].
		
	"The high bits table can be obtained with:
	(1 to: 8) inject: #[0] into: [:highBits :rank | highBits , (highBits collect: [:e | rank])]."
	^bitNo + ( #[0 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] at: shifted + 1)! !

!Integer methodsFor: 'bit manipulation' stamp: 'sqr 1/26/2014 13:47'!
lowBit
	"Answer the index of the low order bit of this number"
	
	| index digit |
	index := 1.
	[
		digit := self digitAt: index.
		digit = 0
	] whileTrue: [index := index + 1].
	^index - 1 * 8 + digit lowBit! !

!Integer methodsFor: 'private' stamp: 'sqr 1/26/2014 13:38'!
digitDiv: arg neg: ng 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ (ZeroDivide dividend: self) signal].
	"TFEI added this line"
	l _ self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^ Array with: 0 with: self].
	"shortcut against #highBit"
	d _ 8 - arg lastDigit highBitOfMagnitude.
	div _ arg digitLshift: d.
	div _ div growto: div digitLength + 1.
	"shifts so high order word is >=128"
	rem _ self digitLshift: d.
	rem digitLength = self digitLength ifTrue: [rem _ rem growto: self digitLength + 1].
	"makes a copy and shifts"
	quo _ Integer new: l neg: ng.
	dl _ div digitLength - 1.
	"Last actual byte of data"
	ql _ l.
	dh _ div digitAt: dl.
	dnh _ dl = 1
				ifTrue: [0]
				ifFalse: [div digitAt: dl - 1].
	1 to: ql do: 
		[:k | 
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j _ rem digitLength + 1 - k.
		"r1 _ rem digitAt: j."
		(rem digitAt: j)
			= dh
			ifTrue: [qhi _ qlo _ 15
				"i.e. q=255"]
			ifFalse: 
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
				Note that r1,r2 are bytes, not nibbles.  
				Be careful not to generate intermediate results exceeding 13  
				bits."
				"r2 _ (rem digitAt: j - 1)."
				t _ ((rem digitAt: j)
							bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitShift: -4).
				qhi _ t // dh.
				t _ (t \\ dh bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitAnd: 15).
				qlo _ t // dh.
				t _ t \\ dh.
				"Next compute (hi,lo) _ q*dnh"
				hi _ qhi * dnh.
				lo _ qlo * dnh + ((hi bitAnd: 15)
								bitShift: 4).
				hi _ (hi bitShift: -4)
							+ (lo bitShift: -8).
				lo _ lo bitAnd: 255.
				"Correct overestimate of q.  
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 _ j < 3
							ifTrue: [0]
							ifFalse: [rem digitAt: j - 2].
				[(t < hi
					or: [t = hi and: [r3 < lo]])
					and: 
						["i.e. (t,r3) < (hi,lo)"
						qlo _ qlo - 1.
						lo _ lo - dnh.
						lo < 0
							ifTrue: 
								[hi _ hi - 1.
								lo _ lo + 256].
						hi >= dh]]
					whileTrue: [hi _ hi - dh].
				qlo < 0
					ifTrue: 
						[qhi _ qhi - 1.
						qlo _ qlo + 16]].
		"Subtract q*div from rem"
		l _ j - dl.
		a _ 0.
		1 to: div digitLength do: 
			[:i | 
			hi _ (div digitAt: i)
						* qhi.
			lo _ a + (rem digitAt: l) - ((hi bitAnd: 15)
							bitShift: 4) - ((div digitAt: i)
							* qlo).
			rem digitAt: l put: lo - (lo // 256 * 256).
			"sign-tolerant form of (lo bitAnd: 255)"
			a _ lo // 256 - (hi bitShift: -4).
			l _ l + 1].
		a < 0
			ifTrue: 
				["Add div back into rem, decrease q by 1"
				qlo _ qlo - 1.
				l _ j - dl.
				a _ 0.
				1 to: div digitLength do: 
					[:i | 
					a _ (a bitShift: -8)
								+ (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l _ l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4)
				+ qlo].
	rem _ rem
				digitRshift: d
				bytes: 0
				lookfirst: dl.
	^ Array with: quo with: rem! !


!Integer class methodsFor: 'class initialization' stamp: 'sqr 1/26/2014 13:56'!
initialize	"Integer initialize"
	"Ensure we have the right compact class index"

	"LPI has been a compact class forever - just ensure basic correctness"
	(LargePositiveInteger indexIfCompact = 5) ifFalse:[
		(Smalltalk compactClassesArray at: 5)
			ifNil:[LargePositiveInteger becomeCompactSimplyAt: 5]
			ifNotNil:[self error: 'Unexpected compact class setup']].

	"Cog requires LNI to be compact at 4 (replacing PseudoContext)"
	(LargeNegativeInteger indexIfCompact = 4) ifFalse:[
		"PseudoContext will likely get removed at some point so write this test
		without introducing a hard dependency"
		(Smalltalk compactClassesArray at: 4) name == #PseudoContext
			ifTrue:[Smalltalk compactClassesArray at: 4 put: nil].
		(Smalltalk compactClassesArray at: 4)
			ifNil:[LargeNegativeInteger becomeCompactSimplyAt: 4]
			ifNotNil:[self error: 'Unexpected compact class setup']]! !


!LargePositiveInteger methodsFor: 'bit manipulation' stamp: 'sqr 1/26/2014 13:52'!
highBitOfMagnitude
	"Answer the index of the high order bit of the magnitude of the  
	receiver, or zero if the receiver is zero.  
	This method is used for LargeNegativeIntegers as well,  
	since Squeak's LargeIntegers are sign/magnitude."
	| realLength lastDigit |
	realLength _ self digitLength.
	[(lastDigit _ self digitAt: realLength) = 0]
		whileTrue: [(realLength _ realLength - 1) = 0 ifTrue: [^ 0]].
	^realLength - 1 * 8 + lastDigit highBitOfMagnitude! !


!LargeNegativeInteger methodsFor: 'bit manipulation' stamp: 'sqr 1/26/2014 13:39'!
highBit
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."

	^self error: 'highBit is not defined for negative integers'! !


!SmallInteger methodsFor: 'bit manipulation' stamp: 'sqr 1/26/2014 13:48'!
lowBit
	" Answer the index of the low order one bit.
		2r00101000 lowBit       (Answers: 4)
		2r-00101000 lowBit      (Answers: 4)"

	self = 0 ifTrue: [^0].
	^(self bitXor: self - 1) highBit! !

!methodRemoval: SmallInteger #highBit!
SmallInteger removeSelector: #highBit!
!methodRemoval: SmallInteger #highBitOfMagnitude!
SmallInteger removeSelector: #highBitOfMagnitude!
!methodRemoval: SmallInteger #highBitOfPositiveReceiver!
SmallInteger removeSelector: #highBitOfPositiveReceiver!

!SmallInteger reorganize!
('arithmetic' * + - / // gcd: quo: \\)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed hashMultiply lowBit)
('testing' even odd)
('comparing' < <= = > >= identityHash ~=)
('copying' clone shallowCopy)
('converting' asCharacter asFloat)
('printing' decimalDigitLength destinationBuffer: numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength instVarAt: nextInstance nextObject)
('private' fromString:radix:)
('mathematical functions' sqrt)
!

!methodRemoval: Integer class #initializeLowBitPerByteTable!
Integer class removeSelector: #initializeLowBitPerByteTable!
Integer initialize!
!classDefinition: #Integer category: #'Kernel-Numbers'!
Number subclass: #Integer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!
