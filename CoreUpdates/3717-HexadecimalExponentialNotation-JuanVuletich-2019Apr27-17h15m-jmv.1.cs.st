'From Cuis 5.0 of 7 November 2016 [latest update: #3716] on 27 April 2019 at 5:17:15 pm'!

!Number class methodsFor: 'instance creation' stamp: 'jmv 4/27/2019 17:14:36'!
readFrom: stringOrStream 
	"Answer a number as described on aStream.  The number may
	include a leading radix specification, as in 16rFADE"
	| value base aStream sign |
	aStream _ (stringOrStream isMemberOf: String)
		ifTrue: [ReadStream on: stringOrStream]
		ifFalse: [stringOrStream].
	(aStream nextMatchAll: 'NaN') ifTrue: [^ Float nan].
	sign _ (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	(aStream nextMatchAll: 'Infinity') ifTrue: [^ Float infinity * sign].
	base _ 10.
	value _ Integer readFrom: aStream base: base.
	(aStream peekFor: $r)
		ifTrue: [
			"<base>r<integer>"
			(base _ value) < 2 ifTrue: [
				base = 1 ifTrue: [ ^Integer readBaseOneFrom: aStream ].
				^self error: 'Invalid radix'].
			(aStream peekFor: $-) ifTrue: [sign _ sign negated].
			value _ Integer readFrom: aStream base: base].
	(aStream peekFor: $x)
		ifTrue: [
			"0x<integer>" "Hexadecimal"
			base _ 16.
			(aStream peekFor: $-) ifTrue: [sign _ sign negated].
			value _ Integer readFrom: aStream base: base].
	^ self readRemainderOf: value from: aStream base: base withSign: sign.! !

!Number class methodsFor: 'instance creation' stamp: 'jmv 4/27/2019 16:54:05'!
readRemainderOf: integerPart from: aStream base: base withSign: sign
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
					aStream skip: -1.
					"un-gobble the period"
					^ value * sign"Number readFrom: '3r-22.2'"]].
	peekChar := aStream peek.
	('deqp' includes: peekChar)
		ifTrue: [			"<number>(e|d|q)<exponent>>" "<number>(p)<binaryExponent>>"
			aStream next.
			(aStream atEnd not and: [ (aStream peek digitValue between: 0 and: 9) or: [ '+-' includes: aStream peek ]])
				ifTrue:  [
					exp := Integer readFrom: aStream.
					scale := (peekChar = $p ifTrue: [2] ifFalse: [base]) raisedToInteger: exp.
					value := value * scale ]
				ifFalse: [
					"oops - just <number>."
					aStream skip: -1.]].
	^convertToFloat
		ifTrue: [
			(value = 0.0 and: [ sign = -1 ])
				ifTrue: [ Float negativeZero ]
				ifFalse: [ (value * sign) asFloat ]]
		ifFalse: [ value * sign ]! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 4/27/2019 17:10:13'!
scanNumber
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	('rx' includes: c)
		ifTrue: [
			base := Integer readFrom: (ReadStream on: (source copyFrom: start to: sourcePosition - 1)).
			self peekChar == $- ifTrue:[self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c == $. 
				ifTrue: [
					(self isBigDigit: self nextChar base: base) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipBigDigits: base]].
			c := self currentChar.
			('deqp'includes: c)
				ifTrue: [
					((nc := self nextChar) isDigit or: [nc == $- and:[self peekChar isDigit]]) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			c == $s 
				ifTrue: [
					self nextChar isDigit 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits.].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $. 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [
					sourcePosition := sourcePosition - 1.
					currentToken := source copyFrom: start to: sourcePosition - 1.
					^currentTokenSourcePosition := start]
				ifTrue: [self skipDigits]].
	c := self currentChar.
	('deqp' includes: c)
		ifTrue: [
			((nc := self nextChar) isDigit or: [nc == $-  and:[self peekChar isDigit]]) 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^currentTokenSourcePosition := start! !

