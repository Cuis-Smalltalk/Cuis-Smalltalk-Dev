'From Cuis7.9 [latest update: #8340] on 19 November 2026 at 7:37:20 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 17/Nov/2026, 10:22:49 pm (UTC)'!
rangeOfLiteral: aLiteral scannedAt: aRange

	"A number that is the last character of the source code is scanned as starting one character before,
	as it happens to a variable. See #variable"

	^(aLiteral isNumber and: [ (self startsWithDigit: aRange) not ])
		ifTrue: [ aRange first + 1 to: aRange last ]
		ifFalse: [ aRange ]! !

!Parser methodsFor: 'expression types' stamp: 'HAW 17/Nov/2026, 10:22:49 pm (UTC)'!
startsWithDigit: aRange

	| sourceIndex |

	sourceIndex := aRange first - requestorOffset.

	^(source originalContents at: sourceIndex ifAbsent: [ ^false ]) isDigit! !


!Parser methodsFor: 'expression types' stamp: 'HAW 17/Nov/2026, 10:22:49 pm (UTC)'!
literalExpression

	parseNode := self advanceWithRangeDo: [ :lexema :range |
		encoder
			encodeLiteral: lexema
			range: (self rangeOfLiteral: lexema scannedAt: range) ].

	^true! !

!methodRemoval: Parser #isDigitAt: stamp: 'HAW 17/Nov/2026, 10:22:49 pm (UTC)'!
Parser removeSelector: #isDigitAt:!
