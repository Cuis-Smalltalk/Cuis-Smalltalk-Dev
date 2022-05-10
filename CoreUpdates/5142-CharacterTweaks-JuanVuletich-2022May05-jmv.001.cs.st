'From Cuis 6.0 [latest update: #5138] on 5 May 2022 at 5:02:49 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:26:23'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ self  = $_ ]! !

!Character methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:26:27'!
isValidStartOfIdentifiers
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	^self isLetter or: [ self  = $_ ]! !

