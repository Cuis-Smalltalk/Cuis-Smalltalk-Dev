'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 16 November 2015 at 3:13:10.312117 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 11/16/2015 15:07'!
isValidInIdentifiers
	"Can c be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ #( $_ $` ) statePointsTo: self ]! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 11/16/2015 15:11'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character value: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 asciiValue to: $9 asciiValue) put: #xDigit.
	newTable atAll: '!!%&+-*/\,<=>?@~' asByteArray put: #xBinary.

	newTable at: 30 put: #doIt.
	newTable at: $" asciiValue put: #xDoubleQuote.
	newTable at: $# asciiValue put: #xLitQuote.
	newTable at: $$ asciiValue put: #xDollar.
	newTable at: $' asciiValue put: #xSingleQuote.
	newTable at: $: asciiValue put: #xColon.
	newTable at: $( asciiValue put: #leftParenthesis.
	newTable at: $) asciiValue put: #rightParenthesis.
	newTable at: $. asciiValue put: #period.
	newTable at: $; asciiValue put: #semicolon.
	newTable at: $[ asciiValue put: #leftBracket.
	newTable at: $] asciiValue put: #rightBracket.
	newTable at: ${ asciiValue put: #leftBrace.
	newTable at: $} asciiValue put: #rightBrace.
	newTable at: $^ asciiValue put: #upArrow.
	newTable at: $_ asciiValue put: #xUnderscore.
	newTable at: $| asciiValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !

