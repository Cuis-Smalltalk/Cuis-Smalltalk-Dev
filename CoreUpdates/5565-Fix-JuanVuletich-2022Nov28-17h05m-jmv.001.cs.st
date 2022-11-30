'From Cuis 6.0 [latest update: #5565] on 28 November 2022 at 5:06:46 pm'!

!Scanner class methodsFor: 'class initialization' stamp: 'jmv 11/28/2022 17:05:58'!
initialize
	"
	Scanner initialize
	"
	self initTypeTable.
	self initializeDoitCharacter! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 11/28/2022 17:05:42'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	newTable atAll: '!!%&+-*/\,<=>?@~' asByteArray put: #xBinary.
	128 to: 255 do: [:index |
		(Character numericValue: index) isSymbolLike
			ifTrue: [
				newTable at: index put: #xBinary ]].
	newTable at: $­ numericValue put: #xBinary.
	1 to: 255 do: [:index |
		(Character numericValue: index) isValidInIdentifiers
			ifTrue: [
				"Digits and $_ are overwritten later"
				newTable at: index put: #xLetter ]].
	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $` numericValue put: #xBacktick.
	newTable at: $: numericValue put: #xColon.
	newTable at: $( numericValue put: #leftParenthesis.
	newTable at: $) numericValue put: #rightParenthesis.
	newTable at: $. numericValue put: #period.
	newTable at: $; numericValue put: #semicolon.
	newTable at: $[ numericValue put: #leftBracket.
	newTable at: $] numericValue put: #rightBracket.
	newTable at: ${ numericValue put: #leftBrace.
	newTable at: $} numericValue put: #rightBrace.
	newTable at: $^ numericValue put: #upArrow.
	newTable at: $_ numericValue put: #xUnderscore.
	newTable at: $| numericValue put: #verticalBar.
	newTable at: Character smalltalkLeftArrow numericValue put: #leftArrow.
	newTable at: Character smalltalkUpArrow numericValue put: #upArrow.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !

Scanner initialize!
