'From Cuis 4.2 of 25 July 2013 [latest update: #2612] on 1 December 2015 at 10:10:37.55611 am'!

!Scanner class methodsFor: 'class initialization' stamp: 'jmv 12/1/2015 10:09'!
initialize
	"
	Scanner initialize
	"
	self initTypeTable! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 12/1/2015 10:09'!
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
	newTable atAll:  '!!%&+-*/\±×÷¬­¯,<=>”•–—˜™š›œ«»?@~€‚ƒ‘’“Ÿ°·' asByteArray put: #xBinary.

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

Scanner initialize!
