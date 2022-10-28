'From Cuis 6.0 [latest update: #5537] on 28 October 2022 at 4:45:11 pm'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 16:13:45'!
smalltalkRightArrow
	"
	Character smalltalkRightArrow
	"
	^ Character numericValue: 30! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 16:13:51'!
smalltalkUpArrow
	"
	Character smalltalkUpArrow
	"
	^ Character numericValue: 29! !


!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 16:24:43'!
arrowDown
	"
	UnicodeCodePoint arrowDown asString
	"
	^ self codePoint: 16r2193! !

!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 16:24:07'!
arrowLeft
	"
	UnicodeCodePoint arrowLeft asString
	Some examples to try.
		2190 - 	Leftwards Arrow
		21D0 - 	Leftwards Double Arrow
		21E0	 - Leftwards Dashed Arrow
		21E6 - Leftwards White Arrow
		21FD	 - Leftwards Open-Headed Arrow
		27F5 - Long Leftwards Arrow
		2B05	 - Leftwards Black Arrow	"

	^ self codePoint: 16r2190! !

!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 16:24:24'!
arrowRight
	"
	UnicodeCodePoint arrowRight asString
	"
	^ self codePoint: 16r2192! !

!UnicodeCodePoint class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 16:23:01'!
arrowUp
	"
	UnicodeCodePoint arrowUp asString
	Some examples to try.
		2191 - 	Upwards Arrow
		21D1 - 	Upwards Double Arrow
		21E1	 - Upwards Dashed Arrow
		21E7 - Upwards White Arrow
		2B06	 - Upwards Black Arrow	"

	^ self codePoint: 16r2191! !


!Scanner class methodsFor: 'class initialization' stamp: 'jmv 10/28/2022 16:44:41'!
initialize
	"
	Scanner initialize
	"
	self initTypeTable.
	self initializeDoitCharacter! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 10/28/2022 16:44:34'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	'¡!!%&+-*/\·×÷¬­¯,<=>«»¿?@~' do: [ :c | newTable at: c codePoint put: #xBinary ].
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
	newTable at: Character smalltalkRightArrow numericValue put: #xBinary.
	newTable at: Character smalltalkDownArrow numericValue put: #xBinary.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/28/2022 15:27:26'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^Utf8String streamContents: [ :stream |
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: aSourceCode		]! !

!methodRemoval: TextModel class #arrowStringForAssignment stamp: 'jmv 10/28/2022 16:40:01'!
TextModel class removeSelector: #arrowStringForAssignment!
!methodRemoval: TextModel class #arrowStringForReturn stamp: 'jmv 10/28/2022 16:40:01'!
TextModel class removeSelector: #arrowStringForReturn!

!TextModel class reorganize!
('instance creation' openLabel: withText:)
('window color' windowColor)
!

Scanner initialize!
