'From Cuis 6.0 [latest update: #5566] on 1 December 2022 at 1:08:06 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 12/1/2022 13:00:23'!
isValidInBinarySelectors
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	"
	self isNullCharacter ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'testing' stamp: 'jmv 12/1/2022 13:05:09'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'testing' stamp: 'jmv 12/1/2022 13:06:21'!
isValidStartOfIdentifiers
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	^#(xLetter xUnderscore) statePointsTo: (Scanner typeTableAt: self).! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 12/1/2022 13:07:05'!
isValidInBinarySelectors
	"
	$< isValidInBinarySelectors
	$| isValidInBinarySelectors
	$^ isValidInBinarySelectors
	$: isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r22C7) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r042F) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r0431) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r20D7) isValidInBinarySelectors
	"
	self isNullCharacter ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 12/1/2022 13:05:13'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 12/1/2022 13:06:25'!
isValidStartOfIdentifiers
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	^#(xLetter xUnderscore) statePointsTo: (Scanner typeTableAt: self).! !


!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 12/1/2022 12:40:49'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory |
	UppercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields _ strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self halt ].
			codePoint _ ('16r', fields first) asNumber.
			codePointName _ fields at: 2.
			generalCategory _ fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				({ 'Lu'. 'Lt' } includes: generalCategory) ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Ll'. 'Lo' } includes: generalCategory) ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Pc'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Mn'. 'Mc'. 'Me' } includes: generalCategory) ifTrue: [ DiacriticalMarksTable bitBooleanAt: codePoint+1 put: true ]].
			"
			cannonicalCombiningClasses _ fields at: 4.
			characterDecompositionMapping _ fields at: 6.
			uppercaseMapping _ fields at: 13. lowercaseMapping _ fields at: 14. titlecaseMapping _ fields at: 15.
			Random next > 10.999 | (generalCategory = 'So') ifTrue: [
				{codePoint hex. codePointName. generalCategory} print ].
			decimalDigitValue _ fields at: 7. digitValue _ fields at: 8. numericValue _ fields at: 9.
			decimalDigitValue isEmpty & digitValue isEmpty & numericValue isEmpty ifFalse: [
				{'numeric'. codePoint. codePointName. generalCategory. decimalDigitValue. digitValue. numericValue} print].
			"
			]].! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 12/1/2022 12:53:37'!
typeTable: aTypeTable at: aCharacterOrCodePoint

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint asciiValue ifNotNil: [ :ascii |
		^aTypeTable at: ascii ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint = $­ ifTrue: [ ^#xBinary ].
	"Many should actually be #xIllegal"
	^#xLetter! !

!Scanner class methodsFor: 'class initialization' stamp: 'jmv 12/1/2022 12:56:01'!
initialize
	"
	Scanner initialize.
	"
	self initTypeTable.
	self initializeDoitCharacter! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 12/1/2022 12:55:47'!
initTypeTable
	| newTable |
	newTable := Array new: 127 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: '!!%&+-*/\,<=>?@~' asByteArray put: #xBinary.
	1 to: 127 do: [:index |
		(Character numericValue: index) isLetter ifTrue: [
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

UnicodeCodePoint initialize!
Scanner initialize!
