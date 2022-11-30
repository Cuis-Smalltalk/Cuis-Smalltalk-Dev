'From Cuis 6.0 [latest update: #5561] on 28 November 2022 at 12:25:27 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 11/28/2022 11:06:37'!
isLetter
	"Answer whether the receiver is a letter."

	^self isLowercase or: [ self isUppercase ]! !

!Character methodsFor: 'testing' stamp: 'jmv 11/28/2022 11:00:39'!
isLowercase
	^self asUnicodeCodePoint isLowercase! !

!Character methodsFor: 'testing' stamp: 'jmv 11/28/2022 11:00:31'!
isUppercase
	^self asUnicodeCodePoint isUppercase! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 11/28/2022 12:24:17'!
initialize
	"Create the table of unique Characters.
	Character initialize
	"
	self initializeLookupTables.
	self initializeUnicodeCodePoints.! !

!Character class methodsFor: 'class initialization' stamp: 'jmv 11/28/2022 11:07:48'!
initializeLookupTables
	LowercaseMappingTable _ Array new: 256.
	UppercaseMappingTable _ Array new: 256.
	UnaccentedTable _ ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char _ self numericValue: idx.
			LowercaseMappingTable
				at: idx + 1
				put: char.
			UppercaseMappingTable
				at: idx + 1
				put: char.
			UnaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		group size > 1
			ifTrue: [ | lowercaseChar uppercaseChar |
				uppercase _ group first numericValue.
				lowercase _ group second numericValue.
				lowercaseChar _ self numericValue: lowercase.
				uppercaseChar _ self numericValue: uppercase.
				LowercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				LowercaseMappingTable
					at: uppercase + 1
					put: lowercaseChar.
				UppercaseMappingTable
					at: lowercase + 1
					put: uppercaseChar.
				UppercaseMappingTable
					at: uppercase + 1
					put: uppercaseChar.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase _ group third numericValue.
						unaccentedLowercase _ group fourth numericValue.
						UnaccentedTable at: uppercase+1 put: unaccentedUppercase.
						UnaccentedTable at: lowercase+1 put: unaccentedLowercase]]
			ifFalse: [ | lowercaseChar |
				lowercase _ group first numericValue.
				lowercaseChar _ self numericValue: lowercase.
				LowercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				UppercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar ]].! !

!Character class methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2022 12:22:16'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Object immediateSubclass: #Character
	instanceVariableNames: ''value''
	classVariableNames: ''CharacterTable UnaccentedTable UnicodeCodePoints LowercaseMappingTable UppercaseMappingTable''
	poolDictionaries: ''''
	category: ''', self category, ''''
! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 11/28/2022 10:42:48'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255 do: [:index |
		(Character numericValue: index) isValidInIdentifiers
			ifTrue: [
				"Digits and $_ are overwritten later"
				newTable at: index put: #xLetter ].
		(Character numericValue: index) isSymbolLike
			ifTrue: [
				"$:, $^, $|, and arrows overwritten later"
				newTable at: index put: #xBinary ].
	].
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
	newTable at: Character smalltalkRightArrow numericValue put: #xBinary.
	newTable at: Character smalltalkDownArrow numericValue put: #xBinary.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !

Character initialize!
