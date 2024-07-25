'From Cuis7.1 [latest update: #6530] on 25 July 2024 at 3:09:16 pm'!

!Character methodsFor: 'comparing' stamp: 'jmv 7/25/2024 14:57:44'!
collatesBefore: aCharacter
	"Compare with the standard case sensitive collation order.
	Sort by #collatingCategory, and if same category,
	sort by unaccented version of the character."

	| myCat theirCat |
	self == aCharacter ifTrue: [ ^false ].

	myCat := self collatingCategory.
	theirCat := aCharacter collatingCategory.
	myCat = theirCat ifFalse: [
		"Lower collating category goes first"
		^myCat < theirCat ].

	self isLetter ifTrue: [ "Then both are"
		| meWithoutDiacritics theyWithoutDiacritics |
		meWithoutDiacritics := self  asUnaccented.
		theyWithoutDiacritics := aCharacter asUnaccented.
		"If not the same base character, compare base character"
		meWithoutDiacritics = theyWithoutDiacritics ifFalse: [
			^meWithoutDiacritics codePoint < theyWithoutDiacritics codePoint ].
		"Same base character... just compare code points"
		^self codePoint < aCharacter codePoint ].
	
	"Non-letters in same collating category? -> Lower codePoint goes first."
	^self codePoint < aCharacter codePoint.! !

!Character methodsFor: 'comparing' stamp: 'jmv 7/25/2024 15:07:16'!
collatingCategory
	"Needs to be consistent with #basicCaseSensitiveOrder"
	"
	0: Character null
	1: Separators
	2: #isSymbolLine
	3: Decimal digits
	4: Uppercase Letters
	5: Lowercase Letters
	6: other
	"
	self codePoint = 0 ifTrue: [ ^0 ].
	self isSeparator ifTrue: [ ^1 ].
	self isSymbolLike ifTrue: [ ^2 ].
	self isDigit ifTrue: [ ^3 ].
	self isUppercase ifTrue: [ ^4 ].
	self isLowercase ifTrue: [ ^5 ].
	^6! !

