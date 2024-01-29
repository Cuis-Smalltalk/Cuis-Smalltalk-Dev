'From Cuis6.3 [latest update: #6182] on 9 January 2024 at 10:37:27 am'!

!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 10:34:51'!
isLowercase
	"Answer whether the receiver is a letter."
	"
	(Character codePoint: 16r22C7) isLowercase
	(Character codePoint: 16r042F) isLowercase
	(Character codePoint: 16r0431) isLowercase
	"
	| i |
	i := self codePoint + 1.
	LowercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^LowercaseLettersTable bitBooleanAt: i.! !

!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 10:35:17'!
isSymbolLike
	"
	(Character codePoint: 16r0041) isSymbolLike
	(Character codePoint: 16r002B) isSymbolLike
	(Character codePoint: 16r03F6) isSymbolLike
	(Character codePoint: 16r2140) isSymbolLike
	"
	| i |
	i := self codePoint + 1.
	SymbolsLikeTable bitSize < i ifTrue: [ ^false ].
	^ SymbolsLikeTable bitBooleanAt: i.! !

!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 10:36:00'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
	"
	(Character codePoint: 16r22C7) isUppercase
	(Character codePoint: 16r042F) isUppercase
	(Character codePoint: 16r0431) isUppercase
	"
	| i |
	i := self codePoint + 1.
	UppercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^UppercaseLettersTable bitBooleanAt: i.! !

!methodRemoval: UnicodeCodePoint #asUnicodeCodePoint stamp: 'jmv 1/9/2024 10:32:25'!
UnicodeCodePoint removeSelector: #asUnicodeCodePoint!
!methodRemoval: Character #asUnicodeCodePoint stamp: 'jmv 1/9/2024 10:32:24'!
Character removeSelector: #asUnicodeCodePoint!

!Character reorganize!
('accessing' asciiValue codePoint digitValue)
('comparing' < <= = > >= hash identityHash)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isCharacter isDiacriticalMark isDigit isLetter isLineSeparator isLowercase isNullCharacter isSeparator isSymbolLike isUnaccented isUppercase isValidInBinarySelectors isValidInFilenames isValidInIdentifiers isValidStartOfIdentifiers isVowel tokenish)
('copying' shallowCopy)
('printing' hex isLiteral printOn: storeOn:)
('converting' asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!

