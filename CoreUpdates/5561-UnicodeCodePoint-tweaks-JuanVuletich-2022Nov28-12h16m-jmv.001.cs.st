'From Cuis 6.0 [latest update: #5560] on 28 November 2022 at 12:17:28 pm'!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value '
	classVariableNames: 'BinarySelectorsTable DiacriticalMarksTable LowercaseLettersTable SymbolsLikeTable UppercaseLettersTable '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:59:36'!
isLowercase
	"Answer whether the receiver is a letter."
	"
	(UnicodeCodePoint codePoint: 16r22C7) isLowercase
	(UnicodeCodePoint codePoint: 16r042F) isLowercase
	(UnicodeCodePoint codePoint: 16r0431) isLowercase
	"
	| i |
	i _ value + 1.
	LowercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^LowercaseLettersTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:59:47'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
	"
	(UnicodeCodePoint codePoint: 16r22C7) isUppercase
	(UnicodeCodePoint codePoint: 16r042F) isUppercase
	(UnicodeCodePoint codePoint: 16r0431) isUppercase
	"
	| i |
	i _ value + 1.
	UppercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^UppercaseLettersTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:20:16'!
isValidInBinarySelectors
	"
	(UnicodeCodePoint codePoint: 16r22C7) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r042F) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r0431) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r20D7) isValidInBinarySelectors
	"
	| i |
	(Character codePoint: value) ifNotNil: [ :ch | ^ch isValidInBinarySelectors ].
	i _ value + 1.
	SymbolsLikeTable bitSize < i ifTrue: [ ^false ].
	^ (SymbolsLikeTable bitBooleanAt: i) or: [DiacriticalMarksTable bitBooleanAt: i].! !

!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: 'DiacriticalMarksTable LowercaseLettersTable SymbolsLikeTable UppercaseLettersTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!
