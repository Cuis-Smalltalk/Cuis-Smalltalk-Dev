Class {
	#name : 'SymbolTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Text',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Text'
}

{ #category : 'tests - selector validation' }
SymbolTest >> testIsNotValidBinarySelectorIfItContainsACharacterNotAllowed [

	self deny: #'(' isValidSelector
]

{ #category : 'tests - selector validation' }
SymbolTest >> testIsNotValidKeywordSelectorIfItContainsSeparators [

	self deny: #'between: and:' isValidSelector
]

{ #category : 'tests - selector validation' }
SymbolTest >> testIsNotValidUnarySelectorIfItContainsCharactersUsedInBinarySelectors [

	self deny: #'a+b' isValidSelector
]

{ #category : 'tests - selector validation' }
SymbolTest >> testIsNotValidUnarySelectorIfItStartsWithALetterAndContainsSeparators [

	self deny: #'with spaces' isValidSelector
]

{ #category : 'tests - selector validation' }
SymbolTest >> testIsValidBinarySelectorIfItContainsAnAllowedCharacterOrSequenceOfCharacters [

	self
		assert: #+ isValidSelector;
		assert: #+-+ isValidSelector
]

{ #category : 'tests - selector validation' }
SymbolTest >> testIsValidKeywordSelectorIfItContainsKeywordsWithColons [

	self
		assert: #includes: isValidSelector;
		assert: #between:and: isValidSelector
]

{ #category : 'tests - selector validation' }
SymbolTest >> testIsValidUnarySelectorIfItStartsWithALetterAndDoesNotContainSeparators [

	self assert: #t234 isValidSelector
]

{ #category : 'tests - symbol consistency' }
SymbolTest >> testNoDuplicateSymbols [

	| symbols |
	symbols := Symbol allInstances sort.
	1 to: symbols size - 1 do:
		[:k |
			self deny: ((symbols at: k) hasEqualElements: (symbols at: k + 1))
		]
]
