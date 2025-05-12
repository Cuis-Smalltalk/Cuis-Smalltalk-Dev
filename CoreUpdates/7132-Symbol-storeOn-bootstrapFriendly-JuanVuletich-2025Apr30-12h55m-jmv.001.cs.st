'From Cuis7.3 [latest update: #7120] on 30 April 2025 at 1:00:56 pm'!

!Symbol commentStamp: '<historical>' prior: 0!
I represent Strings that are created uniquely.
So, if aString = anotherString, then we know aString asSymbol == anotherString asSymbol!


!Symbol class methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/30/2025 12:51:07'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| char |
	aSymbol size = 0 ifTrue: [^ false].
	aSymbol = ':' ifTrue: [ ^true ].
	aSymbol = '|' ifTrue: [ ^true ].
	(aSymbol allSatisfy: [ :c | Character binarySelectorCharacters includes: c ])
		ifTrue: [ ^true ].
	char := aSymbol at: 1.
	(char isLetter or: [ char = $_ ]) ifTrue: [
		(aSymbol allSatisfy: [ :c |
			c isLetter or: [ c = $_ or: [ c isDigit or: [ c = $: ]]]])
				ifTrue: [ ^true ]].		
	^false! !


!Character class methodsFor: 'constants' stamp: 'jmv 4/30/2025 12:42:37'!
binarySelectorCharacters
	^ '!!%&+-*/\,<=>?@~'! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 4/30/2025 12:51:30'!
isValidInstanceVariableName: aString
	"Answer whether aString is a legal instance variable name."

	^ ((Symbol isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self pseudoVariableNames includes:  aString) not]! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 4/30/2025 12:42:50'!
initTypeTable
	| newTable |
	newTable := Array new: 127 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: Character binarySelectorCharacters asByteArray put: #xBinary.
	1 to: 127 do: [:index |
		(Character codePoint: index) isLetter ifTrue: [
			newTable at: index put: #xLetter ]].
	newTable atAll: ($0 codePoint to: $9 codePoint) put: #xDigit.
	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" codePoint put: #xDoubleQuote.
	newTable at: $# codePoint put: #xLitQuote.
	newTable at: $$ codePoint put: #xDollar.
	newTable at: $' codePoint put: #xSingleQuote.
	newTable at: $` codePoint put: #xBacktick.
	newTable at: $: codePoint put: #xColon.
	newTable at: $( codePoint put: #leftParenthesis.
	newTable at: $) codePoint put: #rightParenthesis.
	newTable at: $. codePoint put: #period.
	newTable at: $; codePoint put: #semicolon.
	newTable at: $[ codePoint put: #leftBracket.
	newTable at: $] codePoint put: #rightBracket.
	newTable at: ${ codePoint put: #leftBrace.
	newTable at: $} codePoint put: #rightBrace.
	newTable at: $^ codePoint put: #upArrow.
	newTable at: $_ codePoint put: #xUnderscore.
	newTable at: $| codePoint put: #verticalBar.
	newTable at: Character smalltalkLeftArrow codePoint put: #leftArrow.
	newTable at: Character smalltalkUpArrow codePoint put: #upArrow.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!UnicodeSymbol methodsFor: 'printing' stamp: 'jmv 4/30/2025 12:51:20'!
storeOn: aStream 

	aStream nextPut: $#.
	(Symbol isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !


!Symbol methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/30/2025 12:51:16'!
storeOn: aStream 

	aStream nextPut: $#.
	(Symbol isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !

!methodRemoval: Scanner class #isLiteralSymbol: stamp: 'jmv 4/30/2025 12:57:48'!
Scanner class removeSelector: #isLiteralSymbol:!

!Symbol reorganize!
('minimalKernel - Bootstrap' = storeOn:)
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('printing' print)
('system primitives' flushCache)
('private' errorNoModification species string:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
!


!Symbol class reorganize!
('minimalKernel - Bootstrap' isLiteralSymbol:)
('access' accessProtect allSymbolTablesDo: allSymbols allSymbolsSelect: findInterned: selectorsContaining: selectorsMatching: symbolCount thatStartsCaseSensitive:)
('class initialization' initialize)
('instance creation' intern: lookup: newFrom: readFrom:)
('private' basicFromString: rehash)
('services' hasInterned:and: hasInterned:ifTrue: hasInternedAndImplemented: hasInternedAndImplementedOrReferenced: possibleSelectorsFor:)
!

