'From Cuis 4.2 of 25 July 2013 [latest update: #2292] on 2 May 2015 at 4:29:29.988691 pm'!

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 5/2/2015 16:27'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: ($0 asciiValue to: $9 asciiValue) put: #xDigit.

	1 to: 255
		do: [:index |
			(Character value: index) isLetter
				ifTrue: [newTable at: index put: #xLetter]].

	newTable atAll: '!!%&+-*/\,<=>?@~' asByteArray put: #xBinary.

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

	"Scanner initialize"! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 5/2/2015 16:29'!
releaseClassCachedState

	TypeTable _ nil! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 5/2/2015 16:28'!
typeTable

	TypeTable ifNil: [
		self initTypeTable ].
	^TypeTable! !


!Scanner methodsFor: 'initialization' stamp: 'jmv 5/2/2015 16:28'!
initScanner

	buffer := WriteStream on: (String new: 40).
	typeTable := Scanner typeTable.
	isForTokenization := false! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 5/2/2015 16:28'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| i ascii type |
	i := aSymbol size.
	i = 0 ifTrue: [^ false].
	ascii := (aSymbol at: 1) asciiValue.
	"TypeTable should have been origined at 0 rather than 1 ..."
	ascii = 0 ifTrue: [^ false].
	type := self typeTable at: ascii ifAbsent: [#xLetter].
	type == #xBinary ifTrue:
		[^i <= 2
		  and: [i = 1 or: [(self typeTable at: (aSymbol at: 2) asciiValue ifAbsent: []) == #xBinary]]].
	(type == #xColon or: [type == #verticalBar]) ifTrue:
		[^i = 1].
	(type == #xLetter or: [type == #xUnderscore and:[Preferences allowUnderscoreSelectors]]) ifTrue:
		[[i > 1] whileTrue:
			[ascii := (aSymbol at: i) asciiValue.
			ascii = 0 ifTrue: [^false].
			type := self typeTable at: ascii ifAbsent: [#xLetter].
			((type == #xLetter or: [type == #xUnderscore and:[Preferences allowUnderscoreSelectors]])or: [type == #xDigit or: [type == #xColon]]) ifFalse:
				[^false].
			i := i - 1].
		^true].
	^false! !

!methodRemoval: Scanner class #initialize!
Scanner class removeSelector: #initialize!
Scanner initialize!

!Scanner class reorganize!
('testing' findSelectorIn: findSelectorTests inviolateInstanceVariableNames isLegalInstVarName: isLiteralSymbol: wellFormedInstanceVariableNameFrom:)
('instance creation' new)
('class initialization')
('cached class state' initTypeTable releaseClassCachedState typeTable)
!

