'From Cuis 4.2 of 25 July 2013 [latest update: #1962] on 27 August 2014 at 10:25:56.937538 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'SqR 8/25/2014 00:10'!
parseCascadeAndChain

	self parseKeyword.
	currentTokenFirst == $; ifTrue:
		[
			self scanPast: #cascadeSeparator.
			^self parseCascadeAndChain
		].
	currentTokenFirst = $: ifTrue:
		[
			self scanPast: #chainSeparator.

			"These lines implement double colon chains"
			currentTokenFirst = $: ifFalse: [^self].
			self scanPast: #chainSeparator.
			
			^self parseCascadeAndChain
		]! !


!Parser methodsFor: 'expression types' stamp: 'SqR 8/24/2014 23:53'!
chain
	" {: message} => more messages"

	self advance.

	"The following two lines implement double colon chains"
	hereType == #colon ifFalse:[self expected: 'Chain'].
	self advance.

	(self messagePart: 3 repeat: true) ifFalse: [self expected: 'Chain'].
	hereType == #semicolon ifTrue: [self cascade].
	hereType == #colon ifTrue: [self chain]! !


!CodePackageList methodsFor: 'accessing' stamp: 'SqR 8/24/2014 21:19'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count _ 0.
		selection classesDo: [ :cls | count _ count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methods size.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		linesOfCode / (methods asFloat max: 1.0) printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method).' ]! !


!BlockNode methodsFor: 'testing' stamp: 'SqR 8/24/2014 21:33'!
isComplex
	(statements isNil or: [arguments isNil or: [temporaries isNil]]) ifTrue: [^false].
	^ statements size > 1 or:
		[ (statements size = 1 and: [ statements first isComplex ]) or:
			[ arguments size > 0 or: [ temporaries notEmpty ] ] ]! !

!BlockNode methodsFor: 'printing' stamp: 'SqR 8/24/2014 21:31'!
printArgumentsOn: aStream indent: level
	(arguments isNil or: [arguments isEmpty]) ifTrue: [^ self].
	aStream space.
	arguments do:
		[ :arg | aStream nextPut: $:; nextPutAll: arg key; space ].
	aStream nextPut: $|! !

!BlockNode methodsFor: 'printing' stamp: 'SqR 8/24/2014 21:32'!
printOn: aStream indent: level
	| separateLines |
	aStream nextPut: $[.
	self
		printArgumentsOn: aStream
		indent: level.
	separateLines _ (self
		printTemporaries: temporaries
		on: aStream
		doPrior: [ aStream space ]) or: [arguments notNil and: [arguments notEmpty] ].
	Preferences prettyPrintRectangularBlocks
		ifTrue: [
			"If args+temps > 0 and statements > 1 (or just one complex statement),
			put all statements on separate lines"
			separateLines
				ifTrue: [
					(statements size > 1 or: [
						statements size = 1 and: [ statements first isComplex ]])
							ifTrue: [ aStream newLineTab: (1 max: level) ]
							ifFalse: [ aStream space ] ]
				ifFalse: [
					(statements size = 1 and: [ statements first isComplex not ])
						ifTrue: [ aStream space ]]]
		ifFalse: [
			self isComplex
				ifTrue: [ aStream newLineTab: (1 max: level) ]
				ifFalse: [ aStream space ] ].
	((self printStatementsOn: aStream indent: level) > 0 and: [ aStream peekLast ~= $] ])
		ifTrue: [ aStream space ].
	aStream nextPut: $]! !

!BlockNode methodsFor: 'printing' stamp: 'SqR 8/24/2014 21:36'!
printStatementsOn: aStream indent: levelOrZero
	| len shown thisStatement level |
	level _ 1 max: levelOrZero.
	comment ifNotNil: [
		self printCommentOn: aStream indent: level.
		aStream newLineTab: level].
	statements isNil ifTrue: [^0].
	len _ shown _ statements size.
	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])
		ifTrue: [ shown _ 1 max: shown - 1]
		ifFalse: ["should a trailing nil be printed or not? Not if it is an implicit result."
				(arguments size = 0
				and: [ len >= 1
				and: [ (statements at: len) == NodeNil
				and: [ len = 1
					or: [ len > 1
						and: [(statements at: len - 1) isMessageNode
						and: [(statements at: len - 1) isNilIf ]]]]]])
					ifTrue: [ shown _ shown - 1 ]].
	1 to: shown do: 
		[ :i |
		thisStatement _ statements at: i.
		thisStatement printOn: aStream indent: level.
		i < shown ifTrue: [ aStream nextPut: $.; newLineTab: level ].
		"Add a final period. This helps when pretty-diffing a method and a version of it that adds stuff after the end."
		(i = shown and: [ levelOrZero = 0 ]) ifTrue: [ aStream nextPut: $. ].
		(thisStatement comment notNil and: [ thisStatement comment size > 0 ])
			ifTrue: [
				i = shown ifTrue: [ aStream newLineTab: level ].
				thisStatement printCommentOn: aStream indent: level.
				i < shown ifTrue: [ aStream newLineTab: level ]]].
	^shown! !


!SHParserST80 methodsFor: 'parse' stamp: 'SqR 8/24/2014 23:10'!
parseExpression
	| assignType |
	self isName 
		ifTrue: [
			self scanPast: (self resolve: currentToken).
			self isAssignment 
				ifTrue: [
					assignType := self isAnsiAssignment 
						ifTrue: [#ansiAssignment]
						ifFalse: [#assignment].
					self scanPast: assignType.
					self parseExpression]
				ifFalse: [self parseCascadeAndChain]]
		ifFalse: [
			self parseTerm.
			self parseCascadeAndChain]! !


!Parser methodsFor: 'expression types' stamp: 'SqR 8/24/2014 21:41'!
expression

	(hereType == #word and: [tokenType == #leftArrow])
		ifTrue: [^ self assignment: self variable].
	hereType == #leftBrace
		ifTrue: [self braceExpression]
		ifFalse: [self primaryExpression ifFalse: [^ false]].
	(self messagePart: 3 repeat: true) ifTrue:
		[
			hereType == #semicolon ifTrue: [self cascade].
			hereType == #colon ifTrue: [self chain]
		].
	^ true! !


!Theme methodsFor: 'private - shout mappings' stamp: 'SqR 8/24/2014 23:14'!
defaults

	^ #(
		default
		arrayStart
		arrayEnd
		arrayStart1
		arrayEnd1
		leftBrace
		rightBrace
		cascadeSeparator
		chainSeparator
		statementSeparator
		externalCallType
		externalCallTypePointerIndicator
		rightParenthesis1
		blockArgColon
		leftParenthesis
		rightParenthesis
		blockStart
		blockEnd
	)! !

!methodRemoval: SHParserST80 #parseCascade!
SHParserST80 removeSelector: #parseCascade!
