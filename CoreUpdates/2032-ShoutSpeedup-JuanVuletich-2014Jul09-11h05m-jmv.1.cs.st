'From Cuis 4.2 of 25 July 2013 [latest update: #2031] on 9 July 2014 at 11:18:20.604919 am'!

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 7/8/2014 21:52'!
peekChar
	^source at: sourcePosition + 1 ifAbsent: $ ! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 7/9/2014 11:06'!
resolve: aString

	"(self isReservedName: aString) ifTrue: [^aString asSymbol]."
	"faster: "
	aString = #self ifTrue: [ ^ #self ].
	aString = #true ifTrue: [ ^ #true ].
	aString = #false ifTrue: [ ^ #false ].
	aString = #nil ifTrue: [ ^ #nil ].
	aString = #super ifTrue: [ ^ #super ].
	aString = #thisContext ifTrue: [ ^ #thisContext ].

	(self isBlockTempName: aString) ifTrue: [^#blockTempVar].
	(self isBlockArgName: aString) ifTrue: [^#blockArg].
	(self isMethodTempName: aString) ifTrue: [^#tempVar].
	(self isMethodArgName: aString) ifTrue: [^#methodArg].
	(self isInstVarName: aString) ifTrue: [^#instVar].
	(self isWorkspaceVarName: aString) ifTrue: [^#workspaceVar].
	Symbol hasInterned: aString ifTrue: [ :symbol |
		(self isClassVarName: symbol) ifTrue: [ ^#classVar ].
		(self isPoolConstantName: symbol) ifTrue: [ ^#poolConstant].
		(self isGlobal: symbol) ifTrue: [^#globalVar]].
	^self resolvePartial: aString! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/8/2014 22:12'!
parse
    "Parse the receiver's text as a Smalltalk method"

    self parse: (classOrMetaClass notNil)! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/8/2014 22:13'!
parseBlockArguments
	[ currentTokenFirst == $: ]
		whileTrue: [
			self scanPast: #blockArgColon.
			self failUnless: self isName.
			self scanPast: #blockPatternArg ].
	currentTokenFirst == $| 
		ifTrue: [ self scanPast: #blockArgsBar ]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/8/2014 22:11'!
parseLiteral: inArray 
	currentTokenFirst == $$ 
		ifTrue: [
			| pos |
			self failWhen: self currentChar isNil.
			self rangeType: #'$'.
			pos := currentTokenSourcePosition + 1.
			self nextChar.
			self scanPast: #character start: pos end: pos.
			^self ].
	currentTokenFirst isDigit 
		ifTrue: [
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentToken = '-' 
		ifTrue: [
			| c |
			c := self currentChar.
			(inArray and: [c isNil or: [c isDigit not]]) 
				ifTrue: [
					"single - can be a symbol in an Array"
					self scanPast: #symbol.
					^self ].
			self scanPast: #-.
			self failWhen: currentToken isNil.
			"token isNil ifTrue: [self error: 'Unexpected End Of Input']."
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentTokenFirst == $' ifTrue: [
		self parseString.
		^self ].
	currentTokenFirst == $# ifTrue: [
		self parseSymbol.
		^self ].
	(inArray and: [currentToken notNil]) ifTrue: [
		self scanPast: #symbol.
		^self ].
	self failWhen: currentTokenFirst == $. .
	self error	": 'argument missing'"! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/8/2014 22:06'!
parseLiteralArrayElement
	(self isValidStartOfIdentifiers: currentTokenFirst) ifTrue: [
		#true = currentToken ifTrue: [
			self scanPast: #true.
			^ self ].
		#false = currentToken ifTrue: [
			self scanPast: #false.
			^ self ].
		#nil = currentToken ifTrue: [
			self scanPast: #nil.
			^ self ].
		self scanPast: #symbol.
		^ self ].
	currentTokenFirst == $( ifTrue: [
		self scanPast: #arrayStart.
		self parseArray.
		^ self ].
	self parseLiteral: true! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/9/2014 11:10'!
parseString
	| first c last |
	first _ sourcePosition.
	[
		(c _ self currentChar) ifNil: [
			self
				rangeType: #unfinishedString
				start: first - 1
				end: source size.
			self error": 'unfinished string'" ].
		c ~~ $' or: [
			self peekChar == $' and: [
				sourcePosition _ sourcePosition + 1.
				true ]]] whileTrue: [ sourcePosition _ sourcePosition + 1 ].
	last _ sourcePosition.
	self nextChar.
	self
		scanPast: #string
		start: first - 1
		end: last! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/8/2014 22:21'!
parseSymbol
	| c |
	currentToken = '#' 
		ifTrue: [
			"if token is just the #, then scan whitespace and comments
			and then process the next character.
			Allows space between the # and the start of the symbol 
			e.g. # (),  #  a, #  'sym' "
			self rangeType: #symbol.
			self scanWhitespace].
	c _ self currentChar.
	self failWhen: (c isNil or: [c isSeparator]).
	c == $( 
		ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseArray].
	c == $' ifTrue: [
		self parseSymbolString.
		^self ].
	c == $[ ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseByteArray].
	((self isSelectorCharacter: c) or: [c == $-]) ifTrue: [
		self parseSymbolSelector.
		^self ].
	((self isValidStartOfIdentifiers: c) or: [c == $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/9/2014 11:18'!
parseSymbolIdentifier
	| c start end |
	c _ self currentChar.
	self failUnless: ((self isValidStartOfIdentifiers: c) or: [ c == $: ]).
	start _ sourcePosition.
	[
		c _ self nextChar.
		(self isValidInIdentifiers: c) or: [ c == $: ]
	] whileTrue: [].
	end _ sourcePosition - 1.
	self scanPast: #symbol start: start - 1 end: end! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/8/2014 22:08'!
parseTerm
	self failWhen: currentToken isNil.
	currentTokenFirst == $( 
		ifTrue: [
			bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst == $).
			self scanPast: #rightParenthesis level: bracketDepth.
			bracketDepth := bracketDepth - 1.
			^self ].
	currentTokenFirst == $[ ifTrue: [^self parseBlock].
	currentTokenFirst == ${ 
		ifTrue: [
			self scanPast: #leftBrace.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false! !

