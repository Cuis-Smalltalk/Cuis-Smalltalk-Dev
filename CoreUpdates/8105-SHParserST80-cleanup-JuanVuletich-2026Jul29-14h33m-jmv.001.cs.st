'From Cuis7.9 [latest update: #8098] on 29 July 2026 at 2:34:23 pm'!

!SHParserST80 methodsFor: 'error handling' stamp: 'tween 7/Aug/2005, 5:31:00 pm (UTC)'!
failIf: aBoolean
	aBoolean ifTrue:[self error]! !


!SHParserST80 methodsFor: 'parse' stamp: 'jmv 22/May/2022, 1:36:12 am (UTC)'!
parseByteArray
	"Literal ByteArray or literal FloatArray"
	[currentTokenFirst == $]] whileFalse: [
		currentTokenFirst isDigit | (currentTokenFirst == $-)
			ifTrue: [
				"do not parse the number, can be time consuming"
				self scanPast: #number]
			ifFalse: [
				self failIf: currentTokenFirst == $. .
				self error]].
	self scanPast: #arrayEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'eem 24/Feb/2026, 9:08:48 pm (UTC)'!
parseExternalCall
	self scanNext.
	self failIf: currentToken isNil.
	self scanPast: #externalCallType.
	currentToken = '*' 
		ifTrue: [self scanPast: #externalCallTypePointerIndicator].
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: 	[
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)] 
		whileTrue: [
			self failIf: currentToken isNil.
			self scanPast: #externalCallType.
			currentToken = '*' 
				ifTrue: [self scanPast: #externalCallTypePointerIndicator]].
	self scanPast: #rightParenthesis.
	currentToken = 'module:' 
		ifTrue: [
			self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	currentToken = 'error:' ifTrue: [
		self scanPast: #primitive. "there's no rangeType for error"
		self isName
			ifTrue: [ self scanPast: #patternTempVar ]
			ifFalse: [ self parseStringOrSymbol ] ].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'eem 7/Dec/2025, 7:17:59 pm (UTC)'!
parseExternalErrorReaperFunction
	self scanNext.
	self failIf: currentToken isNil.
	self scanPast: #externalCallType.
	self failUnless: currentTokenFirst == $'.
	self parseString.
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)] 
		whileTrue: [
			self failIf: currentToken isNil.
			self scanPast: #externalCallType].
	self scanPast: #rightParenthesis.
	currentToken = 'module:' 
		ifTrue: [
			self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'eem 7/Dec/2025, 7:24:04 pm (UTC)'!
parseExternalErrorReaperVariable
	self scanNext.
	self failIf: currentToken isNil.
	currentToken = 'const' ifTrue:
		[self scanPast: #comment].
	self scanPast: #externalCallType.
	
	currentTokenFirst == $'
		ifTrue: [self parseString]
		ifFalse:
			[self failUnless: currentToken notNil.
			 self scanPast: #string].

	currentToken = 'module:'  ifTrue:
		[self scanPast: #module.
		self failUnless: currentTokenFirst == $'.
		self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 22/May/2022, 1:44:55 am (UTC)'!
parseLiteral: inArray 
	currentTokenFirst == $$ 
		ifTrue: [
			| pos |
			self failIf: self currentChar isNil.
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
			self failIf: currentToken isNil.
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
	self failIf: currentTokenFirst == $. .
	self error	": 'argument missing'"! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 22/May/2022, 1:45:04 am (UTC)'!
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
	c := self currentChar.
	self failIf: (c isNil or: [c isSeparator]).
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
	(self isBinarySelectorCharacter: c) ifTrue: [
		self parseSymbolSelector.
		^self ].
	( c isValidStartOfIdentifiers or: [c == $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 22/May/2022, 1:45:14 am (UTC)'!
parseTerm
	self failIf: currentToken isNil.
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
	currentTokenFirst == $` ifTrue: [^self parseBacktick].
	currentTokenFirst == ${ 
		ifTrue: [
			braceDepth := braceDepth + 1.
			self scanPast: #leftBrace level: braceDepth.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false! !

!methodRemoval: SHParserST80 #failWhen: stamp: 'jmv 29/Jul/2026, 5:34:14 pm (UTC)'!
SHParserST80 removeSelector: #failWhen:!
