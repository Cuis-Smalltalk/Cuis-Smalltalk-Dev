'From Cuis7.1 [latest update: #6426] on 4 June 2024 at 4:26:27 pm'!

!SHParserST80 methodsFor: 'parse' stamp: 'eem 6/2/2024 17:21:15'!
parsePrimitive
	self scanNext.
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: [
			self failUnless: ('''#' includes: currentTokenFirst).
			self parseStringOrSymbol.
			currentToken = 'module:' ifTrue: [
				self scanPast: #module.
				self failUnless: ('''#' includes: currentTokenFirst).
				self parseStringOrSymbol]].
	currentToken = 'error:' ifTrue: [
		self scanPast: #primitive. "there's no rangeType for error"
		self isName
			ifTrue: [ self scanPast: #patternTempVar ]
			ifFalse: [ self parseStringOrSymbol ] ].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

