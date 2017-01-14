'From Cuis 5.0 of 7 November 2016 [latest update: #3025] on 13 January 2017 at 9:55:58 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'ul 10/12/2010 02:43'!
parseStringOrSymbol

	currentTokenFirst == $' ifTrue: [ ^self parseString ].
	currentTokenFirst == $# ifTrue: [ ^self parseSymbol ].
	self error! !


!SHParserST80 methodsFor: 'parse' stamp: 'jmv 1/13/2017 09:53:38'!
parsePrimitive
	self scanNext.
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: [
			self failUnless: currentTokenFirst == $'.
			self parseString.
			currentToken = 'module:' 
				ifTrue: [
					self scanPast: #module.
					self failUnless: currentTokenFirst == $'.
					self parseString]].
	currentToken = 'error:' ifTrue: [
		self scanPast: #primitive. "there's no rangeType for error"
		self isName
			ifTrue: [ self scanPast: #patternTempVar ]
			ifFalse: [ self parseStringOrSymbol ] ].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd! !

