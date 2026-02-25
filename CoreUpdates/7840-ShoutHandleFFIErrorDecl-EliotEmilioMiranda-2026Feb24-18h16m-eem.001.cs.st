'From Cuis7.7 [latest update: #7839] on 24 February 2026 at 6:16:54 pm'!

!SHParserST80 methodsFor: 'parse' stamp: 'eem 24/Feb/2026 18:08:48'!
parseExternalCall
	self scanNext.
	self failWhen: currentToken isNil.
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
			self failWhen: currentToken isNil.
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

