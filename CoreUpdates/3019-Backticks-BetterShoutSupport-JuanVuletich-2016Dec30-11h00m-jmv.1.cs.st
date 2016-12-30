'From Cuis 5.0 of 7 November 2016 [latest update: #3016] on 30 December 2016 at 11:01:51 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 12/30/2016 10:51:03'!
parseBacktick
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $`.
	self scanPast: #backtick.
	self leaveBlock! !


!SHParserST80 methodsFor: 'scan' stamp: 'tween 2/17/2007 14:51'!
scanWhitespace
	| c |
	
	[c := self currentChar.
	c notNil and: [c isSeparator]] 
		whileTrue: [sourcePosition := sourcePosition + 1].
	c == $" ifTrue: [self scanComment]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 12/30/2016 10:51:08'!
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
	currentTokenFirst == $` ifTrue: [^self parseBacktick].
	currentTokenFirst == ${ 
		ifTrue: [
			self scanPast: #leftBrace.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false! !

