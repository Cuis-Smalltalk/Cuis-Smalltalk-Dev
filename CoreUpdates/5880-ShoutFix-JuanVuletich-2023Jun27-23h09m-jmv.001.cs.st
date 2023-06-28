'From Cuis 6.0 [latest update: #5879] on 27 June 2023 at 11:10:50 pm'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 6/27/2023 23:04:57'!
parseStatement
	currentToken size = 1 ifTrue: [
		currentTokenFirst = $^ ifTrue: [self scanPast: #return].
		currentTokenFirst = Character smalltalkUpArrow ifTrue: [self scanPast: #return]].
	self parseExpression! !

