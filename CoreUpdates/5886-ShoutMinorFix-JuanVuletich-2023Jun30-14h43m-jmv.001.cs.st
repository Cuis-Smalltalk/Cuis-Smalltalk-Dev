'From Cuis 6.0 [latest update: #5882] on 30 June 2023 at 5:56:08 pm'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 6/30/2023 17:55:30'!
parseStatement
	(currentToken isNil or: [	currentToken size = 1]) ifTrue: [
		currentTokenFirst = $^ ifTrue: [self scanPast: #return].
		currentTokenFirst = Character smalltalkUpArrow ifTrue: [self scanPast: #return]].
	self parseExpression! !

