'From Cuis7.1 [latest update: #6717] on 13 September 2024 at 11:39:09 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/13/2024 11:38:10'!
parseStatementListForBraceArray
	"same as parseStatementList, but does not allow empty statements e.g {...$a...}.
	A single terminating . IS allowed e.g. {$a.} "

	
	[currentTokenFirst ~= $} ifTrue: [self parseExpression].
	currentTokenFirst = $.] 
		whileTrue: [self scanPast: #statementSeparator]! !

