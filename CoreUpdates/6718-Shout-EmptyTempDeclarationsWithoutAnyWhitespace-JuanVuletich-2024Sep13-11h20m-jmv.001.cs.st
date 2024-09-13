'From Cuis7.1 [latest update: #6717] on 13 September 2024 at 11:35:22 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/13/2024 11:32:52'!
parseBlockTemporaries

	currentToken = '||' "Empty temp declaration without whitespace"
		ifTrue: [ ^self scanPast: #blockTempBar ].

	currentTokenFirst = $| 
		ifTrue: [
			self scanPast: #blockTempBar.
			[self isName] 
				whileTrue: [self scanPast: #blockPatternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #blockTempBar]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/13/2024 11:33:12'!
parseMethodTemporaries

	currentToken = '||' "Empty temp declaration without whitespace"
		ifTrue: [ ^self scanPast: #methodTempBar ].

	currentTokenFirst = $| 
		ifTrue: [
			self scanPast: #methodTempBar.
			[self isName] 
				whileTrue: [self scanPast: #patternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #methodTempBar]! !

