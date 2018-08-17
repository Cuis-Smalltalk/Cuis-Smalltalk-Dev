'From Cuis 5.0 of 7 November 2016 [latest update: #3418] on 16 August 2018 at 7:01:54 am'!

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 07:00:43'!
isAt: aChar
	
	^ hereChar == aChar and: [aheadChar == aChar ifTrue: [self step. false] ifFalse: [true]]! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 07:01:00'!
readUpToNext: aChar ifNotFound: aNotFoundBlock

	self step.
	buffer reset.
	
	[self isAt: aChar]
		whileFalse: 
			[buffer nextPut: self step.
			(hereChar = self doItCharacter and: [source atEnd]) ifTrue: [^aNotFoundBlock value ]].
	
	self step.
	token := buffer contents.
	! !


!Scanner reorganize!
('expression types - private' isBacktickAndShouldIgnoreIt skipDelimiters skipDelimitersAndBacktickIfNecessary)
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('xBacktick processing - private' compileBacktickCode compileBacktickCodeHandlingErrors isAtBacktick readUpToNextBacktick)
('multi-character scans' doItCharacter isAt: readUpToNext:ifNotFound: typeTableAt: xBacktick xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('initialization' ignoreBacktick: initScanner initScannerForTokenization scan:)
!

