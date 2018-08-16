'From Cuis 5.0 of 7 November 2016 [latest update: #3418] on 15 August 2018 at 7:54:00 pm'!
!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals ignoreBacktick '
	classVariableNames: 'TypeTable '
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Scanner methodsFor: 'expression types - private' stamp: 'HAW 8/15/2018 19:53:14'!
isBacktickAndShouldIgnoreIt 
	
	"I compare with true because there are many ways to initialize the scanner and ingoreBacktick could be nil - Hernan"
	^ ignoreBacktick == true and: [tokenType = #xBacktick]! !

!Scanner methodsFor: 'expression types - private' stamp: 'HAW 8/15/2018 19:53:27'!
skipDelimiters

	[(tokenType := self typeTableAt: hereChar) == #xDelimiter] whileTrue: [self step].  
! !

!Scanner methodsFor: 'expression types - private' stamp: 'HAW 8/15/2018 19:53:35'!
skipDelimitersAndBacktickIfNecessary

	[self skipDelimiters.
	self isBacktickAndShouldIgnoreIt ] whileTrue: [self step].  ! !


!Scanner methodsFor: 'initialization' stamp: 'HAW 8/15/2018 19:51:51'!
initScanner

	buffer := WriteStream on: (String new: 40).
	typeTable := Scanner typeTable.
	isForTokenization := false.
	sentInLiterals := Set new.
	ignoreBacktick := true! !

!Scanner methodsFor: 'initialization' stamp: 'HAW 8/15/2018 19:52:00'!
initScannerForTokenization
	"Don't raise xIllegal when enocuntering an _"
	"Simpler implementation for Cuis"
	isForTokenization _ true.
	ignoreBacktick _ true.! !

!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals ignoreBacktick'
	classVariableNames: 'TypeTable'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Scanner reorganize!
('expression types - private' isBacktickAndShouldIgnoreIt skipDelimiters skipDelimitersAndBacktickIfNecessary)
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('multi-character scans' typeTableAt: xBacktick xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('initialization' initScanner initScannerForTokenization scan:)
!

