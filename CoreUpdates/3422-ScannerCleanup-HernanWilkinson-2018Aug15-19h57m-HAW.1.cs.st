'From Cuis 5.0 of 7 November 2016 [latest update: #3418] on 16 August 2018 at 6:44:54 am'!

!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 8/16/2018 06:37:45'!
compileBacktickCode

	| compiler method methodNode |
	
	compiler _ Compiler new.
	methodNode _ compiler compileNoPattern: buffer contents in: UndefinedObject context: nil notifying: nil ifFail: [].
	method _ methodNode generate.
	
	"Grab all messages sent while evaluating literal, so the main method will be shown as senders of them"
	sentInLiterals addAll: method messages.
	method literalsDo: [ :literal | literal isSymbol ifTrue: [ literal = #DoIt ifFalse: [sentInLiterals add: literal ]]].
		
	"Evaluate now."
	token _ nil withArgs: #() executeMethod: method ! !

!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 8/16/2018 06:37:02'!
compileBacktickCodeHandlingErrors

	[[[self compileBacktickCode ] 
		on: SyntaxErrorNotification
		do: [ :ex | self notify: 'Can not compile: ', ex errorMessage at: mark]]
		on: UndeclaredVariableReference
		do: [ :ex | self notify: 'Can not compile: Variable ''', ex varName, ''' is not declared' at: mark ]]
		on: Error
		do: [ :ex | self notify: 'Can not evaluate code: ', ex description at: mark ].
	
	tokenType _ #literal! !

!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 8/16/2018 06:28:34'!
isAtBacktick
	
	^ hereChar == $` and: [aheadChar == $` ifTrue: [self step. false] ifFalse: [true]]! !

!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 8/16/2018 06:28:54'!
readUpToNextBacktick
	
	self step.
	buffer reset.
	
	[self isAtBacktick]
		whileFalse: [
			buffer nextPut: self step.
			(hereChar = Scanner doItCharacterValue asCharacter and: [source atEnd]) ifTrue: [^false]].
	self step.
	
	^true! !


!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 06:38:36'!
xBacktick
	
	"Smalltalk code evaluated at compile time as a literal."

	self readUpToNextBacktick ifFalse: [^self offEnd: 'Unmatched back quote'].
	self compileBacktickCodeHandlingErrors.! !


!Scanner reorganize!
('expression types - private' isBacktickAndShouldIgnoreIt skipDelimiters skipDelimitersAndBacktickIfNecessary)
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('xBacktick processing - private' compileBacktickCode compileBacktickCodeHandlingErrors isAtBacktick readUpToNextBacktick)
('multi-character scans' typeTableAt: xBacktick xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('initialization' ignoreBacktick: initScanner initScannerForTokenization scan:)
!

