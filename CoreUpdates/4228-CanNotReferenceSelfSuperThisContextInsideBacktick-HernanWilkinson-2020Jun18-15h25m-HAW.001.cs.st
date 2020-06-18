'From Cuis 5.0 [latest update: #4226] on 18 June 2020 at 4:10:54 pm'!

!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 6/18/2020 16:09:06'!
assertNodesIn: methodNode canBeReferencedInsideBacktickUsing: compiler

	^ methodNode nodesDo: [ :aNode |
		(self canNotBeReferencedInsideBacktick: aNode) ifTrue: [ 
			compiler notify: (self class canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: aNode key) ]]! !

!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 6/18/2020 16:09:27'!
canNotBeReferencedInsideBacktick: aNode

	^ aNode isSelfPseudoVariable 
		or: [ aNode isSuperPseudoVariable 
		or: [ aNode isThisContextPseudoVariable ]]! !


!Scanner class methodsFor: 'xBacktick processing - private' stamp: 'HAW 6/18/2020 15:39:25'!
canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: pseudoVariable

	^ 'Can not reference ', pseudoVariable, ' inside backtick'! !


!Scanner methodsFor: 'xBacktick processing - private' stamp: 'HAW 6/18/2020 16:09:14'!
compileBacktickCode

	| compiler method methodNode |
	
	compiler _ Compiler new.
	methodNode _ compiler compileNoPattern: buffer contents in: UndefinedObject context: nil notifying: nil ifFail: [].
	self assertNodesIn: methodNode canBeReferencedInsideBacktickUsing: compiler.
	method _ methodNode generate.
	
	"Grab all messages sent while evaluating literal, so the main method will be shown as senders of them"
	sentInLiterals addAll: method messages.
	method literalsDo: [ :literal | literal isSymbol ifTrue: [ literal = self class doItSelector ifFalse: [sentInLiterals add: literal ]]].
		
	"Evaluate now."
	token _ nil withArgs: #() executeMethod: method ! !

!methodRemoval: Scanner #canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: stamp: 'HAW 6/18/2020 15:39:25'!
Scanner removeSelector: #canNotReferencePseudoVarInsideBacktickErrorDescriptionFor:!
!methodRemoval: Scanner #canNotReferencePseudoVarInsideBacktickErrorDescriptionFor stamp: 'HAW 6/18/2020 15:38:51'!
Scanner removeSelector: #canNotReferencePseudoVarInsideBacktickErrorDescriptionFor!

!Scanner reorganize!
('expression types - private' isBacktickAndShouldIgnoreIt skipDelimiters skipDelimitersAndBacktickIfNecessary)
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('xBacktick processing - private' assertNodesIn:canBeReferencedInsideBacktickUsing: canNotBeReferencedInsideBacktick: compileBacktickCode compileBacktickCodeHandlingErrors)
('multi-character scans' isAt: readUpToNext:ifNotFound: typeTableAt: xBacktick xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('initialization' ignoreBacktick: initScanner initScannerForTokenization scan:)
!

