'From Cuis 5.0 [latest update: #3982] on 20 December 2019 at 8:59:35 pm'!

!Parser methodsFor: 'error correction' stamp: 'EB 12/20/2019 20:54:07'!
pasteTemp: tempName inBlock: aBlockNode  

	| theTextString blockTempsMark |

	theTextString := requestor text string.
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	
	(theTextString at: blockTempsMark) = $|
		ifTrue: [
	  		"Paste it before the second vertical bar"
			self pasteTemp: tempName before: blockTempsMark ] 
		ifFalse: [ 
			"The tempsMark is at the beginning of the block, we should insert after it."
			 self createTempDeclarationInBlockWith: tempName before: blockTempsMark ].! !


!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken match: matchReturn matchToken: startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain doitPatternInContext: expression keywordPattern messagePart:repeat: method:doIt:context: newMethodNode pattern:inContext: performInteractiveChecks: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable)
('temps' bindArg: bindArg:range: bindTemp:)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createTempDeclarationInBlockWith:before: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: insertWord:at: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTemp:inBlock: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
!

