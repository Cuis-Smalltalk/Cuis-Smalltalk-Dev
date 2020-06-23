'From Cuis 5.0 [latest update: #4226] on 22 June 2020 at 4:38:20 pm'!
!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals ignoreBacktick '
	classVariableNames: 'DoItCharacter TypeTable '
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!ParseNode methodsFor: 'testing' stamp: 'HAW 6/22/2020 16:35:06'!
isBacktickNode

	^false! !


!BacktickNode methodsFor: 'expression' stamp: 'HAW 6/22/2020 16:34:36'!
expression

	^expression! !

!BacktickNode methodsFor: 'testing' stamp: 'HAW 6/22/2020 16:33:53'!
equivalentTo: aParseNode

	^ aParseNode isBacktickNode
		and: [ expression equivalentTo: aParseNode expression ]! !

!BacktickNode methodsFor: 'testing' stamp: 'HAW 6/22/2020 16:34:24'!
isBacktickNode

	^true! !

!methodRemoval: Scanner #skipDelimitersAndBacktickIfNecessary stamp: 'HAW 6/22/2020 16:28:26'!
Scanner removeSelector: #skipDelimitersAndBacktickIfNecessary!
!methodRemoval: Scanner #isBacktickAndShouldIgnoreIt stamp: 'HAW 6/22/2020 16:28:33'!
Scanner removeSelector: #isBacktickAndShouldIgnoreIt!
!methodRemoval: Scanner #ignoreBacktick: stamp: 'HAW 6/22/2020 16:28:44'!
Scanner removeSelector: #ignoreBacktick:!
!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals'
	classVariableNames: 'DoItCharacter TypeTable'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Scanner reorganize!
('expression types - private' skipDelimiters)
('expression types' advance checkpoint scanAllTokenPositionsInto: scanForFindSelectorUpTo: scanLitByteVec scanLitVec scanLitWord scanToken step)
('error handling' errorMultibyteCharacter notify: notify:at: offEnd:)
('public access' scanFieldNames: scanMessageParts: scanTokenPositionsIn:into: scanTokens: typedScanTokens:)
('multi-character scans' isAt: readUpToNext:ifNotFound: typeTableAt: xBacktick xBinary xColon xDelimiter xDigit xDollar xDoubleQuote xIllegal xLetter xLitQuote xSingleQuote xUnderscore)
('initialization' initScanner initScannerForTokenization scan:)
!


!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken match: matchReturn matchToken: startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression keywordPattern messagePart:repeat: method:doIt:context: newMethodNode pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable)
('temps' bindArg: bindArg:range: bindTemp: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('interactive checks' performInteractiveChecks: possibleSelfRecursionWarningMessage: warnIfPossibilityOfSelfRecursion:)
('backtick' addToSentInLiterals: backtickExpression)
!


!BacktickNode reorganize!
('initialization' addSymbolsOfExpressionToParser initializeExpression:source:parser:range:)
('literal node' evaluateBacktickSourceCode initializeLiteralNode literalNode)
('visiting' accept: visitExpressionWith: visitLiteralWith:)
('printing' printOn:indent:)
('code generation' analyseTempsWithin:rootNode:assignmentPools: emitCodeForValue:encoder: sizeCodeForValue:)
('expression' expression)
('testing' equivalentTo: isBacktickNode)
!
