'From Cuis 5.0 of 7 November 2016 [latest update: #3411] on 13 August 2018 at 7:15:52 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 8/13/2018 19:15:42'!
performInteractiveChecks: aMethodNode

	self
		declareUndeclaredTemps: aMethodNode;
		removeUnusedTemps! !


!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: endOfLastToken match: matchReturn matchToken: startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector transformVerticalBarAndUpArrowIntoABinarySelectorIfNecessary)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain doitPatternInContext: expression keywordPattern messagePart:repeat: method:context: newMethodNode pattern:inContext: performInteractiveChecks: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporariesIn: temporaryBlockVariablesFor: unaryPattern variable)
('temps' bindArg: bindArg:range: bindTemp: bindTemp:in:)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: defineClass: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined removeUnusedTemps substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:context:notifying:ifFail: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector: sourceStreamGetter:)
('*Refactoring' parse:class:noPattern:)
('*Cuis-University-Model' possibleSelfRecursionWarningMessage: warnIfPossibilityOfSelfRecursion:)
!

