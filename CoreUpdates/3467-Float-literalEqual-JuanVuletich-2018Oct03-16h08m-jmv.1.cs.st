'From Cuis 5.0 of 7 November 2016 [latest update: #3466] on 3 October 2018 at 4:37:19 pm'!

!Float methodsFor: 'comparing' stamp: 'nice 10/27/2014 21:57'!
literalEqual: aFloat
	"Two float literals can be replaced by a single one only if their representation have the same bits.
	For example, zero and negativeZero are equal, but not literally equal."

	^self class == aFloat class and: [(self at: 1) = (aFloat at: 1) and: [(self at: 2) = (aFloat at: 2)]]! !


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
!

