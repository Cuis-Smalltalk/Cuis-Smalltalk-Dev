'From Cuis 5.0 [latest update: #4024] on 12 January 2020 at 9:41:15 pm'!
"Change Set:		4025-CuisCore-AuthorName-2020Jan12-21h12m
Date:			12 January 2020
Author:			Nahuel Garbezza

Ability to extract quoted expressions in the extract method refactoring"!


!ParseNode methodsFor: 'source ranges' stamp: 'RNG 1/12/2020 21:20:28'!
expandIfEnclosed: sourceRange on: sourceCode
	"takes a source range and a source code and if the source range represents an
	expression that can be expanded and still is valid, it returns the source range
	'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*"

	| firstChar lastChar |
	firstChar _ sourceCode at: sourceRange first - 1 ifAbsent: [ nil ].
	lastChar _ sourceCode at: sourceRange last + 1 ifAbsent: [ nil ].
	^ ((firstChar = $( and: [ lastChar = $) ])
		or: [ firstChar = $` and: [ lastChar = $` ] ])
			ifTrue: [ sourceRange first - 1 to: sourceRange last + 1 ]
			ifFalse: [ sourceRange ]! !


!ParseNode methodsFor: 'source ranges' stamp: 'RNG 1/12/2020 21:12:37'!
expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode

	^ (self consolidateAsCollection: sourceRanges)
		collect: [ :sourceRange | self expandIfEnclosed: sourceRange on: sourceCode ]! !


!MessageNode methodsFor: 'source ranges' stamp: 'RNG 1/12/2020 21:12:37'!
expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	receiverExpandedRanges _ self isCascade
		ifTrue: [ self flag: #RNG . aSourceRange ] "fix source ranges for cascade messages"
		ifFalse: [ receiver expandRanges: (self receiverSourceRangesFrom: sourceRanges) basedOn: sourceRanges using: sourceCode ].
	expandedRangeWithReceiver _ self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ self consolidateAsCollection: (self expandIfEnclosed: expandedRangeWithReceiver on: sourceCode)! !

!methodRemoval: ParseNode #expandIfEnclosedWithParentheses:on: stamp: 'RNG 1/12/2020 21:12:37'!
ParseNode removeSelector: #expandIfEnclosedWithParentheses:on:!

!ParseNode reorganize!
('visiting' accept: nodesDo:)
('tiles' currentValueIn:)
('converting' asReturnNode)
('testing' assignmentCheck:at: canCascade ensureCanCascade: equivalentTo: isArg isAssignmentNode isAssignmentToTemporary isBlockNode isComplex isConstantNumber isFalsePseudoVariable isInstanceVariableNode isJust: isLiteralNode isLiteralVariableNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilPseudoVariable isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelfBasicNewMessageSend isSelfNewMessageSend isSelfPseudoVariable isSpecialConstant isSuperPseudoVariable isTemp isTempOrArg isThisContextPseudoVariable isTruePseudoVariable isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine toDoIncrement:)
('comment' comment comment:)
('code generation' pc pc:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('encoding' encodeSelector:)
('private' consolidateAsCollection: ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('source ranges' expandIfEnclosed:on: expandRange:basedOn: expandRanges:basedOn:using:)
!


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' expandRanges:basedOn:using: keywordAndParameterPositionAt:encodedWith:ifAbsent: keywordPositionAt: keywordRanges receiverSourceRangesFrom:)
('testing' canCascade ensureCanCascade: equivalentTo: isCascade isComplex isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf isSelfBasicNewMessageSend isSelfNewMessageSend toDoIncrement: toDoLimit:)
('cascading' cascadeReceiver receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: ifNilReceiver pvtCheckForPvtSelector:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transform: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
('accessing' selectorSymbol)
!

