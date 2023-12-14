'From Cuis6.0 [latest update: #6114] on 10 December 2023 at 11:02:13 am'!

!ParseNode methodsFor: 'accessing' stamp: 'HAW 12/10/2023 11:01:53'!
receiverOrCascadeReceiver
	
	"To contemplate cases like: 
	a := Object
		value;
		va
	Thanks Nico Papagna.
	Heavily used by LiveTyping, please do not remove or change without testing LiveTyping - Hernan"
	
	self nodesDo: [ :aNode | (aNode isCascadeNode or: [ aNode isMessageNode]) ifTrue: [ ^aNode receiverOrCascadeReceiver ]].
	
	"If there is no message node there is nothing valid I could return, but I return self for the rest to work - Hernan"
	^self! !


!CascadeNode methodsFor: 'accessing' stamp: 'HAW 12/10/2023 10:32:44'!
receiverOrCascadeReceiver
	
	^receiver ! !


!MessageNode methodsFor: 'equation translation' stamp: 'HAW 12/10/2023 10:34:13'!
receiverOrCascadeReceiver
	
	"When in cascade, receiver is nil and originalReceiver is the cascade receiver - Hernan"
	
	^receiver ifNil: [ originalReceiver ]! !


!MessageNode methodsFor: 'private' stamp: 'HAW 12/10/2023 10:34:53'!
hasEquivalentReceiverWith: aMessageNode

	^ self receiverOrCascadeReceiver equivalentTo: aMessageNode receiverOrCascadeReceiver ! !


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval originalArguments originalReceiver receiver receiver: receiverOrCascadeReceiver selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' expandRanges:basedOn:using: keywordAndParameterPositionAt:encodedWith:ifAbsent: keywordPositionAt: keywordRanges receiverSourceRangesFrom:)
('testing' canCascade ensureCanCascade: equivalentTo: isCascade isComplex isInfix isKeywordMessageSend isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf isSelfBasicNewMessageSend isSelfNewMessageSend isUnaryMessageSend referencesSelf referencesSuper toDoIncrement: toDoLimit:)
('cascading' receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: compare:with: hasEquivalentArgumentsWith: hasEquivalentReceiverWith: ifNilReceiver pvtCheckForPvtSelector: transform:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' optimizeSpecialSends: receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
('accessing' selectorSymbol)
('initialize-release' receiver:selector:arguments:precedence:from:sourceRange:)
('expression types' cascadeReceiver)
!

