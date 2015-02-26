'From Cuis 4.2 of 25 July 2013 [latest update: #2177] on 20 February 2015 at 8:29:29.489717 am'!
!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalSelector originalArguments originalReceiver '
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers StdTypers ThenFlag '
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!MessageNode methodsFor: 'testing' stamp: 'bf 2/20/2015 08:25'!
ensureCanCascade: encoder

	special > 0 ifTrue: [
		special := 0.
		receiver := originalReceiver.
		selector := encoder encodeSelector: originalSelector.
		arguments := originalArguments.
		receiver isBlockNode ifTrue: [receiver deoptimize].
		arguments do:
			[:each|
			each isBlockNode ifTrue:
				[each deoptimize]]]! !

!MessageNode methodsFor: 'cascading' stamp: 'bf 2/20/2015 08:26'!
receiver: rcvr arguments: args precedence: p

	receiver := rcvr.
	originalReceiver := rcvr copy.
	arguments := args.
	originalArguments := arguments copy.
	sizes := Array new: arguments size.
	precedence := p! !

!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments'
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers StdTypers ThenFlag'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent: test)
('testing' canCascade ensureCanCascade: isComplex isMessage isMessage:receiver:arguments: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf toDoIncrement: toDoLimit:)
('cascading' cascadeReceiver receiver:arguments:precedence:)
('private' checkBlock:as:from: checkBlock:as:from:maxArgs: ifNilReceiver pvtCheckForPvtSelector:)
('code generation' emitCodeForEffect:encoder: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForToDo:value: sizeCodeForValue:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value: sizeCodeForWhile:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transform: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange: selector:)
!

