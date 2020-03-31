'From Cuis 5.0 [latest update: #4062] on 29 March 2020 at 4:42:17 pm'!
"Change Set:		4064-CuisCore-AuthorName-2020Mar29-16h26m
Date:			29 March 2020
Author:			Nahuel Garbezza

Encapsulate iteration of collections for some parse nodes"!


!BraceNode methodsFor: 'enumerating' stamp: 'RNG 3/29/2020 16:29:30'!
elementsDo: aBlock

	elements do: aBlock! !


!BlockNode methodsFor: 'enumerating' stamp: 'RNG 3/29/2020 16:37:51'!
statementsDo: aBlock

	statements do: aBlock! !


!BraceNode methodsFor: 'code generation (closures)' stamp: 'RNG 3/29/2020 16:29:52'!
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"

	self elementsDo: [ :node |
		node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools ]! !

!BraceNode methodsFor: 'code generation (new scheme)' stamp: 'RNG 3/29/2020 16:35:42'!
emitCodeForValue: stack encoder: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[ self elementsDo: [:node| node emitCodeForValue: stack encoder: encoder].
		 encoder genPushConsArray: elements size.
		 stack
			pop: elements size;
			push: 1.
		 ^self].
	^emitNode emitCodeForValue: stack encoder: encoder! !


!MessageNode methodsFor: 'macro transformations' stamp: 'RNG 3/29/2020 16:36:18'!
transformCase: encoder

	| caseNode |
	caseNode := arguments first.
	(caseNode isMemberOf: BraceNode) ifFalse: [^false].
	(caseNode blockAssociationCheck: encoder) ifFalse: [^false].
	(arguments size = 1
	 or: [self checkBlock: arguments last as: 'otherwise arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].
	 caseNode elementsDo:
		[:messageNode |
		messageNode receiver noteOptimizedIn: self.
		messageNode arguments first noteOptimizedIn: self].
	 arguments size = 2 ifTrue:
		[arguments last noteOptimizedIn: self].
	 ^true! !


!TemporariesDeclarationNode methodsFor: 'enumerating' stamp: 'RNG 3/29/2020 16:38:52'!
temporaryDeclarationNodesDo: aBlock

	tempDeclarationNodes do: aBlock! !


!ParseNodeVisitor methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:27:55'!
visitBlockNode: aBlockNode

	self visitIfNotNil: aBlockNode temporariesDeclaration.
	aBlockNode statementsDo:
		[ :statement| statement accept: self ]! !

!ParseNodeVisitor methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:36:33'!
visitBraceNode: aBraceNode

	aBraceNode elementsDo:
		[ :element | element accept: self ]! !


!VariableScopeFinder methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:28:09'!
visitBlockNode: aBlockNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one statement then aBlockNode is the
	 enclosing node, otherwise it is which ever single block node that includes it, if any."
	^(self enclosingNodeFor: [:aBlock| aBlockNode statementsDo: aBlock] of: aBlockNode) ifNotNil:
		[:aNode|
		aNode isBlockNode ifTrue: [aNode] ifFalse: [aBlockNode]]! !

!VariableScopeFinder methodsFor: 'visiting' stamp: 'RNG 3/29/2020 16:37:34'!
visitBraceNode: aBraceNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aBraceNode
	 is the enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock| aBraceNode elementsDo: aBlock]
		of: aBraceNode! !


!BlockNode reorganize!
('visiting' accept:)
('code generation (closures)' actualScope addHoistedTemps: addRemoteTemp:rootNode: addTempNode: analyseArguments:temporaries:rootNode: analyseTempsWithin:rootNode:assignmentPools: blockExtent computeCopiedValues: constructClosureCreationNode: deoptimize emitCodeForClosureValue:encoder: emitCodeForEvaluatedClosureValue:encoder: ifHasRemoteTempNodeEnsureInitializationStatementExists: makeTemporariesRemovable nilReadBeforeWrittenTemps noteOptimizedIn: optimizedBlockHoistTempsInto: postNumberingProcessTempsWithin:rootNode: reindexingLocalsDo:encoder: remoteTempNodeName removeTempNode:ifAbsent: sizeCodeForClosureValue: sizeCodeForEvaluatedClosureValue:)
('accessing' addArgument: arguments arguments: block closureCreationNode firstArgument nArgsSlot nArgsSlot: numberOfArguments optimized returnLast returnNilIfNoOther returnSelfIfNoOther: startOfLastStatement startOfLastStatement: temporaries temporaries: temporariesDeclaration temporariesDeclaration: tempsMark tempsMark:)
('testing' hasArgumentOrTemporaryNamed: isBlockNode isComplex isJust: isJustCaseError isQuick printsInNewLine returns)
('code generation' code)
('printing' decompileString printArgumentsOn:indent: printOn:indent: printStatementsOn:indent: printTemporaries:on:doPrior: printWithClosureAnalysisArgumentsOn:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisStatementsOn:indent: printWithClosureAnalysisTemporariesOn:indent:)
('code generation (new scheme)' emitCodeExceptLast:encoder: emitCodeForEvaluatedEffect:encoder: emitCodeForEvaluatedValue:encoder: emitCodeForValue:encoder: sizeCodeExceptLast: sizeCodeForEvaluatedEffect: sizeCodeForEvaluatedValue: sizeCodeForValue:)
('equation translation' statements statements:)
('initialization' arguments:statements:returns:from: noteSourceRangeStart:end:encoder: statements:returns:)
('source ranges' expandRanges:basedOn:using:)
('enumerating' statementsDo:)
!

