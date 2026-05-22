Class {
	#name : 'ExtractAsParameter',
	#superclass : 'ScopedRefactoring',
	#instVars : [
		'sourceMethod',
		'addParameter',
		'intervals'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'implementors and senders' }
ExtractAsParameter class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor [

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		forClassAndMetaOf: aPotentialClassToRefactor
]

{ #category : 'implementors and senders' }
ExtractAsParameter class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization [

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inCategoriesAndHierarchyOf: aClass
		organizedBy: anOrganization
]

{ #category : 'implementors and senders' }
ExtractAsParameter class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization [

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inCategory: aCategory
		organizedBy: anOrganization
]

{ #category : 'implementors and senders' }
ExtractAsParameter class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass [

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inHierarchyOf: aClass

]

{ #category : 'implementors and senders' }
ExtractAsParameter class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem [

	^self addParameterClass
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inSystem: aSystem

]

{ #category : 'instance creation' }
ExtractAsParameter class >> addParameterClass [

	"This method is important for LiveTyping parameterization. Do not remove - Hernan"
	^ AddParameter
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assert: aSourceMethod isInImplementors: implementorsCollection [

	^ (implementorsCollection includes: aSourceMethod) ifFalse: [ self signalOrigialMethodMustBeInImplementorsToChange ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertAndCreateNamed: aNewParameter  [
	extractedFromAll: allIntervals
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection
	creatingAddParameterWith: addParameterCreator

	| addParameter sourceCodeToExtract trimmedIntervalToReplace intervals |
	
	self assert: aSourceMethod isInImplementors: implementorsCollection.
	intervals := self assertCanExtractedFromAll: allIntervals for: aSourceMethod.
	
	trimmedIntervalToReplace := intervals first second.
	sourceCodeToExtract := aSourceMethod sourceCode copyFrom: trimmedIntervalToReplace first to: trimmedIntervalToReplace last.
	
	addParameter := addParameterCreator value: sourceCodeToExtract.

	^self new 
		initializeReplacingAll: (intervals collect: [ :both | both first ])
		at: aSourceMethod 
		addingParameterWith: addParameter
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertCanExtractFrom: anInterval for: sourceCode methodNode: methodNode last: lastIntervalsAndNode [

	| trimmedIntervalToReplace node intervalToReplace |

	ExtractMethodNewMethod assertIntervalToExtractIsNotEmpty: anInterval.
	ExtractMethodNewMethod assert: anInterval isWithinBoundsOf: sourceCode.
	
	intervalToReplace := self removeDotsAt: anInterval in: sourceCode.
	trimmedIntervalToReplace := intervalToReplace asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	node := self nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: intervalToReplace.
	self assertIsValidToExtract: node asPartOf: methodNode at: trimmedIntervalToReplace.
	lastIntervalsAndNode ifNotNil: [ self assertIsSameExpressionToExtractFrom: node to: lastIntervalsAndNode third ].
	
	^{ intervalToReplace. trimmedIntervalToReplace. node }
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertCanExtractedFromAll: allIntervals for: aSourceMethod [

	| sourceCode methodNode lastIntervalsAndNode |

	allIntervals isEmpty ifTrue: [ self signalNoExpressionToExtract ].

	sourceCode := aSourceMethod sourceCode.
	methodNode := aSourceMethod notOptimizedMethodNode.
	lastIntervalsAndNode := nil.

	^allIntervals collect: [ :anInterval |
		lastIntervalsAndNode := self
			assertCanExtractFrom: anInterval
			for: sourceCode
			methodNode: methodNode
			last: lastIntervalsAndNode ]	
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertHasExtractableBraceElements: aBraceNode asPartOf: methodNode at: trimmedIntervalToReplace    [
	
	aBraceNode elementsDo: [ :anElementNode | self assertIsValidToExtract: anElementNode asPartOf: methodNode at: trimmedIntervalToReplace ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsNotLeftAssigment: aNode asPartOf: methodNode at: intervalToReplace   [
	
	methodNode nodesDo: [ :traversedNode | 
		(self isAssignment: traversedNode and: aNode leftPartIn: methodNode at: intervalToReplace)
			ifTrue: [ self signalInvalidExpressionToExtractAsParameter ]]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsSameExpressionToExtractFrom: node to: lastNode [

	(node = lastNode or: [ node equivalentTo: lastNode ]) ifFalse: [
		self signalNotAllExpressionsToExtractAreEqual ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidAssigmentToExtract: anAssignmentNode asPartOf: methodNode at: intervalToReplace    [
	
	self assertIsValidToExtract: anAssignmentNode variable asPartOf: methodNode at: intervalToReplace.
	self assertIsValidToExtract: anAssignmentNode value asPartOf: methodNode at: intervalToReplace 
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidBlockNodeToExtract: aBlockNode asPartOf: methodNode at: intervalToReplace    [
	
	aBlockNode block statementsDo: [ :aStatement | self assertIsValidToExtract: aStatement asPartOf: methodNode at: intervalToReplace ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidKeywordForNewParameter: aNewKeyword [

	self addParameterClass assertIsValidKeywordForNewParameter: aNewKeyword
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidLiteralNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace   [

	aNode isBraceNode ifTrue: [ ^self assertHasExtractableBraceElements: aNode asPartOf: methodNode at: intervalToReplace].

	^ (aNode isLiteralNode 
		or: [ aNode isTruePseudoVariable
		or: [ aNode isFalsePseudoVariable 
		or: [ aNode isNilPseudoVariable 
		or: [ (aNode isLiteralVariableNode and: [ 
				aNode isReferencingBehavior 
					or: [ aNode isReferencingGlobal
					or: [ aNode isReferencingClassVariable ]]])]]]]) ifFalse: [ self signalInvalidExpressionToExtractAsParameter ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidMessageNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace   [

	self assertIsValidToExtract: aNode receiver asPartOf: methodNode at: intervalToReplace.
	aNode arguments do: [ :anArgument | self assertIsValidToExtract: anArgument asPartOf: methodNode at: intervalToReplace ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidParameterName: aName [

	self addParameterClass assertIsValidParameterName: aName 
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidTempOrArgNodeToExtract: aTempVariableNode  [
	
	aTempVariableNode isDeclaredAtMethodLevel ifTrue: [ self signalInvalidExpressionToExtractAsParameter ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> assertIsValidToExtract: aNode asPartOf: methodNode at: intervalToReplace   [

	aNode isMessageNode ifTrue: [ ^self assertIsValidMessageNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace].
	aNode isBlockNode ifTrue: [ ^self assertIsValidBlockNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace ].
	aNode isTempOrArg ifTrue: [  ^self assertIsValidTempOrArgNodeToExtract: aNode ].
	aNode isAssignmentToTemporary ifTrue: [ ^self assertIsValidAssigmentToExtract: aNode asPartOf: methodNode at: intervalToReplace ].
	aNode isInstanceVariableNode ifTrue: [ ^self assertIsNotLeftAssigment: aNode asPartOf: methodNode at: intervalToReplace ].
	self assertIsValidLiteralNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace 
]

{ #category : 'error messages' }
ExtractAsParameter class >> errorMessageForInvalidExpressionToExtractAsParameter [
	
	^'Only literals, message sends to literals with literal parameters and 
blocks with the previous conditions can be extracted as parameters'
]

{ #category : 'error messages' }
ExtractAsParameter class >> errorMessageForInvalidSelection [
	
	^'The selected source code is invalid for extraction as parameter'
]

{ #category : 'error messages' }
ExtractAsParameter class >> errorMessageForNoExpressionToExtract [
	
	^'No expression to extract'
]

{ #category : 'error messages' }
ExtractAsParameter class >> errorMessageForOrigialMethodMustBeInImplementorsToChange [
	
	^'Method with code to extract must be as implementor to change'
]

{ #category : 'error messages' }
ExtractAsParameter class >> errorMessageNotAllExpressionsToExtractAreEqual [
	
	^'Expressions to extract are not equal'
]

{ #category : 'intervals' }
ExtractAsParameter class >> intervalsForEquivalentExpressionIn: method at: interval [

	| methodNode node sourceCode trimmedIntervalToReplace rangeOrRanges |

	sourceCode := method sourceCode.
	methodNode := method notOptimizedMethodNode.
	trimmedIntervalToReplace := interval asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	node := self nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: interval.

	rangeOrRanges := methodNode rangeForNode: node ifAbsent: [ self shouldNotHappenBecause: 'node already exist' ].
	^(methodNode isMultipleRanges: rangeOrRanges)
		ifTrue: [ rangeOrRanges ]
		ifFalse: [ self intervalsForEquivalentNodesTo: node in: methodNode ]
				
]

{ #category : 'intervals' }
ExtractAsParameter class >> intervalsForEquivalentNodesTo: node in: methodNode [

	| completeSourceRanges intervalsForEquivalentNodes | 
	
	intervalsForEquivalentNodes := OrderedCollection new.
	completeSourceRanges := methodNode completeSourceRanges.
	
	methodNode nodesDo: [ :aNode | 
		(aNode equivalentTo: node) ifTrue: [ 
			"There can not be more than one range because of the is not a multi range node. See senders - Hernan"
			intervalsForEquivalentNodes add: (completeSourceRanges at: aNode) first ]].
	
	^intervalsForEquivalentNodes
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> is: aRange equalTo: sourceInterval or: intervalToReplace  [
	
	"When selecting literals like 1, the range first is one less than the initial character of the literal - Hernan"
	
	^aRange = sourceInterval 
		or: [ aRange = intervalToReplace
		or: [ aRange first + 1 = sourceInterval first and: [ aRange value last = sourceInterval last]]]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> isAssignment: aPotentialAssigmentNode and: aPotentialLeftAssigment leftPartIn: methodNode at: interval [

	^ aPotentialAssigmentNode isAssignmentNode 
		and: [ aPotentialLeftAssigment = aPotentialAssigmentNode variable 
		and: [ | assigmentRange |
			assigmentRange := (methodNode completeSourceRangesOf: aPotentialAssigmentNode ifAbsent: [Set with: (0 to: 0)]) anyOne.
			assigmentRange includesAllOf: interval ]]
]

{ #category : 'instance creation' }
ExtractAsParameter class >> named: aNewParameter  [
	extractedFrom: anInterval
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		named: aNewParameter 
		extractedFromAll: { anInterval }
		at: aSourceMethod 
		implementors: implementorsCollection 
		senders: sendersCollection 
]

{ #category : 'instance creation' }
ExtractAsParameter class >> named: aNewParameter  [
	extractedFrom: anInterval
	at: aNewKeywordIndex
	newKeyword: newKeyword
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		named: aNewParameter 
		extractedFromAll: { anInterval }
		at: aNewKeywordIndex 
		newKeyword: newKeyword 
		at: aSourceMethod 
		implementors: implementorsCollection 
		senders: sendersCollection 
]

{ #category : 'instance creation' }
ExtractAsParameter class >> named: aNewParameter  [
	extractedFromAll: intervals
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		assertAndCreateNamed: aNewParameter 
		extractedFromAll: intervals
		at: aSourceMethod  
		implementors: implementorsCollection 
		senders: sendersCollection
		creatingAddParameterWith: [ :sourceCodeToExtract | 
			self addParameterClass 
				named: aNewParameter 
				initializedWith: sourceCodeToExtract 
				toUnarySelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]
]

{ #category : 'instance creation' }
ExtractAsParameter class >> named: aNewParameter  [
	extractedFromAll: intervals
	at: aNewKeywordIndex
	newKeyword: newKeyword
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		assertAndCreateNamed: aNewParameter 
		extractedFromAll: intervals
		at: aSourceMethod  
		implementors: implementorsCollection 
		senders: sendersCollection
		creatingAddParameterWith: [ :sourceCodeToExtract | 
			self addParameterClass 
				named: aNewParameter 
				at: aNewKeywordIndex
				initializedWith: sourceCodeToExtract 
				using: newKeyword 
				toKeywordSelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: intervalToReplace [
	
	| nodeWithRangeToExtract nodesWithFirstPosition |
	
	nodesWithFirstPosition := methodNode parseNodesPathAt: trimmedIntervalToReplace first ifAbsent: [ self signalInvalidSelection ].
	nodeWithRangeToExtract := nodesWithFirstPosition 
		detect: [ :nodeAndRange | self is: nodeAndRange value equalTo: trimmedIntervalToReplace or: intervalToReplace ]
		ifNone: [ self signalInvalidSelection ].
	 
	^nodeWithRangeToExtract key.
	
]

{ #category : 'pre-conditions' }
ExtractAsParameter class >> removeDotsAt: anInterval in: sourceCode [
	
	| lastToReplace |
	
	lastToReplace := anInterval last.
	[(sourceCode at: lastToReplace) == $. ] whileTrue: [ lastToReplace := lastToReplace - 1].
	
	^anInterval first to: lastToReplace
]

{ #category : 'exceptions' }
ExtractAsParameter class >> signalInvalidExpressionToExtractAsParameter [
	
	self refactoringError: self errorMessageForInvalidExpressionToExtractAsParameter 
]

{ #category : 'exceptions' }
ExtractAsParameter class >> signalInvalidSelection [
	
	self refactoringError: self errorMessageForInvalidSelection
]

{ #category : 'exceptions' }
ExtractAsParameter class >> signalNoExpressionToExtract [
	
	self refactoringError: self errorMessageForNoExpressionToExtract 
]

{ #category : 'exceptions' }
ExtractAsParameter class >> signalNotAllExpressionsToExtractAreEqual [
	
	self refactoringError: self errorMessageNotAllExpressionsToExtractAreEqual
]

{ #category : 'exceptions' }
ExtractAsParameter class >> signalOrigialMethodMustBeInImplementorsToChange [
	
	self refactoringError: self errorMessageForOrigialMethodMustBeInImplementorsToChange
]

{ #category : 'applying' }
ExtractAsParameter >> apply [
	
	self 
		applyAddParameter;
		useNewParameter.
		
	^addParameter changes
	
	
]

{ #category : 'applying - private' }
ExtractAsParameter >> applyAddParameter [

	^ addParameter apply
]

{ #category : 'initialization' }
ExtractAsParameter >> initializeReplacingAll: allIntervals at: aSourceMethod addingParameterWith: anAddParameter  [
	
	intervals := allIntervals.
	sourceMethod := aSourceMethod.
	addParameter := anAddParameter.
]

{ #category : 'selectors' }
ExtractAsParameter >> newSelector [
	
	^addParameter newSelector 
]

{ #category : 'applying - private' }
ExtractAsParameter >> newSourceCode: anIntervalToReplace from: sourceCode originalSize: originalSourceCodeSize [
	
	| newInterval newSourceCode |
	
	newInterval := anIntervalToReplace  + (sourceCode size - originalSourceCodeSize).
	newSourceCode := sourceCode copyReplaceFrom: newInterval first to: newInterval last with: addParameter newParameter.
	
	^newSourceCode 
]

{ #category : 'applying - private' }
ExtractAsParameter >> useNewParameter [
	
	| newSourceCodeWithAllReplacements originalSourceCodeSize |
	
	originalSourceCodeSize := sourceMethod sourceCode size.
	newSourceCodeWithAllReplacements := intervals 
		inject: (sourceMethod methodClass >> self newSelector) sourceCode
		into: [ :newSourceCode :intervalToReplace | self newSourceCode: intervalToReplace from: newSourceCode originalSize: originalSourceCodeSize ].
		
	sourceMethod methodClass compile: newSourceCodeWithAllReplacements
]
