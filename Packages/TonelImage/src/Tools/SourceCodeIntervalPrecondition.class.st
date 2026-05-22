"
I am an abstract class that validates things that are required for source code intervals.
"
Class {
	#name : 'SourceCodeIntervalPrecondition',
	#superclass : 'RefactoringPrecondition',
	#instVars : [
		'intervalToExtract',
		'method',
		'sourceCode',
		'methodNode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'error messages' }
SourceCodeIntervalPrecondition class >> errorMessageForExtractingPartOfMethodSignature [

	^ self refactoringErrorMessagePrefix , 'it is not possible to extract part of method signature'
]

{ #category : 'error messages' }
SourceCodeIntervalPrecondition class >> errorMessageForExtractingTemporaryVariablesDefinition [

	^ self refactoringErrorMessagePrefix , 'it is not possible to extract temporary variable definitions'
]

{ #category : 'error messages' }
SourceCodeIntervalPrecondition class >> errorMessageForSourceCodeContainingInvalidExpression [

	^ self refactoringErrorMessagePrefix , 'the selected code contains an invalid expression'
]

{ #category : 'error messages' }
SourceCodeIntervalPrecondition class >> errorMessageForSourceCodeIncludingAReturnStatement [

	^ self refactoringErrorMessagePrefix , 'the selected code includes a return statement'
]

{ #category : 'instance creation' }
SourceCodeIntervalPrecondition class >> for: anIntervalToExtract of: aMethod [

	^ self new initializeFor: anIntervalToExtract of: aMethod
]

{ #category : 'private - error messages' }
SourceCodeIntervalPrecondition class >> refactoringErrorMessagePrefix [

	self subclassResponsibility
]

{ #category : 'evaluating' }
SourceCodeIntervalPrecondition class >> valueFor: anIntervalToExtract of: aMethod [

	^ (self for: anIntervalToExtract of: aMethod) value
]

{ #category : 'pre-conditions' }
SourceCodeIntervalPrecondition >> assertSourceCodeDoesNotIncludeReturnExpressions [

	self thereAreReturnExpressions
		ifTrue: [ self signalSourceCodeIncludesAReturnStatementError ]
]

{ #category : 'pre-conditions' }
SourceCodeIntervalPrecondition >> assertSourceCodeIsNotPartOfMethodSignature [

	self intervalToExtractIncludesPartOfMethodSignature
		ifTrue: [ self signalExtractingPartOfMethodSignatureError ]
]

{ #category : 'pre-conditions' }
SourceCodeIntervalPrecondition >> assertSourceCodeIsNotPartOfTemporariesDeclaration [

	self isNotInsideATempDeclaration
		ifFalse: [ self signalExtractingTemporaryVariableDefinitionError ]
]

{ #category : 'initialization' }
SourceCodeIntervalPrecondition >> initializeFor: anIntervalToExtract of: aMethodUnderValidation [

	intervalToExtract := anIntervalToExtract.
	method := aMethodUnderValidation.
	methodNode := method notOptimizedMethodNode.
	sourceCode := method sourceCode.
]

{ #category : 'private' }
SourceCodeIntervalPrecondition >> intervalToExtractIncludesPartOfMethodSignature [

	^ intervalToExtract first < self methodDefinitionStartPosition
]

{ #category : 'private' }
SourceCodeIntervalPrecondition >> intervalToExtractIsIncludedInAnyOf: sourceRanges [

	^ sourceRanges anySatisfy: [ :sourceRange |
		sourceRange first <= intervalToExtract first and: [ sourceRange last >= intervalToExtract last ] ]
]

{ #category : 'private' }
SourceCodeIntervalPrecondition >> isNotInsideATempDeclaration [

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isTemporariesDeclaration
			and: [ self intervalToExtractIsIncludedInAnyOf: sourceRanges ])
			ifTrue: [ ^ false ] ].
	^ true
]

{ #category : 'private' }
SourceCodeIntervalPrecondition >> methodDefinitionStartPosition [

	^methodNode definitionStartPosition
]

{ #category : 'exceptions' }
SourceCodeIntervalPrecondition >> signalExtractingPartOfMethodSignatureError [

	self refactoringError: self class errorMessageForExtractingPartOfMethodSignature
]

{ #category : 'exceptions' }
SourceCodeIntervalPrecondition >> signalExtractingTemporaryVariableDefinitionError [

	self refactoringError: self class errorMessageForExtractingTemporaryVariablesDefinition
]

{ #category : 'exceptions' }
SourceCodeIntervalPrecondition >> signalSourceCodeContainsAnInvalidExpressionError [

	self refactoringError: self class errorMessageForSourceCodeContainingInvalidExpression
]

{ #category : 'exceptions' }
SourceCodeIntervalPrecondition >> signalSourceCodeIncludesAReturnStatementError [

	self refactoringError: self class errorMessageForSourceCodeIncludingAReturnStatement
]

{ #category : 'private' }
SourceCodeIntervalPrecondition >> thereAreReturnExpressions [

	^ (methodNode
		allParseNodesWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isReturn ]) not
]

{ #category : 'evaluating' }
SourceCodeIntervalPrecondition >> value [

	self subclassResponsibility
]
