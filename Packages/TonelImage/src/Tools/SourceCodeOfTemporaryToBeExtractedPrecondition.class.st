"
I check if a piece of source code selected for ExtractToTemporary can actually be extracted.

Many conditions have to happen:
* we are selecting from the beginning of an AST node to an end of an AST node
* it is a complete smalltalk expression, a single statement or a sequence of complete statements
* the expression does not contain returns, nor assignments
"
Class {
	#name : 'SourceCodeOfTemporaryToBeExtractedPrecondition',
	#superclass : 'SourceCodeIntervalPrecondition',
	#instVars : [
		'sourceCodeToExtract',
		'parseNodeToExtract'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'error messages' }
SourceCodeOfTemporaryToBeExtractedPrecondition class >> errorMessageForSourceCodeToExtractHasSyntaxError: anErrorDescription [

	^ 'Can not extract a source code with syntax error: ', anErrorDescription 
]

{ #category : 'error messages' }
SourceCodeOfTemporaryToBeExtractedPrecondition class >> errorMessageForSourceCodeToExtractHasToBeOneStatement [

	^ self refactoringErrorMessagePrefix , 'cannot extract more than one statement'
]

{ #category : 'private - error messages' }
SourceCodeOfTemporaryToBeExtractedPrecondition class >> refactoringErrorMessagePrefix [

	^ 'Cannot extract temporary: '
]

{ #category : 'private' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> anySourceRangeMatchesExactlyTheIntervalToExtract [

	| sourceRangeCollections allSourceRanges |
	sourceRangeCollections := methodNode completeSourceRanges values.
	allSourceRanges := sourceRangeCollections
		inject: Set new
		into: [ :sourceRanges :sourceRangeCollection | sourceRanges addAll: sourceRangeCollection; yourself ].
	^ allSourceRanges anySatisfy: [ :sourceRange |
		(sourceRange trimToMatchExpressionOn: sourceCode) = intervalToExtract ]
]

{ #category : 'pre-conditions' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> assertSourceCodeContainValidNodes [

	self parseNodeUnderIntervalToExtractCanBeExtractedToAVariable
		ifFalse: [ self signalSourceCodeContainsAnInvalidExpressionError ]
]

{ #category : 'pre-conditions' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> assertSourceCodeHasOneStatement [

	parseNodeToExtract numberOfStatements ~= 1
		ifTrue: [ self signalCollaborationToExtractHasToBeOneStatementError ]
]

{ #category : 'pre-conditions' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> assertSourceCodeIsACompleteExpression [

	self anySourceRangeMatchesExactlyTheIntervalToExtract
		ifFalse: [ self signalSourceCodeContainsAnInvalidExpressionError ]
]

{ #category : 'initialization' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> initializeFor: anIntervalToExtract of: aMethodUnderValidation [

	super initializeFor: anIntervalToExtract of: aMethodUnderValidation.
	sourceCodeToExtract := sourceCode copyFrom: intervalToExtract first to: intervalToExtract last
]

{ #category : 'private' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> initializeParseNodeToExtract [

	parseNodeToExtract := [ Parser parse: sourceCodeToExtract class: method methodClass noPattern: true ]
		on: SyntaxErrorNotification
		do: [ :anError | self signalCollaborationToExtractHasSyntaxError: anError ]
]

{ #category : 'private' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> parseNodeUnderIntervalToExtractCanBeExtractedToAVariable [

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isAssignmentNode
			and: [ sourceRanges anySatisfy: [ :sourceRange | sourceRange first = intervalToExtract first ] ])
				ifTrue: [ ^ false ]
	].
	^ true
]

{ #category : 'exceptions' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> signalCollaborationToExtractHasSyntaxError: aSyntaxErrorNotification [

	self refactoringError: (self class errorMessageForSourceCodeToExtractHasSyntaxError: aSyntaxErrorNotification messageText)
]

{ #category : 'exceptions' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> signalCollaborationToExtractHasToBeOneStatementError [

	self refactoringError: self class errorMessageForSourceCodeToExtractHasToBeOneStatement
]

{ #category : 'evaluating' }
SourceCodeOfTemporaryToBeExtractedPrecondition >> value [

	self
		initializeParseNodeToExtract;
		assertSourceCodeDoesNotIncludeReturnExpressions;
		assertSourceCodeIsNotPartOfTemporariesDeclaration;
		assertSourceCodeIsNotPartOfMethodSignature;
		assertSourceCodeHasOneStatement;
		assertSourceCodeIsACompleteExpression;
		assertSourceCodeContainValidNodes
]
