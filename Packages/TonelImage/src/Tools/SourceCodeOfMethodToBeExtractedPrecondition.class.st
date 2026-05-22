"
I check if a piece of source code selected for ExtractMethod can actually be extracted.

Many conditions have to happen:
* we are selecting from the beginning of an AST node to an end of an AST node
* it is a smalltalk expression, a single statement or a sequence of complete statements
* the expression does not contain returns, or temporary variable assignments (if there is a temporary variable assignment, the declaration and all the usages should be extracted as well)
"
Class {
	#name : 'SourceCodeOfMethodToBeExtractedPrecondition',
	#superclass : 'SourceCodeIntervalPrecondition',
	#instVars : [
		'initialNode',
		'finalNode',
		'initialNodeAncestors',
		'finalNodeAncestors'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'error messages' }
SourceCodeOfMethodToBeExtractedPrecondition class >> errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration [

	^ self refactoringErrorMessagePrefix , 'an assignment is being extracted without its declaration'
]

{ #category : 'error messages' }
SourceCodeOfMethodToBeExtractedPrecondition class >> errorMessageForExtractingLeftSideOfAssignment [

	^ self refactoringErrorMessagePrefix , 'it is not possible to extract the left side of an assignment'
]

{ #category : 'error messages' }
SourceCodeOfMethodToBeExtractedPrecondition class >> errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval [

	^ self refactoringErrorMessagePrefix , 'there are temporary variables used outside of the code selection'
]

{ #category : 'private - error messages' }
SourceCodeOfMethodToBeExtractedPrecondition class >> refactoringErrorMessagePrefix [

	^ 'Cannot extract method: '
]

{ #category : 'pre-conditions' }
SourceCodeOfMethodToBeExtractedPrecondition >> assertSourceCodeContainsAValidExpression [

	(self intervalCoversCompleteAstNodes 
		and: [ self startAndEndNodesShareAParentNode
			or: [ self intervalMatchesBeginningOfStatement 
				and: [ self intervalMatchesEndOfStatement ]]])
		ifFalse: [ self signalSourceCodeContainsAnInvalidExpressionError ]
]

{ #category : 'pre-conditions' }
SourceCodeOfMethodToBeExtractedPrecondition >> assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval [

	self isNotATempDeclarationWithUsagesOutOfIntervalToExtract
		ifFalse: [ self signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError ]
]

{ #category : 'pre-conditions' }
SourceCodeOfMethodToBeExtractedPrecondition >> assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration [

	self thereAreNoLocalVariableAssignmentsWithoutDeclaration
		ifFalse: [ self signalTemporaryAssignmentWithoutDeclarationError ]
]

{ #category : 'pre-conditions' }
SourceCodeOfMethodToBeExtractedPrecondition >> assertSourceCodeIsNotLeftSideOfAssignment [

	self isLeftSideOfAssignment
		ifTrue: [ self signalExtractingLeftSideOfAssignmentError ]
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> findSourceRangeOfCloserStatementIn: listOfAncestors ifNone: noneBlock [

	^ (listOfAncestors
		detect: [ :assoc | assoc key isBlockNode or: [ assoc key isReturn ]]
		ifFound: [ :assoc | listOfAncestors before: assoc ifNone: noneBlock ]
		ifNone: noneBlock) value
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> initializeParseNodesMatchingSelectionInterval [

	initialNodeAncestors := methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	finalNodeAncestors := methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	initialNode := initialNodeAncestors detect: [ :aNodeRange | aNodeRange key isCascadeNode ] ifNone: [ initialNodeAncestors first ].
	finalNode := finalNodeAncestors first
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> intervalCoversCompleteAstNodes  [

	^ (self trimmed: (SourceCodeInterval from: initialNode value first to: finalNode value last)) = intervalToExtract
		or: [ (initialNode value first to: (self trimmed: finalNode value) last) = intervalToExtract ]
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> intervalMatchesBeginningOfStatement  [

	^ (self findSourceRangeOfCloserStatementIn: initialNodeAncestors ifNone: [ initialNodeAncestors last ]) first = intervalToExtract first
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> intervalMatchesEndOfStatement [

	| closerStatementLastPosition |
	
	closerStatementLastPosition := (self findSourceRangeOfCloserStatementIn: finalNodeAncestors ifNone: [ finalNodeAncestors first ]) last.
	^ closerStatementLastPosition = intervalToExtract last or: [ closerStatementLastPosition - 1 = intervalToExtract last ].
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> isLeftSideOfAssignment [

	^ initialNodeAncestors size > 1
		and: [ self startAndEndParseNodesAreTheSame ]
		and: [ initialNodeAncestors second key isAssignmentNode ]
		and: [ initialNodeAncestors second key variable = initialNode key ]
		and: [ initialNodeAncestors second key variable isInstanceVariableNode not ]
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> isNotATempDeclarationWithUsagesOutOfIntervalToExtract [

	initialNode key isTemporariesDeclaration ifFalse: [ ^ true ].
	
	^ (methodNode
		anyParseNodeWithin: (intervalToExtract last to: sourceCode size)
		satisfy: [ :parseNode |
			parseNode isVariableNode
			and: [ initialNode key declaresVariable: parseNode ] ]) not
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> isNotDeclaredWithinIntervalToExtract: aVariableNode [

	^ (methodNode
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isTemporariesDeclaration
			and: [ parseNode declaresVariable: aVariableNode ] ]) not
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> parseNodesInCommon [

	^ initialNodeAncestors intersection: finalNodeAncestors
]

{ #category : 'exceptions' }
SourceCodeOfMethodToBeExtractedPrecondition >> signalExtractingLeftSideOfAssignmentError [

	self refactoringError: self class errorMessageForExtractingLeftSideOfAssignment
]

{ #category : 'exceptions' }
SourceCodeOfMethodToBeExtractedPrecondition >> signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError [

	self refactoringError: self class errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval
]

{ #category : 'exceptions' }
SourceCodeOfMethodToBeExtractedPrecondition >> signalTemporaryAssignmentWithoutDeclarationError [

	self refactoringError: self class errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> startAndEndNodesShareAParentNode   [

	| parseNodesInCommon |
	parseNodesInCommon := self parseNodesInCommon.
	
	^ parseNodesInCommon notEmpty and: [
		(self trimmed: parseNodesInCommon first value) = intervalToExtract]



]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> startAndEndParseNodesAreTheSame [

	^ initialNode key = finalNode key
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> thereAreNoLocalVariableAssignmentsWithoutDeclaration [

	^ (methodNode
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode |
			parseNode isAssignmentToTemporary
				and: [ self isNotDeclaredWithinIntervalToExtract: parseNode variable ] ]) not
]

{ #category : 'private' }
SourceCodeOfMethodToBeExtractedPrecondition >> trimmed: aSourceCodeInterval [

	^ aSourceCodeInterval trimToMatchExpressionOn: sourceCode
]

{ #category : 'evaluating' }
SourceCodeOfMethodToBeExtractedPrecondition >> value [

	self
		initializeParseNodesMatchingSelectionInterval;
		assertSourceCodeDoesNotIncludeReturnExpressions;
		assertSourceCodeIsNotLeftSideOfAssignment;
		assertSourceCodeIsNotPartOfTemporariesDeclaration;
		assertSourceCodeIsNotPartOfMethodSignature;
		assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration;
		assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval;
		assertSourceCodeContainsAValidExpression
]
