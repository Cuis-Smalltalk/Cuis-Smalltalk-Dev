'From Cuis 5.0 [latest update: #4253] on 12 July 2020 at 7:19:18 pm'!
"Change Set:		4254-CuisCore-AuthorName-2020Jul12-15h05m
Date:			12 July 2020
Author:			Nahuel Garbezza

Improvements on ExtractMethod and ExtractToTemporary refactorings:

* update class comments and introduce new ones for some classes
* extract source code interval validations from the extract temporary class to a precondition class
* make hierarchy of source code preconditions for extract method and extract temporary
* fix a bug where argument names from the method signature can be extracted
* renamed ExtractMethodNewSelectorPrecondition to NewSelectorPrecondition because it does not have anything in particular for extract method"!


!RefactoringWarning commentStamp: '<historical>' prior: 0!
I represent a situation that needs user intervention, in the scope of refactorings. It can be resumed or not. For instance, trying to override an existing method.!


!ExtractMethod commentStamp: '<historical>' prior: 0!
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfMethodToBeExtractedPrecondition and NewSelectorPrecondition most of these checks. Refer to those classes' comments for more information.!


!ExtractToTemporary commentStamp: '<historical>' prior: 0!
I am a refactoring that extracts a selected piece of code to a temporary variable. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new temporary variable name

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfTemporaryToBeExtractedPrecondition and NewTemporaryPrecondition most of these checks. Refer to those classes' comments for more information.!


!NewInstanceVariablePrecondition commentStamp: '<historical>' prior: 0!
I am responsible for checking if a new instance variable can be introduced in a specific class. If that is not possible, I raise a refactoring error.!

Smalltalk renameClassNamed: #ExtractMethodNewSelectorPrecondition as: #NewSelectorPrecondition!

!NewSelectorPrecondition commentStamp: '<historical>' prior: 0!
I am responsible for checking if a new method with a given name can be introduced in a specific class. If that is not possible, I raise a refactoring error.!

!classDefinition: #SourceCodeIntervalPrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #SourceCodeIntervalPrecondition
	instanceVariableNames: 'intervalToExtract method sourceCode methodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SourceCodeIntervalPrecondition commentStamp: '<historical>' prior: 0!
I am an abstract class that validates things that are required for source code intervals.!

!classDefinition: #SourceCodeOfMethodToBeExtractedPrecondition category: #'Tools-Refactoring'!
SourceCodeIntervalPrecondition subclass: #SourceCodeOfMethodToBeExtractedPrecondition
	instanceVariableNames: 'initialNode finalNode initialNodeAncestors finalNodeAncestors '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SourceCodeOfMethodToBeExtractedPrecondition commentStamp: '<historical>' prior: 0!
I check if a piece of source code selected for ExtractMethod can actually be extracted.

Many conditions have to happen:
* we are selecting from the beginning of an AST node to an end of an AST node
* it is a smalltalk expression, a single statement or a sequence of complete statements
* the expression does not contain returns, or temporary variable assignments (if there is a temporary variable assignment, the declaration and all the usages should be extracted as well)!

!classDefinition: #SourceCodeOfTemporaryToBeExtractedPrecondition category: #'Tools-Refactoring'!
SourceCodeIntervalPrecondition subclass: #SourceCodeOfTemporaryToBeExtractedPrecondition
	instanceVariableNames: 'sourceCodeToExtract parseNodeToExtract'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SourceCodeOfTemporaryToBeExtractedPrecondition commentStamp: '<historical>' prior: 0!
I check if a piece of source code selected for ExtractToTemporary can actually be extracted.

Many conditions have to happen:
* we are selecting from the beginning of an AST node to an end of an AST node
* it is a complete smalltalk expression, a single statement or a sequence of complete statements
* the expression does not contain returns, nor assignments!


!SourceCodeIntervalPrecondition methodsFor: 'evaluating' stamp: 'RNG 7/12/2020 18:53:35'!
value

	self subclassResponsibility! !

!SourceCodeIntervalPrecondition methodsFor: 'exceptions' stamp: 'RNG 7/12/2020 18:56:32'!
signalExtractingPartOfMethodSignatureError

	self refactoringError: self class errorMessageForExtractingPartOfMethodSignature! !

!SourceCodeIntervalPrecondition methodsFor: 'exceptions' stamp: 'RNG 7/12/2020 18:56:36'!
signalExtractingTemporaryVariableDefinitionError

	self refactoringError: self class errorMessageForExtractingTemporaryVariablesDefinition! !

!SourceCodeIntervalPrecondition methodsFor: 'exceptions' stamp: 'RNG 7/12/2020 18:57:12'!
signalSourceCodeContainsAnInvalidExpressionError

	self refactoringError: self class errorMessageForSourceCodeContainingInvalidExpression! !

!SourceCodeIntervalPrecondition methodsFor: 'exceptions' stamp: 'RNG 7/12/2020 18:54:41'!
signalSourceCodeIncludesAReturnStatementError

	self refactoringError: self class errorMessageForSourceCodeIncludingAReturnStatement! !

!SourceCodeIntervalPrecondition methodsFor: 'initialization' stamp: 'RNG 7/12/2020 19:02:30'!
initializeFor: anIntervalToExtract of: aMethodUnderValidation

	intervalToExtract := anIntervalToExtract.
	method := aMethodUnderValidation.
	methodNode := method methodNode.
	sourceCode := method sourceCode.! !

!SourceCodeIntervalPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 7/12/2020 18:54:20'!
assertSourceCodeDoesNotIncludeReturnExpressions

	self thereAreReturnExpressions
		ifTrue: [ self signalSourceCodeIncludesAReturnStatementError ]! !

!SourceCodeIntervalPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 7/12/2020 18:56:09'!
assertSourceCodeIsNotPartOfMethodSignature

	self intervalToExtractIncludesPartOfMethodSignature
		ifTrue: [ self signalExtractingPartOfMethodSignatureError ]! !

!SourceCodeIntervalPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 7/12/2020 18:56:16'!
assertSourceCodeIsNotPartOfTemporariesDeclaration

	self isNotInsideATempDeclaration
		ifFalse: [ self signalExtractingTemporaryVariableDefinitionError ]! !

!SourceCodeIntervalPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 19:03:03'!
firstParseNodeOfMethodDefinition

	^ methodNode hasTemporaryVariables
		ifTrue: [ methodNode temporariesDeclaration ]
		ifFalse: [ methodNode block statements first
				ifNotNil: [ :statement | statement ]
				ifNil: [ methodNode ] ]! !

!SourceCodeIntervalPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 19:03:15'!
intervalToExtractIncludesPartOfMethodSignature

	^ intervalToExtract first < self methodDefinitionStartPosition! !

!SourceCodeIntervalPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 19:05:39'!
intervalToExtractIsIncludedInAnyOf: sourceRanges

	^ sourceRanges anySatisfy: [ :sourceRange |
		sourceRange first <= intervalToExtract first and: [ sourceRange last >= intervalToExtract last ] ]! !

!SourceCodeIntervalPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 19:03:37'!
isNotInsideATempDeclaration

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isTemporariesDeclaration
			and: [ self intervalToExtractIsIncludedInAnyOf: sourceRanges ])
			ifTrue: [ ^ false ] ].
	^ true! !

!SourceCodeIntervalPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 19:03:22'!
methodDefinitionStartPosition

	^ methodNode
		singleCompleteSourceRangeOf: self firstParseNodeOfMethodDefinition
		ifPresent: [ :sourceRange | sourceRange first ]
		ifAbsent: [ sourceCode size ]! !

!SourceCodeIntervalPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 19:03:28'!
thereAreReturnExpressions

	^ (methodNode
		allParseNodesWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isReturn ]) not! !


!SourceCodeIntervalPrecondition class methodsFor: 'instance creation' stamp: 'RNG 7/12/2020 19:07:02'!
for: anIntervalToExtract of: aMethod

	^ self new initializeFor: anIntervalToExtract of: aMethod! !

!SourceCodeIntervalPrecondition class methodsFor: 'evaluating' stamp: 'RNG 7/12/2020 19:07:07'!
valueFor: anIntervalToExtract of: aMethod

	^ (self for: anIntervalToExtract of: aMethod) value! !

!SourceCodeIntervalPrecondition class methodsFor: 'private - error messages' stamp: 'RNG 7/12/2020 19:07:39'!
refactoringErrorMessagePrefix

	self subclassResponsibility! !

!SourceCodeIntervalPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 19:08:04'!
errorMessageForExtractingPartOfMethodSignature

	^ self refactoringErrorMessagePrefix , 'it is not possible to extract part of method signature'! !

!SourceCodeIntervalPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 19:08:09'!
errorMessageForExtractingTemporaryVariablesDefinition

	^ self refactoringErrorMessagePrefix , 'it is not possible to extract temporary variable definitions'! !

!SourceCodeIntervalPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 19:08:19'!
errorMessageForSourceCodeContainingInvalidExpression

	^ self refactoringErrorMessagePrefix , 'the selected code contains an invalid expression'! !

!SourceCodeIntervalPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 19:08:23'!
errorMessageForSourceCodeIncludingAReturnStatement

	^ self refactoringErrorMessagePrefix , 'the selected code includes a return statement'! !


!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'private - error messages' stamp: 'RNG 7/12/2020 15:57:25'!
refactoringErrorMessagePrefix

	^ 'Cannot extract method: '! !


!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'evaluating' stamp: 'RNG 7/12/2020 16:52:51'!
value

	self
		initializeParseNodeToExtract;
		assertSourceCodeDoesNotIncludeReturnExpressions;
		assertSourceCodeIsNotPartOfTemporariesDeclaration;
		assertSourceCodeIsNotPartOfMethodSignature;
		assertSourceCodeHasOneStatement;
		assertSourceCodeIsACompleteExpression;
		assertSourceCodeContainValidNodes! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 7/12/2020 17:04:38'!
signalCollaborationToExtractHasSyntaxError: aSyntaxErrorNotification

	self refactoringError: (self class errorMessageForSourceCodeToExtractHasSyntaxError: aSyntaxErrorNotification messageText)! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 7/12/2020 17:04:50'!
signalCollaborationToExtractHasToBeOneStatementError

	self refactoringError: self class errorMessageForSourceCodeToExtractHasToBeOneStatement! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'initialization' stamp: 'RNG 7/12/2020 19:02:49'!
initializeFor: anIntervalToExtract of: aMethodUnderValidation

	super initializeFor: anIntervalToExtract of: aMethodUnderValidation.
	sourceCodeToExtract := sourceCode copyFrom: intervalToExtract first to: intervalToExtract last! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 7/12/2020 18:58:24'!
assertSourceCodeContainValidNodes

	self parseNodeUnderIntervalToExtractCanBeExtractedToAVariable
		ifFalse: [ self signalSourceCodeContainsAnInvalidExpressionError ]! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 7/12/2020 17:03:34'!
assertSourceCodeHasOneStatement

	parseNodeToExtract numberOfStatements ~= 1
		ifTrue: [ self signalCollaborationToExtractHasToBeOneStatementError ]! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 7/12/2020 16:26:44'!
assertSourceCodeIsACompleteExpression

	self anySourceRangeMatchesExactlyTheIntervalToExtract
		ifFalse: [ self signalSourceCodeContainsAnInvalidExpressionError ]! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 17:08:18'!
anySourceRangeMatchesExactlyTheIntervalToExtract

	| sourceRangeCollections allSourceRanges |
	sourceRangeCollections := methodNode completeSourceRanges values.
	allSourceRanges := sourceRangeCollections
		inject: Set new
		into: [ :sourceRanges :sourceRangeCollection | sourceRanges addAll: sourceRangeCollection; yourself ].
	^ allSourceRanges anySatisfy: [ :sourceRange |
		(sourceRange trimToMatchExpressionOn: sourceCode) = intervalToExtract ]! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 17:03:15'!
initializeParseNodeToExtract

	parseNodeToExtract := [ Parser parse: sourceCodeToExtract class: method methodClass noPattern: true ]
		on: SyntaxErrorNotification
		do: [ :anError | self signalCollaborationToExtractHasSyntaxError: anError ]! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 7/12/2020 17:07:28'!
parseNodeUnderIntervalToExtractCanBeExtractedToAVariable

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isAssignmentNode
			and: [ sourceRanges anySatisfy: [ :sourceRange | sourceRange first = intervalToExtract first ] ])
				ifTrue: [ ^ false ]
	].
	^ true! !


!SourceCodeOfTemporaryToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 16:54:14'!
errorMessageForSourceCodeToExtractHasSyntaxError: anErrorDescription

	^ 'Can not extract a source code with syntax error: ', anErrorDescription ! !

!SourceCodeOfTemporaryToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 16:19:18'!
errorMessageForSourceCodeToExtractHasToBeOneStatement

	^ self refactoringErrorMessagePrefix , 'cannot extract more than one statement'! !

!SourceCodeOfTemporaryToBeExtractedPrecondition class methodsFor: 'private - error messages' stamp: 'RNG 7/12/2020 15:58:44'!
refactoringErrorMessagePrefix

	^ 'Cannot extract temporary: '! !


!MethodNode methodsFor: 'testing' stamp: 'RNG 7/12/2020 16:57:24'!
anyParseNodeWithin: aSourceCodeInterval satisfy: aCondition

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			(sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ])
				ifTrue: [ ^ true ] ] ].
	^ false! !


!ExtractMethod class methodsFor: 'private - pre-conditions' stamp: 'RNG 7/12/2020 19:13:15'!
assert: aSelector canBeDefinedIn: aClass

	NewSelectorPrecondition valueFor: aSelector on: aClass! !


!ExtractToTemporary class methodsFor: 'preconditions' stamp: 'RNG 7/12/2020 16:58:30'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	|  trimmedSourceCodeToExtract sourceCodeToExtract trimmedIntervalToExtract originalSourceCode |

	self assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor.
	originalSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := originalSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: originalSourceCode.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.

	SourceCodeOfTemporaryToBeExtractedPrecondition
		valueFor: trimmedIntervalToExtract of: aMethodToRefactor! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'evaluating' stamp: 'RNG 7/12/2020 15:05:27'!
value

	self
		initializeParseNodesMatchingSelectionInterval;
		assertSourceCodeDoesNotIncludeReturnExpressions;
		assertSourceCodeIsNotLeftSideOfAssignment;
		assertSourceCodeIsNotPartOfTemporariesDeclaration;
		assertSourceCodeIsNotPartOfMethodSignature;
		assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration;
		assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval;
		assertSourceCodeContainsAValidExpression! !


!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 15:57:56'!
errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration

	^ self refactoringErrorMessagePrefix , 'an assignment is being extracted without its declaration'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 15:58:04'!
errorMessageForExtractingLeftSideOfAssignment

	^ self refactoringErrorMessagePrefix , 'it is not possible to extract the left side of an assignment'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 7/12/2020 15:58:17'!
errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval

	^ self refactoringErrorMessagePrefix , 'there are temporary variables used outside of the code selection'! !

!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForExtractingPartOfMethodSignature stamp: 'RNG 7/12/2020 19:08:04'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForExtractingPartOfMethodSignature!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration stamp: 'RNG 7/12/2020 16:12:32'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForSourceCodeContainingInvalidExpression stamp: 'RNG 7/12/2020 19:08:19'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForSourceCodeContainingInvalidExpression!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #extractMethodErrorMessagePrefix stamp: 'RNG 7/12/2020 15:57:25'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #extractMethodErrorMessagePrefix!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForExtractingTemporaryVariablesDefinition stamp: 'RNG 7/12/2020 19:08:09'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForExtractingTemporaryVariablesDefinition!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #for:of: stamp: 'RNG 7/12/2020 19:07:02'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #for:of:!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForSourceCodeIncludingAReturnStatement stamp: 'RNG 7/12/2020 19:08:23'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForSourceCodeIncludingAReturnStatement!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForSourceCodeToExtractCanNotIncludeReturn stamp: 'RNG 7/12/2020 17:10:17'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForSourceCodeToExtractCanNotIncludeReturn!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval stamp: 'RNG 7/12/2020 16:12:01'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #errorMessageForExtractingLeftSideOfAssignment stamp: 'RNG 7/12/2020 16:12:29'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #errorMessageForExtractingLeftSideOfAssignment!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition class #valueFor:of: stamp: 'RNG 7/12/2020 19:07:07'!
SourceCodeOfTemporaryToBeExtractedPrecondition class removeSelector: #valueFor:of:!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #thereAreNoLocalVariableAssignmentsWithoutDeclaration stamp: 'RNG 7/12/2020 16:07:33'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #thereAreNoLocalVariableAssignmentsWithoutDeclaration!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #assertSourceCodeDoesNotIncludeReturnExpressions stamp: 'RNG 7/12/2020 18:55:03'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #assertSourceCodeDoesNotIncludeReturnExpressions!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #assertSourceCodeIsNotLeftSideOfAssignment stamp: 'RNG 7/12/2020 16:06:12'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #assertSourceCodeIsNotLeftSideOfAssignment!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration stamp: 'RNG 7/12/2020 16:06:04'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #intervalMatchesBeginningOfStatement stamp: 'RNG 7/12/2020 16:06:40'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #intervalMatchesBeginningOfStatement!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #isLeftSideOfAssignment stamp: 'RNG 7/12/2020 16:07:16'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #isLeftSideOfAssignment!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalSourceCodeContainsAnInvalidExpressionError stamp: 'RNG 7/12/2020 18:57:22'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalSourceCodeContainsAnInvalidExpressionError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #trimmed: stamp: 'RNG 7/12/2020 16:11:45'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #trimmed:!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError stamp: 'RNG 7/12/2020 16:09:58'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #intervalToExtractIsIncludedInAnyOf: stamp: 'RNG 7/12/2020 19:05:39'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #intervalToExtractIsIncludedInAnyOf:!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #isNotInsideATempDeclaration stamp: 'RNG 7/12/2020 19:03:37'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #isNotInsideATempDeclaration!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #firstParseNodeOfMethodDefinition stamp: 'RNG 7/12/2020 19:03:03'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #firstParseNodeOfMethodDefinition!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #intervalMatchesEndOfStatement stamp: 'RNG 7/12/2020 16:06:43'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #intervalMatchesEndOfStatement!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalExtractingLeftSideOfAssignmentError stamp: 'RNG 7/12/2020 16:09:15'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalExtractingLeftSideOfAssignmentError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #startAndEndNodesShareAParentNode stamp: 'RNG 7/12/2020 16:07:55'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #startAndEndNodesShareAParentNode!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalSourceCodeSelectionIncludesAnInvalidExpressionError stamp: 'RNG 7/12/2020 18:58:27'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalSourceCodeSelectionIncludesAnInvalidExpressionError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #intervalToExtractIncludesPartOfMethodSignature stamp: 'RNG 7/12/2020 19:03:15'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #intervalToExtractIncludesPartOfMethodSignature!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #isNotATempDeclarationWithUsagesOutOfIntervalToExtract stamp: 'RNG 7/12/2020 16:07:20'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #isNotATempDeclarationWithUsagesOutOfIntervalToExtract!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #assertSourceCodeIsNotPartOfTemporariesDeclaration stamp: 'RNG 7/12/2020 18:56:16'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #assertSourceCodeIsNotPartOfTemporariesDeclaration!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #assertSourceCodeIsNotPartOfMethodSignature stamp: 'RNG 7/12/2020 18:56:09'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #assertSourceCodeIsNotPartOfMethodSignature!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #parseNodesInCommon stamp: 'RNG 7/12/2020 16:08:01'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #parseNodesInCommon!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval stamp: 'RNG 7/12/2020 16:05:59'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalColaborationToExtractHasToBeOneStatementError stamp: 'RNG 7/12/2020 17:03:34'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalColaborationToExtractHasToBeOneStatementError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #findSourceRangeOfCloserStatementIn: stamp: 'RNG 7/12/2020 16:10:23'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #findSourceRangeOfCloserStatementIn:!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalSourceCodeIncludesAReturnStatementError stamp: 'RNG 7/12/2020 18:54:53'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalSourceCodeIncludesAReturnStatementError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalTemporaryAssignmentWithoutDeclarationError stamp: 'RNG 7/12/2020 16:10:00'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalTemporaryAssignmentWithoutDeclarationError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #intervalCoversCompleteAstNodes stamp: 'RNG 7/12/2020 16:06:35'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #intervalCoversCompleteAstNodes!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #methodDefinitionStartPosition stamp: 'RNG 7/12/2020 19:03:22'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #methodDefinitionStartPosition!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalExtractingPartOfMethodSignatureError stamp: 'RNG 7/12/2020 18:56:32'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalExtractingPartOfMethodSignatureError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #initializeParseNodesMatchingSelectionInterval stamp: 'RNG 7/12/2020 16:08:35'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #initializeParseNodesMatchingSelectionInterval!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #startAndEndParseNodesAreTheSame stamp: 'RNG 7/12/2020 16:07:58'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #startAndEndParseNodesAreTheSame!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #signalExtractingTemporaryVariableDefinitionError stamp: 'RNG 7/12/2020 18:56:36'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #signalExtractingTemporaryVariableDefinitionError!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #assertSourceCodeContainsAValidExpression stamp: 'RNG 7/12/2020 16:05:56'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #assertSourceCodeContainsAValidExpression!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #thereAreReturnExpressions stamp: 'RNG 7/12/2020 19:03:28'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #thereAreReturnExpressions!
!methodRemoval: SourceCodeOfTemporaryToBeExtractedPrecondition #isNotDeclaredWithinIntervalToExtract: stamp: 'RNG 7/12/2020 16:10:36'!
SourceCodeOfTemporaryToBeExtractedPrecondition removeSelector: #isNotDeclaredWithinIntervalToExtract:!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #errorMessageForExtractingPartOfMethodSignature stamp: 'RNG 7/12/2020 19:08:36'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #errorMessageForExtractingPartOfMethodSignature!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #errorMessageForExtractingPartOfMethodDefinition stamp: 'RNG 7/12/2020 15:19:45'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #errorMessageForExtractingPartOfMethodDefinition!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #extractMethodErrorMessagePrefix stamp: 'RNG 7/12/2020 15:57:25'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #extractMethodErrorMessagePrefix!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #errorMessageForSourceCodeContainingInvalidExpression stamp: 'RNG 7/12/2020 19:08:42'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #errorMessageForSourceCodeContainingInvalidExpression!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #errorMessageForExtractingTemporaryVariablesDefinition stamp: 'RNG 7/12/2020 19:08:39'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #errorMessageForExtractingTemporaryVariablesDefinition!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #for:of: stamp: 'RNG 7/12/2020 19:06:45'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #for:of:!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #errorMessageForSourceCodeIncludingAReturnStatement stamp: 'RNG 7/12/2020 19:08:45'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #errorMessageForSourceCodeIncludingAReturnStatement!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #valueFor:of: stamp: 'RNG 7/12/2020 19:06:49'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #valueFor:of:!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #initializeFor:of: stamp: 'RNG 7/12/2020 19:02:30'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #initializeFor:of:!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #isNotInsideATempDeclaration stamp: 'RNG 7/12/2020 19:03:48'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #isNotInsideATempDeclaration!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #intervalToExtractIsIncludedInAnyOf: stamp: 'RNG 7/12/2020 19:05:27'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #intervalToExtractIsIncludedInAnyOf:!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #signalExtractingPartOfMethodSignatureError stamp: 'RNG 7/12/2020 18:56:44'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #signalExtractingPartOfMethodSignatureError!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #assertSourceCodeDoesNotIncludeReturnExpressions stamp: 'RNG 7/12/2020 18:54:20'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #assertSourceCodeDoesNotIncludeReturnExpressions!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #signalExtractingTemporaryVariableDefinitionError stamp: 'RNG 7/12/2020 18:56:49'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #signalExtractingTemporaryVariableDefinitionError!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #assertSourceCodeIsNotPartOfTemporariesDeclaration stamp: 'RNG 7/12/2020 18:59:20'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #assertSourceCodeIsNotPartOfTemporariesDeclaration!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #assertSourceCodeIsNotPartOfMethodSignature stamp: 'RNG 7/12/2020 18:59:18'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #assertSourceCodeIsNotPartOfMethodSignature!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #signalSourceCodeContainsAnInvalidExpressionError stamp: 'RNG 7/12/2020 18:57:12'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #signalSourceCodeContainsAnInvalidExpressionError!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #thereAreReturnExpressions stamp: 'RNG 7/12/2020 19:02:16'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #thereAreReturnExpressions!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #signalSourceCodeIncludesAReturnStatementError stamp: 'RNG 7/12/2020 18:54:41'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #signalSourceCodeIncludesAReturnStatementError!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #intervalToExtractIncludesPartOfMethodSignature stamp: 'RNG 7/12/2020 19:03:55'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #intervalToExtractIncludesPartOfMethodSignature!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #methodDefinitionStartPosition stamp: 'RNG 7/12/2020 19:03:45'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #methodDefinitionStartPosition!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #firstParseNodeOfMethodDefinition stamp: 'RNG 7/12/2020 19:04:59'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #firstParseNodeOfMethodDefinition!
!methodRemoval: ExtractToTemporary class #assert:enclosesAValidNodeOn: stamp: 'RNG 7/12/2020 17:06:53'!
ExtractToTemporary class removeSelector: #assert:enclosesAValidNodeOn:!
!methodRemoval: ExtractToTemporary class #parseNodeUnder:canBeExtractedToAVariableOn: stamp: 'RNG 7/12/2020 17:07:32'!
ExtractToTemporary class removeSelector: #parseNodeUnder:canBeExtractedToAVariableOn:!
!methodRemoval: ExtractToTemporary class #signalSourceCodeSelectionIncludesAnInvalidExpression stamp: 'RNG 7/12/2020 16:54:49'!
ExtractToTemporary class removeSelector: #signalSourceCodeSelectionIncludesAnInvalidExpression!
!methodRemoval: ExtractToTemporary class #signalColaborationToExtractHasSyntaxError: stamp: 'RNG 7/12/2020 16:54:27'!
ExtractToTemporary class removeSelector: #signalColaborationToExtractHasSyntaxError:!
!methodRemoval: ExtractToTemporary class #anySourceRangeOf:matches: stamp: 'RNG 7/12/2020 17:08:21'!
ExtractToTemporary class removeSelector: #anySourceRangeOf:matches:!
!methodRemoval: ExtractToTemporary class #signalSourceCodeToExtractCanNotIncludeReturn stamp: 'RNG 7/12/2020 16:27:51'!
ExtractToTemporary class removeSelector: #signalSourceCodeToExtractCanNotIncludeReturn!
!methodRemoval: ExtractToTemporary class #errorMessageForSourceCodeIncludingAnInvalidExpression stamp: 'RNG 7/12/2020 17:09:05'!
ExtractToTemporary class removeSelector: #errorMessageForSourceCodeIncludingAnInvalidExpression!
!methodRemoval: ExtractToTemporary class #signalColaborationToExtractHasToBeOneStatement stamp: 'RNG 7/12/2020 16:24:18'!
ExtractToTemporary class removeSelector: #signalColaborationToExtractHasToBeOneStatement!
!methodRemoval: ExtractToTemporary class #assertHasOneStatement: stamp: 'RNG 7/12/2020 16:24:21'!
ExtractToTemporary class removeSelector: #assertHasOneStatement:!
!methodRemoval: ExtractToTemporary class #assert:enclosesACompleteExpressionOn: stamp: 'RNG 7/12/2020 16:54:40'!
ExtractToTemporary class removeSelector: #assert:enclosesACompleteExpressionOn:!
!methodRemoval: ExtractToTemporary class #assertIsNotReturn: stamp: 'RNG 7/12/2020 16:24:48'!
ExtractToTemporary class removeSelector: #assertIsNotReturn:!
!methodRemoval: ExtractToTemporary class #errorMessageForSourceCodeToExtractHasSyntaxError: stamp: 'RNG 7/12/2020 16:54:22'!
ExtractToTemporary class removeSelector: #errorMessageForSourceCodeToExtractHasSyntaxError:!
!methodRemoval: ExtractToTemporary class #tryToParse:on: stamp: 'RNG 7/12/2020 16:55:28'!
ExtractToTemporary class removeSelector: #tryToParse:on:!
!methodRemoval: ExtractToTemporary class #errorMessageForSourceCodeToExtractCanNotIncludeReturn stamp: 'RNG 7/12/2020 16:16:48'!
ExtractToTemporary class removeSelector: #errorMessageForSourceCodeToExtractCanNotIncludeReturn!
!methodRemoval: ExtractToTemporary class #errorMessageForSourceCodeToExtractHasToBeOneStatement stamp: 'RNG 7/12/2020 16:18:07'!
ExtractToTemporary class removeSelector: #errorMessageForSourceCodeToExtractHasToBeOneStatement!

!ExtractMethod class reorganize!
('error messages' noSelectionErrorMessage outOfBoundsSelectionErrorMessage wrongNumberOfArgumentsGivenErrorMessage)
('exceptions' signalExtractMethodWithWrongNumberOfArgumentsError signalNoSelectedCodeError signalOutOfBoundsIntervalError)
('instance creation' fromInterval:of:to:categorizedAs:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
('private - pre-conditions' assert:canBeDefinedIn: assert:containsValidCodeToBeExtractedAt: assert:hasValidParametersForExtracting:from: assert:isWithinBoundsOf: assertIntervalToExtractIsNotEmpty: is:withinBoundsOf: isNotEmpty:)
!


!SourceCodeIntervalPrecondition reorganize!
('evaluating' value)
('exceptions' signalExtractingPartOfMethodSignatureError signalExtractingTemporaryVariableDefinitionError signalSourceCodeContainsAnInvalidExpressionError signalSourceCodeIncludesAReturnStatementError)
('initialization' initializeFor:of:)
('pre-conditions' assertSourceCodeDoesNotIncludeReturnExpressions assertSourceCodeIsNotPartOfMethodSignature assertSourceCodeIsNotPartOfTemporariesDeclaration)
('private' firstParseNodeOfMethodDefinition intervalToExtractIncludesPartOfMethodSignature intervalToExtractIsIncludedInAnyOf: isNotInsideATempDeclaration methodDefinitionStartPosition thereAreReturnExpressions)
!


!SourceCodeIntervalPrecondition class reorganize!
('instance creation' for:of:)
('evaluating' valueFor:of:)
('private - error messages' refactoringErrorMessagePrefix)
('error messages' errorMessageForExtractingPartOfMethodSignature errorMessageForExtractingTemporaryVariablesDefinition errorMessageForSourceCodeContainingInvalidExpression errorMessageForSourceCodeIncludingAReturnStatement)
!

!classDefinition: #SourceCodeOfMethodToBeExtractedPrecondition category: #'Tools-Refactoring'!
SourceCodeIntervalPrecondition subclass: #SourceCodeOfMethodToBeExtractedPrecondition
	instanceVariableNames: 'initialNode finalNode initialNodeAncestors finalNodeAncestors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SourceCodeOfMethodToBeExtractedPrecondition reorganize!
('evaluating' value)
('exceptions' signalExtractingLeftSideOfAssignmentError signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError signalTemporaryAssignmentWithoutDeclarationError)
('pre-conditions' assertSourceCodeContainsAValidExpression assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration assertSourceCodeIsNotLeftSideOfAssignment)
('private' findSourceRangeOfCloserStatementIn: initializeParseNodesMatchingSelectionInterval intervalCoversCompleteAstNodes intervalMatchesBeginningOfStatement intervalMatchesEndOfStatement isLeftSideOfAssignment isNotATempDeclarationWithUsagesOutOfIntervalToExtract isNotDeclaredWithinIntervalToExtract: parseNodesInCommon startAndEndNodesShareAParentNode startAndEndParseNodesAreTheSame thereAreNoLocalVariableAssignmentsWithoutDeclaration trimmed:)
!


!SourceCodeOfMethodToBeExtractedPrecondition class reorganize!
('error messages' errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration errorMessageForExtractingLeftSideOfAssignment errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval)
('private - error messages' refactoringErrorMessagePrefix)
!


!SourceCodeOfTemporaryToBeExtractedPrecondition reorganize!
('evaluating' value)
('exceptions' signalCollaborationToExtractHasSyntaxError: signalCollaborationToExtractHasToBeOneStatementError)
('initialization' initializeFor:of:)
('pre-conditions' assertSourceCodeContainValidNodes assertSourceCodeHasOneStatement assertSourceCodeIsACompleteExpression)
('private' anySourceRangeMatchesExactlyTheIntervalToExtract initializeParseNodeToExtract parseNodeUnderIntervalToExtractCanBeExtractedToAVariable)
!


!SourceCodeOfTemporaryToBeExtractedPrecondition class reorganize!
('error messages' errorMessageForSourceCodeToExtractHasSyntaxError: errorMessageForSourceCodeToExtractHasToBeOneStatement)
('private - error messages' refactoringErrorMessagePrefix)
!
