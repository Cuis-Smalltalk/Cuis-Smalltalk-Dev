'From Cuis 5.0 [latest update: #4171] on 25 May 2020 at 12:59:52 am'!
"Change Set:		4172-CuisCore-AuthorName-2020May24-19h34m
Date:			25 May 2020
Author:			Nahuel Garbezza

[extract temporary / rename temporary]
  - fix error message for instance variable already defined in class
  - allow to perform the refactoring in a debugger

[extract method]
  - extracting to an existing selector (in the current class or any superclass) now raises a warning
  - allow to perform the refactoring in a debugger
  - improve error messages and validation logic
  
[documentation]
  - add a comment on RefactoringPrecondition class"!

!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments codeProvider '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractToTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractToTemporaryApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newVariable codeProvider '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RefactoringPrecondition commentStamp: '<historical>' prior: 0!
I represent a precondition, a prerequisite for a refactoring to be evaluated successfully. My public instance protocol includes only one message, #value, which could raise either a RefactoringError (in case the refactoring cannot be performed) or a RefactoringWarning (in case something needs the programmer's attention, but it can be resumed to continue with the refactoring).!

Smalltalk renameClassNamed: #ExtractMethodExpressionValidation as: #SourceCodeOfMethodToBeExtractedPrecondition!
!classDefinition: #SourceCodeOfMethodToBeExtractedPrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #SourceCodeOfMethodToBeExtractedPrecondition
	instanceVariableNames: 'method intervalToExtract methodNode sourceCode initialNode finalNode initialNodeAncestors finalNodeAncestors '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SourceCodeOfMethodToBeExtractedPrecondition commentStamp: '<historical>' prior: 0!
I check if a piece of source code selected for extract method can actually be extracted.

Many conditions have to happen:
* we are selecting from the beginning of an AST node to an end of an AST node
* it is a smalltalk expression, a single statement or a sequence of complete statements
* the expression does not contain returns, or temporary variable assignments (if there is a temporary variable assignment, the declaration and all the usages should be extracted as well)!


!CodeProvider methodsFor: 'contents' stamp: 'RNG 5/24/2020 21:27:20'!
currentMethodRefactored

	self acceptedContentsChanged
! !


!Debugger methodsFor: 'contents' stamp: 'RNG 5/24/2020 21:27:20'!
currentMethodRefactored

	| newMethod |
	
	newMethod := self selectedClass compiledMethodAt: self selectedContext selector.
	self resetToSelectedContextWith: newMethod ! !


!SmalltalkEditor methodsFor: 'private' stamp: 'RNG 5/24/2020 21:34:17'!
performCodeExtractionRefactoringWith: aRefactoringApplierClass

	"To prevent the refactoring to be evaluated on editors w/o methods like the workspace - Nahuel"
	self hasValidCurrentCompiledMethod ifFalse: [ ^ nil ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		
	aRefactoringApplierClass createAndValueHandlingExceptions: [
		aRefactoringApplierClass
			on: self codeProvider
			for: self selectionInterval asSourceCodeInterval
			of: self codeProvider currentCompiledMethod ] ]! !


!ExtractMethod class methodsFor: 'error messages' stamp: 'RNG 5/24/2020 19:50:32'!
wrongNumberOfArgumentsGivenErrorMessage

	^ 'The number of arguments in the given selector is not correct'! !

!ExtractMethod class methodsFor: 'pre-conditions' stamp: 'RNG 5/24/2020 22:36:58'!
assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract

	self
		assertIntervalToExtractIsNotEmpty: anIntervalToExtract;
		assert: anIntervalToExtract isWithinBoundsOf: aMethodToExtractCodeFrom sourceCode;
		assert: aMethodToExtractCodeFrom containsValidCodeToBeExtractedAt: anIntervalToExtract! !

!ExtractMethod class methodsFor: 'private - pre-conditions' stamp: 'RNG 5/24/2020 22:33:47'!
assert: aMethodToRefactor containsValidCodeToBeExtractedAt: anIntervalToExtract

	SourceCodeOfMethodToBeExtractedPrecondition valueFor: anIntervalToExtract of: aMethodToRefactor! !

!ExtractMethod class methodsFor: 'private - pre-conditions' stamp: 'RNG 5/24/2020 22:37:04'!
assert: anIntervalToExtract isWithinBoundsOf: sourceCode

	(self is: anIntervalToExtract withinBoundsOf: sourceCode)
		ifFalse: [ self signalOutOfBoundsIntervalError ]! !

!ExtractMethod class methodsFor: 'private - pre-conditions' stamp: 'RNG 5/24/2020 22:35:01'!
assertIntervalToExtractIsNotEmpty: anIntervalToExtract

	(self isNotEmpty: anIntervalToExtract)
		ifFalse: [ self signalNoSelectedCodeError ]! !


!ExtractMethodApplier methodsFor: 'initialization' stamp: 'RNG 5/24/2020 21:11:57'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := aMethodToExtractCodeFrom.
	newMessageArguments := Dictionary new! !


!ExtractMethodApplier class methodsFor: 'instance creation' stamp: 'RNG 5/24/2020 22:32:42'!
on: aCodeProvider for: anIntervalToExtract of: aMethodToRefactor

	| trimmedIntervalToExtract sourceCode |
	sourceCode := aMethodToRefactor sourceCode.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: sourceCode..

	self assertCanApplyRefactoringOn: aMethodToRefactor at: trimmedIntervalToExtract.

	^ self new initializeOn: aCodeProvider for: trimmedIntervalToExtract of: aMethodToRefactor! !


!ExtractToTemporaryApplier methodsFor: 'initialization' stamp: 'RNG 5/24/2020 21:12:30'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := aMethodToExtractCodeFrom.! !


!ExtractToTemporaryApplier class methodsFor: 'instance creation' stamp: 'RNG 5/24/2020 21:10:47'!
on: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract.

	^ self new initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom! !


!ExtractMethodNewSelectorPrecondition methodsFor: 'evaluating - private' stamp: 'RNG 5/24/2020 20:25:43'!
assertNewSelectorIsNotAlreadyDefinedWithinTheClassHierarchy

	(classToDefineSelector whichClassIncludesSelector: selectorToValidate)
		ifNotNil: [ :classDefiningSelector | self warn: selectorToValidate isAlreadyDefinedIn: classDefiningSelector ]! !

!ExtractMethodNewSelectorPrecondition methodsFor: 'warnings' stamp: 'RNG 5/24/2020 20:37:46'!
warn: aSelector isAlreadyDefinedIn: aClassDefiningSelector

	self refactoringWarning: (self class warningMessageFor: aSelector isAlreadyDefinedIn: aClassDefiningSelector)! !


!ExtractMethodNewSelectorPrecondition class methodsFor: 'warning messages' stamp: 'RNG 5/24/2020 20:41:41'!
warningMessageFor: existingSelector isAlreadyDefinedIn: classDefiningSelector 

	^ existingSelector , ' is already defined in ' , classDefiningSelector name! !


!NewTemporaryPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/24/2020 20:01:16'!
errorMessageFor: aNewVariable canNotBeNamedDueToInstanceVariableDefinedIn: aClass

	^ aNewVariable, ' cannot be used as temporary variable name because it is an instance variable defined in ', aClass name! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'evaluating' stamp: 'RNG 5/25/2020 00:51:57'!
value

	self
		initializeParseNodesMatchingSelectionInterval;
		assertSourceCodeDoesNotIncludeReturnExpressions;
		assertSourceCodeIsNotLeftSideOfAssignment;
		assertSourceCodeIsNotPartOfTemporariesDeclaration;
		assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration;
		assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval;
		assertSourceCodeContainsAValidExpression! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 5/24/2020 22:08:41'!
signalExtractingLeftSideOfAssignmentError

	self refactoringError: self class errorMessageForExtractingLeftSideOfAssignment! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 5/25/2020 00:13:13'!
signalExtractingTemporaryVariableDefinitionError

	self refactoringError: self class errorMessageForExtractingTemporaryVariablesDefinition! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 5/25/2020 00:48:13'!
signalSourceCodeContainsAnInvalidExpressionError

	self refactoringError: self class errorMessageForSourceCodeContainingInvalidExpression! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 5/24/2020 21:57:41'!
signalSourceCodeIncludesAReturnStatementError

	self refactoringError: self class errorMessageForSourceCodeIncludingAReturnStatement! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 5/25/2020 00:54:43'!
signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError

	self refactoringError: self class errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'RNG 5/25/2020 00:30:02'!
signalTemporaryAssignmentWithoutDeclarationError

	self refactoringError: self class errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 5/25/2020 00:52:33'!
assertSourceCodeContainsAValidExpression

	(self intervalCoversCompleteAstNodes and: [ self startAndEndNodesShareAParentNode
		or: [ self intervalMatchesBeginningOfStatement and: [ self intervalMatchesEndOfStatement ] ] ])

		ifFalse: [ self signalSourceCodeContainsAnInvalidExpressionError ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 5/25/2020 00:39:55'!
assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval

	self isNotATempDeclarationWithUsagesOutOfIntervalToExtract
		ifFalse: [ self signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 5/24/2020 22:03:51'!
assertSourceCodeDoesNotIncludeReturnExpressions

	self thereAreReturnExpressions
		ifTrue: [ self signalSourceCodeIncludesAReturnStatementError ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 5/25/2020 00:28:44'!
assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration

	self thereAreNoLocalVariableAssignmentsWithoutDeclaration
		ifFalse: [ self signalTemporaryAssignmentWithoutDeclarationError ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 5/25/2020 00:07:42'!
assertSourceCodeIsNotLeftSideOfAssignment

	self isLeftSideOfAssignment
		ifTrue: [ self signalExtractingLeftSideOfAssignmentError ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 5/25/2020 00:12:37'!
assertSourceCodeIsNotPartOfTemporariesDeclaration

	self isNotInsideATempDeclaration
		ifFalse: [ self signalExtractingTemporaryVariableDefinitionError ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 5/25/2020 00:47:29'!
initializeParseNodesMatchingSelectionInterval

	initialNodeAncestors := methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	finalNodeAncestors := methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ self signalSourceCodeContainsAnInvalidExpressionError ].
	initialNode := initialNodeAncestors first.
	finalNode := finalNodeAncestors first! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 5/25/2020 00:22:31'!
intervalToExtractIsIncludedInAnyOf: sourceRanges

	^ sourceRanges anySatisfy: [ :sourceRange |
		sourceRange first <= intervalToExtract first and: [ sourceRange last >= intervalToExtract last ] ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 5/25/2020 00:44:55'!
startAndEndParseNodesAreTheSame

	^ initialNode key = finalNode key! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 5/24/2020 22:03:51'!
thereAreReturnExpressions 

	^ (methodNode
		allParseNodesWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isReturn ]) not! !


!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/25/2020 00:26:39'!
errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration

	^ self extractMethodErrorMessagePrefix , 'an assignment is being extracted without its declaration'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/24/2020 22:14:48'!
errorMessageForExtractingLeftSideOfAssignment

	^ self extractMethodErrorMessagePrefix , 'it is not possible to extract the left side of an assignment'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/24/2020 22:57:11'!
errorMessageForExtractingTemporaryVariablesDefinition

	^ self extractMethodErrorMessagePrefix , 'it is not possible to extract temporary variable definitions'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/25/2020 00:48:55'!
errorMessageForSourceCodeContainingInvalidExpression

	^ self extractMethodErrorMessagePrefix , 'the selected code contains an invalid expression'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/24/2020 22:15:04'!
errorMessageForSourceCodeIncludingAReturnStatement

	^ self extractMethodErrorMessagePrefix , 'the selected code includes a return statement'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'RNG 5/25/2020 00:38:28'!
errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval

	^ self extractMethodErrorMessagePrefix , 'there are temporary variables used outside of the code selection'! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'evaluating' stamp: 'RNG 5/24/2020 22:19:38'!
valueFor: anIntervalToExtract of: aMethod

	^ (self for: anIntervalToExtract of: aMethod) value! !

!SourceCodeOfMethodToBeExtractedPrecondition class methodsFor: 'private - error messages' stamp: 'RNG 5/24/2020 22:14:33'!
extractMethodErrorMessagePrefix

	^ 'Cannot extract method: '! !


!MethodNode methodsFor: 'testing' stamp: 'RNG 5/24/2020 21:54:35'!
allParseNodesWithin: aSourceCodeInterval satisfy: aCondition

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			(sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ])
				ifTrue: [ ^ false ] ] ].
	^ true! !


!SmalltalkEditor methodsFor: 'extract to temporary' stamp: 'RNG 5/24/2020 21:35:15'!
extractToTemporary

	self performCodeExtractionRefactoringWith: ExtractToTemporaryApplier! !

!SmalltalkEditor methodsFor: 'extract method' stamp: 'RNG 5/24/2020 21:34:55'!
extractMethod

	self performCodeExtractionRefactoringWith: ExtractMethodApplier! !


!ExtractMethod class methodsFor: 'error messages' stamp: 'RNG 5/24/2020 21:38:48'!
outOfBoundsSelectionErrorMessage

	^ 'The requested source code selection interval is out of bounds'! !

!ExtractMethod class methodsFor: 'exceptions' stamp: 'RNG 5/24/2020 19:50:32'!
signalExtractMethodWithWrongNumberOfArgumentsError

	self refactoringError: self wrongNumberOfArgumentsGivenErrorMessage! !

!ExtractMethod class methodsFor: 'instance creation' stamp: 'RNG 5/24/2020 22:37:50'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory

	| trimmedIntervalToExtract |
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory! !


!ExtractMethodApplier methodsFor: 'refactoring - changes' stamp: 'RNG 5/24/2020 21:27:20'!
showChanges

	codeProvider currentMethodRefactored! !


!ExtractMethodApplier class methodsFor: 'pre-conditions' stamp: 'RNG 5/24/2020 22:32:28'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	ExtractMethod
		assertCanApplyRefactoringOn: aMethodToRefactor
		at: anIntervalToExtract! !


!ExtractToTemporaryApplier methodsFor: 'refactoring - changes' stamp: 'RNG 5/24/2020 21:27:20'!
showChanges

	codeProvider currentMethodRefactored! !


!RenameInstanceVariableApplier methodsFor: 'refactoring - changes' stamp: 'RNG 5/24/2020 21:27:51'!
informChangesToBrowser
	
	browser currentMethodRefactored! !


!ExtractMethodNewSelectorPrecondition methodsFor: 'evaluating' stamp: 'RNG 5/24/2020 20:14:39'!
value

	self
		assertNewSelectorIsNotEmpty;
		assertNewSelectorDoesNotContainSeparators;
		assertNewSelectorBeginsWithAValidCharacter;
		assertNewSelectorContainsOnlyValidCharacters;
		assertNewSelectorIsNotAlreadyDefinedWithinTheClassHierarchy! !


!NewTemporaryPrecondition methodsFor: 'exceptions' stamp: 'RNG 5/24/2020 20:01:16'!
signalNewVariableCanNotHideInstanceVariableDefinedIn: aClass

	self refactoringError: (
		self class
			errorMessageFor: newTemporaryVariableName
			canNotBeNamedDueToInstanceVariableDefinedIn: aClass)! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'initialization' stamp: 'RNG 5/24/2020 22:50:39'!
initializeFor: anIntervalToExtract of: aMethodUnderValidation

	intervalToExtract := anIntervalToExtract.
	method := aMethodUnderValidation.
	methodNode := method methodNode.
	sourceCode := method sourceCode.! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 5/25/2020 00:45:10'!
isLeftSideOfAssignment

	^ initialNodeAncestors size > 1
		and: [ self startAndEndParseNodesAreTheSame ]
		and: [ initialNodeAncestors second key isAssignmentNode ]
		and: [ initialNodeAncestors second key variable = initialNode key ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'RNG 5/25/2020 00:22:21'!
isNotInsideATempDeclaration

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isTemporariesDeclaration
			and: [ self intervalToExtractIsIncludedInAnyOf: sourceRanges ])
			ifTrue: [ ^ false ] ].
	^ true! !

!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #thereAreNoReturnExpressions stamp: 'RNG 5/24/2020 21:59:08'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #thereAreNoReturnExpressions!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #isNotLeftSideOfAssignment stamp: 'RNG 5/24/2020 22:12:43'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #isNotLeftSideOfAssignment!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #passed stamp: 'RNG 5/24/2020 22:18:47'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #passed!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #containsValidNodes stamp: 'RNG 5/25/2020 00:29:29'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #containsValidNodes!
!methodRemoval: NewTemporaryPrecondition class #errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: stamp: 'RNG 5/24/2020 20:01:16'!
NewTemporaryPrecondition class removeSelector: #errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn:!
!methodRemoval: ExtractMethodNewSelectorPrecondition class #newSelectorAlreadyDefinedOnTheClassErrorMessage stamp: 'RNG 5/24/2020 20:24:31'!
ExtractMethodNewSelectorPrecondition class removeSelector: #newSelectorAlreadyDefinedOnTheClassErrorMessage!
!methodRemoval: ExtractMethodNewSelectorPrecondition #assertNewSelectorIsNotAlreadyDefinedInTheClass stamp: 'RNG 5/24/2020 20:14:39'!
ExtractMethodNewSelectorPrecondition removeSelector: #assertNewSelectorIsNotAlreadyDefinedInTheClass!
!methodRemoval: ExtractMethodNewSelectorPrecondition #signalNewSelectorIsAlreadyDefinedInTheClassError stamp: 'RNG 5/24/2020 20:24:52'!
ExtractMethodNewSelectorPrecondition removeSelector: #signalNewSelectorIsAlreadyDefinedInTheClassError!
!methodRemoval: ExtractToTemporaryApplier class #for:of: stamp: 'RNG 5/24/2020 21:05:44'!
ExtractToTemporaryApplier class removeSelector: #for:of:!
!methodRemoval: ExtractToTemporaryApplier #initializeFor:of: stamp: 'RNG 5/24/2020 21:09:33'!
ExtractToTemporaryApplier removeSelector: #initializeFor:of:!
!methodRemoval: ExtractMethodApplier class #for:of: stamp: 'RNG 5/24/2020 21:05:44'!
ExtractMethodApplier class removeSelector: #for:of:!
!methodRemoval: ExtractMethodApplier #initializeFor:of: stamp: 'RNG 5/24/2020 21:09:33'!
ExtractMethodApplier removeSelector: #initializeFor:of:!
!methodRemoval: ExtractMethod class #assert:isValidIntervalOn: stamp: 'RNG 5/24/2020 22:33:10'!
ExtractMethod class removeSelector: #assert:isValidIntervalOn:!
!methodRemoval: ExtractMethod class #signalSelectedCodeIsInvalidForExtractError stamp: 'RNG 5/24/2020 22:24:43'!
ExtractMethod class removeSelector: #signalSelectedCodeIsInvalidForExtractError!
!methodRemoval: ExtractMethod class #wrongNumberOrArgumentsGivenErrorMessage stamp: 'RNG 5/24/2020 19:50:32'!
ExtractMethod class removeSelector: #wrongNumberOrArgumentsGivenErrorMessage!
!methodRemoval: ExtractMethod class #selectedCodeInvalidForExtractErrorMessage stamp: 'RNG 5/24/2020 22:25:42'!
ExtractMethod class removeSelector: #selectedCodeInvalidForExtractErrorMessage!
!methodRemoval: ExtractMethod class #method:containsAValidExpressionOn: stamp: 'RNG 5/24/2020 22:22:36'!
ExtractMethod class removeSelector: #method:containsAValidExpressionOn:!
!methodRemoval: Debugger #instanceVariableRenamed stamp: 'RNG 5/24/2020 21:28:14'!
Debugger removeSelector: #instanceVariableRenamed!
!methodRemoval: CodeProvider #instanceVariablePushedUp stamp: 'RNG 5/24/2020 21:27:41'!
CodeProvider removeSelector: #instanceVariablePushedUp!
!methodRemoval: CodeProvider #instanceVariableRenamed stamp: 'RNG 5/24/2020 21:27:58'!
CodeProvider removeSelector: #instanceVariableRenamed!

!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context: resetToSelectedContextWith:)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('breakpoints' toggleBreakOnEntry)
('class list' selectedClass)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('contents' currentMethodRefactored)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('dependents access' updateInspectors wantsSteps)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inCategory:fromClass:context: implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('testing' is: isEditingMethod)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: fixReceiverInspector lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
!


!SmalltalkEditor reorganize!
('accessing-selection' selection)
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualRemoveParameter contextualRemoveParameter:)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' compileSelectionFor:in: compileSelectionFor:in:ifFail: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: selectionDoItSourceCodeIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu: openSmalltalkEditorRefactoringMenu refactoringMenu)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
!


!ExtractMethod class reorganize!
('error messages' noSelectionErrorMessage outOfBoundsSelectionErrorMessage wrongNumberOfArgumentsGivenErrorMessage)
('exceptions' signalExtractMethodWithWrongNumberOfArgumentsError signalNoSelectedCodeError signalOutOfBoundsIntervalError)
('instance creation' fromInterval:of:to:categorizedAs:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
('private - pre-conditions' assert:canBeDefinedIn: assert:containsValidCodeToBeExtractedAt: assert:hasValidParametersForExtracting:from: assert:isWithinBoundsOf: assertIntervalToExtractIsNotEmpty: is:withinBoundsOf: isNotEmpty:)
!

!classDefinition: #ExtractMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractMethodApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newSelector newMessageArguments codeProvider'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodApplier reorganize!
('initialization' initializeOn:for:of:)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' requestRefactoringParameters)
('private - new message' buildNewMessage newMessageArgumentNames saveUnarySelector:)
('private - parameterizing' buildInitialSelectorAnswer: formatAsKeyword: formatAsMethodArgument: parseNodesToParameterize saveBinaryOrKeywordSelector:withArguments: saveMessageArgumentsForEach:using: selectorTokensOf: validateRequiredParameters:haveACorrespondingKeywordIn:)
!


!ExtractMethodApplier class reorganize!
('instance creation' on:for:of:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
!

!classDefinition: #ExtractToTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractToTemporaryApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newVariable codeProvider'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractToTemporaryApplier reorganize!
('initialization' initializeOn:for:of:)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' askNewVariableName requestRefactoringParameters)
!


!ExtractToTemporaryApplier class reorganize!
('instance creation' on:for:of:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
!


!ExtractMethodNewSelectorPrecondition reorganize!
('evaluating' value)
('evaluating - private' assertNewSelectorBeginsWithAValidCharacter assertNewSelectorContainsOnlyValidCharacters assertNewSelectorDoesNotContainSeparators assertNewSelectorIsNotAlreadyDefinedWithinTheClassHierarchy assertNewSelectorIsNotEmpty)
('exceptions' signalNewSelectorBeginsWithAnInvalidCharacter signalNewSelectorCanNotBeEmptyError signalNewSelectorCanNotContainSeparatorsError signalNewSelectorContainsInvalidCharactersError)
('initialization' initializeFor:on:)
('warnings' warn:isAlreadyDefinedIn:)
!


!ExtractMethodNewSelectorPrecondition class reorganize!
('error messages' invalidCharacterInsideNewSelectorErrorMessage invalidStartingCharacterOfNewSelectorErrorMessage newSelectorCanNotBeEmptyErrorMessage newSelectorCanNotContainSeparatorsErrorMessage)
('evaluating' valueFor:on:)
('instance creation' for:on:)
('warning messages' warningMessageFor:isAlreadyDefinedIn:)
!


!NewTemporaryPrecondition reorganize!
('private' isDeclaredInAnyOf: isDeclaredInChildrenOfBlockNode isDeclaredInParentsOfBlockNode)
('evaluating' value)
('initialization' initializeFor:in:of:)
('pre-conditions' assertIsNotAReservedName assertIsNotDeclaredInParentsOrChildrenScopes assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass assertIsNotEmpty assertIsValidVariableName)
('exceptions' signalInvalidTemporaryVariable signalNewTemporaryVariableCanNotBeAReservedName signalNewTemporaryVariableisAlreadyDefined signalNewVariableCanNotBeEmpty signalNewVariableCanNotHideInstanceVariableDefinedIn:)
!

!classDefinition: #SourceCodeOfMethodToBeExtractedPrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #SourceCodeOfMethodToBeExtractedPrecondition
	instanceVariableNames: 'method intervalToExtract methodNode sourceCode initialNode finalNode initialNodeAncestors finalNodeAncestors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SourceCodeOfMethodToBeExtractedPrecondition reorganize!
('evaluating' value)
('exceptions' signalExtractingLeftSideOfAssignmentError signalExtractingTemporaryVariableDefinitionError signalSourceCodeContainsAnInvalidExpressionError signalSourceCodeIncludesAReturnStatementError signalTemporaryAssignmentUsedOutsideOfSelectionIntervalError signalTemporaryAssignmentWithoutDeclarationError)
('initialization' initializeFor:of:)
('pre-conditions' assertSourceCodeContainsAValidExpression assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval assertSourceCodeDoesNotIncludeReturnExpressions assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration assertSourceCodeIsNotLeftSideOfAssignment assertSourceCodeIsNotPartOfTemporariesDeclaration)
('private' findSourceRangeOfCloserStatementIn: initializeParseNodesMatchingSelectionInterval intervalCoversCompleteAstNodes intervalMatchesBeginningOfStatement intervalMatchesEndOfStatement intervalToExtractIsIncludedInAnyOf: isLeftSideOfAssignment isNotATempDeclarationWithUsagesOutOfIntervalToExtract isNotDeclaredWithinIntervalToExtract: isNotInsideATempDeclaration parseNodesInCommon startAndEndNodesShareAParentNode startAndEndParseNodesAreTheSame thereAreNoLocalVariableAssignmentsWithoutDeclaration thereAreReturnExpressions trimmed:)
!


!SourceCodeOfMethodToBeExtractedPrecondition class reorganize!
('error messages' errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration errorMessageForExtractingLeftSideOfAssignment errorMessageForExtractingTemporaryVariablesDefinition errorMessageForSourceCodeContainingInvalidExpression errorMessageForSourceCodeIncludingAReturnStatement errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval)
('evaluating' valueFor:of:)
('instance creation' for:of:)
('private - error messages' extractMethodErrorMessagePrefix)
!

