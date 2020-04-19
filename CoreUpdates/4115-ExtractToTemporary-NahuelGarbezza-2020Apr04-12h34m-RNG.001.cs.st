'From Cuis 5.0 [latest update: #4109] on 13 April 2020 at 12:10:39 am'!
"Change Set:		4104-CuisCore-AuthorName-2020Apr04-12h34m
Date:			13 April 2020
Author:			Nahuel Garbezza

Alpha version of the Extract to Temporary refactoring"!

!classDefinition: #TemporariesDeclarationNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #TemporariesDeclarationNode
	instanceVariableNames: 'tempDeclarationNodes declarationWritten '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #ExtractToTemporary category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractToTemporary
	instanceVariableNames: 'newVariableName parseNodeToExtract methodNodeToRefactor methodToRefactor updatedSourceCode intervalToExtract sourceCodeToExtract parseNodeWithNewVariableScope'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractToTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractToTemporaryApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newVariable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Interval methodsFor: 'refactorings' stamp: 'RNG 4/13/2020 00:06:02'!
shouldTrimToMatchExpressionOn: sourceCode atIndex: currentIndex

	| currentChar |
	currentChar := sourceCode at: currentIndex ifAbsent: [ ^ false ].
	^ currentChar isSeparator or: [ currentChar = $. ]
! !

!Interval methodsFor: 'refactorings' stamp: 'RNG 4/13/2020 00:05:57'!
trimMatchingParenthesesOn: aSourceCode

	| trimmedInterval |
	trimmedInterval := self.
	[
		| initialChar endingChar |
		initialChar := aSourceCode at: trimmedInterval first ifAbsent: [ nil ].
		endingChar := aSourceCode at: trimmedInterval last ifAbsent: [ nil ].
		initialChar = $( and: [ endingChar = $) ]
	 ]
		whileTrue: [ trimmedInterval _ trimmedInterval first + 1 to: trimmedInterval last - 1 ].
	^ trimmedInterval! !

!Interval methodsFor: 'refactorings' stamp: 'RNG 4/13/2020 00:06:21'!
trimToMatchExpressionOn: aSourceCode

	 | trimmedInterval |
	trimmedInterval := self.
	[ self shouldTrimToMatchExpressionOn: aSourceCode atIndex: trimmedInterval first ]
		whileTrue: [ trimmedInterval := trimmedInterval first + 1 to: trimmedInterval last ].
	[ self shouldTrimToMatchExpressionOn: aSourceCode atIndex: trimmedInterval last ]
		whileTrue: [ trimmedInterval := trimmedInterval first to: trimmedInterval last - 1 ].

	^ trimmedInterval trimMatchingParenthesesOn: aSourceCode! !


!ParseNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:03:46'!
isBraceNode

	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:09:00'!
isSelectorNode

	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 12:54:25'!
isTemporaryDeclaration

	^ false! !


!AssignmentNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 12:56:01'!
equivalentTo: aParseNode

	^ aParseNode isAssignmentNode
		and: [ self variable equivalentTo: aParseNode variable ]
		and: [ self value equivalentTo: aParseNode value ]! !


!BraceNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:07:05'!
equivalentTo: aParseNode

	^ aParseNode isBraceNode and: [ self hasEquivalentElementsTo: aParseNode ]! !

!BraceNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:03:33'!
isBraceNode

	^ true! !

!BraceNode methodsFor: 'private' stamp: 'RNG 4/4/2020 13:06:30'!
hasEquivalentElementsTo: aBraceNode

	elements with: aBraceNode elements do: [ :myElement :otherElement |
		(myElement equivalentTo: otherElement) ifFalse: [ ^ false ] ].
	^ true! !


!CodeNode methodsFor: 'accessing' stamp: 'RNG 4/5/2020 19:45:20'!
temporariesDeclaration

	self subclassResponsibility! !

!CodeNode methodsFor: 'accessing' stamp: 'RNG 4/5/2020 19:45:28'!
temporariesDeclaration: aTemporariesDeclarationNode

	self subclassResponsibility! !

!CodeNode methodsFor: 'private' stamp: 'RNG 4/4/2020 13:41:00'!
hasEquivalentArgumentsWith: aCodeNode

	self arguments with: aCodeNode arguments do: [ :myArgument :otherCodeNodeArgument |
		(myArgument equivalentTo: otherCodeNodeArgument) ifFalse: [ ^ false ] ].
	^ true! !

!CodeNode methodsFor: 'private' stamp: 'RNG 4/12/2020 20:44:29'!
hasEquivalentTemporariesDeclarationWith: aCodeNode

	^ (self temporariesDeclaration isNil and: [ aCodeNode temporariesDeclaration isNil ])
		or: [ self temporariesDeclaration equivalentTo: aCodeNode temporariesDeclaration ]! !

!CodeNode methodsFor: 'testing' stamp: 'RNG 4/5/2020 19:45:40'!
hasTemporaryVariables

	^ self temporariesDeclaration declaresAnyVariable! !


!BlockNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:40:17'!
equivalentTo: aParseNode

	^ aParseNode isBlockNode
		and: [ self hasEquivalentArgumentsWith: aParseNode ]
		and: [ self hasEquivalentTemporariesDeclarationWith: aParseNode ]
		and: [ self hasEquivalentStatementsWith: aParseNode ]! !

!BlockNode methodsFor: 'private' stamp: 'RNG 4/12/2020 17:14:17'!
hasEquivalentStatementsWith: aCodeNode

	self statements with: aCodeNode statements do: [ :myStatement :otherCodeNodeStatement |
		(myStatement equivalentTo: otherCodeNodeStatement) ifFalse: [ ^ false ] ].
	^ true! !


!MethodNode methodsFor: 'source mapping' stamp: 'RNG 4/4/2020 20:02:38'!
singleCompleteSourceRangeOf: requestedParseNode
	"Returns the source range associated with the requested parse node.
	Fails if there is no source range, or if there are multiple source ranges."

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode equivalentTo: requestedParseNode) ifTrue: [
			sourceRanges size > 1 ifTrue: [ self error: 'there are multiple source ranges for this parse node' ].
			^ sourceRanges first ] ].
	self error: 'could not find source range for this parse node'! !


!LeafNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:35:08'!
equivalentTo: aParseNode

	^ self class = aParseNode class and: [ self key = aParseNode key ]! !


!SelectorNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:21:32'!
equivalentTo: aParseNode

	^ aParseNode isSelectorNode and: [ super equivalentTo: aParseNode ]! !

!SelectorNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:08:43'!
isSelectorNode

	^ true! !


!VariableNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:21:40'!
equivalentTo: aParseNode

	^ aParseNode isVariableNode and: [ super equivalentTo: aParseNode ]! !


!MessageNode methodsFor: 'equation translation' stamp: 'RNG 4/12/2020 20:42:11'!
originalArguments

	^ originalArguments! !

!MessageNode methodsFor: 'equation translation' stamp: 'RNG 4/12/2020 20:39:00'!
originalReceiver

	^ originalReceiver! !

!MessageNode methodsFor: 'private' stamp: 'RNG 4/12/2020 23:20:49'!
compare: myArguments with: othersArguments

	myArguments with: othersArguments do: [ :myArgument :otherArgument |
		(myArgument equivalentTo: otherArgument) ifFalse: [ ^ false ] ].
	^ true! !

!MessageNode methodsFor: 'private' stamp: 'RNG 4/12/2020 23:21:37'!
hasEquivalentReceiverWith: aMessageNode

	^ self isCascade
		ifTrue: [ originalReceiver equivalentTo: aMessageNode originalReceiver ]
		ifFalse: [ receiver equivalentTo: aMessageNode receiver ]! !


!ReturnNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 12:50:40'!
equivalentTo: aParseNode

	^ aParseNode isReturn and: [ expr equivalentTo: aParseNode expr ]! !


!TemporariesDeclarationNode methodsFor: 'initialization' stamp: 'RNG 4/4/2020 12:51:49'!
initializeWithAll: aCollectionOfTempDeclarationNodes declarationWritten: aBoolean

	tempDeclarationNodes := aCollectionOfTempDeclarationNodes.
	declarationWritten := aBoolean! !

!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 12:53:45'!
declarationWritten

	^ declarationWritten! !

!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'RNG 4/13/2020 00:09:45'!
declaresSameVariablesThan: aTemporariesDeclarationNode

	self temporaryDeclarationNodes with: aTemporariesDeclarationNode temporaryDeclarationNodes do: [ :myTempDeclaration :otherTempDeclaration |
		(myTempDeclaration equivalentTo: otherTempDeclaration) ifFalse: [ ^ false ] ].
	^ true! !

!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 12:52:52'!
equivalentTo: aParseNode

	^ aParseNode isTemporariesDeclaration
		and: [ self declaresSameVariablesThan: aParseNode  ]! !


!TemporariesDeclarationNode class methodsFor: 'instance creation' stamp: 'RNG 4/4/2020 12:51:26'!
withAll: tempDeclarationNodes declarationWritten: aBoolean

	^ self new initializeWithAll: tempDeclarationNodes declarationWritten: aBoolean! !


!TemporaryDeclarationNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 12:54:43'!
equivalentTo: aParseNode

	^ aParseNode isTemporaryDeclaration
		and: [ self declaresVariable: aParseNode variableNode ]! !

!TemporaryDeclarationNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 12:54:00'!
isTemporaryDeclaration

	^ true! !


!SmalltalkEditor methodsFor: 'extract to temporary' stamp: 'RNG 4/4/2020 13:28:28'!
extractToTemporary

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		ExtractToTemporaryApplier createAndValueHandlingExceptions: [
			ExtractToTemporaryApplier for: self selectionInterval of: self codeProvider currentCompiledMethod ] ]! !

!SmalltalkEditor methodsFor: 'extract to temporary' stamp: 'RNG 4/4/2020 13:28:19'!
extractToTemporary: aKeyboardEvent

	self extractToTemporary.
	^true! !

!SmalltalkEditor methodsFor: 'private' stamp: 'RNG 4/4/2020 13:29:56'!
hasValidCurrentCompiledMethod

	^ (self codeProvider respondsTo: #currentCompiledMethod)
		and: [ self codeProvider currentCompiledMethod notNil ]! !


!ExtractToTemporary methodsFor: 'applying' stamp: 'RNG 2/25/2020 19:06:03'!
apply

	self
		replaceExtractedCodeWithNewTemporaryVariable;
		writeAssignmentStatementOfNewTemporaryVariable;
		declareNewTemporaryVariable;
		reflectSourceCodeChanges! !

!ExtractToTemporary methodsFor: 'initialization' stamp: 'RNG 3/29/2020 23:13:34'!
initializeNamed: aNewVariable extractingCodeAt: anIntervalToExtract from: aMethodToRefactor declaringTempIn: blockNodeOfNewVariable

	newVariableName _ aNewVariable.
	intervalToExtract _ anIntervalToExtract.
	methodToRefactor _ aMethodToRefactor.
	methodNodeToRefactor _ methodToRefactor methodNode.
	updatedSourceCode _ aMethodToRefactor sourceCode.
	sourceCodeToExtract _ updatedSourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	parseNodeWithNewVariableScope _ blockNodeOfNewVariable! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 4/4/2020 19:32:46'!
addNewTemporaryVariableToExistingDeclarationStatement

	| sourceRangeOfLastTempDeclaration positionOfLastTempDeclaration |
	parseNodeWithNewVariableScope hasTemporaryVariables
		ifTrue: [
			sourceRangeOfLastTempDeclaration := methodNodeToRefactor singleCompleteSourceRangeOf: self lastTemporaryDeclaration.
			positionOfLastTempDeclaration := sourceRangeOfLastTempDeclaration last + 1 ]
		ifFalse: [
			 sourceRangeOfLastTempDeclaration := methodNodeToRefactor singleCompleteSourceRangeOf: parseNodeWithNewVariableScope temporariesDeclaration.
			positionOfLastTempDeclaration := sourceRangeOfLastTempDeclaration last - 1 ].

	self insertAt: positionOfLastTempDeclaration newCodeWith: ' ' , newVariableName! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 3/29/2020 23:16:07'!
declareNewTemporaryVariable

	self hasTemporariesDeclarationBlock
		ifTrue: [ self addNewTemporaryVariableToExistingDeclarationStatement ]
		ifFalse: [ self insertNewTemporaryDeclarationWithNewVariable ]! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 4/13/2020 00:03:15'!
formattedNewVariableAssignment

	| newVariableAssignment |
	newVariableAssignment := newVariableName , ' ' , self preferredAssignmentOperator , ' ' , sourceCodeToExtract , '.'.
	^ newVariableAssignment , String newLineString , String tab! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 3/29/2020 23:38:38'!
hasTemporariesDeclarationBlock

	^ parseNodeWithNewVariableScope temporariesDeclaration declarationWritten! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 4/13/2020 00:02:27'!
insertAt: aPositionInSourceCode newCodeWith: sourceCodeContents

	updatedSourceCode := updatedSourceCode
		copyReplaceFrom: aPositionInSourceCode
		to: aPositionInSourceCode - 1
		with: sourceCodeContents! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 4/13/2020 00:02:42'!
insertNewTemporaryDeclarationWithNewVariable

	| newVariableDeclaration positionToInsertTempVarDeclaration sourceRangeOfFirstStatement |
	sourceRangeOfFirstStatement := methodNodeToRefactor singleCompleteSourceRangeOf: self siblingStatementsOfTemporaryAssignment first.
	positionToInsertTempVarDeclaration := sourceRangeOfFirstStatement first.
	newVariableDeclaration := '| ' , newVariableName , ' |' , String newLineString , String tab.

	self insertAt: positionToInsertTempVarDeclaration newCodeWith: newVariableDeclaration! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 3/29/2020 23:14:02'!
lastTemporaryDeclaration

	^ parseNodeWithNewVariableScope temporariesDeclaration temporaryDeclarationNodes last! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 2/26/2020 12:36:45'!
positionToInsertNewTemporaryVariableAssignment

	^ (methodNodeToRefactor singleCompleteSourceRangeOf: self statementNodeIncludingCodeToExtract) first! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 3/24/2020 22:21:22'!
preferredAssignmentOperator

	^ Preferences leftArrowAssignmentsInGeneratedCodeWithComputedDefault
		ifTrue: [ '_' ]
		ifFalse: [ ':=' ]! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 4/2/2020 22:29:19'!
reflectSourceCodeChanges

	self flag: #RNG. "remove the error handler once all the cases are supported"

	[ methodToRefactor methodClass
		compile: updatedSourceCode
		classified: methodToRefactor category ]
	on: SyntaxErrorNotification
	do: [ :syntaxError | self class refactoringError: 'Syntax error: unsupported refactoring case' ]! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 4/13/2020 00:03:26'!
replaceExtractedCodeWithNewTemporaryVariable

	updatedSourceCode := updatedSourceCode
		copyReplaceFrom: intervalToExtract first
		to: intervalToExtract last
		with: newVariableName! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 3/29/2020 23:18:06'!
siblingStatementsOfTemporaryAssignment

	^ parseNodeWithNewVariableScope isBlockNode
		ifTrue: [ parseNodeWithNewVariableScope statements ]
		ifFalse: [ parseNodeWithNewVariableScope block statements ]! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 4/13/2020 00:03:00'!
statementNodeIncludingCodeToExtract

	^ self siblingStatementsOfTemporaryAssignment detect: [ :statement |
		(methodNodeToRefactor singleCompleteSourceRangeOf: statement) last >= intervalToExtract last ]! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 2/26/2020 12:37:27'!
writeAssignmentStatementOfNewTemporaryVariable

	self
		insertAt: self positionToInsertNewTemporaryVariableAssignment
		newCodeWith: self formattedNewVariableAssignment! !


!ExtractToTemporary class methodsFor: 'error messages' stamp: 'RNG 4/12/2020 20:13:23'!
errorMessageForSourceCodeIncludingAnInvalidExpression

	^ 'The source code selection contains an invalid expression'! !

!ExtractToTemporary class methodsFor: 'error messages' stamp: 'RNG 2/25/2020 15:14:33'!
errorMessageForSourceCodeSelectionOutOfBounds

	^ 'The source code selection interval is out of bounds'! !

!ExtractToTemporary class methodsFor: 'error messages' stamp: 'RNG 2/24/2020 22:59:12'!
errorMessageForSourceCodeToExtractCanNotBeEmpty

	^ 'Source code to extract can not be empty'! !

!ExtractToTemporary class methodsFor: 'error messages' stamp: 'RNG 2/24/2020 23:02:10'!
errorMessageForSourceCodeToExtractCanNotIncludeReturn

	^ 'An expression containing a return can not be extracted'! !

!ExtractToTemporary class methodsFor: 'error messages' stamp: 'RNG 2/24/2020 22:59:08'!
errorMessageForSourceCodeToExtractHasSyntaxError: anErrorDescription

	^ 'Can not extract a source code with syntax error: ', anErrorDescription ! !

!ExtractToTemporary class methodsFor: 'error messages' stamp: 'RNG 2/24/2020 23:00:15'!
errorMessageForSourceCodeToExtractHasToBeOneStatement

	^ 'Can not extract more than one statement'! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'HAW 6/30/2017 06:34:15'!
signalColaborationToExtractHasSyntaxError: aSyntaxErrorNotification

	self refactoringError: (self errorMessageForSourceCodeToExtractHasSyntaxError: aSyntaxErrorNotification messageText)! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'RNG 2/24/2020 23:00:16'!
signalColaborationToExtractHasToBeOneStatement

	self refactoringError: self errorMessageForSourceCodeToExtractHasToBeOneStatement ! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'RNG 2/25/2020 15:21:32'!
signalOutOfBoundsIntervalError

	self refactoringError: self errorMessageForSourceCodeSelectionOutOfBounds! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'RNG 4/12/2020 23:09:03'!
signalSourceCodeSelectionIncludesAnInvalidExpression

	self refactoringError: self errorMessageForSourceCodeIncludingAnInvalidExpression! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'RNG 2/24/2020 22:58:36'!
signalSourceCodeToExtractCanNotBeEmpty

	self refactoringError: self errorMessageForSourceCodeToExtractCanNotBeEmpty! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'RNG 2/24/2020 23:02:10'!
signalSourceCodeToExtractCanNotIncludeReturn

	self refactoringError: self errorMessageForSourceCodeToExtractCanNotIncludeReturn ! !

!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'RNG 4/12/2020 23:54:43'!
named: aNewVariable at: anIntervalToExtract from: aMethodToRefactor

	|  trimmedNewVariable trimmedIntervalToExtract codeNodeForNewVariable methodNodeToRefactor |

	self assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract.
	methodNodeToRefactor := aMethodToRefactor methodNode.
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToRefactor sourceCode.
	codeNodeForNewVariable := self methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: codeNodeForNewVariable of: methodNodeToRefactor.

	^ self new
		initializeNamed: trimmedNewVariable
		extractingCodeAt: trimmedIntervalToExtract
		from: aMethodToRefactor
		declaringTempIn: codeNodeForNewVariable! !

!ExtractToTemporary class methodsFor: 'preconditions' stamp: 'RNG 4/12/2020 23:54:29'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	|  trimmedSourceCodeToExtract methodNodeToExtract sourceCodeToExtract trimmedIntervalToExtract originalSourceCode |

	self assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor.
	originalSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := originalSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: originalSourceCode.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	methodNodeToExtract := self tryToParse: trimmedSourceCodeToExtract on: aMethodToRefactor methodClass.
	self assertHasOneStatement: methodNodeToExtract.
	self assertIsNotReturn: trimmedSourceCodeToExtract.
	self assert: trimmedIntervalToExtract enclosesAValidExpressionOn: aMethodToRefactor.! !

!ExtractToTemporary class methodsFor: 'private - parsing' stamp: 'RNG 2/25/2020 15:18:08'!
is: anIntervalToExtract withinBoundsOf: aSourceCode

	^ anIntervalToExtract first >= 1 and: [ anIntervalToExtract last <= aSourceCode size ]! !

!ExtractToTemporary class methodsFor: 'private - parsing' stamp: 'RNG 4/4/2020 12:41:16'!
methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor
	"Finds the appropriate block node to define a variable that will reference the code in the interval to extract.
	The possibles results are the top-level methodNode or a block node inside some of the method statements."

	self flag: #RNG. "use Interval>>isIncludedIn: once is merged"
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode
			and: [ parseNode ~= methodNodeToRefactor block ]
			and: [ sourceRanges anySatisfy: [ :sourceRange |
					(sourceRange rangeIncludes: anIntervalToExtract first) and: [ sourceRange rangeIncludes: anIntervalToExtract last ] ] ])
				ifTrue: [ ^ parseNode ]
		].
	^ methodNodeToRefactor! !

!ExtractToTemporary class methodsFor: 'private - parsing' stamp: 'RNG 4/12/2020 19:21:29'!
tryToParse: aSourceCode on: aClassToRefactor

	^ [ Parser parse: aSourceCode class: aClassToRefactor noPattern: true ]
		on: SyntaxErrorNotification
		do: [ :anError | self signalColaborationToExtractHasSyntaxError: anError ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/12/2020 23:54:12'!
anySourceRangeOf: aMethodToRefactor matches: anIntervalToExtract

	| sourceRangeCollections allSourceRanges sourceCode |
	sourceRangeCollections := aMethodToRefactor methodNode completeSourceRanges values.
	allSourceRanges := sourceRangeCollections
		inject: Set new
		into: [ :sourceRanges :sourceRangeCollection | sourceRanges addAll: sourceRangeCollection; yourself ].
	sourceCode := aMethodToRefactor sourceCode.
	^ allSourceRanges anySatisfy: [ :sourceRange |
		(sourceRange trimToMatchExpressionOn: sourceCode) = anIntervalToExtract ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/12/2020 23:28:52'!
assert: anIntervalToExtract enclosesAValidExpressionOn: aMethodToRefactor

	(self anySourceRangeOf: aMethodToRefactor matches: anIntervalToExtract)
		ifFalse: [ self signalSourceCodeSelectionIncludesAnInvalidExpression ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/12/2020 23:08:37'!
assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor

	(self is: anIntervalToExtract withinBoundsOf: aMethodToRefactor sourceCode)
		ifFalse: [ self signalOutOfBoundsIntervalError ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/12/2020 23:14:04'!
assertHasOneStatement: aMethodNodeToExtract

	aMethodNodeToExtract numberOfStatements ~= 1
		ifTrue: [ self signalColaborationToExtractHasToBeOneStatement ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/12/2020 23:08:48'!
assertIsNotReturn: aSourceCodeToExtract

	"I have to use the source code to check this because parsing it returns the same methodNode for
	1 and ^1, that is, using methodNode does not help to distinguish if there was or not a return - Hernan"

	aSourceCodeToExtract withBlanksTrimmed first = $^
		ifTrue: [ self signalSourceCodeToExtractCanNotIncludeReturn ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 2/24/2020 22:57:24'!
assertSourceCodeIsNotEmpty: aSourceCodeToExtract

	aSourceCodeToExtract isEmpty ifTrue: [ self signalSourceCodeToExtractCanNotBeEmpty ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 2/22/2020 22:09:40'!
newTemporaryPreconditionClass

	^ NewTemporaryPrecondition! !


!ExtractToTemporaryApplier methodsFor: 'initialization' stamp: 'RNG 2/15/2020 16:16:53'!
initializeFor: anIntervalToExtract of: aMethodToExtractCodeFrom

	intervalToExtract _ anIntervalToExtract.
	methodToExtractCodeFrom _ aMethodToExtractCodeFrom.! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - changes' stamp: 'RNG 2/15/2020 16:50:13'!
showChanges

	! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - creation' stamp: 'RNG 2/15/2020 17:05:41'!
createRefactoring

	^ self refactoringClass
		named: newVariable
		at: intervalToExtract
		from: methodToExtractCodeFrom! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - creation' stamp: 'RNG 2/15/2020 16:26:52'!
refactoringClass

	^ ExtractToTemporary! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'RNG 4/13/2020 00:04:58'!
askNewVariableName

	newVariable := (self request: 'Enter new temp name:' initialAnswer: '') withBlanksTrimmed! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'RNG 2/15/2020 16:22:35'!
requestRefactoringParameters

	self askNewVariableName! !


!ExtractToTemporaryApplier class methodsFor: 'as yet unclassified' stamp: 'RNG 3/24/2020 23:40:22'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	ExtractToTemporary
		assertCanApplyRefactoringOn: aMethodToRefactor
		at: anIntervalToExtract! !

!ExtractToTemporaryApplier class methodsFor: 'as yet unclassified' stamp: 'RNG 2/25/2020 16:36:24'!
for: anIntervalToExtract of: aMethodToExtractCodeFrom

	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract.

	^ self new initializeFor: anIntervalToExtract of: aMethodToExtractCodeFrom! !


!ParseNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:01:23'!
equivalentTo: aParseNode

	^ false! !


!LiteralNode methodsFor: 'testing' stamp: 'RNG 4/4/2020 13:21:24'!
equivalentTo: aParseNode

	^ aParseNode isLiteralNode and: [ super equivalentTo: aParseNode ]! !


!MessageNode methodsFor: 'testing' stamp: 'RNG 4/12/2020 20:34:23'!
equivalentTo: aParseNode

	^ aParseNode isMessageNode
		and: [ self hasEquivalentReceiverWith: aParseNode ]
		and: [ self selector equivalentTo: aParseNode selector ]
		and: [ self hasEquivalentArgumentsWith: aParseNode ]! !

!MessageNode methodsFor: 'private' stamp: 'RNG 4/12/2020 23:20:14'!
hasEquivalentArgumentsWith: aMessageNode

	^ self isCascade
		ifTrue: [ self compare: originalArguments with: aMessageNode originalArguments ]
		ifFalse: [ self compare: arguments with: aMessageNode arguments ]! !


!TemporariesDeclarationNode class methodsFor: 'instance creation' stamp: 'RNG 4/4/2020 12:52:12'!
empty

	^ self withAll: #() declarationWritten: false! !

!TemporariesDeclarationNode class methodsFor: 'instance creation' stamp: 'RNG 4/4/2020 12:52:06'!
withAll: tempDeclarationNodes

	^ self new initializeWithAll: tempDeclarationNodes declarationWritten: true! !


!SmalltalkEditor methodsFor: 'extract method' stamp: 'RNG 4/4/2020 13:29:24'!
extractMethod

	"To prevent the extract method to be evaluated on editors w/o methods like the workspace"
	self hasValidCurrentCompiledMethod ifFalse: [ ^ nil ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		ExtractMethodApplier createAndValueHandlingExceptions: [
			ExtractMethodApplier for: self selectionInterval of: self codeProvider currentCompiledMethod ] ]! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/12/2020 23:49:38'!
trimmed: anInterval

	^ anInterval trimToMatchExpressionOn: sourceCode! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'RNG 4/12/2020 23:47:26'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory

	| trimmedIntervalToExtract |
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assert: trimmedIntervalToExtract isValidIntervalOn: aMethodToExtractCodeFrom;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory! !


!ExtractMethodApplier class methodsFor: 'instance creation' stamp: 'RNG 4/13/2020 00:01:51'!
for: anIntervalToExtract of: aMethodToExtractCodeFrom

	| trimmedIntervalToExtract sourceCode |
	sourceCode := aMethodToExtractCodeFrom sourceCode.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: sourceCode.

	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract.

	^ self new initializeFor: trimmedIntervalToExtract of: aMethodToExtractCodeFrom! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'RNG 4/4/2020 13:32:20'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		20.
				#label 			-> 		'Extract Temporary... (J)'.
				#selector 		-> 		#extractToTemporary.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		30.
				#label 			-> 		'Extract Method... (K)'.
				#selector 		-> 		#extractMethod.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		40.
				#label 			-> 		'More Refactorings...'.
				#selector 		-> 		#openSmalltalkEditorRefactoringMenu.
				#icon 			-> 		#listAddIcon
		} asDictionary.
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'RNG 4/4/2020 13:27:51'!
smalltalkEditorCmdShortcutsSpec

	^#(
		#($R	#contextualRename:					'Renames what is under cursor')
		#($A	#contextualAddParameter:			'Adds parameter to message that is under cursor')
		#($S	#contextualRemoveParameter:		'Removes parameter to message that is under cursor')
		#($O	#temporaryToInstanceVariable:		'Extracts temporary variable to instance variable')
		#($J	#extractToTemporary:					'Extracts the selected code into a temporary variable')
		#($K	#extractMethod:						'Extracts the selected code into a separate method')
	)! !

!methodRemoval: TemporaryDeclarationNode #isTemporariesDeclaration stamp: 'RNG 4/4/2020 12:54:02'!
TemporaryDeclarationNode removeSelector: #isTemporariesDeclaration!

!Interval reorganize!
('accessing' anyOne at: at:put: extent first increment last rangeIncludes: size)
('comparing' = hash)
('adding' add:)
('removing' remove:)
('enumerating' do: permutationsDo: reverseDo:)
('printing' printOn: storeOn:)
('private' setFrom:to:count: species)
('testing' includes: isInterval)
('arithmetic' + -)
('refactorings' shouldTrimToMatchExpressionOn:atIndex: trimMatchingParenthesesOn: trimToMatchExpressionOn:)
!


!ParseNode reorganize!
('code generation' pc pc:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('comment' comment comment:)
('converting' asReturnNode)
('encoding' encodeSelector:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandIfEnclosed:on: expandRange:basedOn: expandRanges:basedOn:using:)
('testing' assignmentCheck:at: canCascade ensureCanCascade: equivalentTo: isArg isAssignmentNode isAssignmentToTemporary isBlockNode isBraceNode isComplex isConstantNumber isFalsePseudoVariable isInstanceVariableNode isJust: isLiteralNode isLiteralVariableNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilPseudoVariable isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelectorNode isSelfBasicNewMessageSend isSelfNewMessageSend isSelfPseudoVariable isSpecialConstant isSuperPseudoVariable isTemp isTempOrArg isTemporariesDeclaration isTemporaryDeclaration isThisContextPseudoVariable isTruePseudoVariable isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine toDoIncrement:)
('tiles' currentValueIn:)
('visiting' accept: nodesDo:)
('private' consolidateAsCollection: ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
!


!AssignmentNode reorganize!
('code generation' emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCodeForEffect: sizeCodeForValue:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' variable)
('initialization' toDoIncrement: value variable:value: variable:value:from: variable:value:from:sourceRange:)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRanges:basedOn:using:)
('testing' equivalentTo: isAssignmentNode isAssignmentToTemporary isComplex)
('visiting' accept:)
!


!BraceNode reorganize!
('code generation' selectorForShortForm:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools: elements maxElementsForConsArray)
('code generation (new scheme)' emitCodeForValue:encoder: sizeCodeForValue:)
('enumerating' casesForwardDo: casesReverseDo: elementsDo:)
('initialization' elements: elements:sourceLocations: matchBraceStreamReceiver:messages: matchBraceWithReceiver:selector:arguments:)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' blockAssociationCheck: equivalentTo: isBraceNode isComplex numElements)
('visiting' accept:)
('private' hasEquivalentElementsTo:)
!


!CodeNode reorganize!
('accessing' arguments arguments: block temporaries temporaries: temporariesDeclaration temporariesDeclaration:)
('printing' decompileString)
('private' hasEquivalentArgumentsWith: hasEquivalentTemporariesDeclarationWith:)
('testing' hasLocallyArgumentNamed: hasLocallyArgumentOrTemporaryNamed: hasLocallyTemporaryNamed: hasNodeIn:named: hasTemporaryVariables isLocalArgumentOrTemporary:)
!


!BlockNode reorganize!
('accessing' addArgument: arguments arguments: block closureCreationNode firstArgument nArgsSlot nArgsSlot: numberOfArguments optimized returnLast returnNilIfNoOther returnSelfIfNoOther: startOfLastStatement startOfLastStatement: temporaries temporaries: temporariesDeclaration temporariesDeclaration: tempsMark tempsMark:)
('code generation' code)
('code generation (closures)' actualScope addHoistedTemps: addRemoteTemp:rootNode: addTempNode: analyseArguments:temporaries:rootNode: analyseTempsWithin:rootNode:assignmentPools: blockExtent computeCopiedValues: constructClosureCreationNode: deoptimize emitCodeForClosureValue:encoder: emitCodeForEvaluatedClosureValue:encoder: ifHasRemoteTempNodeEnsureInitializationStatementExists: makeTemporariesRemovable nilReadBeforeWrittenTemps noteOptimizedIn: optimizedBlockHoistTempsInto: postNumberingProcessTempsWithin:rootNode: reindexingLocalsDo:encoder: remoteTempNodeName removeTempNode:ifAbsent: sizeCodeForClosureValue: sizeCodeForEvaluatedClosureValue:)
('code generation (new scheme)' emitCodeExceptLast:encoder: emitCodeForEvaluatedEffect:encoder: emitCodeForEvaluatedValue:encoder: emitCodeForValue:encoder: sizeCodeExceptLast: sizeCodeForEvaluatedEffect: sizeCodeForEvaluatedValue: sizeCodeForValue:)
('enumerating' statementsDo:)
('equation translation' statements statements:)
('initialization' arguments:statements:returns:from: noteSourceRangeStart:end:encoder: statements:returns:)
('printing' decompileString printArgumentsOn:indent: printOn:indent: printStatementsOn:indent: printTemporaries:on:doPrior: printWithClosureAnalysisArgumentsOn:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisStatementsOn:indent: printWithClosureAnalysisTemporariesOn:indent:)
('source ranges' expandRanges:basedOn:using:)
('testing' equivalentTo: hasArgumentOrTemporaryNamed: isBlockNode isComplex isJust: isJustCaseError isQuick printsInNewLine returns)
('visiting' accept:)
('private' hasEquivalentStatementsWith:)
!


!LeafNode reorganize!
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation' code emitCodeForLoad:forValue:encoder: reserve: sizeCodeForLoad:forValue:)
('private' code:type:)
('code generation (new scheme)' emitCodeForEffect:encoder: sizeCodeForEffect: sizeCodeForValue:)
('accessing' key key:)
('initialization' key:code: key:index:type: name:key:code:)
('testing' equivalentTo:)
!


!SelectorNode reorganize!
('code generation' reserve:)
('code generation (new scheme)' emitCode:args:encoder: emitCode:args:encoder:super: emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCode:args:super: sizeCodeForEffect: sizeCodeForValue:)
('initialization' name:key:index:type:)
('printing' key: printAsFFICallWithArguments:on:indent: printOn:indent: printWithClosureAnalysisOn:indent:)
('testing' equivalentTo: isForFFICall isInitializePvtSelector isPvtSelector isSelectorNode)
('visiting' accept:)
!


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval originalArguments originalReceiver receiver receiver: selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' expandRanges:basedOn:using: keywordAndParameterPositionAt:encodedWith:ifAbsent: keywordPositionAt: keywordRanges receiverSourceRangesFrom:)
('testing' canCascade ensureCanCascade: equivalentTo: isCascade isComplex isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf isSelfBasicNewMessageSend isSelfNewMessageSend toDoIncrement: toDoLimit:)
('cascading' cascadeReceiver receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: compare:with: hasEquivalentArgumentsWith: hasEquivalentReceiverWith: ifNilReceiver pvtCheckForPvtSelector:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transform: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
('accessing' selectorSymbol)
!

!classDefinition: #TemporariesDeclarationNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #TemporariesDeclarationNode
	instanceVariableNames: 'tempDeclarationNodes declarationWritten'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!TemporariesDeclarationNode reorganize!
('accessing' allDeclaredVariableNodes temporaryDeclarationNodes)
('enumerating' temporaryDeclarationNodesDo:)
('initialization' initializeWithAll:declarationWritten:)
('printing' printEachTempVarDeclarationOn: printOn:indent:)
('testing' declarationWritten declaresAnyVariable declaresSameVariablesThan: declaresVariable: equivalentTo: isTemporariesDeclaration)
('visiting' accept:)
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
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('private' codeProvider hasValidCurrentCompiledMethod)
!


!ExtractMethod class reorganize!
('error messages' noSelectionErrorMessage outOfBoundsSelectionErrorMessage selectedCodeInvalidForExtractErrorMessage wrongNumberOrArgumentsGivenErrorMessage)
('exceptions' signalExtractMethodWithWrongNumberOfArgumentsError signalNoSelectedCodeError signalOutOfBoundsIntervalError signalSelectedCodeIsInvalidForExtractError)
('instance creation' fromInterval:of:to:categorizedAs:)
('preconditions' assert:isValidIntervalOn:)
('private - preconditions' assert:canBeDefinedIn: assert:hasValidParametersForExtracting:from: is:withinBoundsOf: isNotEmpty: method:containsAValidExpressionOn:)
!


!ExtractToTemporary class reorganize!
('error messages' errorMessageForSourceCodeIncludingAnInvalidExpression errorMessageForSourceCodeSelectionOutOfBounds errorMessageForSourceCodeToExtractCanNotBeEmpty errorMessageForSourceCodeToExtractCanNotIncludeReturn errorMessageForSourceCodeToExtractHasSyntaxError: errorMessageForSourceCodeToExtractHasToBeOneStatement)
('exceptions' signalColaborationToExtractHasSyntaxError: signalColaborationToExtractHasToBeOneStatement signalOutOfBoundsIntervalError signalSourceCodeSelectionIncludesAnInvalidExpression signalSourceCodeToExtractCanNotBeEmpty signalSourceCodeToExtractCanNotIncludeReturn)
('instance creation' named:at:from:)
('preconditions' assertCanApplyRefactoringOn:at:)
('private - parsing' is:withinBoundsOf: methodOrBlockNodeIncluding:in: tryToParse:on:)
('private - preconditions' anySourceRangeOf:matches: assert:enclosesAValidExpressionOn: assert:isValidIntervalOn: assertHasOneStatement: assertIsNotReturn: assertSourceCodeIsNotEmpty: newTemporaryPreconditionClass)
!

!classRemoval: #ExtractMethodIntervalTrimmer stamp: 'RNG 4/12/2020 23:54:53'!
Smalltalk removeClassNamed: #ExtractMethodIntervalTrimmer!

"Postscript:"
SmalltalkEditor initializeCmdShortcuts.!
