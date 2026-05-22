"
I am a refactoring that extracts a selected piece of code to a temporary variable. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new temporary variable name

Many conditions have to be satisfied for this refactoring to be made, I delegate into SourceCodeOfTemporaryToBeExtractedPrecondition and NewTemporaryPrecondition most of these checks. Refer to those classes' comments for more information.
"
Class {
	#name : 'ExtractToTemporary',
	#superclass : 'Refactoring',
	#instVars : [
		'newVariableName',
		'methodNodeToRefactor',
		'methodToRefactor',
		'updatedSourceCode',
		'intervalToExtract',
		'sourceCodeToExtract',
		'parseNodeWithNewVariableScope',
		'completeSourceRanges'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'private - preconditions' }
ExtractToTemporary class >> assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor [

	(self is: anIntervalToExtract withinBoundsOf: aMethodToRefactor sourceCode)
		ifFalse: [ self signalOutOfBoundsIntervalError ]
]

{ #category : 'preconditions' }
ExtractToTemporary class >> assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract [

	|  trimmedSourceCodeToExtract sourceCodeToExtract trimmedIntervalToExtract originalSourceCode |

	self assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor.
	originalSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := originalSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: originalSourceCode.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.

	SourceCodeOfTemporaryToBeExtractedPrecondition
		valueFor: trimmedIntervalToExtract of: aMethodToRefactor
]

{ #category : 'private - preconditions' }
ExtractToTemporary class >> assertSourceCodeIsNotEmpty: aSourceCodeToExtract [

	aSourceCodeToExtract isEmpty ifTrue: [ self signalSourceCodeToExtractCanNotBeEmpty ]
]

{ #category : 'error messages' }
ExtractToTemporary class >> errorMessageForSourceCodeSelectionOutOfBounds [

	^ 'The source code selection interval is out of bounds'
]

{ #category : 'error messages' }
ExtractToTemporary class >> errorMessageForSourceCodeToExtractCanNotBeEmpty [

	^ 'Source code to extract can not be empty'
]

{ #category : 'private - parsing' }
ExtractToTemporary class >> is: anIntervalToExtract withinBoundsOf: aSourceCode [

	^ anIntervalToExtract first >= 1 and: [ anIntervalToExtract last <= aSourceCode size ]
]

{ #category : 'private - parsing' }
ExtractToTemporary class >> methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor [
	"Finds the appropriate block node to define a variable that will reference the code in the interval to extract.
	The possibles results are the top-level methodNode or a block node inside some of the method statements."

	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode
			and: [ parseNode ~= methodNodeToRefactor block ]
			and: [ sourceRanges anySatisfy: [ :sourceRange |
					sourceRange first < anIntervalToExtract first and: [ sourceRange last > anIntervalToExtract last ] ] ])
				ifTrue: [ ^ parseNode ]
		].
	^ methodNodeToRefactor
]

{ #category : 'instance creation' }
ExtractToTemporary class >> named: aNewVariable at: anIntervalToExtract from: aMethodToRefactor [

	|  trimmedNewVariable trimmedIntervalToExtract codeNodeForNewVariable methodNodeToRefactor |

	self assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract.
	methodNodeToRefactor := aMethodToRefactor notOptimizedMethodNode.
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToRefactor sourceCode.
	codeNodeForNewVariable := self methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: codeNodeForNewVariable of: methodNodeToRefactor.

	^ self new
		initializeNamed: trimmedNewVariable
		extractingCodeAt: trimmedIntervalToExtract
		from: aMethodToRefactor
		methodNode: methodNodeToRefactor
		declaringTempIn: codeNodeForNewVariable
]

{ #category : 'private - preconditions' }
ExtractToTemporary class >> newTemporaryPreconditionClass [

	^ NewTemporaryPrecondition
]

{ #category : 'exceptions' }
ExtractToTemporary class >> signalOutOfBoundsIntervalError [

	self refactoringError: self errorMessageForSourceCodeSelectionOutOfBounds
]

{ #category : 'exceptions' }
ExtractToTemporary class >> signalSourceCodeToExtractCanNotBeEmpty [

	self refactoringError: self errorMessageForSourceCodeToExtractCanNotBeEmpty
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> addAssignmentToCurrentStatement [

	self
		insertAt: intervalToExtract first
		newCodeWith: newVariableName , ' ' , self preferredAssignmentOperator , ' '
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> addNewTemporaryVariableToExistingDeclarationStatement [

	self
		insertAt: self positionToInsertNewVariableDeclaration
		newCodeWith: ' ' , newVariableName
]

{ #category : 'applying' }
ExtractToTemporary >> apply [

	self
		resolveNewVariableAssignment;
		declareNewTemporaryVariable;
		reflectSourceCodeChanges.
	^ updatedSourceCode
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> declareNewTemporaryVariable [

	self hasTemporariesDeclarationBlock
		ifTrue: [ self addNewTemporaryVariableToExistingDeclarationStatement ]
		ifFalse: [ self insertNewTemporaryDeclarationWithNewVariable ]
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> extractingAnEntireStatement [

	^self siblingStatementsOfTemporaryAssignment anySatisfy: [ :statement | self isExtractingHole: statement].

]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> formattedNewVariableAssignment [

	| newVariableAssignment |
	newVariableAssignment := newVariableName , ' ' , self preferredAssignmentOperator , ' ' , sourceCodeToExtract , '.'.
	^ newVariableAssignment , String newLineString , String tab
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> hasTemporariesDeclarationBlock [

	^ parseNodeWithNewVariableScope temporariesDeclaration declarationWritten
]

{ #category : 'initialization' }
ExtractToTemporary >> initializeNamed: aNewVariable extractingCodeAt: anIntervalToExtract from: aMethodToRefactor methodNode: aMethodNodeToRefactor declaringTempIn: blockNodeOfNewVariable [

	newVariableName := aNewVariable.
	intervalToExtract := anIntervalToExtract.
	methodToRefactor := aMethodToRefactor.
	methodNodeToRefactor := aMethodNodeToRefactor.
	updatedSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := updatedSourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	parseNodeWithNewVariableScope := blockNodeOfNewVariable.
	completeSourceRanges := methodNodeToRefactor completeSourceRanges 
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> insertAt: aPositionInSourceCode newCodeWith: sourceCodeContents [

	updatedSourceCode := updatedSourceCode
		copyReplaceFrom: aPositionInSourceCode
		to: aPositionInSourceCode - 1
		with: sourceCodeContents
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> insertNewTemporaryDeclarationWithNewVariable [

	| newVariableDeclaration positionToInsertTempVarDeclaration sourceRangeOfFirstStatement firstStatement |
	
	firstStatement := self siblingStatementsOfTemporaryAssignment first.
	sourceRangeOfFirstStatement := (completeSourceRanges at: firstStatement ifAbsent: [self shouldNotHappen]) first.
	positionToInsertTempVarDeclaration := sourceRangeOfFirstStatement first.
	newVariableDeclaration := '| ' , newVariableName , ' |' , String newLineString , String tab.

	self insertAt: positionToInsertTempVarDeclaration newCodeWith: newVariableDeclaration
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> isExtractingHole: statement [

	| ranges |
		
	ranges := completeSourceRanges at: statement ifAbsent: [^false].
	^ranges anySatisfy: [ :range | range = intervalToExtract ]
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> positionToInsertNewTemporaryVariableAssignment [

	| siblings |
	
	siblings := self siblingStatementsOfTemporaryAssignment.
	siblings do: [ :sibling |
		(completeSourceRanges at: sibling) 
			detect: [ :range | range includesAllOf: intervalToExtract ]
			ifFound: [ :range | ^range first ]
			ifNone: nil].
		
	self shouldNotHappenBecause: 'sibling should be part of completeSourceRanges'
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> positionToInsertNewVariableDeclaration [

	| lastSeparatorIndex |
	lastSeparatorIndex := self temporariesDeclarationInterval last.
	
	^ (self updatedSourceCodeHasSpaceAt: lastSeparatorIndex - 1)
		ifTrue: [ lastSeparatorIndex - 1 ]
		ifFalse: [ lastSeparatorIndex ]
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> preferredAssignmentOperator [

	^ ':='
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> reflectSourceCodeChanges [

	methodToRefactor methodClass
		compile: updatedSourceCode
		classified: methodToRefactor category
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> replaceExtractedCodeWithNewTemporaryVariable [

	updatedSourceCode := updatedSourceCode
		copyReplaceFrom: intervalToExtract first
		to: intervalToExtract last
		with: newVariableName
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> resolveNewVariableAssignment [

	self extractingAnEntireStatement
		ifTrue: [ self addAssignmentToCurrentStatement ]
		ifFalse: [
			self 
				replaceExtractedCodeWithNewTemporaryVariable;
				writeAssignmentStatementOfNewTemporaryVariable ]
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> siblingStatementsOfTemporaryAssignment [

	^ parseNodeWithNewVariableScope statements
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> temporariesDeclarationInterval [

	^ (completeSourceRanges 
		at: parseNodeWithNewVariableScope temporariesDeclaration
		ifAbsent: [self shouldNotHappen]) first
]

{ #category : 'private - applying steps' }
ExtractToTemporary >> updatedSourceCodeHasSpaceAt: lastSeparatorIndex [

	^ (updatedSourceCode at: lastSeparatorIndex ifAbsent: [ self shouldNotHappen ]) = Character space
]

{ #category : 'private - applying steps ' }
ExtractToTemporary >> writeAssignmentStatementOfNewTemporaryVariable [

	self
		insertAt: self positionToInsertNewTemporaryVariableAssignment
		newCodeWith: self formattedNewVariableAssignment
]
