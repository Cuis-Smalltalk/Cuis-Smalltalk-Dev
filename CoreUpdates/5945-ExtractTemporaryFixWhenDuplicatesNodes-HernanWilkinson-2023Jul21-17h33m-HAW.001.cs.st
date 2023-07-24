'From Cuis 6.0 [latest update: #5943] on 24 July 2023 at 3:46:59 pm'!
!classDefinition: #ExtractToTemporary category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractToTemporary
	instanceVariableNames: 'newVariableName methodNodeToRefactor methodToRefactor updatedSourceCode intervalToExtract sourceCodeToExtract parseNodeWithNewVariableScope completeSourceRanges '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractToTemporary methodsFor: 'initialization' stamp: 'HAW 7/24/2023 15:27:55'!
initializeNamed: aNewVariable extractingCodeAt: anIntervalToExtract from: aMethodToRefactor methodNode: aMethodNodeToRefactor declaringTempIn: blockNodeOfNewVariable

	newVariableName := aNewVariable.
	intervalToExtract := anIntervalToExtract.
	methodToRefactor := aMethodToRefactor.
	methodNodeToRefactor := aMethodNodeToRefactor.
	updatedSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := updatedSourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	parseNodeWithNewVariableScope := blockNodeOfNewVariable.
	completeSourceRanges := methodNodeToRefactor completeSourceRanges ! !

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/24/2023 15:41:18'!
isExtractingHole: statement

	| ranges |
		
	ranges := completeSourceRanges at: statement ifAbsent: [self shouldNotHappen].
	^ranges anySatisfy: [ :range | range = intervalToExtract ]! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 7/24/2023 12:34:05'!
completeSourceRangesOf: requestedParseNode ifAbsent: emptySourceRangesBlock
	"Returns the source ranges associated with the requested parse node."

	| completeSourceRanges |

	completeSourceRanges := Set new.

	"There may be more than one equivalent key in the complete source ranges map
	if more than one block declare a temporary variable with the same name - Fernando"
	self completeSourceRangesDo: [:parseNode :sourceRanges |
		(parseNode equivalentTo: requestedParseNode) ifTrue: [completeSourceRanges addAll: sourceRanges]].

	^completeSourceRanges ifEmpty: emptySourceRangesBlock 
	! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/24/2023 15:41:08'!
extractingAnEntireStatement

	^self siblingStatementsOfTemporaryAssignment anySatisfy: [ :statement | self isExtractingHole: statement].
! !

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/24/2023 15:41:57'!
insertNewTemporaryDeclarationWithNewVariable

	| newVariableDeclaration positionToInsertTempVarDeclaration sourceRangeOfFirstStatement firstStatement |
	
	firstStatement := self siblingStatementsOfTemporaryAssignment first.
	sourceRangeOfFirstStatement := (completeSourceRanges at: firstStatement ifAbsent: [self shouldNotHappen]) first.
	positionToInsertTempVarDeclaration := sourceRangeOfFirstStatement first.
	newVariableDeclaration := '| ' , newVariableName , ' |' , String newLineString , String tab.

	self insertAt: positionToInsertTempVarDeclaration newCodeWith: newVariableDeclaration! !

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/24/2023 15:36:18'!
positionToInsertNewTemporaryVariableAssignment

	| siblings |
	
	siblings := self siblingStatementsOfTemporaryAssignment.
	siblings do: [ :sibling |
		(completeSourceRanges at: sibling) 
			detect: [ :range | range includesAllOf: intervalToExtract ]
			ifFound: [ :range | ^range first ]
			ifNone: []].
		
	self shouldNotHappenBecause: 'sibling should be part of completeSourceRanges'! !

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/24/2023 15:43:31'!
positionToInsertNewVariableDeclaration

	^(completeSourceRanges 
		at: parseNodeWithNewVariableScope temporariesDeclaration
		ifAbsent: [self shouldNotHappen]) first last - 1! !

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/21/2023 18:06:29'!
resolveNewVariableAssignment

	self extractingAnEntireStatement
		ifTrue: [ self addAssignmentToCurrentStatement ]
		ifFalse: [
			self 
				replaceExtractedCodeWithNewTemporaryVariable;
				writeAssignmentStatementOfNewTemporaryVariable ]! !

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 7/24/2023 15:17:46'!
siblingStatementsOfTemporaryAssignment

	^ parseNodeWithNewVariableScope statements! !


!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'HAW 7/24/2023 12:50:11'!
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
		methodNode: methodNodeToRefactor 
		declaringTempIn: codeNodeForNewVariable! !

!methodRemoval: ExtractToTemporary #initializeNamed:extractingCodeAt:from:declaringTempIn: stamp: 'HAW 7/24/2023 12:49:14'!
ExtractToTemporary removeSelector: #initializeNamed:extractingCodeAt:from:declaringTempIn:!
!methodRemoval: ExtractToTemporary #statementNodeIncludingCodeToExtract stamp: 'HAW 7/24/2023 15:15:31'!
ExtractToTemporary removeSelector: #statementNodeIncludingCodeToExtract!
!classDefinition: #ExtractToTemporary category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractToTemporary
	instanceVariableNames: 'newVariableName methodNodeToRefactor methodToRefactor updatedSourceCode intervalToExtract sourceCodeToExtract parseNodeWithNewVariableScope completeSourceRanges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractToTemporary reorganize!
('applying' apply)
('initialization' initializeNamed:extractingCodeAt:from:methodNode:declaringTempIn:)
('private - applying steps ' addAssignmentToCurrentStatement addNewTemporaryVariableToExistingDeclarationStatement declareNewTemporaryVariable formattedNewVariableAssignment hasTemporariesDeclarationBlock insertAt:newCodeWith: reflectSourceCodeChanges replaceExtractedCodeWithNewTemporaryVariable writeAssignmentStatementOfNewTemporaryVariable)
('private - applying steps' extractingAnEntireStatement insertNewTemporaryDeclarationWithNewVariable isExtractingHole: positionToInsertNewTemporaryVariableAssignment positionToInsertNewVariableDeclaration preferredAssignmentOperator resolveNewVariableAssignment siblingStatementsOfTemporaryAssignment)
!


!ExtractToTemporary class reorganize!
('error messages' errorMessageForSourceCodeSelectionOutOfBounds errorMessageForSourceCodeToExtractCanNotBeEmpty)
('exceptions' signalOutOfBoundsIntervalError signalSourceCodeToExtractCanNotBeEmpty)
('instance creation' named:at:from:)
('preconditions' assertCanApplyRefactoringOn:at:)
('private - parsing' is:withinBoundsOf: methodOrBlockNodeIncluding:in:)
('private - preconditions' assert:isValidIntervalOn: assertSourceCodeIsNotEmpty: newTemporaryPreconditionClass)
!

