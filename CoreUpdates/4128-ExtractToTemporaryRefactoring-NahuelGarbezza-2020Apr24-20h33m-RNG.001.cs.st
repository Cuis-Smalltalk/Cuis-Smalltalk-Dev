'From Cuis 5.0 [latest update: #4120] on 26 April 2020 at 3:30:07 pm'!
"Change Set:		4121-CuisCore-AuthorName-2020Apr24-20h33m
Date:			26 April 2020
Author:			Nahuel Garbezza

Changes on Extract Temporary refactoring:

* make sure it is not possible to extract the left side of an assignment
* allow to extract cascade expressions
* allow to extract entire blocks into variables
* validate new temporary is not a reserved name

Changes on Extract Method refactoring:

* solve bug where 2 statements (one being a block) could not be extracted

Changes on Rename Temporary refactoring:

* validate new temporary is not a reserved name"!

!classDefinition: #ExtractToTemporary category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractToTemporary
	instanceVariableNames: 'newVariableName parseNodeToExtract methodNodeToRefactor methodToRefactor updatedSourceCode intervalToExtract sourceCodeToExtract parseNodeWithNewVariableScope '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ParseNode methodsFor: 'testing' stamp: 'RNG 4/25/2020 13:17:19'!
isCascadeNode

	^ false! !


!CascadeNode methodsFor: 'testing' stamp: 'RNG 4/25/2020 13:17:36'!
equivalentTo: aParseNode

	^ aParseNode isCascadeNode
		and: [ receiver equivalentTo: aParseNode receiver ]
		and: [ self hasEquivalentMessagesWith: aParseNode ]! !

!CascadeNode methodsFor: 'testing' stamp: 'RNG 4/25/2020 13:17:44'!
hasEquivalentMessagesWith: aCascadeNode

	messages with: aCascadeNode messages do: [ :myMessage :otherNodeMessage |
		(myMessage equivalentTo: otherNodeMessage) ifFalse: [ ^ false ] ].
	^ true! !

!CascadeNode methodsFor: 'testing' stamp: 'RNG 4/25/2020 13:17:29'!
isCascadeNode

	^ true! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/24/2020 20:42:39'!
findSourceRangeOfCloserStatementIn: listOfAncestors

	^ (listOfAncestors
		detect: [ :assoc | assoc key isBlockNode ]
		ifFound: [ :assoc | listOfAncestors before: assoc ifNone: [ listOfAncestors last ] ]
		ifNone: [ listOfAncestors last ]) value! !


!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/25/2020 13:20:06'!
assert: anIntervalToExtract enclosesACompleteExpressionOn: aMethodToRefactor

	(self anySourceRangeOf: aMethodToRefactor matches: anIntervalToExtract)
		ifFalse: [ self signalSourceCodeSelectionIncludesAnInvalidExpression ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/25/2020 13:20:22'!
assert: anIntervalToExtract enclosesAValidNodeOn: aMethodToRefactor

	(self parseNodeUnder: anIntervalToExtract canBeExtractedToAVariableOn: aMethodToRefactor)
		ifFalse: [ self signalSourceCodeSelectionIncludesAnInvalidExpression ]! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 4/25/2020 13:20:32'!
parseNodeUnder: anIntervalToExtract canBeExtractedToAVariableOn: aMethodToRefactor

	aMethodToRefactor methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isAssignmentNode
			and: [ sourceRanges anySatisfy: [ :sourceRange | sourceRange first = anIntervalToExtract first ] ])
				ifTrue: [ ^ false ]
	].
	^ true! !


!NewTemporaryPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 4/26/2020 15:16:59'!
assertIsNotAReservedName
	
	(ClassBuilder reservedNames includes: newTemporaryVariableName)
		ifTrue: [ self signalNewTemporaryVariableCanNotBeAReservedName ]! !

!NewTemporaryPrecondition methodsFor: 'exceptions' stamp: 'RNG 4/26/2020 15:19:20'!
signalNewTemporaryVariableCanNotBeAReservedName

	self refactoringError: (
		self class errorMessageForNewTemporaryVariableCanNotBeAReservedName: newTemporaryVariableName)! !


!NewTemporaryPrecondition class methodsFor: 'error messages' stamp: 'RNG 4/26/2020 15:19:20'!
errorMessageForNewTemporaryVariableCanNotBeAReservedName: aName

	^ '''', aName, ''' can not be used as temporary variable name because it is a reserved name'! !


!Encoder methodsFor: 'source mapping' stamp: 'RNG 4/25/2020 12:51:38'!
completeSourceRangesBasedOn: sourceCode

	| completeSourceRanges |
	completeSourceRanges _ Dictionary new.
	sourceRanges keysAndValuesDo: [ :parseNode :nodeRanges |
		"leaf nodes excluded because they have the same complete source ranges than the block nodes they wrap - Nahuel"
		parseNode class = LeafNode ifFalse: [
			| expandedNodeSourceRanges |
			expandedNodeSourceRanges _ parseNode expandRanges: nodeRanges basedOn: sourceRanges using: sourceCode.
			completeSourceRanges at: parseNode put: expandedNodeSourceRanges ] ].
	^ completeSourceRanges! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/25/2020 12:54:00'!
intervalMatchesBeginningOfStatement 

	^ (self findSourceRangeOfCloserStatementIn: initialNodeAncestors) first = intervalToExtract first! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 4/24/2020 20:37:59'!
intervalMatchesEndOfStatement

	^ (self findSourceRangeOfCloserStatementIn: finalNodeAncestors) last = intervalToExtract last! !


!ExtractToTemporary class methodsFor: 'preconditions' stamp: 'RNG 4/25/2020 13:21:54'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	|  trimmedSourceCodeToExtract methodNodeToExtract sourceCodeToExtract trimmedIntervalToExtract originalSourceCode |

	self assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor.
	originalSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := originalSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	trimmedIntervalToExtract := Refactoring trim: anIntervalToExtract toMatchExpressionOn: originalSourceCode.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	methodNodeToExtract := self tryToParse: trimmedSourceCodeToExtract on: aMethodToRefactor methodClass.
	self
		assertHasOneStatement: methodNodeToExtract;
		assertIsNotReturn: trimmedSourceCodeToExtract;
		assert: trimmedIntervalToExtract enclosesACompleteExpressionOn: aMethodToRefactor;
		assert: trimmedIntervalToExtract enclosesAValidNodeOn: aMethodToRefactor! !

!ExtractToTemporary class methodsFor: 'private - parsing' stamp: 'RNG 4/25/2020 12:54:15'!
methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor
	"Finds the appropriate block node to define a variable that will reference the code in the interval to extract.
	The possibles results are the top-level methodNode or a block node inside some of the method statements."

	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode
			and: [ parseNode ~= methodNodeToRefactor block ]
			and: [ sourceRanges anySatisfy: [ :sourceRange |
					sourceRange first < anIntervalToExtract first and: [ sourceRange last > anIntervalToExtract last ] ] ])
				ifTrue: [ ^ parseNode ]
		].
	^ methodNodeToRefactor! !


!NewTemporaryPrecondition methodsFor: 'evaluating' stamp: 'RNG 4/26/2020 15:14:43'!
value

	self
		assertIsNotEmpty;
		assertIsValidVariableName;
		assertIsNotAReservedName;
		assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass;
		assertIsNotDeclaredInParentsOrChildrenScopes! !

!methodRemoval: NewTemporaryPrecondition #signalNewInstanceVariableCanNotBeAReservedName stamp: 'RNG 4/26/2020 15:16:59'!
NewTemporaryPrecondition removeSelector: #signalNewInstanceVariableCanNotBeAReservedName!
!methodRemoval: ExtractToTemporary class #assert:enclosesAValidExpressionOn: stamp: 'RNG 4/25/2020 13:20:53'!
ExtractToTemporary class removeSelector: #assert:enclosesAValidExpressionOn:!
!methodRemoval: ExtractMethodExpressionValidation #findSourceRangeOfNextStatementIn: stamp: 'RNG 4/24/2020 20:37:59'!
ExtractMethodExpressionValidation removeSelector: #findSourceRangeOfNextStatementIn:!

!ExtractMethodExpressionValidation reorganize!
('validation - private' containsValidNodes findSourceRangeOfCloserStatementIn: intervalCoversCompleteAstNodes intervalMatchesBeginningOfStatement intervalMatchesEndOfStatement isLeftSideOfAssignment isNotATempDeclarationWithUsagesOutOfIntervalToExtract isNotDeclaredWithinIntervalToExtract: isNotInsideATempDeclaration isNotLeftSideOfAssignment parseNodesInCommon startAndEndNodesShareAParentNode startAndEndParseNodesAreTheSame thereAreNoLocalVariableAssignmentsWithoutDeclaration thereAreNoReturnExpressions trimmed:)
('validation' passed)
('initialization' initializeFor:of:)
!

!classDefinition: #ExtractToTemporary category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractToTemporary
	instanceVariableNames: 'newVariableName methodNodeToRefactor methodToRefactor updatedSourceCode intervalToExtract sourceCodeToExtract parseNodeWithNewVariableScope'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!NewTemporaryPrecondition reorganize!
('private' isDeclaredInAnyOf: isDeclaredInChildrenOfBlockNode isDeclaredInParentsOfBlockNode)
('evaluating' value)
('initialization' initializeFor:in:of:)
('pre-conditions' assertIsNotAReservedName assertIsNotDeclaredInParentsOrChildrenScopes assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass assertIsNotEmpty assertIsValidVariableName)
('exceptions' signalInvalidTemporaryVariable signalNewTemporaryVariableCanNotBeAReservedName signalNewTemporaryVariableisAlreadyDefined signalNewVariableCanNotBeEmpty signalNewVariableCanNotHideInstanceVariableDefinedIn:)
!

