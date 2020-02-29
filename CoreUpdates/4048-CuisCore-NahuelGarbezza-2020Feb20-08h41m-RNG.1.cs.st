'From Cuis 5.0 [latest update: #4045] on 24 February 2020 at 11:53:19 pm'!
"Change Set:		4046-CuisCore-AuthorName-2020Feb20-08h41m
Date:			24 February 2020
Author:			Nahuel Garbezza

Refactorings and additions on Refactorings package:

* Cleanups on extract method helpers
* Extract new temporary validations to a separate class (helpful for the upcoming ExtractToTemporary)
* Add some class comments"!


!ArgumentDeclarationCounter commentStamp: '<historical>' prior: 0!
I am responsible for counting the times an argument name appears in different block nodes across a method node.!


!ExtractMethodExpressionValidation commentStamp: '<historical>' prior: 0!
I check if an expression selected for extract method can be actually extracted.

Many conditions have to happen:
* we are selecting from the beginning of an AST node to an end of an AST node
* it is a smalltalk expression, a single statement or a sequence of complete statements
* the expression does not contain returns, or temporary variable assignments!


!Refactoring commentStamp: 'RNG 2/24/2020 23:36:38' prior: 0!
I am a refactoring, a code transformation preserving behavior, based on some input (provided from the end user through a RefactoringApplier; or provided programmatically). Instances of me have usually only public method, #apply, which does all the work.

In case the refactoring cannot be made, or there is a problem during the application of it, I can throw errors using the class message #refactoringError:, or warnings using the class message #refactoringWarning:!


!AddInstanceVariable commentStamp: 'RNG 2/24/2020 23:37:30' prior: 0!
I can add a new instance variable to a class. Input parameters are:

* name of the new variable
* class to add that variable!


!ChangeSelector commentStamp: 'RNG 2/24/2020 23:38:29' prior: 0!
I am a refactoring abstract class whose purpose is to change a given selector; either by renaming it or changing arguments (adding, removing, change order)!


!AddParameter commentStamp: 'RNG 2/24/2020 23:43:14' prior: 0!
I am a refactoring that adds a new parameter to a given selector (that has to be a unary or keyword). The input is the following:

* the new parameter name
* the selector that is going to be modified
* the position of the new parameter in the selector
* the keyword for the new parameter
* the default value for senders of this message
* the collection of implementors affected by the change
* the collection of senders affected by the change!


!ExtractMethod commentStamp: 'RNG 2/24/2020 23:48:02' prior: 0!
I am a refactoring that extracts a selected piece of code to a separate method. The input is the following:

* interval of code to extract (from index - to index)
* the CompiledMethod where this change applies
* the new method selector + argument names (instance of Message)
* the category name for the new method

Many conditions have to be satisfied for this refactoring to be made, I delegate into ExtractMethodExpressionValidation and ExtractMethodNewSelectorPrecondition some of these checks. Refer to the class comment of those classes for more information.!

!classDefinition: #NewTemporaryPrecondition category: #'Tools-Refactoring'!
RefactoringPrecondition subclass: #NewTemporaryPrecondition
	instanceVariableNames: 'newTemporaryVariableName methodNode blockNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!NewTemporaryPrecondition commentStamp: '<historical>' prior: 0!
I am responsible for checking if a new temporary variable can be introduced in a specific block node of a method. If that is not possible, I raise a refactoring error.!


!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'RNG 2/23/2020 20:55:30'!
newTemporaryPreconditionClass

	^ NewTemporaryPrecondition! !


!RefactoringPrecondition methodsFor: 'evaluating' stamp: 'RNG 2/24/2020 23:51:09'!
value

	self subclassResponsibility! !


!NewTemporaryPrecondition methodsFor: 'private' stamp: 'RNG 2/23/2020 21:36:35'!
isDeclaredInAnyOf: someBlockOrMethodNodes

	^ someBlockOrMethodNodes anySatisfy: [ :node | node  hasLocallyArgumentOrTemporaryNamed: newTemporaryVariableName ]! !

!NewTemporaryPrecondition methodsFor: 'private' stamp: 'RNG 2/23/2020 21:33:07'!
isDeclaredInChildrenOfBlockNode

	blockNode nodesDo: [ :node |
		(node isBlockNode and: [ node hasLocallyArgumentOrTemporaryNamed: newTemporaryVariableName ]) ifTrue: [ ^ true ] ].

	^ false! !

!NewTemporaryPrecondition methodsFor: 'private' stamp: 'RNG 2/23/2020 21:35:53'!
isDeclaredInParentsOfBlockNode

	| parents |
	parents _ (BlockNodeParentsFinder for: blockNode) parentsIn: methodNode.
	parents add: methodNode.
	^ self isDeclaredInAnyOf: parents! !

!NewTemporaryPrecondition methodsFor: 'evaluating' stamp: 'RNG 2/23/2020 21:33:58'!
value

	self
		assertIsNotEmpty;
		assertIsValidVariableName;
		assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass;
		assertIsNotDeclaredInParentsOrChildrenScopes! !

!NewTemporaryPrecondition methodsFor: 'initialization' stamp: 'RNG 2/23/2020 21:41:48'!
initializeFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode

	newTemporaryVariableName _ aNewTemporaryVariableName.
	blockNode _ aBlockNode.
	methodNode _ aMethodNode! !

!NewTemporaryPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 2/23/2020 21:33:58'!
assertIsNotDeclaredInParentsOrChildrenScopes

	(self isDeclaredInChildrenOfBlockNode or: [ self isDeclaredInParentsOfBlockNode ])
		ifTrue: [ self signalNewTemporaryVariableisAlreadyDefined ]! !

!NewTemporaryPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 2/23/2020 20:36:51'!
assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass

	| classDefiningNewVariable |
	
	classDefiningNewVariable _ methodNode methodClass
		whichClassDefinesInstanceVariable: newTemporaryVariableName ifNone: [ ^ self ].
		
	self signalNewVariableCanNotHideInstanceVariableDefinedIn: classDefiningNewVariable! !

!NewTemporaryPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 2/22/2020 22:15:22'!
assertIsNotEmpty

	newTemporaryVariableName isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty ]! !

!NewTemporaryPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 2/23/2020 20:32:31'!
assertIsValidVariableName

	| scannedNames |
	scannedNames _ Scanner new scanFieldNames: newTemporaryVariableName.
	scannedNames size = 1 ifFalse: [ self signalInvalidTemporaryVariable ].
	scannedNames first = newTemporaryVariableName ifFalse: [ self signalInvalidTemporaryVariable ].! !

!NewTemporaryPrecondition methodsFor: 'pre-conditions' stamp: 'RNG 2/23/2020 21:38:31'!
signalNewTemporaryVariableisAlreadyDefined

	self refactoringError: (
		self class
			errorMessageForNewTemporaryVariable: newTemporaryVariableName
			isAlreadyDefinedIn: methodNode)! !

!NewTemporaryPrecondition methodsFor: 'exceptions' stamp: 'RNG 2/23/2020 20:39:23'!
signalInvalidTemporaryVariable

	self refactoringError: (self class errorMessageForInvalidTemporaryVariable: newTemporaryVariableName)! !

!NewTemporaryPrecondition methodsFor: 'exceptions' stamp: 'RNG 2/23/2020 20:23:57'!
signalNewVariableCanNotBeEmpty
	
	self refactoringError: self class errorMessageForEmptyTemporaryVariable! !

!NewTemporaryPrecondition methodsFor: 'exceptions' stamp: 'RNG 2/23/2020 20:39:02'!
signalNewVariableCanNotHideInstanceVariableDefinedIn: aClass

	self refactoringError: (
		self class
			errorMessageFor: newTemporaryVariableName
			canNotBeNamedAsInstanceVariableDefinedIn: aClass)! !


!NewTemporaryPrecondition class methodsFor: 'evaluating' stamp: 'RNG 2/23/2020 21:42:24'!
valueFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode

	^ (self for: aNewTemporaryVariableName in: aBlockNode of: aMethodNode) value! !

!NewTemporaryPrecondition class methodsFor: 'instance creation' stamp: 'RNG 2/23/2020 21:41:24'!
for: aNewTemporaryVariableName in: aBlockNode of: aMethodNode

	^ self new initializeFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode! !

!NewTemporaryPrecondition class methodsFor: 'error messages' stamp: 'RNG 2/23/2020 20:39:50'!
errorMessageFor: aNewVariable canNotBeNamedAsInstanceVariableDefinedIn: aClass

	^ aNewVariable, ' can not be named as instance variable defined in ', aClass name! !

!NewTemporaryPrecondition class methodsFor: 'error messages' stamp: 'RNG 2/23/2020 20:23:44'!
errorMessageForEmptyTemporaryVariable
	
	^ 'New variable can not be empty'! !

!NewTemporaryPrecondition class methodsFor: 'error messages' stamp: 'RNG 2/22/2020 22:21:57'!
errorMessageForInvalidTemporaryVariable: aName

	^ '''' , aName , ''' is not a valid temporary variable name'! !

!NewTemporaryPrecondition class methodsFor: 'error messages' stamp: 'RNG 2/23/2020 21:38:55'!
errorMessageForNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode

	^ aNewVariable , ' is already defined in ' , aMethodNode classAndSelector! !


!Encoder methodsFor: 'source mapping' stamp: 'RNG 2/20/2020 08:41:40'!
parseNodesPathAt: aPosition using: completeSourceRanges ifAbsent: aBlockClosure

	| nodesWithRangeAtPosition |
	nodesWithRangeAtPosition _ SortedCollection sortBlock: [ :nodePathOne :nodePathTwo |
		nodePathOne value first >= nodePathTwo value first and: [
			nodePathOne value last <= nodePathTwo value last ] ].

	completeSourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange |
			nodesWithRangeAtPosition add: (nodeAtRange key -> aRange) ] ].

	^ nodesWithRangeAtPosition
		ifEmpty: aBlockClosure
		ifNotEmpty: [ nodesWithRangeAtPosition ]! !


!MessageNode methodsFor: 'source ranges' stamp: 'RNG 2/22/2020 20:42:15'!
expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	receiverExpandedRanges _ self isCascade
		ifTrue: [ self flag: #RNG . aSourceRange ] "fix source ranges for cascade messages"
		ifFalse: [ receiver expandRanges: (self receiverSourceRangesFrom: sourceRanges) basedOn: sourceRanges using: sourceCode ].
	expandedRangeWithReceiver _ self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ super
		expandRanges: expandedRangeWithReceiver
		basedOn: expandedRangeWithReceiver
		using: sourceCode! !


!RenameTemporary class methodsFor: 'instance creation' stamp: 'RNG 2/23/2020 21:46:34'!
fromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode

	|  trimmedNewVariable blockNode |

	trimmedNewVariable := aNewVariable withBlanksTrimmed.

	self assertIsArgOrTempNode: anOldVariableNode.
	self assert: anOldVariableNode isPartOf: aMethodNode.
	
	blockNode _ self blockNodeDeclaringTempNode: anOldVariableNode in: aMethodNode.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: blockNode of: aMethodNode.

	^ self new initializeFromOldVariableNode: anOldVariableNode to: trimmedNewVariable in: aMethodNode
! !

!methodRemoval: RenameTemporary class #errorMessageForInvalidTemporaryVariable: stamp: 'RNG 2/23/2020 20:58:09'!
RenameTemporary class removeSelector: #errorMessageForInvalidTemporaryVariable:!
!methodRemoval: RenameTemporary class #assertIsValidVariableName: stamp: 'RNG 2/23/2020 20:58:57'!
RenameTemporary class removeSelector: #assertIsValidVariableName:!
!methodRemoval: RenameTemporary class #assertIsNotEmpty: stamp: 'RNG 2/23/2020 20:57:14'!
RenameTemporary class removeSelector: #assertIsNotEmpty:!
!methodRemoval: RenameTemporary class #signalNewVariableCanNotBeEmpty stamp: 'RNG 2/23/2020 20:57:05'!
RenameTemporary class removeSelector: #signalNewVariableCanNotBeEmpty!
!methodRemoval: RenameTemporary class #assert:isNotDeclaredInParseTreeBranchOfNodeDeclaring:in: stamp: 'RNG 2/23/2020 21:44:04'!
RenameTemporary class removeSelector: #assert:isNotDeclaredInParseTreeBranchOfNodeDeclaring:in:!
!methodRemoval: RenameTemporary class #signalNewTemporaryVariable:isAlreadyDefinedIn: stamp: 'RNG 2/23/2020 21:44:22'!
RenameTemporary class removeSelector: #signalNewTemporaryVariable:isAlreadyDefinedIn:!
!methodRemoval: RenameTemporary class #is:declaredInParentsOf:in: stamp: 'RNG 2/23/2020 21:46:10'!
RenameTemporary class removeSelector: #is:declaredInParentsOf:in:!
!methodRemoval: RenameTemporary class #signal:canNotHideInstanceVariableDefinedIn: stamp: 'RNG 2/23/2020 21:08:55'!
RenameTemporary class removeSelector: #signal:canNotHideInstanceVariableDefinedIn:!
!methodRemoval: RenameTemporary class #is:declaredInChildrenOf: stamp: 'RNG 2/23/2020 21:46:16'!
RenameTemporary class removeSelector: #is:declaredInChildrenOf:!
!methodRemoval: RenameTemporary class #errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: stamp: 'RNG 2/23/2020 21:08:44'!
RenameTemporary class removeSelector: #errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn:!
!methodRemoval: RenameTemporary class #any:declaresTempNamed: stamp: 'RNG 2/23/2020 21:46:48'!
RenameTemporary class removeSelector: #any:declaresTempNamed:!
!methodRemoval: RenameTemporary class #assert:isNotDefinedAsInstanceVariableInHierarchyOf: stamp: 'RNG 2/23/2020 21:09:02'!
RenameTemporary class removeSelector: #assert:isNotDefinedAsInstanceVariableInHierarchyOf:!
!methodRemoval: RenameTemporary class #signalInvalidTemporaryVariable: stamp: 'RNG 2/23/2020 20:58:16'!
RenameTemporary class removeSelector: #signalInvalidTemporaryVariable:!
!methodRemoval: RenameTemporary class #newVariableCanNotBeEmptyErrorMessage stamp: 'RNG 2/23/2020 20:56:50'!
RenameTemporary class removeSelector: #newVariableCanNotBeEmptyErrorMessage!
!methodRemoval: RenameTemporary class #errorMessageForNewTemporaryVariable:isAlreadyDefinedIn: stamp: 'RNG 2/23/2020 21:45:13'!
RenameTemporary class removeSelector: #errorMessageForNewTemporaryVariable:isAlreadyDefinedIn:!

!RenameTemporary class reorganize!
('pre-conditions' assert:isDefinedIn: assert:isPartOf: assertIsArgOrTempNode: blockNodeDeclaringTempNode:in: newTemporaryPreconditionClass)
('exceptions' errorMessageForTemporaryVariable:notDefinedIn: oldVariableNodeMustBeArgOrTempNodeErrorDescription oldVariableNodeNotPartOfMethodNodeErrorDescription signalOldVariableNodeMustBeArgOrTempNodeErrorDescription signalOldVariableNodeNotPartOfMethodNode signalTemporaryVariable:notDefinedIn:)
('instance creation' fromOldVariableNode:to:in:)
('instance creation - private' from:to:in:)
!


!RefactoringPrecondition reorganize!
('exceptions' refactoringError:)
('evaluating' value)
('warnings' refactoringWarning:)
!


!NewTemporaryPrecondition reorganize!
('private' isDeclaredInAnyOf: isDeclaredInChildrenOfBlockNode isDeclaredInParentsOfBlockNode)
('evaluating' value)
('initialization' initializeFor:in:of:)
('pre-conditions' assertIsNotDeclaredInParentsOrChildrenScopes assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass assertIsNotEmpty assertIsValidVariableName signalNewTemporaryVariableisAlreadyDefined)
('exceptions' signalInvalidTemporaryVariable signalNewVariableCanNotBeEmpty signalNewVariableCanNotHideInstanceVariableDefinedIn:)
!


!NewTemporaryPrecondition class reorganize!
('evaluating' valueFor:in:of:)
('instance creation' for:in:of:)
('error messages' errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: errorMessageForEmptyTemporaryVariable errorMessageForInvalidTemporaryVariable: errorMessageForNewTemporaryVariable:isAlreadyDefinedIn:)
!

