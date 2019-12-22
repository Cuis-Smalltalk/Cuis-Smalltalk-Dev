'From Cuis 5.0 [latest update: #3979] on 18 December 2019 at 6:58:51 pm'!

!classDefinition: #BlockNodeParentsFinder category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #BlockNodeParentsFinder
	instanceVariableNames: 'selectedBlockNode root parents found'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!BlockNodeParentsFinder commentStamp: 'EB 12/18/2019 18:55:03' prior: 0!
I find the BlockNode parents of a BlockNode in the ParseNode tree starting from a ParseNode supplied to #parentsIn:. !


!BlockNodeParentsFinder methodsFor: 'initializing' stamp: 'EB 12/18/2019 18:09:42'!
initializeFor: aSelectedBlockNode

	selectedBlockNode := aSelectedBlockNode.
	parents := OrderedCollection new.
	found := false.! !

!BlockNodeParentsFinder methodsFor: 'visiting' stamp: 'EB 12/18/2019 18:09:00'!
visitBlockNode: aBlockNode

	found ifFalse: [
		aBlockNode = selectedBlockNode
			ifTrue: [ found := true ]
			ifFalse: [
				parents add: aBlockNode.
				super visitBlockNode: aBlockNode.
				found ifFalse: [ parents removeLast ]
			]
	]


	! !

!BlockNodeParentsFinder methodsFor: 'accessing' stamp: 'EB 12/18/2019 18:13:27'!
parentsIn: aParseNode

	aParseNode accept: self.
	^parents! !


!BlockNodeParentsFinder class methodsFor: 'instance creation' stamp: 'EB 12/18/2019 18:10:08'!
for: aSelectedBlockNode

	^self new initializeFor: aSelectedBlockNode.! !


!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'EB 12/18/2019 17:01:45'!
any: someBlockOrMethodNodes declaresTempNamed: aTempName

	^someBlockOrMethodNodes anySatisfy: [ :node |
		self is: aTempName declaredIn: node ]! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'EB 12/18/2019 18:50:44'!
assert: aTempName isNotDeclaredInParseTreeBranchOfNodeDeclaring: aTempNode in: aMethodNode

	| blockNode |

	blockNode := self blockNodeDeclaringTempNode: aTempNode in: aMethodNode.
	((self is: aTempName declaredInChildrenOf: blockNode) or: [
		self is: aTempName declaredInParentsOf: blockNode in: aMethodNode ])
		ifTrue: [ self signalNewTemporaryVariable: aTempName isAlreadyDefinedIn: aMethodNode ].! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'EB 12/18/2019 18:51:30'!
blockNodeDeclaringTempNode: aTempNode in: aMethodNode

	| blockNode |

	aMethodNode nodesDo: [ :node |
		(node isBlockNode and: [ node isArgumentOrTemporary: aTempNode ])
			ifTrue: [ blockNode := node ]].
	blockNode ifNil: [ blockNode := aMethodNode body ].
	^blockNode! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'EB 12/18/2019 17:01:52'!
is: aTempName declaredIn: aBlockOrMethodNode

	^(aBlockOrMethodNode temporaries union: aBlockOrMethodNode arguments) anySatisfy: [ :tempNode |
			tempNode name = aTempName ]! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'EB 12/18/2019 18:36:52'!
is: aTempName declaredInChildrenOf: aBlockNode

	aBlockNode nodesDo: [ :node |
		(node isBlockNode and: [ self is: aTempName declaredIn: node ]) ifTrue: [^true]].
	^false! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'EB 12/18/2019 18:53:37'!
is: aTempName declaredInParentsOf: aBlockNode in: aMethodNode

	| parents |

	parents := (BlockNodeParentsFinder for: aBlockNode) parentsIn: aMethodNode.
	parents add: aMethodNode.
	^self any: parents declaresTempNamed: aTempName! !


!RenameTemporary class methodsFor: 'instance creation' stamp: 'EB 12/18/2019 18:47:12'!
fromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode

	|  trimmedNewVariable |

	trimmedNewVariable := aNewVariable withBlanksTrimmed.

	self assertIsArgOrTempNode: anOldVariableNode.
	self assert: anOldVariableNode isPartOf: aMethodNode.
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDeclaredInParseTreeBranchOfNodeDeclaring: anOldVariableNode in: aMethodNode.
	self assert: trimmedNewVariable isNotDefinedAsInstanceVariableInHierarchyOf: aMethodNode methodClass.

	^self new initializeFromOldVariableNode: anOldVariableNode to: trimmedNewVariable in: aMethodNode
! !

!methodRemoval: RenameTemporary class #assert:isNotDefinedIn: stamp: 'EB 12/18/2019 17:19:48'!
RenameTemporary class removeSelector: #assert:isNotDefinedIn:!

!BlockNodeParentsFinder reorganize!
('initializing' initializeFor:)
('visiting' visitBlockNode:)
('accessing' parentsIn:)
!


!BlockNodeParentsFinder class reorganize!
('instance creation' for:)
!


!RenameTemporary class reorganize!
('pre-conditions' any:declaresTempNamed: assert:isDefinedIn: assert:isNotDeclaredInParseTreeBranchOfNodeDeclaring:in: assert:isNotDefinedAsInstanceVariableInHierarchyOf: assert:isPartOf: assertIsArgOrTempNode: assertIsNotEmpty: assertIsValidVariableName: blockNodeDeclaringTempNode:in: is:declaredIn: is:declaredInChildrenOf: is:declaredInParentsOf:in:)
('exceptions' errorMessageForInvalidTemporaryVariable: errorMessageForNewTemporaryVariable:isAlreadyDefinedIn: errorMessageForTemporaryVariable:notDefinedIn: newVariableCanNotBeEmptyErrorMessage oldVariableNodeMustBeArgOrTempNodeErrorDescription oldVariableNodeNotPartOfMethodNodeErrorDescription signalInvalidTemporaryVariable: signalNewTemporaryVariable:isAlreadyDefinedIn: signalNewVariableCanNotBeEmpty signalOldVariableNodeMustBeArgOrTempNodeErrorDescription signalOldVariableNodeNotPartOfMethodNode signalTemporaryVariable:notDefinedIn:)
('instance creation' fromOldVariableNode:to:in:)
('warnings' errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: signal:canNotHideInstanceVariableDefinedIn:)
('instance creation - private' from:to:in:)
!
