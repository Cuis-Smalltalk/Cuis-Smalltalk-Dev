'From Cuis 5.0 [latest update: #3984] on 22 December 2019 at 8:33:28 pm'!

!ParseNode methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:21:52'!
hasNodeIn: aCollectionOfNodes named: aName

	^aCollectionOfNodes anySatisfy: [ :tempNode | tempNode name = aName ]! !


!BlockNode methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:22:33'!
hasLocallyArgumentOrTemporaryNamed: aVariableName

	^(self hasNodeIn: self temporaries named: aVariableName)
		or: [self hasNodeIn: self arguments named: aVariableName ]! !

!BlockNode methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:30:26'!
isLocalArgumentOrTemporary: aParseNode

	^(self temporaries includes: aParseNode) or: [ self arguments includes: aParseNode ]
	! !


!MethodNode methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:28:55'!
hasLocallyArgumentOrTemporaryNamed: aVariableName

	"See #hasArgumentOrTemporaryNamed: comment - Hernan"
	^(self hasNodeIn: self temporaries named: aVariableName)
		or: [self hasNodeIn: self arguments named: aVariableName ]! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:30:47'!
isLocalArgumentOrTemporary: aParseNode

	"Looks only in this scope - Hernan"
	^(self temporaries includes: aParseNode) or: [ self arguments includes: aParseNode ]
	! !


!CompiledMethod methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:23:52'!
hasArgumentOrTemporaryNamed: aVariable

	^self methodNode hasArgumentOrTemporaryNamed: aVariable
! !


!MethodNode methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:28:34'!
hasArgumentOrTemporaryNamed: aVariableName

	"
	- hasArgumentOrTemporaryNamed: returns true if there is a temp or variable in this scope or subscopes 
	   named aVariableName
	- hasLocalName: returns true if there is a variable in the scopeTable name aVariableName. That includes 
	  temps, arguments, instance variables and pseudo-variables
	- hasLocallyArgumentOrTemporaryNamed: returns true if ony this scope defines a temp or argument 
	  named aVariableName. - Hernan" 
	^self tempNames includes: aVariableName! !

!MethodNode methodsFor: 'testing' stamp: 'HAW 12/22/2019 20:28:48'!
hasLocalNamed: aName

	"See #hasArgumentOrTemporaryNamed: comment - Hernan"
	^ encoder hasLocalNamed: aName ! !


!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 12/22/2019 20:20:02'!
any: someBlockOrMethodNodes declaresTempNamed: aTempName
	
	^someBlockOrMethodNodes anySatisfy: [ :node | node  hasLocallyArgumentOrTemporaryNamed: aTempName]! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 12/22/2019 20:30:26'!
assert: anOldVariableNode isPartOf: aMethodNode

	"I can not use tempNode becuase it uses scopeTable that does not have
	repeated nodes for variables with same name - Hernan"

	(aMethodNode isLocalArgumentOrTemporary: anOldVariableNode) ifTrue: [ ^self ].

	aMethodNode nodesDo: [ :aNode |
		aNode isBlockNode ifTrue: [
			(aNode isLocalArgumentOrTemporary: anOldVariableNode) ifTrue: [ ^self ]]].

	self signalOldVariableNodeNotPartOfMethodNode ! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 12/22/2019 20:33:00'!
blockNodeDeclaringTempNode: aTempNode in: aMethodNode

	| blockNode |
	
	aMethodNode nodesDo: [ :node |
		(node isBlockNode and: [ node isLocalArgumentOrTemporary: aTempNode ])
			ifTrue: [ blockNode := node ]].
	blockNode ifNil: [ blockNode := aMethodNode body ].
	
	^blockNode! !

!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 12/22/2019 20:20:14'!
is: aTempName declaredInChildrenOf: aBlockNode
	
	aBlockNode nodesDo: [ :node |
		(node isBlockNode and: [ node hasLocallyArgumentOrTemporaryNamed: aTempName ]) ifTrue: [^true]].
	
	^false! !

!methodRemoval: RenameTemporary class #is:declaredIn: stamp: 'HAW 12/22/2019 20:20:23'!
RenameTemporary class removeSelector: #is:declaredIn:!
!methodRemoval: MethodNode #isArgumentOrTemporary: stamp: 'HAW 12/22/2019 20:30:26'!
MethodNode removeSelector: #isArgumentOrTemporary:!
!methodRemoval: BlockNode #isArgumentOrTemporary: stamp: 'HAW 12/22/2019 20:30:26'!
BlockNode removeSelector: #isArgumentOrTemporary:!

!RenameTemporary class reorganize!
('pre-conditions' any:declaresTempNamed: assert:isDefinedIn: assert:isNotDeclaredInParseTreeBranchOfNodeDeclaring:in: assert:isNotDefinedAsInstanceVariableInHierarchyOf: assert:isPartOf: assertIsArgOrTempNode: assertIsNotEmpty: assertIsValidVariableName: blockNodeDeclaringTempNode:in: is:declaredInChildrenOf: is:declaredInParentsOf:in:)
('exceptions' errorMessageForInvalidTemporaryVariable: errorMessageForNewTemporaryVariable:isAlreadyDefinedIn: errorMessageForTemporaryVariable:notDefinedIn: newVariableCanNotBeEmptyErrorMessage oldVariableNodeMustBeArgOrTempNodeErrorDescription oldVariableNodeNotPartOfMethodNodeErrorDescription signalInvalidTemporaryVariable: signalNewTemporaryVariable:isAlreadyDefinedIn: signalNewVariableCanNotBeEmpty signalOldVariableNodeMustBeArgOrTempNodeErrorDescription signalOldVariableNodeNotPartOfMethodNode signalTemporaryVariable:notDefinedIn:)
('instance creation' fromOldVariableNode:to:in:)
('warnings' errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: signal:canNotHideInstanceVariableDefinedIn:)
('instance creation - private' from:to:in:)
!

