Class {
	#name : 'RenameTemporary',
	#superclass : 'Refactoring',
	#instVars : [
		'oldVariableNode',
		'newVariable',
		'methodNode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RenameTemporary class >> assert: aVariable isDefinedIn: aMethodNode  [
	
	(aMethodNode tempNames includes: aVariable) ifFalse: [ self signalTemporaryVariable: aVariable notDefinedIn: aMethodNode ]
]

{ #category : 'pre-conditions' }
RenameTemporary class >> assert: anOldVariableNode isPartOf: aMethodNode [

	"I can not use tempNode becuase it uses scopeTable that does not have
	repeated nodes for variables with same name - Hernan"

	(aMethodNode isLocalArgumentOrTemporary: anOldVariableNode) ifTrue: [ ^self ].

	aMethodNode nodesDo: [ :aNode |
		aNode isBlockNode ifTrue: [
			(aNode isLocalArgumentOrTemporary: anOldVariableNode) ifTrue: [ ^self ]]].

	self signalOldVariableNodeNotPartOfMethodNode 
]

{ #category : 'pre-conditions' }
RenameTemporary class >> assertIsArgOrTempNode: anOldVariableNode [

	anOldVariableNode isTempOrArg ifFalse: [ self signalOldVariableNodeMustBeArgOrTempNodeErrorDescription ]
]

{ #category : 'pre-conditions' }
RenameTemporary class >> blockNodeDeclaringTempNode: aTempNode in: aMethodNode [

	| blockNode |
	
	aMethodNode nodesDo: [ :node |
		(node isBlockNode and: [ node isLocalArgumentOrTemporary: aTempNode ])
			ifTrue: [ blockNode := node ]].
	blockNode ifNil: [ blockNode := aMethodNode body ].
	
	^blockNode
]

{ #category : 'exceptions' }
RenameTemporary class >> errorMessageForTemporaryVariable: aVariable notDefinedIn: aMethodNode [

	^'Temporary variable ', aVariable, ' is not defined in ', aMethodNode classAndSelector 
]

{ #category : 'instance creation - private' }
RenameTemporary class >> from: anOldVariable to: aNewVariable in: aMethodNode [

	| oldVariableNode |

	"I keept this message for testing only, the applier now uses the one that receives the
	old variable node, that fixes the problem when renaming a temp that is in more than
	one block - Hernan"
	oldVariableNode := aMethodNode tempNodes
		detect: [ :aTempNode | aTempNode name = anOldVariable ]
		ifNone: [ self signalTemporaryVariable: anOldVariable notDefinedIn: aMethodNode ].

	^self fromOldVariableNode: oldVariableNode to: aNewVariable in: aMethodNode 
]

{ #category : 'instance creation' }
RenameTemporary class >> fromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode [

	|  trimmedNewVariable blockNode |

	trimmedNewVariable := aNewVariable withBlanksTrimmed.

	self assertIsArgOrTempNode: anOldVariableNode.
	self assert: anOldVariableNode isPartOf: aMethodNode.
	
	blockNode := self blockNodeDeclaringTempNode: anOldVariableNode in: aMethodNode.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: blockNode of: aMethodNode.

	^ self new initializeFromOldVariableNode: anOldVariableNode to: trimmedNewVariable in: aMethodNode

]

{ #category : 'pre-conditions' }
RenameTemporary class >> newTemporaryPreconditionClass [

	^ NewTemporaryPrecondition
]

{ #category : 'exceptions' }
RenameTemporary class >> oldVariableNodeMustBeArgOrTempNodeErrorDescription [

	^'Old variable node must be argument or temporary node'
]

{ #category : 'exceptions' }
RenameTemporary class >> oldVariableNodeNotPartOfMethodNodeErrorDescription [

	^'Node of variable to rename is not part of method''s method node'
]

{ #category : 'exceptions' }
RenameTemporary class >> signalOldVariableNodeMustBeArgOrTempNodeErrorDescription [

	self refactoringError: self oldVariableNodeMustBeArgOrTempNodeErrorDescription
]

{ #category : 'exceptions' }
RenameTemporary class >> signalOldVariableNodeNotPartOfMethodNode [

	self refactoringError: self oldVariableNodeNotPartOfMethodNodeErrorDescription
]

{ #category : 'exceptions' }
RenameTemporary class >> signalTemporaryVariable: aVariable notDefinedIn: aMethodNode [

	self refactoringError: (self errorMessageForTemporaryVariable: aVariable notDefinedIn: aMethodNode)
]

{ #category : 'applying' }
RenameTemporary >> apply [

	| newSource ranges |

	ranges := methodNode rangeForNode: oldVariableNode ifAbsent: [ #() ].
	newSource := methodNode sourceText copyReplacing: ranges with: newVariable.

	^ newSource
]

{ #category : 'initialization' }
RenameTemporary >> initializeFromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode [

	oldVariableNode := anOldVariableNode.
	newVariable := aNewVariable.
	methodNode := aMethodNode 
]

{ #category : 'applying' }
RenameTemporary >> methodNodeAfterApply [
	
	^methodNode methodClass methodNodeFor: self apply.
	
	
]
