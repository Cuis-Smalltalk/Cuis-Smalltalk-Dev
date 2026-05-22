Class {
	#name : 'ParseNodesDeclaringTemporaryVariableVisitor',
	#superclass : 'ParseNodeVisitor',
	#instVars : [
		'temporaryVariable'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ParseNodesDeclaringTemporaryVariableVisitor class >> for: aTemporaryVariable [

	^self new initializeFor: aTemporaryVariable.
]

{ #category : 'initialization' }
ParseNodesDeclaringTemporaryVariableVisitor >> initializeFor: aTemporaryVariable [

	temporaryVariable := aTemporaryVariable.

]

{ #category : 'visiting' }
ParseNodesDeclaringTemporaryVariableVisitor >> isNodeDeclaringTemporary: aParseNode [

	^aParseNode hasLocallyTemporaryNamed: temporaryVariable 
	
]

{ #category : 'visiting' }
ParseNodesDeclaringTemporaryVariableVisitor >> visitBlockNode: aBlockNode [

	(self isNodeDeclaringTemporary: aBlockNode) ifTrue: [ 
		self visitBlockNodeDeclaringTemporary: aBlockNode ].
	
	super visitBlockNode: aBlockNode.
]

{ #category : 'visiting' }
ParseNodesDeclaringTemporaryVariableVisitor >> visitBlockNodeDeclaringTemporary: aBlockNode [

	self subclassResponsibility.
]

{ #category : 'visiting' }
ParseNodesDeclaringTemporaryVariableVisitor >> visitMethodNode: aMethodNode [

	(self isNodeDeclaringTemporary: aMethodNode) ifTrue: [
		self visitMethodNodeDeclaringTemporary: aMethodNode ].
	
	super visitMethodNode: aMethodNode.
]

{ #category : 'visiting' }
ParseNodesDeclaringTemporaryVariableVisitor >> visitMethodNodeDeclaringTemporary: aMethodNode [

	self subclassResponsibility.
]
