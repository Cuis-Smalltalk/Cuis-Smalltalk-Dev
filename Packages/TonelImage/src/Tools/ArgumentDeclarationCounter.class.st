"
I am responsible for counting the times an argument name appears in different block nodes across a method node.
"
Class {
	#name : 'ArgumentDeclarationCounter',
	#superclass : 'ParseNodeVisitor',
	#instVars : [
		'argumentName',
		'counter'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ArgumentDeclarationCounter class >> for: anArgumentName [

	^self new initializeFor: anArgumentName 
]

{ #category : 'accessing' }
ArgumentDeclarationCounter >> count [

	^counter
]

{ #category : 'visiting' }
ArgumentDeclarationCounter >> declaresSoughtArgument: aParseNode [

	^aParseNode hasLocallyArgumentNamed: argumentName 
	
	
]

{ #category : 'initialization' }
ArgumentDeclarationCounter >> initializeFor: anArgumentName [

	argumentName := anArgumentName.
	counter := 0
]

{ #category : 'visiting' }
ArgumentDeclarationCounter >> visitBlockNode: aBlockNode [

	self visitPotentiallyDeclaringParseNode: aBlockNode.
	super visitBlockNode: aBlockNode
]

{ #category : 'visiting' }
ArgumentDeclarationCounter >> visitMethodNode: aMethodNode [

	self visitPotentiallyDeclaringParseNode: aMethodNode.
	super visitMethodNode: aMethodNode
]

{ #category : 'visiting' }
ArgumentDeclarationCounter >> visitPotentiallyDeclaringParseNode: aParseNode [

	(self declaresSoughtArgument: aParseNode) ifTrue: [ counter := counter + 1 ]
]
