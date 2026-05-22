Class {
	#name : 'TemporaryVariableDeclarationCounter',
	#superclass : 'ParseNodesDeclaringTemporaryVariableVisitor',
	#instVars : [
		'count'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'count' }
TemporaryVariableDeclarationCounter >> count [

	^count.
]

{ #category : 'initialization' }
TemporaryVariableDeclarationCounter >> initialize [

	count := 0.
]

{ #category : 'visiting' }
TemporaryVariableDeclarationCounter >> visitBlockNodeDeclaringTemporary: aBlockNode [

	count := count + 1.
]

{ #category : 'visiting' }
TemporaryVariableDeclarationCounter >> visitMethodNodeDeclaringTemporary: aMethodNode [

	count := count + 1.
]
