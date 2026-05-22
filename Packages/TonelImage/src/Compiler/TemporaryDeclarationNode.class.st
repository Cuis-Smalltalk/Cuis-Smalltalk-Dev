Class {
	#name : 'TemporaryDeclarationNode',
	#superclass : 'ParseNode',
	#instVars : [
		'variableNode'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'instance creation' }
TemporaryDeclarationNode class >> of: aVariableNode [

	^ self new initializeVariableNode: aVariableNode
]

{ #category : 'visiting' }
TemporaryDeclarationNode >> accept: aVisitor [

	^ aVisitor visitTemporaryDeclarationNode: self
]

{ #category : 'testing' }
TemporaryDeclarationNode >> declaresVariable: aVariableNode [

	^ variableNode isNamed: aVariableNode name
]

{ #category : 'testing' }
TemporaryDeclarationNode >> equivalentTo: aParseNode [

	^ aParseNode isTemporaryDeclaration
		and: [ self declaresVariable: aParseNode variableNode ]
]

{ #category : 'initialization' }
TemporaryDeclarationNode >> initializeVariableNode: aVariableNode [

	variableNode := aVariableNode
]

{ #category : 'testing' }
TemporaryDeclarationNode >> isTemporaryDeclaration [

	^ true
]

{ #category : 'printing' }
TemporaryDeclarationNode >> printOn: aStream indent: anInteger [

	aStream nextPut: $|; space.
	variableNode printOn: aStream indent: anInteger.
	aStream space; nextPut: $|.
]

{ #category : 'accessing' }
TemporaryDeclarationNode >> variableName [

	^ self variableNode name
]

{ #category : 'accessing' }
TemporaryDeclarationNode >> variableNode [

	^ variableNode
]
