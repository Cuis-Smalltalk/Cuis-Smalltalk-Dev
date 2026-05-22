Class {
	#name : 'TemporariesDeclarationNode',
	#superclass : 'ParseNode',
	#instVars : [
		'tempDeclarationNodes',
		'declarationWritten'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'instance creation' }
TemporariesDeclarationNode class >> empty [

	^ self withAll: #() declarationWritten: false
]

{ #category : 'instance creation' }
TemporariesDeclarationNode class >> withAll: tempDeclarationNodes [

	^ self new initializeWithAll: tempDeclarationNodes declarationWritten: true
]

{ #category : 'instance creation' }
TemporariesDeclarationNode class >> withAll: tempDeclarationNodes declarationWritten: aBoolean [

	^ self new initializeWithAll: tempDeclarationNodes declarationWritten: aBoolean
]

{ #category : 'visiting' }
TemporariesDeclarationNode >> accept: aVisitor [

	^ aVisitor visitTemporariesDeclarationNode: self
]

{ #category : 'accessing' }
TemporariesDeclarationNode >> allDeclaredVariableNodes [

	^ tempDeclarationNodes collect: [ :tempDeclaration | tempDeclaration variableNode ]
]

{ #category : 'testing' }
TemporariesDeclarationNode >> declarationWritten [

	^ declarationWritten
]

{ #category : 'testing' }
TemporariesDeclarationNode >> declaresAnyVariable [

	^ tempDeclarationNodes notEmpty
]

{ #category : 'testing' }
TemporariesDeclarationNode >> declaresSameVariablesThan: aTemporariesDeclarationNode [

	^self areNodesOf: tempDeclarationNodes equivalentTo: aTemporariesDeclarationNode temporaryDeclarationNodes 
]

{ #category : 'testing' }
TemporariesDeclarationNode >> declaresVariable: aVariableNode [

	^ tempDeclarationNodes anySatisfy: [ :tempDeclaration | tempDeclaration declaresVariable: aVariableNode ]
]

{ #category : 'testing' }
TemporariesDeclarationNode >> equivalentTo: aParseNode [

	^ aParseNode isTemporariesDeclaration
		and: [ self declaresSameVariablesThan: aParseNode  ]
]

{ #category : 'initialization' }
TemporariesDeclarationNode >> initializeWithAll: aCollectionOfTempDeclarationNodes [

	tempDeclarationNodes := aCollectionOfTempDeclarationNodes
]

{ #category : 'initialization' }
TemporariesDeclarationNode >> initializeWithAll: aCollectionOfTempDeclarationNodes declarationWritten: aBoolean [

	tempDeclarationNodes := aCollectionOfTempDeclarationNodes.
	declarationWritten := aBoolean
]

{ #category : 'testing' }
TemporariesDeclarationNode >> isTemporariesDeclaration [

	^ true
]

{ #category : 'printing' }
TemporariesDeclarationNode >> printEachTempVarDeclarationOn: aStream [

	^ tempDeclarationNodes do: [ :tempDeclarationNode |
		aStream nextPutAll: tempDeclarationNode variableName; space]
]

{ #category : 'printing' }
TemporariesDeclarationNode >> printOn: aStream indent: anInteger [

	aStream nextPut: $|; space.
	self printEachTempVarDeclarationOn: aStream.
	aStream nextPut: $|.
]

{ #category : 'accessing' }
TemporariesDeclarationNode >> temporaryDeclarationNodes [

	^ tempDeclarationNodes
]

{ #category : 'enumerating' }
TemporariesDeclarationNode >> temporaryDeclarationNodesDo: aBlock [

	tempDeclarationNodes do: aBlock
]

{ #category : 'initialization' }
TemporariesDeclarationNode >> withOutDeclarations: aTemporariesDeclarationToRemove [

	tempDeclarationNodes := tempDeclarationNodes reject: [ :aTempDeclarationNode | 
		aTemporariesDeclarationToRemove declaresVariable: aTempDeclarationNode variableNode ]
]
