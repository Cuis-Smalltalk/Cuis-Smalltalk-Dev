"
I am a node for a vector of remote temps, created to share temps between closures when those temps are written to in closures other than their defining ones.
"
Class {
	#name : 'RemoteTempVectorNode',
	#superclass : 'TempVariableNode',
	#instVars : [
		'remoteTemps',
		'readNode',
		'writeNode'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
RemoteTempVectorNode >> accept: aVisitor [
	^aVisitor visitRemoteTempVectorNode: self
]

{ #category : 'closure analysis' }
RemoteTempVectorNode >> addRemoteTemp: aTempVariableNode encoder: encoder [
	remoteTemps ifNil:
		[remoteTemps := OrderedCollection new].
	remoteTemps addLast: aTempVariableNode.
	aTempVariableNode referenceScopesAndIndicesDo:
		[:scopeBlock "<BlockNode>" :location "<Integer>"|
		 self addReadWithin: scopeBlock at: location]
]

{ #category : 'code generation' }
RemoteTempVectorNode >> emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder [
	encoder
		genStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index
]

{ #category : 'code generation' }
RemoteTempVectorNode >> emitCodeForStorePopInto: aTempVariableNode stack: stack encoder: encoder [
	encoder
		genStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index.
	stack pop: 1
]

{ #category : 'code generation' }
RemoteTempVectorNode >> emitCodeForValueOf: aTempVariableNode stack: stack encoder: encoder [
	encoder
		genPushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index.
	stack push: 1
]

{ #category : 'code generation (closures)' }
RemoteTempVectorNode >> isIndirectTempVector [
	^true
]

{ #category : 'code generation' }
RemoteTempVectorNode >> nodeToInitialize: encoder [
	^AssignmentNode new
		variable: self
		value: (NewArrayNode new numElements: remoteTemps size)
]

{ #category : 'printing' }
RemoteTempVectorNode >> printDefinitionForClosureAnalysisOn: aStream  [
	| refs |
	aStream
		nextPut: ${;
		nextPutAll: key.
	definingScope ifNotNil: [definingScope blockExtent ifNotNil: [:be| aStream nextPutAll: ' d@'; print: be first]].
	readingScopes ifNotNil: [
		refs := Set new.
		readingScopes do: [:elems| refs addAll: elems].
		refs asSortedCollection do: [:read| aStream nextPutAll: ' r@'; print: read]].
	remoteTemps
		do: [:rt| rt printDefinitionForClosureAnalysisOn: aStream]
		separatedBy: [aStream nextPut: $,; space].
	aStream nextPut: $}
]

{ #category : 'code generation (closures)' }
RemoteTempVectorNode >> referenceScopesAndIndicesDo: aBinaryBlock [
	self shouldNotImplement
]

{ #category : 'accessing' }
RemoteTempVectorNode >> remoteTemps [
	^remoteTemps
]

{ #category : 'decompiler' }
RemoteTempVectorNode >> remoteTemps: anArray [
	remoteTemps := anArray.
	anArray do: [:tempNode| tempNode remoteNode: self]
]

{ #category : 'code generation (closures)' }
RemoteTempVectorNode >> scope [
	"Answer scope of temporary variables.
	 Currently only the following distinctions are made:
		 0	outer level: args and user-declared temps
		 1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:
		-3	an indirect temp vector"
	^-3
]

{ #category : 'code generation' }
RemoteTempVectorNode >> sizeCodeForStoreInto: aTempVariableNode encoder: encoder [
	^encoder
		sizeStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index
]

{ #category : 'code generation' }
RemoteTempVectorNode >> sizeCodeForStorePopInto: aTempVariableNode encoder: encoder [
	^encoder
		sizeStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index
]

{ #category : 'code generation' }
RemoteTempVectorNode >> sizeCodeForValueOf: aTempVariableNode encoder: encoder [
	^encoder
		sizePushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
		inVectorAt: index
]
