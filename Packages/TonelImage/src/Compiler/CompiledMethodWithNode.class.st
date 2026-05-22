Class {
	#name : 'CompiledMethodWithNode',
	#superclass : 'Object',
	#instVars : [
		'node',
		'method'
	],
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'instance creation' }
CompiledMethodWithNode class >> generateMethodFromNode: aMethodNode trailer: bytes [
	^ self method: (aMethodNode generate: bytes) node: aMethodNode.
]

{ #category : 'instance creation' }
CompiledMethodWithNode class >> method: aCompiledMethod node: aMethodNode [
	^ self new method: aCompiledMethod; node: aMethodNode.
]

{ #category : 'accessing' }
CompiledMethodWithNode >> method [
	^ method
]

{ #category : 'private' }
CompiledMethodWithNode >> method: aCompiledMethod [
	method := aCompiledMethod
]

{ #category : 'accessing' }
CompiledMethodWithNode >> node [
	^ node
]

{ #category : 'private' }
CompiledMethodWithNode >> node: aMethodNode [
	node := aMethodNode
]

{ #category : 'accessing' }
CompiledMethodWithNode >> selector [
	^ self node selector
]
