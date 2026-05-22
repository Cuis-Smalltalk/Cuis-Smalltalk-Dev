Class {
	#name : 'InstanceVariableNode',
	#superclass : 'VariableNode',
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'visiting' }
InstanceVariableNode >> accept: aVisitor [
	^aVisitor visitInstanceVariableNode: self
]

{ #category : 'printing' }
InstanceVariableNode >> descriptiveString [

	^'Instance Variable named: ', name printString
]

{ #category : 'code generation (new scheme)' }
InstanceVariableNode >> emitCodeForStore: stack encoder: encoder [
	encoder genStoreInstVar: index
]

{ #category : 'code generation (new scheme)' }
InstanceVariableNode >> emitCodeForStorePop: stack encoder: encoder [
	encoder genStorePopInstVar: index.
	stack pop: 1
]

{ #category : 'code generation (new scheme)' }
InstanceVariableNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	^encoder genPushInstVar: index
]

{ #category : 'testing' }
InstanceVariableNode >> isInstanceVariableNode [

	^true
]

{ #category : 'initialization' }
InstanceVariableNode >> name: varName index: varIndex [
	^self name: varName index: varIndex-1 type: LdInstType
]

{ #category : 'code generation (new scheme)' }
InstanceVariableNode >> sizeCodeForStore: encoder [
	^encoder sizeStoreInstVar: index
]

{ #category : 'code generation (new scheme)' }
InstanceVariableNode >> sizeCodeForStorePop: encoder [
	^encoder sizeStorePopInstVar: index
]

{ #category : 'code generation (new scheme)' }
InstanceVariableNode >> sizeCodeForValue: encoder [
	^encoder sizePushInstVar: index
]
