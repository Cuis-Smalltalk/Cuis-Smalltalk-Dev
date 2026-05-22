Class {
	#name : 'ReturnNodeTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'test data' }
ReturnNodeTest >> methodReferencingSelfWithImplicitReturn [

	self yourself 
	
	
]

{ #category : 'test data' }
ReturnNodeTest >> methodWithImplicitReturn [
	
	
]

{ #category : 'test data' }
ReturnNodeTest >> methodWithoutImplicitReturn [
	
	^self
]

{ #category : 'test support' }
ReturnNodeTest >> returnNodeOf: methodNode [
	
	methodNode nodesDo: [:node | node isReturn ifTrue: [^node]].
	
	self error: 'No return node found'
]

{ #category : 'tests' }
ReturnNodeTest >> test01isImplicitSelfReturnInReturnsTrueWithMethodWithOutAnySourceCode [

	| methodNode returnNode |
	
	methodNode := (self class >> #methodWithImplicitReturn) methodNode.
	returnNode := methodNode block statements first.
	
	self assert: (returnNode isImplicitSelfReturnIn: methodNode)
]

{ #category : 'tests' }
ReturnNodeTest >> test02isImplicitSelfReturnInReturnsTrueWithMethodReferencingSelf [

	| methodNode returnNode |
	
	methodNode := (self class >> #methodReferencingSelfWithImplicitReturn) methodNode.
	returnNode := self returnNodeOf: methodNode.
	
	self assert: (returnNode isImplicitSelfReturnIn: methodNode)
]

{ #category : 'tests' }
ReturnNodeTest >> test03isImplicitSelfReturnInReturnsFalseWithMethodReturningSelf [

	| methodNode returnNode |
	
	methodNode := (self class >> #methodWithoutImplicitReturn) methodNode.
	returnNode := self returnNodeOf: methodNode.
	
	self deny: (returnNode isImplicitSelfReturnIn: methodNode)
]
