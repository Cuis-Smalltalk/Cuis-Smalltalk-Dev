Class {
	#name : 'VariableNodeTest',
	#superclass : 'ParseNodeTest',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'references to self' }
VariableNodeTest >> testItDoesNotReferencesSelfWhenTheVariableNameIsNotSelf [

	| variableNode |
	variableNode _ VariableNode named: 'abc'.

	self deny: variableNode referencesSelf
]

{ #category : 'references to self' }
VariableNodeTest >> testItReferencesSelfWhenTheVariableNameIsSelf [

	| variableNode |
	variableNode _ VariableNode named: 'self'.

	self assert: variableNode referencesSelf
]
