Class {
	#name : 'ParseNodeTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'parse nodes helpers' }
ParseNodeTest >> message: aSelector to: aVariableName [

	| variableNode selectorNode |
	variableNode _ VariableNode named: aVariableName.
	selectorNode _ SelectorNode new key: aSelector code: 34.
	^ MessageNode new
		receiver: variableNode
		selector: selectorNode
		arguments: #()
		precedence: 1
]
