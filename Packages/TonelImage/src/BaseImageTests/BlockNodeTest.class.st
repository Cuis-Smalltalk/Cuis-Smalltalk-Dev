Class {
	#name : 'BlockNodeTest',
	#superclass : 'ParseNodeTest',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'detecting self recursion' }
BlockNodeTest >> testItDoesNotSendsAGivenMessageToSelfWhenTheBlockIsEmpty [

	| anEmptyBlock |
	anEmptyBlock _ BlockNode empty.

	self deny: (anEmptyBlock sendsMessageToSelf: #myMethod)
]

{ #category : 'detecting self recursion' }
BlockNodeTest >> testItSendsAGivenMessageToSelfWhenItContainsAnStatementWithAMessageSendToSelf [

	| anEmptyBlock |
	anEmptyBlock _ BlockNode withJust: (self message: #myMethod to: 'self').

	self assert: (anEmptyBlock sendsMessageToSelf: #myMethod)
]
