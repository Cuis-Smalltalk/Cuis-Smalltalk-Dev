Class {
	#name : 'MessageNodeTest',
	#superclass : 'ParseNodeTest',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'tests' }
MessageNodeTest >> cascadeMessageSend [

	1 printString; yourself
]

{ #category : 'tests' }
MessageNodeTest >> messageSend [

	1 printString
]

{ #category : 'tests' }
MessageNodeTest >> test01hasEquivalentReceiverWithWorksForAnyCombinationOfMessageSend [

	| cascadeMessageSend messageSend |
	
	messageSend := (self class >> #messageSend) methodNode block statements first.
	"I look for the MessageNode in the sourceRanges because there is where the receiver is nil and 
	#hasEquivalentReceiverWith: was not taking care of that case - Hernan"
	cascadeMessageSend := (self class >> #cascadeMessageSend) methodNode completeSourceRanges keys detect: [ :a | a isMessageNode and: [ a selector key = #printString ]].
	
	self assert: (messageSend hasEquivalentReceiverWith: cascadeMessageSend).
	self assert: (cascadeMessageSend hasEquivalentReceiverWith: messageSend).
	self assert: (cascadeMessageSend hasEquivalentReceiverWith: cascadeMessageSend).
	self assert: (messageSend hasEquivalentReceiverWith: messageSend).
	
]

{ #category : 'detecting self recursion' }
MessageNodeTest >> testItDoesNotSendTheMessageToSelfWhenTheReceiverIsNotSelf [

	| messageNode |
	messageNode _ self message: #myMethod to: 'super'.

	self deny: (messageNode sendsMessageToSelf: #myMethod)
]

{ #category : 'detecting self recursion' }
MessageNodeTest >> testItDoesNotSendTheMessageToSelfWhenTheReceiverIsSelfButTheSelectorIsDifferent [

	| messageNode |
	messageNode _ self message: #myMethod to: 'self'.

	self deny: (messageNode sendsMessageToSelf: #otherMethod)
]

{ #category : 'detecting self recursion' }
MessageNodeTest >> testItSendsTheMessageToSelfWhenTheReceiverIsSelf [

	| messageNode |
	messageNode _ self message: #myMethod to: 'self'.

	self assert: (messageNode sendsMessageToSelf: #myMethod)
]
