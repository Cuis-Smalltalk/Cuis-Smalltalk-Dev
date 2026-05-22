Class {
	#name : 'WeakMessageSendTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Objects',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Objects'
}

{ #category : 'testing' }
WeakMessageSendTest >> testHasArgumentGarbageWhenOneArgumentIsNil [

	| message |
	
	message := WeakMessageSend receiver: self selector: #printOn: arguments: #(1).
	message arguments at: 1 put: nil.
	
	self assert: message isAnyArgumentGarbage
]

{ #category : 'testing' }
WeakMessageSendTest >> testHasNoArgumentGarbageWhenNoArguments [

	| message |
	
	message := WeakMessageSend receiver: self selector: #printString.
	
	self deny: message isAnyArgumentGarbage
]

{ #category : 'testing' }
WeakMessageSendTest >> testHasNoArgumentGarbageWhenNoNilArgument [

	| message |
	
	message := WeakMessageSend receiver: self selector: #printOn: arguments: #(1).
	
	self deny: message isAnyArgumentGarbage
]

{ #category : 'testing' }
WeakMessageSendTest >> testIsMessageSend [
	self assert: (WeakMessageSend new is: #MessageSend).
]
