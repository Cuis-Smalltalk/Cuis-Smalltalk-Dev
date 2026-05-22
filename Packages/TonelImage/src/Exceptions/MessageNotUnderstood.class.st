"
This exception is provided to support Object>>doesNotUnderstand:.
"
Class {
	#name : 'MessageNotUnderstood',
	#superclass : 'Error',
	#instVars : [
		'message',
		'receiver',
		'reachedDefaultHandler'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'exceptionDescription' }
MessageNotUnderstood >> defaultAction [
	reachedDefaultHandler := true.
	super defaultAction
]

{ #category : 'initialization' }
MessageNotUnderstood >> initialize [
	super initialize.
	reachedDefaultHandler := false
]

{ #category : 'exceptionDescription' }
MessageNotUnderstood >> isResumable [
	"Determine whether an exception is resumable."

	^true
]

{ #category : 'exceptionDescription' }
MessageNotUnderstood >> message [
	"Answer the selector and arguments of the message that failed."

	^message
]

{ #category : 'exceptionBuilder' }
MessageNotUnderstood >> message: aMessage [

	message := aMessage
]

{ #category : 'exceptionBuilder' }
MessageNotUnderstood >> messageText [
	"Return an exception's message text."

	^ messageText
		ifNil: [
			message
				ifNil: [super messageText]
				ifNotNil: [message lookupClass printString , '>>' , message selector asPlainString]]
]

{ #category : 'accessing' }
MessageNotUnderstood >> reachedDefaultHandler [
	^reachedDefaultHandler
]

{ #category : 'exceptionDescription' }
MessageNotUnderstood >> receiver [
	"Answer the receiver that did not understand the message"

	^ receiver
]

{ #category : 'exceptionBuilder' }
MessageNotUnderstood >> receiver: obj [

	receiver := obj
]
