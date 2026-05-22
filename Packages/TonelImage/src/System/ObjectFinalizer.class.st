Class {
	#name : 'ObjectFinalizer',
	#superclass : 'Object',
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'finalization' }
ObjectFinalizer >> finalize [
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority."

	receiver perform: selector withArguments: arguments
]

{ #category : 'initialization' }
ObjectFinalizer >> receiver: aReceiver selector: aSelector [
	receiver := aReceiver.
	selector := aSelector.
	arguments := #()
]

{ #category : 'initialization' }
ObjectFinalizer >> receiver: aReceiver selector: aSelector argument: anObject [
	receiver := aReceiver.
	selector := aSelector.
	arguments := Array with: anObject
]
