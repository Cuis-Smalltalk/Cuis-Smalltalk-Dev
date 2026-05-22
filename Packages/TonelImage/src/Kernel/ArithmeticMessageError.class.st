Class {
	#name : 'ArithmeticMessageError',
	#superclass : 'Error',
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'exceptionDescription' }
ArithmeticMessageError >> defaultAction [
	(receiver isFloatOrFloatComplex or: [ arguments notNil and: [arguments anySatisfy: [ :a | a isFloatOrFloatComplex ]]]) ifTrue: [
		^self floatErrorValue ].
	^ super defaultAction
]

{ #category : 'signaling' }
ArithmeticMessageError >> floatErrorValue [
	^ self subclassResponsibility
]

{ #category : 'initialization' }
ArithmeticMessageError >> receiver: aNumber selector: aSymbol argument: otherNumber [
	self receiver: aNumber selector: aSymbol arguments: {otherNumber}
]

{ #category : 'initialization' }
ArithmeticMessageError >> receiver: aNumber selector: aSymbol arguments: aCollection [
	receiver := aNumber.
	selector := aSymbol.
	arguments := aCollection
]

{ #category : 'signaling' }
ArithmeticMessageError >> signalReceiver: aNumber selector: aSymbol argument: otherNumber [

	^self
		receiver: aNumber selector: aSymbol argument: otherNumber;
		signal
]

{ #category : 'signaling' }
ArithmeticMessageError >> signalReceiver: aNumber selector: aSymbol arguments: aCollection [

	^self
		receiver: aNumber selector: aSymbol arguments: aCollection;
		signal
]
