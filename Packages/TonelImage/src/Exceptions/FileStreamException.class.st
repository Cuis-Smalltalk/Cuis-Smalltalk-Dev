Class {
	#name : 'FileStreamException',
	#superclass : 'Error',
	#instVars : [
		'fileName'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'exceptionInstantiator' }
FileStreamException class >> fileName: aFileName [
	^self new fileName: aFileName
]

{ #category : 'exceptionBuilder' }
FileStreamException >> fileName: aFileName [
	fileName := aFileName
]

{ #category : 'exceptionDescription' }
FileStreamException >> isResumable [
	"Determine whether an exception is resumable."

	^true
]

{ #category : 'exceptionDescription' }
FileStreamException >> messageText [
	
	"Return an exception's message text."

	^messageText ifNil: [fileName printString]
]
