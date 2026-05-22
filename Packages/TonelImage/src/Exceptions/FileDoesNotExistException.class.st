Class {
	#name : 'FileDoesNotExistException',
	#superclass : 'FileStreamException',
	#instVars : [
		'readOnly'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'examples' }
FileDoesNotExistException class >> example [
	"
	FileDoesNotExistException example
	"

	| result |
	result := [ 'error42.log' asFileEntry textContents ]
		on: FileDoesNotExistException
		do: [:ex | 'No error log'].
	Transcript show: result; newLine
]

{ #category : 'exceptionDescription' }
FileDoesNotExistException >> defaultAction [
	"The default action taken if the exception is signaled."


	^ readOnly
		ifTrue: [ UniFileStream readOnlyFileDoesNotExistUserHandling: fileName ]
		ifFalse: [ UniFileStream fileDoesNotExistUserHandling: fileName ]
]

{ #category : 'initialization' }
FileDoesNotExistException >> initialize [
	super initialize.
	readOnly := false
]

{ #category : 'accessing' }
FileDoesNotExistException >> readOnly: aBoolean [
	readOnly := aBoolean
]
