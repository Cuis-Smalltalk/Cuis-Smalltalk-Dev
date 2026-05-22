Class {
	#name : 'FileExistsException',
	#superclass : 'FileStreamException',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'exceptionDescription' }
FileExistsException >> defaultAction [
	"The default action taken if the exception is signaled."

	^ UniFileStream fileExistsUserHandling: fileName
]
