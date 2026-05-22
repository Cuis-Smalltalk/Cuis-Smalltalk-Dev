"
OutOfMemory is signaled when an allocation fails due to not having enough memory. Its default action signals the low-space semaphore.
"
Class {
	#name : 'OutOfMemory',
	#superclass : 'Error',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'as yet unclassified' }
OutOfMemory >> defaultAction [
	Smalltalk signalLowSpace.
]

{ #category : 'as yet unclassified' }
OutOfMemory >> isResumable [
	^true
]
