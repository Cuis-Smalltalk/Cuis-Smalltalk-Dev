"
Signaled in case of a failed test (failure). The test framework distinguishes between failures and errors. A failure is anticipated and checked for with assertions. Errors are unanticipated problems like a division by 0 or an index out of bounds ...
"
Class {
	#name : 'TestFailure',
	#superclass : 'Exception',
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'Camp Smalltalk' }
TestFailure >> defaultAction [

	self noHandler
]

{ #category : 'Camp Smalltalk' }
TestFailure >> isResumable [
	
	^ false
]
