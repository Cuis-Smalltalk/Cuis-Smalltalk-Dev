"
A Warning is a Notification which by default should be brought to the attention of the user.
"
Class {
	#name : 'Warning',
	#superclass : 'Notification',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'exceptionDescription' }
Warning >> defaultAction [
	"The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message."

	Debugger
		openContext: thisContext
		label: 'Warning'
		preDebugMessage: self messageText.
	self resume
]
