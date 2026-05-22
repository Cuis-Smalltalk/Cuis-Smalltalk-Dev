Class {
	#name : 'UnusedVariable',
	#superclass : 'ParserNotification',
	#category : 'Compiler-Exceptions',
	#package : 'Compiler',
	#tag : 'Exceptions'
}

{ #category : 'exception handling' }
UnusedVariable >> defaultAction [
	
	| shouldResume |
	
	shouldResume := PopUpMenu confirm: name, ' appears to be\unused in this method.\OK to remove it?' withNewLines.
	self resume: shouldResume 
]
