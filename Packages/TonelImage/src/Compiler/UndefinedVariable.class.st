Class {
	#name : 'UndefinedVariable',
	#superclass : 'ParserNotification',
	#category : 'Compiler-Exceptions',
	#package : 'Compiler',
	#tag : 'Exceptions'
}

{ #category : 'exception handling' }
UndefinedVariable >> defaultAction [

	| shouldResume |
	
	shouldResume := PopUpMenu confirm: name, ' appears to be\undefined at this point.\Proceed anyway?' withNewLines.
	^ self resume: shouldResume 
]
