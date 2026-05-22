"
This is a resumable error you get if you try to assign a readonly variable a value.
Name definitions in the module system can be read only and are then created using instances of ReadOnlyVariableBinding instead of Association.
See also LookupKey>>beReadWriteBinding and LookupKey>>beReadOnlyBinding.


"
Class {
	#name : 'AttemptToWriteReadOnlyGlobal',
	#superclass : 'Error',
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'exceptionDescription' }
AttemptToWriteReadOnlyGlobal >> description [
	"Return a textual description of the exception."

	| desc |
	desc := 'Error'.
	^self messageText
		ifNil: [ desc]
		ifNotNil: [ :mt | desc, ': ', mt ]
]

{ #category : 'private' }
AttemptToWriteReadOnlyGlobal >> isResumable [
	^true
]
