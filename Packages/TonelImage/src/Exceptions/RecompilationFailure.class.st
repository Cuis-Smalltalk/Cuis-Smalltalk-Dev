"
Recompilation of some existing method failed.

The usual handling of this exception should be to cancel the action that triggered recompilation. If not handled, a debugger will open.
"
Class {
	#name : 'RecompilationFailure',
	#superclass : 'Error',
	#instVars : [
		'class'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'instance creation' }
RecompilationFailure class >> class: aClass messageText: aString [
	^ self new class: aClass messageText: aString
]

{ #category : 'private' }
RecompilationFailure >> class: aClass messageText: aString [
	class := aClass.
	messageText := aString
]
