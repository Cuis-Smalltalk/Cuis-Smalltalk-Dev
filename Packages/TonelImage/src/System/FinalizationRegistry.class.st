"
The default FinalizationRegistry holds objects that need finalization.

Objects added with the #add: message will simply be send #finalize before being collected.
For Objects added with the #add:executor: message, #finalize will be sent to the executor (an instance of ObjectFinalizer) instead.
"
Class {
	#name : 'FinalizationRegistry',
	#superclass : 'EphemeronIdentityDictionary',
	#classVars : [
		'Default'
	],
	#category : 'System-Finalization',
	#package : 'System',
	#tag : 'Finalization'
}

{ #category : 'accessing' }
FinalizationRegistry class >> default [
	^Default ifNil:[Default := self new]
]

{ #category : 'adding' }
FinalizationRegistry >> add: anObject [
	"Add an object to be finalized."

	self at: anObject put: nil.
]

{ #category : 'adding' }
FinalizationRegistry >> add: anObject executor: anObjectFinalizer [
	"Add an object to be finalized."

	self at: anObject put: anObjectFinalizer.
]

{ #category : 'finalization' }
FinalizationRegistry >> finalizeEphemeron: anEphemeron [
	"Finalize the ephemeron's key and remove the ephemeron from myself.
	 N.B. Here we send #finalize since finalization is called for by placing
	 objects in a FinalizationRegistry. i.e. the response to the ephemeron
	 being triggered is a property of the container of the ephemeron, not
	 the ephemeron itself."

	self removeKey: anEphemeron key ifAbsent: nil.
	
	"Usually no executor is needed. The object that is about to be collected, the key needs to be finalized.
	 Some users may prefer to include an executor (an instance of ObjectFinalizer) as the value.
	 N.B. errors here will not stall the finalization process."
	
	(anEphemeron value ifNil: [anEphemeron key]) finalize
]

{ #category : 'removing' }
FinalizationRegistry >> remove: anObject [
	"Does NOT run finalization."
	self removeKey: anObject
]

{ #category : 'removing' }
FinalizationRegistry >> remove: anObject ifAbsent: exceptionBlock [
	"Does NOT run finalization."
	self removeKey: anObject ifAbsent: exceptionBlock
]
