"
By John M McIntosh johnmci@smalltalkconsulting.com
This class was written to mange the external semaphore table. When I was writing a Socket test server I discovered various race conditions on the access to the externalSemaphore table. This new class uses class side methods to restrict access using a mutex semaphore. It seemed cleaner to deligate the reponsibility here versus adding more code and another class variable to SystemDictionary 

Note that in Smalltalk recreateSpecialObjectsArray we still directly play with the table.
"
Class {
	#name : 'ExternalSemaphoreTable',
	#superclass : 'Object',
	#classVars : [
		'ProtectTable'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'accessing' }
ExternalSemaphoreTable class >> accessProtect [
	ProtectTable ifNil: [
		ProtectTable := Semaphore forMutualExclusion ].
	^ProtectTable
]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> clearExternalObjects [
	"Clear the array of objects that have been registered for use in non-Smalltalk code."

	self accessProtect critical: [Smalltalk specialObjectsArray at: 39 put: #()].

]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> externalObjects [
	^self accessProtect critical: [Smalltalk specialObjectsArray at: 39]
]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> registerExternalObject: anObject [
	^ self accessProtect critical: [self safelyRegisterExternalObject: anObject]

]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> releaseClassCachedState [

	ProtectTable := nil
]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> safelyRegisterExternalObject: anObject [
	"Register the given object in the external objects array and return its index. If it is already there, just return its index."

	| objects firstEmptyIndex obj sz newObjects |
	objects := Smalltalk specialObjectsArray at: 39.

	"find the first empty slot"
	firstEmptyIndex := 0.
	1 to: objects size do: [:i |
		obj := objects at: i.
		obj == anObject ifTrue: [^ i].  "object already there, just return its index"
		(obj == nil and: [firstEmptyIndex = 0]) ifTrue: [firstEmptyIndex := i]].

	"if no empty slots, expand the array"
	firstEmptyIndex = 0 ifTrue: [
		sz := objects size.
		newObjects := objects species new: sz + 20.  "grow linearly"
		newObjects replaceFrom: 1 to: sz with: objects startingAt: 1.
		firstEmptyIndex := sz + 1.
		Smalltalk specialObjectsArray at: 39 put: newObjects.
		objects := newObjects].

	objects at: firstEmptyIndex put: anObject.
	^ firstEmptyIndex

]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> safelyUnregisterExternalObject: anObject [
	"Unregister the given object in the external objects array. Do nothing if it isn't registered.
	JMM change to return if we clear the element, since it should only appear once in the array"

	| objects |
	anObject ifNil: [^ self].
	objects := Smalltalk specialObjectsArray at: 39.
	1 to: objects size do: [:i |
		(objects at: i) == anObject ifTrue: 
		[objects at: i put: nil.
		^self]].

]

{ #category : 'accessing' }
ExternalSemaphoreTable class >> unregisterExternalObject: anObject [
	self accessProtect critical: [self safelyUnregisterExternalObject: anObject]

]
