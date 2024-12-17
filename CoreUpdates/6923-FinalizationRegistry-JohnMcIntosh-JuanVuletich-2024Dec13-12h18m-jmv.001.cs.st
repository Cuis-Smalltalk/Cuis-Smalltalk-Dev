'From Cuis7.3 [latest update: #6918] on 13 December 2024 at 12:20:51 pm'!
!classDefinition: #FinalizationRegistry category: #'System-Finalization'!
EphemeronIdentityDictionary subclass: #FinalizationRegistry
	instanceVariableNames: ''
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-Finalization'!

!FinalizationRegistry commentStamp: '<historical>' prior: 0!
The default FinalizationRegistry holds objects that need finalization.

Objects added with the #add: message will simply be send #finalize before being collected.
For Objects added with the #add:executor: message, #finalize will be sent to the executor (an instance of ObjectFinalizer) instead.!


!FinalizationRegistry methodsFor: 'adding' stamp: 'jmv 12/13/2024 10:12:24'!
add: anObject
	"Add an object to be finalized."

	self at: anObject put: nil.! !

!FinalizationRegistry methodsFor: 'adding' stamp: 'jmv 12/13/2024 10:34:51'!
add: anObject executor: anObjectFinalizer
	"Add an object to be finalized."

	self at: anObject put: anObjectFinalizer.! !

!FinalizationRegistry methodsFor: 'finalization' stamp: 'jmv 12/13/2024 11:16:28'!
finalizeEphemeron: anEphemeron

	"Finalize the ephemeron finalizer and remove it from myself.
	Avoid errors during the finalization to let the finalization process proceed"

	mutex critical: [

		self removeKey: anEphemeron key ifAbsent: [].

		"Usually no executor is needed. The object that is about to be collected, the key needs to be finalized.
		Some users may prefer to include an executor (an instance of ObjectFinalizer) as the value."
		(anEphemeron value ifNil: [ anEphemeron key ])
			finalize ].! !


!FinalizationRegistry class methodsFor: 'accessing' stamp: 'jmv 12/13/2024 11:37:31'!
default
	^Default ifNil:[Default := self new]! !


!FinalizationRegistry class reorganize!
('accessing' default)
!

