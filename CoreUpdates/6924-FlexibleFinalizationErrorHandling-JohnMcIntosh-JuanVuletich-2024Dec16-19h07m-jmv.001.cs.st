'From Cuis7.3 [latest update: #6924] on 16 December 2024 at 7:10:37 pm'!
!classDefinition: #FinalizationRegistry category: #'System-Finalization'!
EphemeronIdentityDictionary subclass: #FinalizationRegistry
	instanceVariableNames: 'errorHandler '
	classVariableNames: 'Default '
	poolDictionaries: ''
	category: 'System-Finalization'!

!FinalizationRegistry methodsFor: 'nil' stamp: 'JMM 12/13/2024 15:34:09'!
init: n
	super init: n.
	errorHandler := self.! !

!FinalizationRegistry methodsFor: 'accessing' stamp: 'JMM 12/13/2024 15:38:57'!
errorHandler: anErrorHandler 
	errorHandler := anErrorHandler! !

!FinalizationRegistry methodsFor: 'private - synchronization' stamp: 'jmv 12/16/2024 19:10:09'!
handleErrorsDuring: aBlock

	aBlock on: Error do:[:ex| ex return].! !


!FinalizationRegistry methodsFor: 'finalization' stamp: 'JMM 12/16/2024 08:49:42'!
finalizeEphemeron: anEphemeron

	"Finalize the ephemeron finalizer and remove it from myself.
	Avoid errors during the finalization to let the finalization process proceed"

	mutex critical:
	[
	
		self removeKey: anEphemeron key ifAbsent: [].
		errorHandler handleErrorsDuring:
		[ 
			
			"Usually no executor is needed. The object that is about to be collected, the key needs to be finalized.
			Some users may prefer to include an executor (an instance of ObjectFinalizer) as the value."
			
			(anEphemeron value ifNil: [ anEphemeron key ])
				finalize ]].! !


!ObjectFinalizer methodsFor: 'finalization' stamp: 'JMM 12/16/2024 18:29:01'!
finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority."

	receiver perform: selector withArguments: arguments! !

!classDefinition: #FinalizationRegistry category: #'System-Finalization'!
EphemeronIdentityDictionary subclass: #FinalizationRegistry
	instanceVariableNames: 'errorHandler'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-Finalization'!
