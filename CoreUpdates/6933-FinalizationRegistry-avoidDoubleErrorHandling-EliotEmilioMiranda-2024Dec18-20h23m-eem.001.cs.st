'From Cuis7.3 [latest update: #6922] on 18 December 2024 at 8:25:45 pm'!
!classDefinition: #FinalizationRegistry category: #'System-Finalization'!
EphemeronIdentityDictionary subclass: #FinalizationRegistry
	instanceVariableNames: 'errorHandler '
	classVariableNames: 'Default '
	poolDictionaries: ''
	category: 'System-Finalization'!

!FinalizationRegistry methodsFor: 'finalization' stamp: 'eem 12/18/2024 12:02:18'!
finalizeEphemeron: anEphemeron
	"Finalize the ephemeron's key and remove the ephemeron from myself.
	 N.B. Here we send #finalize since finalization is called for by placing
	 objects in a FinalizationRegistry. i.e. the response to the ephemeron
	 being triggered is a property of the container of the phemeron, not
	 the ephemeron itself."

	self removeKey: anEphemeron key ifAbsent: [].
	
	"Usually no executor is needed. The object that is about to be collected, the key needs to be finalized.
	 Some users may prefer to include an executor (an instance of ObjectFinalizer) as the value.
	 N.B. errors here will not stall the finalization process."
	
	(anEphemeron value ifNil: [anEphemeron key]) finalize! !

!methodRemoval: BlockClosure #on:fork: stamp: 'eem 12/18/2024 20:25:01'!
BlockClosure removeSelector: #on:fork:!
!methodRemoval: FinalizationRegistry #handleErrorsDuring: stamp: 'eem 12/18/2024 20:24:54'!
FinalizationRegistry removeSelector: #handleErrorsDuring:!
!methodRemoval: FinalizationRegistry #errorHandler: stamp: 'eem 12/18/2024 20:25:32'!
FinalizationRegistry removeSelector: #errorHandler:!
!methodRemoval: FinalizationRegistry #init: stamp: 'eem 12/18/2024 20:25:37'!
FinalizationRegistry removeSelector: #init:!
!classDefinition: #FinalizationRegistry category: #'System-Finalization'!
EphemeronIdentityDictionary subclass: #FinalizationRegistry
	instanceVariableNames: ''
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-Finalization'!
