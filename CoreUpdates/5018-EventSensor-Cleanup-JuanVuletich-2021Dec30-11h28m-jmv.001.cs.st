'From Cuis 5.0 [latest update: #5017] on 30 December 2021 at 11:28:15 am'!
!classDefinition: #EventSensor category: #'Kernel-Processes'!
InputSensor subclass: #EventSensor
	instanceVariableNames: 'mouseButtons mousePosition keyboardBuffer interruptKey interruptSemaphore eventQueue inputSemaphore lastEventPoll hasInputSemaphore '
	classVariableNames: 'EventTicklerProcess '
	poolDictionaries: ''
	category: 'Kernel-Processes'!
!classDefinition: #EventSensor category: #'Kernel-Processes'!
InputSensor subclass: #EventSensor
	instanceVariableNames: 'mouseButtons mousePosition interruptKey eventQueue inputSemaphore lastEventPoll hasInputSemaphore'
	classVariableNames: 'EventTicklerProcess'
	poolDictionaries: ''
	category: 'Kernel-Processes'!
