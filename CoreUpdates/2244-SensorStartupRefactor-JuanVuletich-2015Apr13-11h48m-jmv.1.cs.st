'From Cuis 4.2 of 25 July 2013 [latest update: #2237] on 13 April 2015 at 12:13:15.18143 pm'!

!InputSensor class methodsFor: 'system startup' stamp: 'jmv 4/13/2015 12:02'!
shutDown
	Sensor shutDownSensor! !

!InputSensor class methodsFor: 'system startup' stamp: 'jmv 4/13/2015 12:03'!
startUp
	
	self installMouseDecodeTable.
	Sensor initialize! !


!EventSensor methodsFor: 'private' stamp: 'jmv 4/13/2015 12:08'!
primInterruptSemaphore: aSemaphore 
	"Primitive. Install the argument as the semaphore to be signalled whenever the user presses the interrupt key. The semaphore will be signaled once each time the interrupt key is pressed."
	interruptSemaphore _ aSemaphore! !

!EventSensor methodsFor: 'initialization' stamp: 'jmv 4/13/2015 12:06'!
initialize
	"Run the I/O process"
	mouseButtons _ 0.
	mousePosition _ 0@0.
	self setInterruptKey: (interruptKey ifNil: [$. asciiValue bitOr: 16r0800 ]). 	"cmd-."
	interruptSemaphore _ (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	self flushAllButDandDEvents.
	inputSemaphore _ Semaphore new.
	hasInputSemaphore _ false.

	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	self installInterruptWatcher.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore _ false.
	inputSemaphore initSignals! !

!EventSensor methodsFor: 'initialization' stamp: 'jmv 4/13/2015 11:55'!
shutDownSensor
	InterruptWatcherProcess ifNotNil: [
		InterruptWatcherProcess terminate.
		InterruptWatcherProcess _ nil ].
	EventTicklerProcess ifNotNil: [
		EventTicklerProcess terminate.
		EventTicklerProcess _ nil. ].
	inputSemaphore ifNotNil: [Smalltalk unregisterExternalObject: inputSemaphore]! !


!EventSensor class methodsFor: 'class initialization' stamp: 'jmv 4/12/2015 20:21'!
install
	"
	EventSensor install
	"
	"Install an EventSensor in place of the current Sensor."

	Sensor ifNotNil: [ Sensor shutDownSensor ].
	Sensor _ self new! !

!methodRemoval: InputSensor class #default!
InputSensor class removeSelector: #default!

!InputSensor class reorganize!
('class initialization' installMouseDecodeTable)
('system startup' shutDown startUp)
('constants' anyModifierKey anyMouseButton commandAltKey controlKey macOptionKey mouseButton1 mouseButton2 mouseButton3 returnKey shiftKey)
!

!methodRemoval: InputSensor #initialize!
InputSensor removeSelector: #initialize!
!methodRemoval: InputSensor #primInterruptSemaphore:!
InputSensor removeSelector: #primInterruptSemaphore:!
!methodRemoval: InputSensor #shutDownSensor!
InputSensor removeSelector: #shutDownSensor!
