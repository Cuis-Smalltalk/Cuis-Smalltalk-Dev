'From Cuis 4.2 of 25 July 2013 [latest update: #2126] on 12 December 2014 at 4:16:12.439106 pm'!

!InputSensor methodsFor: 'initialization' stamp: 'jmv 12/12/2014 16:12'!
initialize

	self installInterruptWatcher! !


!InputSensor class methodsFor: 'system startup' stamp: 'jmv 12/12/2014 16:14'!
startUp
	
	self installMouseDecodeTable.
	self default initialize! !


!EventSensor methodsFor: 'initialization' stamp: 'jmv 12/12/2014 16:13'!
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
	super initialize.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore _ false.
	inputSemaphore initSignals! !


!EventSensor class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 16:14'!
install	"EventSensor install"
	"Install an EventSensor in place of the current Sensor."
	| newSensor |
	Sensor shutDownSensor.
	newSensor _ self new.
	"Note: We must use #become: here to replace all references to the old sensor with the new one, since Sensor is referenced from all the existing controllers."
	Sensor becomeForward: newSensor. "done"! !

!methodRemoval: EventSensor #startUpSensor!
EventSensor removeSelector: #startUpSensor!
!methodRemoval: InputSensor #startUpSensor!
InputSensor removeSelector: #startUpSensor!
