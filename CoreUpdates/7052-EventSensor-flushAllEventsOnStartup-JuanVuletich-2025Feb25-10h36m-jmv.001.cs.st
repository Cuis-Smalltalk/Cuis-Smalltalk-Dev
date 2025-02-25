'From Cuis7.3 [latest update: #7037] on 25 February 2025 at 10:45:02 am'!

!EventSensor methodsFor: 'accessing' stamp: 'jmv 2/25/2025 10:37:05'!
flushAllEvents
	eventQueue := SharedQueue new.
! !


!EventSensor methodsFor: 'initialization' stamp: 'jmv 2/25/2025 10:44:20'!
initialize
	"Run the I/O process"
	mouseButtons := 0.
	mousePosition := `0@0`.
	self setInterruptKey: (interruptKey ifNil: [$. codePoint bitOr: 16r0800 ]). 	"cmd-."
	self flushAllEvents.
	inputSemaphore := Semaphore new.
	hasInputSemaphore := false.

	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	self installInterruptWatcher.
	self installEventTickler.

	"A new Sensor discards anything in the event queue of the previous one. Discard also
		anything not yet queued.
	Additionally, on image startup, discard the possible D&D added by the VM (that tries to
		help old Squeak functionality where the image doesn't parse cmdline arguments,
		and the VM generates a D&D event assuming that cmdline argument is a filename.
		Cuis does any such thing at image level, effectively disable the VM doing this.
		See #processCommandLineArguments:."
	self fetchMoreEvents; flushAllEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore := false.
	inputSemaphore initSignals.! !

!methodRemoval: EventSensor #flushAllButDandDEvents stamp: 'jmv 2/25/2025 10:37:30'!
EventSensor removeSelector: #flushAllButDandDEvents!
