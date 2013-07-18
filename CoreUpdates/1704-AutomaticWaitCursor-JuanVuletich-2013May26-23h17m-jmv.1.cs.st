'From Cuis 4.1 of 12 December 2012 [latest update: #1703] on 26 May 2013 at 11:18:23.135667 pm'!
!classDefinition: #EventSensor category: #'Kernel-Processes'!
InputSensor subclass: #EventSensor
	instanceVariableNames: 'mouseButtons mousePosition keyboardBuffer interruptKey interruptSemaphore eventQueue inputSemaphore lastEventPoll hasInputSemaphore '
	classVariableNames: 'EventPollPeriod EventTicklerProcess '
	poolDictionaries: ''
	category: 'Kernel-Processes'!

!InputSensor methodsFor: 'user interrupts' stamp: 'jmv 5/26/2013 09:58'!
installInterruptWatcher
	"Initialize the interrupt watcher process. Terminate the old process if any.
	This process simply waits for the VM to signal the 'user interrupt' semaphore, and opens a debugger."
	"
	Sensor installInterruptWatcher
	"

	InterruptWatcherProcess ifNotNil: [InterruptWatcherProcess terminate].
	InterruptSemaphore _ Semaphore new.
	InterruptWatcherProcess _ [self userInterruptWatcher] newProcess.
	InterruptWatcherProcess priority: Processor lowIOPriority.
	InterruptWatcherProcess name: 'User interrupt watcher'.
	InterruptWatcherProcess resume.
	self primInterruptSemaphore: InterruptSemaphore! !

!InputSensor methodsFor: 'user interrupts' stamp: 'jmv 5/26/2013 09:59'!
userInterruptWatcher
	"Wait for user interrupts and open a notifier on the active process when one occurs."

	[ true ] whileTrue: [
		InterruptSemaphore wait.
		Display deferUpdates: false.
		SoundService defaultOrNil ifNotNil: [ :ss | ss shutDown ].
		Smalltalk handleUserInterrupt]! !


!EventSensor methodsFor: 'private' stamp: 'jmv 5/26/2013 20:31'!
eventTickler
	"If the UI process hasn't processed events in a while, do it here.
	This is needed to detect the interrupt key."
	| delay delta |
	delay _ Delay forMilliseconds: self class eventPollPeriod.
	self lastEventPoll.	"ensure not nil."
	[
		[ 
			delay wait.
			delta _ Time localMillisecondClock - lastEventPoll.
			delta > self class eventPollPeriod
				ifTrue: [
					"See #doOneCycleNowFor:"
					Cursor currentCursor = Cursor normal ifTrue: [
						Cursor wait show ].
					self fetchMoreEvents ]
		] on: Error do: [ :ex | ].
	] repeat! !

!EventSensor methodsFor: 'private' stamp: 'jmv 5/26/2013 09:57'!
installEventTickler
	"Initialize the event tickler process. Terminate the old process if any."
	"
	Sensor installEventTickler
	"

	EventTicklerProcess ifNotNil: [ EventTicklerProcess terminate ].
	EventTicklerProcess _ [ self eventTickler ] newProcess.
	EventTicklerProcess priority: Processor lowIOPriority.
	EventTicklerProcess name: 'Event Tickler'.
	EventTicklerProcess resume! !


!EventSensor class methodsFor: 'class initialization' stamp: 'jmv 5/26/2013 20:18'!
eventPollPeriod
	^200! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 5/26/2013 20:31'!
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	"See #eventTickler"
	Cursor currentCursor = Cursor wait ifTrue: [
		Cursor normal show ].

	DisplayScreen checkForNewScreenSize.

	"process user input events"
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ h processEventQueue.
		activeHand _ nil.
	].

	"the default is the primary hand"
	activeHand _ self hands first.

	aWorld runStepMethods.		"there are currently some variations here"
	self displayWorldSafely: aWorld.

	^hadAnyEvent! !

!methodRemoval: EventSensor class #eventPollPeriod:!
EventSensor class removeSelector: #eventPollPeriod:!
!classDefinition: #EventSensor category: #'Kernel-Processes'!
InputSensor subclass: #EventSensor
	instanceVariableNames: 'mouseButtons mousePosition keyboardBuffer interruptKey interruptSemaphore eventQueue inputSemaphore lastEventPoll hasInputSemaphore'
	classVariableNames: 'EventTicklerProcess'
	poolDictionaries: ''
	category: 'Kernel-Processes'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Sensor installEventTickler!

