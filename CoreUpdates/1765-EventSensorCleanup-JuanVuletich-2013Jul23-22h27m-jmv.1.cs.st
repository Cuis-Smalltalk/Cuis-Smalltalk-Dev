'From Cuis 4.1 of 12 December 2012 [latest update: #1762] on 23 July 2013 at 11:35:53.192004 pm'!

!EventSensor methodsFor: 'accessing' stamp: 'jmv 7/23/2013 23:34'!
flushAllButDandDEvents
	| newQueue oldQueue  |
	
	newQueue _ SharedQueue new.
	eventQueue ifNil: [
		eventQueue _ newQueue.
		^self].
	oldQueue _ eventQueue.
	[ oldQueue size > 0 ] whileTrue: [ | item type | 
		item _ oldQueue next.
		type _ item at: 1.
		type = EventSensor eventTypeDragDropFiles ifTrue: [ newQueue nextPut: item]].
	eventQueue _ newQueue.
! !

!EventSensor methodsFor: 'accessing' stamp: 'jmv 7/23/2013 23:30'!
flushEvents
	eventQueue flush! !

!EventSensor methodsFor: 'accessing' stamp: 'jmv 7/23/2013 23:34'!
nextEvent
	"Return the next event from the receiver."
	eventQueue isEmpty ifTrue: [ self fetchMoreEvents ].
	^ eventQueue isEmpty
		ifFalse: [ eventQueue next ]! !

!EventSensor methodsFor: 'accessing' stamp: 'jmv 7/23/2013 23:31'!
peekEvent
	"Look ahead at the next event."

	self fetchMoreEvents.
	^eventQueue peek! !

!EventSensor methodsFor: 'initialize' stamp: 'jmv 7/23/2013 23:35'!
initialize
	"Initialize the receiver"
	mouseButtons := 0.
	mousePosition := 0 @ 0.
	self setInterruptKey: (interruptKey ifNil: [$. asciiValue bitOr: 16r0800 ]). 	"cmd-."
	interruptSemaphore := (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	self flushAllButDandDEvents.
	inputSemaphore := Semaphore new.
	hasInputSemaphore := false.! !

!EventSensor methodsFor: 'private' stamp: 'jmv 7/23/2013 23:32'!
flushNonKbdEvents

	eventQueue flushAllSuchThat: [ :buf |
		(self isKbdEvent: buf) not ]! !

!EventSensor methodsFor: 'private' stamp: 'jmv 7/23/2013 23:34'!
primKbdNext
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running kbdTest or the InterpreterSimulator in Morphic"
	| evtBuf |
	self fetchMoreEvents.
	evtBuf _ eventQueue nextOrNilSuchThat: [ :buf | self isKbdEvent: buf].
	self flushNonKbdEvents.
	^ evtBuf ifNotNil: [ evtBuf at: 3 ]! !

!EventSensor methodsFor: 'private' stamp: 'jmv 7/23/2013 23:34'!
primKbdPeek
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running kbdTest or the InterpreterSimulator in Morphic"
	| char |
	self fetchMoreEvents.
	char _ nil.
	"NOTE: must not return out of this block, so loop to end"
	eventQueue nextOrNilSuchThat: [ :buf |
		(self isKbdEvent: buf) ifTrue: [
			char ifNil: [ char _ buf at: 3 ]].
		false  "NOTE: block value must be false so Queue won't advance"].
	^ char! !

!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 7/23/2013 23:32'!
processKeyboardSensorEvent: evt
	"process a keyboard event, updating InputSensor state"

	mouseButtons _ (mouseButtons bitAnd: 7) bitOr: ((evt at: 5) bitShift: 3)! !

!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 7/23/2013 23:33'!
queueEvent: evt
	"Queue the given event in the event queue (if any).
	Note that the event buffer must be copied since it
	will be reused later on."

	eventQueue nextPut: evt clone! !

!methodRemoval: EventSensor #eventQueue!
EventSensor removeSelector: #eventQueue!
!methodRemoval: InputSensor #eventQueue!
InputSensor removeSelector: #eventQueue!
!methodRemoval: InputSensor #flushAllButDandDEvents!
InputSensor removeSelector: #flushAllButDandDEvents!
