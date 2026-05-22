"
EventSensor is a replacement for InputSensor based on a set of (optional) event primitives. An EventSensor updates its state when events are received so that all state based users of Sensor (e.g., Sensor keyboard, Sensor leftShiftDown, Sensor mouseButtons) will work exactly as before, by moving the current VM mechanisms into EventSensor itself. An optional input semaphore is part of the new design.

For platforms that support true asynchronous event notification, the semaphore will be signaled to indicate pending events.
On platforms that do not support asynchronous notifications about events, the UI will have to poll EventSensor periodically to read events from the VM.

Instance variables:
	mouseButtons <Integer>	- mouse button state as replacement for primMouseButtons
	mousePosition <Point>	- mouse position as replacement for primMousePt
	keyboardBuffer <SharedQueue>	- keyboard input buffer
	interruptKey <Integer>			- currently defined interrupt key
	interruptSemaphore <Semaphore>	- the semaphore signaled when the interruptKey is detected
	eventQueue <SharedQueue>	- an optional event queue for event driven applications
	inputSemaphore <Semaphore>- the semaphore signaled by the VM if asynchronous event notification is supported
	lastEventPoll <Integer>		- the last millisecondClockValue at which we called fetchMoreEvents
	hasInputSemaphore <Boolean>	- true if my inputSemaphore has actually been signaled at least once.

Class variables:
	EventPollPeriod <Integer>	- the number of milliseconds to wait between polling for more events in the userInterruptHandler.
	EventTicklerProcess <Process>	- the process that makes sure that events are polled for often enough (at least every EventPollPeriod milliseconds).

Event format:
The current event format is very simple. Each event is recorded into an 8 element array. All events must provide some SmallInteger ID (the first field in the event buffer) and a time stamp (the second field in the event buffer), so that the difference between the time stamp of an event and the current time can be reported.

Currently, the following events are defined:

Null event
=============
The Null event is returned when the ST side asks for more events but no more events are available.
Structure:
[1]		- event type 0
[2-8]	- unused

Mouse event structure
==========================
Mouse events are generated when mouse input is detected.
Structure:
[1]	- event type 1
[2]	- time stamp
[3]	- mouse x position
[4]	- mouse y position
[5]	- button state; bitfield with the following entries:
		1	-	mouseButton2 (e.g., right) button
		2	-	mouseButton3 (e.g., middle) button
		4	-	mouseButton1 (e.g., left) button
		[all other bits are currently undefined]
[6]	- modifier keys; bitfield with the following entries:
		1	-	shift key
		2	-	ctrl key
		4	-	(Mac specific) option key
		8	-	Cmd/Alt key
		[all other bits are currently undefined]
[7]	- reserved.
[8]	- reserved.

Keyboard events
====================
Keyboard events are generated when keyboard input is detected.
[1]	- event type 2
[2]	- time stamp
[3]	- character code
		For now the character code is in Mac Roman encoding.
[4]	- press state; integer with the following meaning
		0	-	character
		1	-	key press (down)
		2	- 	key release (up)
[5]	- modifier keys (same as in mouse events)
[6]	- reserved.
[7]	- reserved.
[8]	- reserved.

"
Class {
	#name : 'EventSensor',
	#superclass : 'InputSensor',
	#instVars : [
		'mouseButtons',
		'mousePosition',
		'interruptKey',
		'eventQueue',
		'inputSemaphore',
		'lastEventPoll',
		'hasInputSemaphore'
	],
	#classVars : [
		'EventTicklerProcess'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'constants' }
EventSensor class >> commandAltKey [
	"Modifier keys.
	command (Mac) / alt(Windows) key
	Encoding specific to EventSensor primitives (not the same of InputSensor)"
	^ 8
]

{ #category : 'constants' }
EventSensor class >> controlKey [
	"Modifier keys.
	control / ctrl
	Encoding specific to EventSensor primitives (not the same of InputSensor)"
	^ 2
]

{ #category : 'constants' }
EventSensor class >> eventKeyChar [
	"Press codes for keyboard events"
	^0
]

{ #category : 'constants' }
EventSensor class >> eventKeyDown [
	"Press codes for keyboard events"
	^1
]

{ #category : 'constants' }
EventSensor class >> eventKeyUp [
	"Press codes for keyboard events"
	^2
]

{ #category : 'class initialization' }
EventSensor class >> eventPollPeriod [
	^500
]

{ #category : 'constants' }
EventSensor class >> eventTypeDragDropFiles [
	"Types of events"
	^3
]

{ #category : 'constants' }
EventSensor class >> eventTypeKeyboard [
	"Types of events"
	^2
]

{ #category : 'constants' }
EventSensor class >> eventTypeMenu [
	"Types of events"
	^4
]

{ #category : 'constants' }
EventSensor class >> eventTypeMouse [
	"Types of events"
	^1
]

{ #category : 'constants' }
EventSensor class >> eventTypeMouseScroll [
	"Types of events,
	
	I am a mouse wheel event."
	^7
]

{ #category : 'constants' }
EventSensor class >> eventTypeNone [
	"Types of events"
	^0
]

{ #category : 'constants' }
EventSensor class >> eventTypeWindow [
	"Types of events"
	^5
]

{ #category : 'class initialization' }
EventSensor class >> install [
	"
	EventSensor install
	"
	"Install an EventSensor in place of the current Sensor."

	Sensor ifNotNil: [ Sensor shutDownSensor ].
	Sensor := self new
]

{ #category : 'constants' }
EventSensor class >> macOptionKey [
	"Modifier keys.
	option (Mac only)  key
	Encoding specific to EventSensor primitives (not the same of InputSensor)"
	^ 4
]

{ #category : 'constants' }
EventSensor class >> shiftKey [
	"Modifier keys.
	shift  key
	Encoding specific to EventSensor primitives (not the same of InputSensor)"
	^ 1
]

{ #category : 'mouse' }
EventSensor >> createMouseEvent [
	"create and return a new mouse event from the current mouse 
	position; this is useful for restarting normal event queue 
	processing after manual polling"

	| buttons modifiers pos mapped eventBuffer |
	eventBuffer := Array new: 8.
	buttons := self primMouseButtons.
	pos := self primMousePt.
	modifiers := buttons bitShift: -3.
	buttons := buttons bitAnd: 7.
	mapped := self mapButtons: buttons modifiers: modifiers.
	eventBuffer
		at: 1 put: EventSensor eventTypeMouse;
		at: 2 put: Time millisecondClockValue; "VMs report events using #millisecondClockValue"
		at: 3 put: pos x;
		at: 4 put: pos y;
		at: 5 put: mapped;
		at: 6 put: modifiers.
	^ eventBuffer
]

{ #category : 'private' }
EventSensor >> eventTickler [
	"If the UI process hasn't processed events in a while, do it here.
	This is needed to detect the interrupt key."

	| delay |
	delay := Delay forMilliseconds: self class eventPollPeriod.
	[
		[
			| delta |
			"Could happen if process is halted and resumed."
			delay beingWaitedOn ifTrue: [ delay := Delay forMilliseconds: delay delayDuration ].
			delay wait.
			delta := Time localMillisecondClock - self lastEventPoll.
			"If the UI (i.e. Morphic) hasen't #nextEvent since the last time we checked,
			process events to allow for the interrupt key to be detected."
			delta >= self class eventPollPeriod ifTrue: [
				Cursor currentCursor = Cursor defaultCursor
					ifTrue: [ (Cursor cursorAt: #waitCursor) activateCursor ].
				"Discard any mouse events. This code is run when the UI is slow, essentially to have a working
				interrupt key. Processing mouse events is pointless: the UI will not handle them anyway.
				In addition, at least on Windows 7, when the machine is suspended and resumed with Cuis
				running, a lot of meaningless mouseMove events with the same coordinates are sent, maing
				Cuis extremely slow and CPU hungry for a few minutes without reason. Discarding mouse
				events makes the 'processing' of those very quick."
				self fetchMoreEventsDiscardingMouseEvents: true ]
		] on: UnhandledError 	do: [ :ex | 			ex return ]
	] repeat
]

{ #category : 'accessing' }
EventSensor >> eventTicklerProcess [
	"Answer my event tickler process, if any"
	^EventTicklerProcess
]

{ #category : 'private-I/O' }
EventSensor >> fetchMoreEvents [
	"Fetch more events from the VM"

	self fetchMoreEventsDiscardingMouseEvents: false
]

{ #category : 'private-I/O' }
EventSensor >> fetchMoreEventsDiscardingMouseEvents: discardMouseEvents [
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore := true.
			inputSemaphore initSignals ].

	"DebuggingAid""
	dd := Time localMillisecondClock - lastEventPoll.
	"

	eventBuffer := Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		"DebuggingAid""
		eventBuffer first = 2 ifTrue: [
			k1 := eventBuffer third.
			u1 := eventBuffer sixth.
			pressType := eventBuffer fourth.
			typeS := 'unknown'.
			pressType = EventSensor eventKeyDown ifTrue: [
				typeS := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				typeS := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				typeS := #keystroke].
			dd >100 ifTrue: ['-' print. dd := 0 ].
			{ k1 < 256 ifTrue: [Character codePoint: k1] ifFalse: [ '[]']. Character codePoint: k1. k1. k1 hex. '-----'. 
				Character codePoint: u1. u1. u1 hex. '----'. pressType. typeS } print.
			].
		"
		type := eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ].

	"Remember the last time that I checked for events."
	lastEventPoll := Time localMillisecondClock.
]

{ #category : 'accessing' }
EventSensor >> flushAllEvents [
	eventQueue := SharedQueue new.

]

{ #category : 'accessing' }
EventSensor >> flushEvents [
	eventQueue flush
]

{ #category : 'private' }
EventSensor >> flushNonKeyboardEvents [

	eventQueue flushAllSuchThat: [ :buf |
		(self isKeyboardEvent: buf) not ]
]

{ #category : 'initialization' }
EventSensor >> initialize [
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
	inputSemaphore initSignals.
]

{ #category : 'private' }
EventSensor >> installEventTickler [
	"Initialize the event tickler process. Terminate the old process if any."
	"
	Sensor installEventTickler
	"

	EventTicklerProcess ifNotNil: [ EventTicklerProcess terminate ].
	EventTicklerProcess := [ self eventTickler ] newProcess.
	EventTicklerProcess priority: Processor lowIOPriority.
	EventTicklerProcess name: '[system] Event Tickler'.
	EventTicklerProcess resume
]

{ #category : 'private' }
EventSensor >> isKeyboardEvent: buf [
	^ (buf at: 1) = EventSensor eventTypeKeyboard and: [(buf at: 4) = EventSensor eventKeyChar]
]

{ #category : 'private' }
EventSensor >> lastEventPoll [
	"Answer the last clock value at which fetchMoreEvents was called."
	^lastEventPoll ifNil: [ lastEventPoll := Time localMillisecondClock ]
]

{ #category : 'private-I/O' }
EventSensor >> mapButtons: buttons modifiers: modifiers [
	"Map the buttons to mouseButton2 or mouseButton3 based on the given modifiers.
	If only the mouseButton1 is pressed, then map
		Cmd-mouseButton1 -> mouseButton2.
		Ctrl-mouseButton1 -> mouseButton3.
	"
	(buttons = InputSensor mouseButton1)
		ifFalse: [ ^ buttons ].
	(modifiers allMask: EventSensor macOptionKey) 
		ifTrue: [ ^ InputSensor mouseButton3 ].
	(modifiers allMask: EventSensor commandAltKey) 
		ifTrue: [ ^ InputSensor mouseButton2 ].
	^buttons
]

{ #category : 'accessing' }
EventSensor >> nextEvent [
	"Return the next event from the receiver."
	eventQueue isEmpty ifTrue: [ self fetchMoreEvents ].
	^ eventQueue isEmpty
		ifFalse: [ eventQueue next ]
]

{ #category : 'accessing' }
EventSensor >> peekButtons [
	self fetchMoreEvents.
	^mouseButtons
]

{ #category : 'accessing' }
EventSensor >> peekEvent [
	"Look ahead at the next event."

	self fetchMoreEvents.
	^eventQueue peek
]

{ #category : 'accessing' }
EventSensor >> peekMousePt [
	^mousePosition
]

{ #category : 'accessing' }
EventSensor >> peekPosition [
	self fetchMoreEvents.
	^mousePosition
]

{ #category : 'private-I/O' }
EventSensor >> primGetNextEvent: array [
	"Store the next OS event available into the provided array.
	Essential. If the VM is not event driven the ST code will fall
	back to the old-style mechanism and use the state based
	primitives instead."
	| kbd buttons modifiers pos mapped |
	<primitive: 94>
	"Simulate the events"
	array at: 1 put: EventSensor eventTypeNone. "assume no more events"

	"First check for keyboard"
	kbd := super primKeyboardNext.
	kbd ifNotNil: [
		"simulate keyboard event"
		array at: 1 put: EventSensor eventTypeKeyboard. "evt type"
		array at: 2 put: Time millisecondClockValue. "VMs report events using #millisecondClockValue"
		array at: 3 put: (kbd bitAnd: 255). "char code"
		array at: 4 put: EventSensor eventKeyChar. "key press/release"
		array at: 5 put: (kbd bitShift: -8). "modifier keys"
		^self].

	"Then check for mouse"
	buttons := super primMouseButtons.
	pos := super primMousePt.
	modifiers := buttons bitShift: -3.
	buttons := buttons bitAnd: 7.
	mapped := self mapButtons: buttons modifiers: modifiers.
	(pos = mousePosition and:[(mapped bitOr: (modifiers bitShift: 3)) = mouseButtons])
		ifTrue:[^self].
	array 
		at: 1 put: EventSensor eventTypeMouse;
		at: 2 put: Time millisecondClockValue; "VMs report events using #millisecondClockValue"
		at: 3 put: pos x;
		at: 4 put: pos y;
		at: 5 put: mapped;
		at: 6 put: modifiers.

]

{ #category : 'private' }
EventSensor >> primKeyboardNext [
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running keyboardTest or the InterpreterSimulator in Morphic"
	| evtBuf |
	self fetchMoreEvents.
	evtBuf := eventQueue nextOrNilSuchThat: [ :buf | self isKeyboardEvent: buf].
	self flushNonKeyboardEvents.
	^ evtBuf ifNotNil: [ evtBuf at: 3 ]
]

{ #category : 'private' }
EventSensor >> primKeyboardPeek [
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running keyboardTest or the InterpreterSimulator in Morphic"
	| char |
	self fetchMoreEvents.
	char := nil.
	"NOTE: must not return out of this block, so loop to end"
	eventQueue nextOrNilSuchThat: [ :buf |
		(self isKeyboardEvent: buf) ifTrue: [
			char ifNil: [ char := buf at: 3 ]].
		false  "NOTE: block value must be false so Queue won't advance"].
	^ char
]

{ #category : 'private' }
EventSensor >> primMouseButtons [
	self fetchMoreEvents.
	self flushNonKeyboardEvents.
	^ mouseButtons
]

{ #category : 'private' }
EventSensor >> primMousePt [
	self fetchMoreEvents.
	self flushNonKeyboardEvents.
	^ mousePosition
]

{ #category : 'private-I/O' }
EventSensor >> primSetInputSemaphore: semaIndex [
	"Set the input semaphore the VM should use for asynchronously signaling the availability of events. Primitive. Optional."
	<primitive: 93>
	^nil
]

{ #category : 'private' }
EventSensor >> primSetInterruptKey: anInteger [
	"Primitive. Register the given keycode as the user interrupt key. The low byte of the keycode is the ISO character and its next four bits are the Smalltalk modifer bits <cmd><opt><ctrl><shift>."
	interruptKey := anInteger.
	"backward compatibility: use the old primitive which is obsolete now"
	super primSetInterruptKey: anInteger
]

{ #category : 'test' }
EventSensor >> printEventBuffer: evtBuf [
	"Print the event buffer, currently only used by the method `test`."

	| type buttons macRomanCode modifiers pressType stamp codePoint |
	type := evtBuf first.
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	type = EventSensor eventTypeMouse
		ifTrue: [ | position |
			position := evtBuf third @ evtBuf fourth.
			buttons := evtBuf fifth.
			modifiers := evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeMouseScroll
		ifTrue: [ | delta |
			delta := evtBuf third @ evtBuf fourth.
			buttons := evtBuf fifth.
			modifiers := evtBuf sixth.
			Transcript
				newLine;
				show: 'Scroll';
				show: ' delta:', delta printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode := evtBuf third.
			codePoint := evtBuf sixth.
			pressType := evtBuf fourth.
			modifiers := evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type := #keystroke].
			{type. 'macRoman:'. macRomanCode. macRomanCode hex.
				'Unicode CodePoint:'. codePoint. codePoint hex. Character codePoint: codePoint} print.
			Transcript nextPutAll: ' modifiers: ', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript nextPutAll: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript nextPutAll: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript nextPutAll: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript nextPutAll: ' [shift]' ].
			''print.
			].
]

{ #category : 'private-I/O' }
EventSensor >> processKeyboardSensorEvent: evt [
	"process a keyboard event, updating InputSensor state"

	mouseButtons := (mouseButtons bitAnd: 7) bitOr: ((evt at: 5) bitShift: 3)
]

{ #category : 'private-I/O' }
EventSensor >> processMouseSensorEvent: evt [
	"process a mouse event, updating InputSensor state"
	| modifiers buttons mapped |
	mousePosition := (evt at: 3) @ (evt at: 4).
	buttons := evt at: 5.
	modifiers := evt at: 6.
	mapped := self mapButtons: buttons modifiers: modifiers.
	mouseButtons := mapped bitOr: (modifiers bitShift: 3).
]

{ #category : 'private-I/O' }
EventSensor >> processMouseSensorWheelEvent: evt [
	"Process a mouse wheel event, updating EventSensor state.
	
	Ported from Squeak 5.3."
	
	| modifiers buttons mapped |
	"Only used by #peekWheelDelta in Squeak, which has no senders.
	Can be added in the future."
	"F: mouseWheelDelta := (evt at: 3) @ (evt at: 4)."
	buttons := evt at: 5.
	modifiers := evt at: 6.
	mapped := self mapButtons: buttons modifiers: modifiers.
	mouseButtons := mapped bitOr: (modifiers bitShift: 3).
]

{ #category : 'private-I/O' }
EventSensor >> processSensorEvent: evt discardingMouseEvents: discardMouseEvents [
	"Process a single event. This method is run at high priority."
	| type |
	type := evt at: 1.

	"Check if the event is a user interrupt"
	(type = EventSensor eventTypeKeyboard and: [ (evt at: 4) = 0 and: [
		((evt at: 3) bitOr: (((evt at: 5) bitAnd: 9) bitShift: 8)) = interruptKey]])
			 ifTrue: [
				"interrupt key is meta - not reported as event"
				^InterruptSemaphore signal].

	"Store the event in the queue if there's any"
	type = EventSensor eventTypeMouse ifTrue: [
		"Only swap secondary and tertiary buttons if there is no command or option modifier keys.
		This swap is done so a 3-button mouse  is
			left -> mouseButton1 (select)
			center -> mouseButton3 (halo)
			right -> mouseButton2 (menu).
		This is only needed on the Mac, Window VM does this mapping by default.
		We avoid ding the swap if there are modifier keys, because in that case the buttons were generated by the VM as follows:
			left -> mouseButton1
			macOption + left -> mouseButton3
			command + left -> mouseButton2,
		but Mac users are already used to 
			macOption + left -> menu
			command + left -> halo.
		See #installMouseDecodeTable"
		((evt at: 6) anyMask: 12) ifFalse: [
			evt at: 5 put: (ButtonDecodeTable at: (evt at: 5) + 1)]].
	
	(discardMouseEvents and: [ type = EventSensor eventTypeMouse ]) ifFalse: [
		self queueEvent: evt ].

	"Update state for InputSensor."
	"KLG: Why not `self class` instead of `EventSensor`?"
	type = EventSensor eventTypeMouse ifTrue: [
		self processMouseSensorEvent: evt ].
	type = EventSensor eventTypeKeyboard ifTrue: [
		self processKeyboardSensorEvent: evt ].
	type = EventSensor eventTypeMouseScroll ifTrue: [
		self processMouseSensorWheelEvent: evt ]
]

{ #category : 'private-I/O' }
EventSensor >> queueEvent: evt [
	"Queue the given event in the event queue (if any).
	Note that the event buffer must be copied since it
	will be reused later on."

	eventQueue nextPut: evt copy
]

{ #category : 'accessing' }
EventSensor >> shiftPressed [
	"Answer whether the shift key on the keyboard is being held down."

	^ mouseButtons anyMask: InputSensor shiftKey
]

{ #category : 'initialization' }
EventSensor >> shutDownSensor [
	InterruptWatcherProcess ifNotNil: [
		InterruptWatcherProcess terminate.
		InterruptWatcherProcess := nil ].
	EventTicklerProcess ifNotNil: [
		EventTicklerProcess terminate.
		EventTicklerProcess := nil. ].
	inputSemaphore ifNotNil: [Smalltalk unregisterExternalObject: inputSemaphore]
]

{ #category : 'test' }
EventSensor >> test [
	"
	Sensor test

Also useful:
	Sensor keyboardTest
	"
	| c lastStamp stamp |
	c := nil.
	lastStamp := 0.
	[ c == $x asciiValue ] whileFalse: [
		Sensor nextEvent ifNotNil: [ :evt |
			stamp := evt at: 2.
			stamp - lastStamp > 300 ifTrue: [ ' ' print.' ' print ].
			Transcript show: evt; newLine.
			self printEventBuffer: evt.
			(evt first = 2 and: [ evt fourth = 0 ]) ifTrue: [
				c := evt third ].
			lastStamp := stamp]]
]
