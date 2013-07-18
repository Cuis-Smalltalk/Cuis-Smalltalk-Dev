'From Cuis 4.1 of 12 December 2012 [latest update: #1681] on 17 April 2013 at 1:00:04 pm'!

!EventSensor methodsFor: 'mouse' stamp: 'jmv 4/17/2013 12:56'!
createMouseEvent
	"create and return a new mouse event from the current mouse 
	position; this is useful for restarting normal event queue 
	processing after manual polling"

	| buttons modifiers pos mapped eventBuffer |
	eventBuffer _ Array new: 8.
	buttons _ self primMouseButtons.
	pos _ self primMousePt.
	modifiers _ buttons bitShift: -3.
	buttons _ buttons bitAnd: 7.
	mapped _ self mapButtons: buttons modifiers: modifiers.
	eventBuffer
		at: 1 put: EventSensor eventTypeMouse;
		at: 2 put: Time localMillisecondClock;
		at: 3 put: pos x;
		at: 4 put: pos y;
		at: 5 put: mapped;
		at: 6 put: modifiers.
	^ eventBuffer! !

!EventSensor methodsFor: 'private' stamp: 'jmv 4/17/2013 12:56'!
eventTickler
	"Poll infrequently to make sure that the UI process is not been stuck. 
	If it has been stuck, then spin the event loop so that I can detect the 
	interrupt key."
	"Move declaration of delta to the method start, to avoid a bug in the decompiler"
	| delay delta |
	delay := Delay forMilliseconds: self class eventPollPeriod.
	self lastEventPoll.	"ensure not nil."
	[
		[ 
			delay wait.
			delta := Time localMillisecondClock - lastEventPoll.
			(delta < 0
					or: [delta > self class eventPollPeriod])
				ifTrue: ["force check on rollover"
					self fetchMoreEvents]
		] on: Error do: [:ex | ].
		true 
	] whileTrue.! !

!EventSensor methodsFor: 'private' stamp: 'jmv 4/17/2013 12:57'!
installEventTickler
	"Initialize the interrupt watcher process. Terminate the old process if any."
	"Sensor installEventTickler"

	EventTicklerProcess ifNotNil: [ EventTicklerProcess terminate ].
	EventTicklerProcess _ [ self eventTickler ] newProcess.
	EventTicklerProcess priority: Processor lowIOPriority.
	EventTicklerProcess name: 'Event Tickler'.
	EventTicklerProcess resume! !

!EventSensor methodsFor: 'private' stamp: 'jmv 4/17/2013 12:56'!
lastEventPoll
	"Answer the last clock value at which fetchMoreEvents was called."
	^lastEventPoll ifNil: [ lastEventPoll _ Time localMillisecondClock ]! !

!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 4/17/2013 12:56'!
fetchMoreEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore _ true.
			inputSemaphore initSignals ].

	"Remember the last time that I checked for events."
	lastEventPoll _ Time localMillisecondClock.

	eventBuffer _ Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		type _ eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [ self processSensorEvent: eventBuffer ]! !

!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 4/17/2013 12:57'!
primGetNextEvent: array
	"Store the next OS event available into the provided array.
	Essential. If the VM is not event driven the ST code will fall
	back to the old-style mechanism and use the state based
	primitives instead."
	| kbd buttons modifiers pos mapped |
	<primitive: 94>
	"Simulate the events"
	array at: 1 put: EventSensor eventTypeNone. "assume no more events"

	"First check for keyboard"
	kbd _ super primKbdNext.
	kbd ifNotNil: [
		"simulate keyboard event"
		array at: 1 put: EventSensor eventTypeKeyboard. "evt type"
		array at: 2 put: Time localMillisecondClock. "time stamp"
		array at: 3 put: (kbd bitAnd: 255). "char code"
		array at: 4 put: EventSensor eventKeyChar. "key press/release"
		array at: 5 put: (kbd bitShift: -8). "modifier keys"
		^self].

	"Then check for mouse"
	buttons _ super primMouseButtons.
	pos _ super primMousePt.
	modifiers _ buttons bitShift: -3.
	buttons _ buttons bitAnd: 7.
	mapped _ self mapButtons: buttons modifiers: modifiers.
	(pos = mousePosition and:[(mapped bitOr: (modifiers bitShift: 3)) = mouseButtons])
		ifTrue:[^self].
	array 
		at: 1 put: EventSensor eventTypeMouse;
		at: 2 put: Time localMillisecondClock;
		at: 3 put: pos x;
		at: 4 put: pos y;
		at: 5 put: mapped;
		at: 6 put: modifiers.
! !

!EventSensor methodsFor: 'test' stamp: 'jmv 4/17/2013 12:57'!
printEventBuffer: evtBuf

	| type buttons macRomanCode modifiers position pressType stamp unicodeCodePoint |
	type _ evtBuf first.
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time localMillisecondClock ].
	type = EventSensor eventTypeMouse
		ifTrue: [
			position _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode _ evtBuf third.
			unicodeCodePoint _ evtBuf sixth.
			pressType _ evtBuf fourth.
			modifiers _ evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type _ #keystroke].
			Transcript
				newLine;
				show: type;
				show: ' macRomanCode:', macRomanCode printString, '-', 
					(Character value: (Character macRomanToLatin1: macRomanCode)) asString, '-';
				show: ' unicodeCodePoint:', unicodeCodePoint printString.
			(Character iso8859s15CodeForUnicodeCodePoint: unicodeCodePoint) ifNotNil: [ :latin15 |
				Transcript show: '-', (Character value: latin15) asString, '-' ].
			Transcript
				show: ' modifiers:', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript show: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript show: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript show: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript show: ' [shift]' ].
			].! !


!Morph methodsFor: 'events-alarms' stamp: 'jmv 4/17/2013 12:58'!
addAlarm: aSelector withArguments: args after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: args at: Time localMillisecondClock + delayTime! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 4/17/2013 12:59'!
processEventQueue
	"Process user input events from the local input devices."

	| evt evtBuf type hadAny |

	hadAny := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		evt := nil.	"for unknown event types"
		type := evtBuf first.
		type = EventSensor eventTypeMouse
			ifTrue: [ evt := self generateMouseEvent: evtBuf ].
		type = EventSensor eventTypeKeyboard 
			ifTrue: [ evt := self generateKeyboardEvent: evtBuf ].
		type = EventSensor eventTypeWindow
			ifTrue: [ evt _ self generateWindowEvent: evtBuf ].
		"All other events are ignored"
		evt
			ifNil: [
				^hadAny]
			ifNotNil: [
				"Finally, handle it"
				self startEventDispatch: evt.
				hadAny := true.
				"For better user feedback, return immediately after a mouse event has been processed."
				evt isMouse ifTrue: [ ^hadAny ]]].
	"note: if we come here we didn't have any mouse events"
	mouseClickState 
		ifNotNil: [ 
			"No mouse events during this cycle. Make sure click states time out accordingly"
			mouseClickState
				handleEvent: (lastMouseEvent asMouseMove: (Time localMillisecondClock - lastMouseEventTime max: 0))
				from: self ].
	^hadAny! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 4/17/2013 12:59'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]].

	"any mouse event but mouseOver"
	lastMouseEvent _ aMouseEvent.	
	lastMouseEventTime _ Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]
		] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setType: #mouseMove
					position: aMouseEvent eventPosition
					buttons: aMouseEvent buttons
					hand: self
					stamp: aMouseEvent timeStamp) ].
			"Drop submorphs on button events"
			self hasSubmorphs
				ifTrue: [
					"Not if we are grabbing them"
					mouseClickState ifNil: [self dropMorphs: aMouseEvent ]]
				ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'initialization' stamp: 'jmv 4/17/2013 12:59'!
initForEvents
	mouseOverHandler _ nil.
	lastMouseEvent _ MouseEvent new setType: #mouseMove position: 0@0 buttons: 0 hand: self.
	lastMouseEventTime _ Time localMillisecondClock.
	lastEventBuffer _ {1. 0. 0. 0. 0. 0. nil. nil}.
	self dontWaitForMoreClicks! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 4/17/2013 12:59'!
generateKeyboardEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| buttons modifiers type keyValue pressType stamp |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [stamp _ Time localMillisecondClock].
	(evtBuf sixth <= 0 or: [
		(keyValue _ (Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth)) isNil ])
			ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].
	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue].
	pressType = EventSensor eventKeyUp ifTrue: [type _ #keyUp].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [		"Control key pressed"
			keyValue < 27 ifTrue: [
				
				"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [		"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ 	"If Equal, Ctrl-alphabetic in Windows => do translate"
							lastKeyDownValue < 47 ]) ifTrue: [		"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifFalse: [ keyValue + 96 ]	"shift not pressed: conver to lowercase letter"
							ifTrue: [ keyValue + 64 ]].	"shift pressed: conver to uppercase letter"
					]
				].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [
				modifiers _ modifiers bitOr: 8 ]
			]].
	buttons _ modifiers bitShift: 3.
	^KeyboardEvent new 
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 4/17/2013 12:59'!
generateMouseEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| pos buttons modifiers type trail stamp oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time localMillisecondClock ].
	pos := evtBuf third @ evtBuf fourth.
	buttons := evtBuf fifth.
	modifiers := evtBuf sixth.
	type := buttons = 0 
		ifTrue: [
			lastEventBuffer fifth = 0 ifTrue: [#mouseMove] ifFalse: [#mouseUp]]
		ifFalse: [
			lastEventBuffer fifth = 0 
						ifTrue: [#mouseDown]
						ifFalse: [#mouseMove]].
	buttons := buttons bitOr: (modifiers bitShift: 3).
	oldButtons := lastEventBuffer fifth 
				bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	type == #mouseMove 
		ifTrue: [
			trail := self mouseTrailFrom: evtBuf.
			^MouseMoveEvent new 
				setType: type
				position: trail last
				buttons: buttons
				hand: self
				stamp: stamp].
	^MouseButtonEvent new 
		setType: type
		position: pos
		which: (oldButtons bitXor: buttons)
		buttons: buttons
		hand: self
		stamp: stamp! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 4/17/2013 12:59'!
generateWindowEvent: evtBuf 
	"Generate the appropriate window event for the given raw event buffer"

	| evt |
	evt := WindowEvent new.
	evt setTimeStamp: evtBuf second.
	evt timeStamp = 0 ifTrue: [evt setTimeStamp: Time localMillisecondClock].
	evt windowAction: evtBuf third.
	evt rectangle: (Rectangle origin: evtBuf fourth @ evtBuf fifth corner: evtBuf sixth @ evtBuf seventh ).
	
	^evt! !


!MorphicEvent methodsFor: 'accessing' stamp: 'jmv 4/17/2013 12:58'!
timeStamp
	"Return the millisecond clock value at which the event was generated"
	^timeStamp ifNil:[timeStamp _ Time localMillisecondClock]! !


!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 4/17/2013 12:58'!
keyboardSearch: aChar 
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection _ oldSelection _ self getCurrentSelectionIndex.
	max _ self maximumSelection.
	milliSeconds _ Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes _ ''].
	lastKeystrokes _ lastKeystrokes , aChar asLowercase asString.
	lastKeystrokeTime _ milliSeconds.
	nextSelectionList _ OrderedCollection newFrom: (self getList copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (self getList copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText _ nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	nextSelection _ self getList findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	^ self changeModelSelection: nextSelection! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Sensor installEventTickler!

