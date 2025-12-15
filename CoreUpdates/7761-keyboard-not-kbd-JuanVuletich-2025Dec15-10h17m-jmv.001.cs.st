'From Cuis7.5 [latest update: #7759] on 15 December 2025 at 10:21:17 am'!

!InputSensor methodsFor: 'private' stamp: 'Squeak1.0 20/Sep/1996 10:50:05'!
primKeyboardNext
	<primitive: 108>
	^ nil! !

!InputSensor methodsFor: 'private' stamp: 'Squeak1.0 20/Sep/1996 10:50:05'!
primKeyboardPeek
	<primitive: 109>
	^ nil! !


!EventSensor methodsFor: 'private' stamp: 'jmv 23/Jul/2013 23:32:00'!
flushNonKeyboardEvents

	eventQueue flushAllSuchThat: [ :buf |
		(self isKeyboardEvent: buf) not ]! !

!EventSensor methodsFor: 'private' stamp: 'jmv 15/Dec/2025 09:59:57'!
isKeyboardEvent: buf
	^ (buf at: 1) = EventSensor eventTypeKeyboard and: [(buf at: 4) = EventSensor eventKeyChar]! !

!EventSensor methodsFor: 'private' stamp: 'jmv 23/Jul/2013 23:34:00'!
primKeyboardNext
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running keyboardTest or the InterpreterSimulator in Morphic"
	| evtBuf |
	self fetchMoreEvents.
	evtBuf := eventQueue nextOrNilSuchThat: [ :buf | self isKeyboardEvent: buf].
	self flushNonKeyboardEvents.
	^ evtBuf ifNotNil: [ evtBuf at: 3 ]! !

!EventSensor methodsFor: 'private' stamp: 'jmv 23/Jul/2013 23:34:00'!
primKeyboardPeek
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
	^ char! !


!InputSensor methodsFor: 'keyboard' stamp: 'Squeak1.0 20/Sep/1996 10:50:05'!
keyboard
	"Answer the next character from the keyboard."

	^ self characterForKeycode: self primKeyboardNext! !

!InputSensor methodsFor: 'keyboard' stamp: 'Squeak1.0 20/Sep/1996 10:50:05'!
keyboardPeek
	"Answer the next character in the keyboard buffer without removing it, or nil if it is empty."

	^ self characterForKeycode: self primKeyboardPeek! !

!InputSensor methodsFor: 'keyboard' stamp: 'Squeak1.0 20/Sep/1996 10:50:05'!
keyboardPressed
	"Answer true if keystrokes are available."

	^self primKeyboardPeek notNil! !


!EventSensor methodsFor: 'private' stamp: 'ar 6/Feb/2004 14:42:00'!
primMouseButtons
	self fetchMoreEvents.
	self flushNonKeyboardEvents.
	^ mouseButtons! !

!EventSensor methodsFor: 'private' stamp: 'ar 6/Feb/2004 14:41:00'!
primMousePt
	self fetchMoreEvents.
	self flushNonKeyboardEvents.
	^ mousePosition! !

!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 23/Dec/2020 15:40:15'!
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
! !

!methodRemoval: EventSensor #primKbdNext stamp: 'jmv 15/Dec/2025 10:17:17'!
EventSensor removeSelector: #primKbdNext!
!methodRemoval: EventSensor #isKbdEvent: stamp: 'jmv 15/Dec/2025 10:17:18'!
EventSensor removeSelector: #isKbdEvent:!
!methodRemoval: EventSensor #flushNonKbdEvents stamp: 'jmv 15/Dec/2025 10:17:18'!
EventSensor removeSelector: #flushNonKbdEvents!
!methodRemoval: EventSensor #primKbdPeek stamp: 'jmv 15/Dec/2025 10:17:18'!
EventSensor removeSelector: #primKbdPeek!
!methodRemoval: InputSensor #primKbdNext stamp: 'jmv 15/Dec/2025 10:17:18'!
InputSensor removeSelector: #primKbdNext!
!methodRemoval: InputSensor #primKbdPeek stamp: 'jmv 15/Dec/2025 10:17:18'!
InputSensor removeSelector: #primKbdPeek!
