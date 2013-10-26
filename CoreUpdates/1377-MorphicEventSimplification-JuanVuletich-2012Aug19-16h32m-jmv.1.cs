'From Cuis 4.0 of 21 April 2012 [latest update: #1374] on 19 August 2012 at 4:43:11 pm'!

!HandMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 16:38'!
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
				handleEvent: (lastMouseEvent asMouseMove: (Time millisecondClockValue - lastMouseEventTime max: 0))
				from: self ].
	^hadAny! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 16:38'!
startEventDispatch: aMorphicEvent

	owner ifNil: [ ^ self ].
	aMorphicEvent isMouseOver ifTrue: [ ^ self sendMouseEvent: aMorphicEvent ].
	
	aMorphicEvent isWindowEvent ifTrue: [
		owner dispatchEvent: aMorphicEvent.
		^ self mouseOverHandler processMouseOver: lastMouseEvent ].

	aMorphicEvent isKeyboard ifTrue: [
		self sendKeyboardEvent: aMorphicEvent.
		^ self mouseOverHandler processMouseOver: lastMouseEvent].

	aMorphicEvent isDropEvent ifTrue: [
		owner dispatchEvent: aMorphicEvent.
		^ self mouseOverHandler processMouseOver: lastMouseEvent].

	aMorphicEvent isMouse ifTrue: [
		lastMouseEvent _ aMorphicEvent.	
		lastMouseEventTime _ Time millisecondClockValue].

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMorphicEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	aMorphicEvent isMove ifTrue: [
		self morphPosition: aMorphicEvent eventPosition.
		self sendMouseEvent: aMorphicEvent.
	] ifFalse: [
		"Issue a synthetic move event if we're not at the position of the event"
		aMorphicEvent eventPosition = self morphPosition ifFalse: [
			"Issue a mouse move event to make the receiver appear at the given position"
			self startEventDispatch: (MouseMoveEvent new
				setType: #mouseMove
				position: aMorphicEvent eventPosition
				buttons: aMorphicEvent buttons
				hand: self
				stamp: aMorphicEvent timeStamp) ].
		"Drop submorphs on button events"
		self hasSubmorphs
			ifTrue: [ self dropMorphs: aMorphicEvent ]
			ifFalse: [ self sendMouseEvent: aMorphicEvent ].
	].
	self mouseOverHandler processMouseOver: lastMouseEvent! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 8/19/2012 16:38'!
processMouseOver: aMouseEvent 
	"Re-establish the z-order for all morphs wrt the given event"

	| hand focus evt |
	hand := aMouseEvent hand.
	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	"Now go looking for eventual mouse overs"
	hand startEventDispatch: aMouseEvent asMouseOver.
	"Get out early if there's no change"
	(leftMorphs isNil or: [			"Should never happen, but it could if you halt during layout."
		(leftMorphs isEmpty and: [enteredMorphs position = 0])])
		ifTrue: [^leftMorphs := enteredMorphs := overMorphs := nil].
	focus := hand mouseFocus.
	"Send #mouseLeave as appropriate"
	evt := aMouseEvent asMouseLeave.
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m | 
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [
					evt sentTo: m ]
				ifFalse: [overMorphs nextPut: m]].
	"Send #mouseEnter as appropriate"
	evt := aMouseEvent asMouseEnter.
	enteredMorphs ifNil: [
			"inform: was called in handleEvent:"
			^leftMorphs := enteredMorphs := overMorphs := nil].
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :m | 
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [
					evt sentTo: m ]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			^leftMorphs := enteredMorphs := overMorphs := nil].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 8/19/2012 16:34'!
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	DisplayScreen checkForNewScreenSize.

	"process user input events"
	self handsDo: [ :h |
		ActiveHand _ h.
		hadAnyEvent _ h processEventQueue.
		ActiveHand _ nil
	].

	"the default is the primary hand"
	ActiveHand _ self hands first.

	aWorld runStepMethods.		"there are currently some variations here"
	self displayWorldSafely: aWorld.

	^hadAnyEvent! !

!methodRemoval: HandMorph #handHandleEvent:!
HandMorph removeSelector: #handHandleEvent:!
!methodRemoval: HandMorph #moveToEvent:!
HandMorph removeSelector: #moveToEvent:!
!methodRemoval: HandMorph #processEvents!
HandMorph removeSelector: #processEvents!
!methodRemoval: HandMorph #waitForClicksOrDrag:event:!
HandMorph removeSelector: #waitForClicksOrDrag:event:!
