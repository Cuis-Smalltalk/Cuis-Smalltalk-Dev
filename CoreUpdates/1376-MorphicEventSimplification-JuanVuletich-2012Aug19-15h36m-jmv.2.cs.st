'From Cuis 4.0 of 21 April 2012 [latest update: #1374] on 19 August 2012 at 4:32:20 pm'!

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 16:18'!
handHandleEvent: aMorphicEvent

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
		aMorphicEvent eventPosition = self morphPosition ifFalse: [ self moveToEvent: aMorphicEvent ].
		"Drop submorphs on button events"
		self hasSubmorphs
			ifTrue: [ self dropMorphs: aMorphicEvent ]
			ifFalse: [ self sendMouseEvent: aMorphicEvent ].
	].
	self mouseOverHandler processMouseOver: lastMouseEvent! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 16:28'!
handleFocusEvent: aMorphicEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."
	^aMorphicEvent sentTo: self! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 16:18'!
processEvents
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
				self handHandleEvent: evt.
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

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/19/2012 16:05'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| dropEvent |
	(aMouseEvent isMouseUp and:[aMorph shouldDropOnMouseUp not]) ifTrue: [ ^self ].
	dropEvent _ DropEvent new setPosition: self morphPosition contents: aMorph hand: self.
	owner dispatchEvent: dropEvent.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self mouseOverHandler processMouseOver: aMouseEvent! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 8/19/2012 16:22'!
moveToEvent: aMorphicEvent
	"Issue a mouse move event to make the receiver appear at the given position"

	self handHandleEvent: (MouseMoveEvent new
		setType: #mouseMove
		position: aMorphicEvent eventPosition
		buttons: aMorphicEvent buttons
		hand: self
		stamp: aMorphicEvent timeStamp)! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 8/19/2012 16:08'!
sendMouseEvent: aMouseEvent
	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."

	| w |
	mouseFocus ifNotNil: [
		(w _ mouseFocus world) ifNil: [
			mouseFocus _ nil.
			^self ].
		^self sendFocusEvent: aMouseEvent to: mouseFocus in: w].

	^owner dispatchEvent: aMouseEvent! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 8/19/2012 16:28'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	self dispatchEvent: aMorphicEvent.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [^ aMorphicEvent sentTo: self ].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue:[^self].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [^ aMorphicEvent sentTo: self ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue:[^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue:[^self]].! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 16:29'!
dispatchWith: aMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| inside |
	"See if we're fully outside aMorphs bounds"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition) ifFalse: [ ^#rejected ]. "outside"

	"Traverse children"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Not rejected. The event was in some submorph of the receiver"
				inside _ true
			]]].

	"Check for being inside the receiver"
	inside ifFalse: [ inside _ aMorph containsPoint: self eventPosition event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph ].
	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 16:25'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside |

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Give aMorph a chance to repel the dropping morph"
	aMorph rejectDropEvent: self.
	self wasHandled ifTrue:[^self].

	"Go looking if any of our submorphs wants it"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				inside _ true
			]]].

	inside ifFalse: [ inside _ aMorph containsPoint: self eventPosition event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph ].
	^#rejected! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 16:29'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| globalPt aMorphHandlesIt handledByInner lastHandler answer |

	"Only for MouseDown"
	self isMouseDown
		ifFalse: [ ^super dispatchWith: aMorph ].

	"Try to get out quickly"
	globalPt _ self eventPosition.
	(aMorph morphFullBoundsInWorld containsPoint: globalPt) ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	lastHandler _ eventHandler.
	aMorphHandlesIt _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isPlayfieldLike or: [ self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self)
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: self eventPosition event: self ])
		ifTrue:[
			"aMorph is in the top-most unlocked, visible morph in the chain."
			aMorphHandlesIt ifTrue: [ self sentTo: aMorph ].
			answer _ self ]
		ifFalse: [
			"Mouse was not on aMorph nor any of its children"
			answer _ #rejected ].

	eventHandler _ lastHandler.
	^answer! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 8/19/2012 16:30'!
processMouseOver: aMouseEvent 
	"Re-establish the z-order for all morphs wrt the given event"

	| hand focus evt |
	hand := aMouseEvent hand.
	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	"Now go looking for eventual mouse overs"
	hand handHandleEvent: aMouseEvent asMouseOver.
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


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 16:29'!
dispatchWith: aMorph
	"Host window events do not have a position and are only dispatched to the World"
	aMorph isWorldMorph ifFalse: [ ^#rejected ].
	self wasHandled ifTrue: [ ^self ].
	^ self sentTo: aMorph! !

!methodRemoval: HandMorph #handleEvent:!
HandMorph removeSelector: #handleEvent:!
!methodRemoval: HandMorph #sendEvent:!
HandMorph removeSelector: #sendEvent:!
!methodRemoval: Morph #handleEvent:!
Morph removeSelector: #handleEvent:!
