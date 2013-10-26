'From Cuis 4.0 of 21 April 2012 [latest update: #1379] on 19 August 2012 at 11:13:23 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 22:52'!
handleDropMorph: anEvent localPosition: localEventPosition
	"Handle a dropping morph."
	| aMorph posInWorld |
	aMorph _ anEvent contents.
	"Do a symmetric check if both morphs like each other"
	((self wantsDroppedMorph: aMorph event: anEvent)	"I want her"
		and: [aMorph wantsToBeDroppedInto: self])		"she wants me"
			ifFalse: [
				^ self].
	anEvent wasHandled: true.
	posInWorld _ aMorph referencePosition.
	aMorph referencePosition: posInWorld.
	self acceptDroppingMorph: aMorph event: anEvent.
	aMorph justDroppedInto: self event: anEvent.
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 22:55'!
handleKeyDown: anEvent localPosition: localEventPosition
	"System level event handling."

	anEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	anEvent wasHandled: true.
	^self keyDown: anEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 22:58'!
handleKeyUp: anEvent localPosition: localEventPosition
	"System level event handling."

	anEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	anEvent wasHandled: true.
	^self keyUp: anEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:09'!
handleKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyStroke: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:07'!
handleMouseDown: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent wasHandled ifTrue: [ ^self ]. "not interested"
	anEvent hand removePendingBalloonFor: self.
	anEvent wasHandled: true.
	self activateWindow.

	"Make me modal during mouse transitions"
	anEvent hand newMouseFocus: self event: anEvent.
	anEvent mouseButton3Changed ifTrue: [^self mouseButton3Down: anEvent].

	self mouseDown: anEvent.
	anEvent hand removeHaloFromClick: anEvent on: self.

	(self handlesMouseStillDown: anEvent) ifTrue:[
		self startStepping: #handleMouseStillDown:localPosition:
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {anEvent copy resetHandlerFields . localEventPosition}
			stepTime: self mouseStillDownStepRate ].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:01'!
handleMouseEnter: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent isDraggingEvent ifTrue: [
		^self].
	self wantsBalloon ifTrue: [
		anEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: anEvent) ifTrue: [
		anEvent wasHandled: true.
		self mouseEnter: anEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:02'!
handleMouseLeave: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent hand removePendingBalloonFor: self.
	anEvent isDraggingEvent ifTrue: [
		^self].
	(self handlesMouseOver: anEvent) ifTrue: [
		anEvent wasHandled: true.
		self mouseLeave: anEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:07'!
handleMouseMove: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent wasHandled ifTrue: [^self]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	(anEvent hand hasSubmorphs) ifTrue: [^self].
	(anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]) ifFalse: [^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #handleMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:04'!
handleMouseOver: aMorphicEvent localPosition: localEventPosition
	"System level event handling."
	aMorphicEvent hand mouseFocus == self ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: aMorphicEvent eventPosition event: aMorphicEvent) ifFalse: [
			^self ]].
	aMorphicEvent hand noticeMouseOver: self event: aMorphicEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:07'!
handleMouseStillDown: anEvent localPosition: localEventPosition
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	(anEvent hand mouseFocus == self) 
		ifFalse: [
			^self stopSteppingSelector: #handleMouseStillDown:localPosition: ].
	self mouseStillDown: anEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:10'!
handleMouseUp: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent wasHandled ifTrue: [^self]. "not interested"
	anEvent hand mouseFocus == self ifFalse: [^self]. "Not interested in other parties"
	anEvent hand releaseMouseFocus: self.
	anEvent wasHandled: true.
	anEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: anEvent ]
		ifFalse: [
			self mouseUp: anEvent.
			self stopSteppingSelector: #handleMouseStillDown:localPosition: ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:11'!
handleUnknownEvent: anEvent localPosition: localEventPosition
	"An event of an unknown type was sent to the receiver. What shall we do?!!"

	Beeper beep. 
	anEvent printString displayAt: 0@0.
	anEvent wasHandled: true! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:11'!
handleWindowEvent: anEvent localPosition: localEventPosition
	"Handle an event concerning our host window"

	anEvent wasHandled ifTrue: [^self]. "not interested"
	(self wantsWindowEvent: anEvent) ifFalse: [^self].
	anEvent wasHandled: true.
	self windowEvent: anEvent.
! !


!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 23:00'!
handleMouseDown: aMorphicEvent localPosition: localEventPosition

	super handleMouseDown: aMorphicEvent localPosition: localEventPosition.
	self send: mouseDownSelector withEvent: aMorphicEvent! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 23:04'!
handleMouseMove: aMorphicEvent localPosition: localEventPosition

	super handleMouseMove: aMorphicEvent localPosition: localEventPosition.
	aMorphicEvent anyButtonPressed ifTrue: [
		self send: mouseMoveSelector withEvent: aMorphicEvent ]! !

!HaloHandleMorph methodsFor: 'event handling' stamp: 'jmv 8/19/2012 23:10'!
handleMouseUp: aMorphicEvent localPosition: localEventPosition

	super handleMouseUp: aMorphicEvent localPosition: localEventPosition.
	self send: mouseUpSelector withEvent: aMorphicEvent! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:06'!
handleMouseMove: anEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]) ifFalse:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 22:59'!
handleKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse:	[^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !

!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:07'!
handleMouseMove: anEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(anEvent hand hasSubmorphs) ifTrue:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]) ifFalse:[^self].
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 8/19/2012 23:10'!
handleMouseUp: anEvent localPosition: localEventPosition
	"The handling of control between menu item requires them to act on mouse up even if not the current focus. This is different from the default behavior which really only wants to handle mouse ups when they got mouse downs before"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	anEvent hand releaseMouseFocus: self.
	anEvent wasHandled: true.
	anEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: anEvent ]
		ifFalse: [ self mouseUp: anEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:02'!
handleMouseEnter: anEvent localPosition: localEventPosition
	"System level event handling."
	"Do #mouseEnter: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	self wantsBalloon ifTrue: [
		anEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: anEvent) ifTrue:[
		anEvent wasHandled: true.
		self mouseEnter: anEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:02'!
handleMouseLeave: anEvent localPosition: localEventPosition
	"System level event handling."
	"Do #mouseLeave: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	anEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: anEvent) ifTrue: [
		anEvent wasHandled: true.
		self mouseLeave: anEvent ]! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 23:08'!
handleMouseMove: anEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]) ifFalse:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields . localEventPosition }
			stepTime: 1].
! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 22:37'!
handleFocusEvent: aMorphicEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."

	^aMorphicEvent sentTo: self localPosition: (self internalizeFromWorld: aMorphicEvent eventPosition)! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/19/2012 22:26'!
internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	"Must include scale and rotation!!"
	self flag: #jmvVer2.
	aPoint ifNil: [ ^nil ].	"sacar esto!!"
	^ aPoint - position! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/19/2012 22:36'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	self flag: #jmvVer2.
	aPoint ifNil: [ ^nil ].	"sacar esto!!"
	^(owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ])
			- position! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/19/2012 22:42'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| w p |

	"Never transfer halo to top-most world"
	(self isWorldMorph and:[owner isNil]) ifFalse: [
		(formerHaloOwner ~~ self) 
			ifTrue: [ ^self addHalo: event from: formerHaloOwner ]].

	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [ ^owner transferHalo: event from: formerHaloOwner ].
		"We're at the top level; throw the event back in to find recipient"
		formerHaloOwner removeHalo.
		p _ self internalizeFromWorld: event eventPosition.
		^self dispatchEvent: event copy resetHandlerFields localPosition: p.
	].
	self submorphsDo: [ :m |
		(m fullContainsPoint: event eventPosition) 
			ifTrue: [ ^m transferHalo: event from: formerHaloOwner ].
	].
	"We're at the bottom most level; throw the event back up to the root to find recipient"
	formerHaloOwner removeHalo.
	(w _ self world) ifNil: [ ^self ].
	^w dispatchEvent: event copy resetHandlerFields localPosition: event eventPosition! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 22:29'!
startEventDispatch: aMorphicEvent

	owner ifNil: [ ^ self ].
	aMorphicEvent isMouseOver ifTrue: [ ^ self sendMouseEvent: aMorphicEvent ].
	
	aMorphicEvent isWindowEvent ifTrue: [
		owner dispatchEvent: aMorphicEvent localPosition: aMorphicEvent eventPosition.
		^ self mouseOverHandler processMouseOver: lastMouseEvent ].

	aMorphicEvent isKeyboard ifTrue: [
		self sendKeyboardEvent: aMorphicEvent.
		^ self mouseOverHandler processMouseOver: lastMouseEvent].

	aMorphicEvent isDropEvent ifTrue: [
		owner dispatchEvent: aMorphicEvent localPosition: aMorphicEvent eventPosition.
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

!HandMorph methodsFor: 'private events' stamp: 'jmv 8/19/2012 22:31'!
sendMouseEvent: aMouseEvent
	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."

	| w |
	mouseFocus ifNotNil: [
		(w _ mouseFocus world) ifNil: [
			mouseFocus _ nil.
			^self ].
		^self sendFocusEvent: aMouseEvent to: mouseFocus in: w].

	^owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 8/19/2012 22:39'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	| eventPositionInOurCoordinates |
	eventPositionInOurCoordinates _ self internalizeFromWorld: aMorphicEvent eventPosition.

	self dispatchEvent: aMorphicEvent localPosition: eventPositionInOurCoordinates.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 22:23'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| inside eventPositionInChild |
	"See if we're fully outside aMorphs bounds"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition) ifFalse: [ ^#rejected ]. "outside"

	"Traverse children"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Not rejected. The event was in some submorph of the receiver"
				inside _ true
			]]].

	"Check for being inside the receiver"
	inside ifFalse: [ inside _ aMorph containsPoint: self eventPosition event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph ].
	^ #rejected! !

!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 23:11'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^ aMorph handleUnknownEvent: self localPosition: positionInAMorph! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 22:22'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside eventPositionInChild |

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
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				inside _ true
			]]].

	inside ifFalse: [ inside _ aMorph containsPoint: self eventPosition event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph].
	^#rejected! !

!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 22:54'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^aMorph handleDropMorph: self localPosition: positionInAMorph! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 23:00'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [ ^ aMorph
			handleKeystroke: self
			localPosition: positionInAMorph ].
	type == #keyDown ifTrue: [ ^ aMorph
			handleKeyDown: self
			localPosition: positionInAMorph ].
	type == #keyUp ifTrue: [ ^ aMorph
			handleKeyUp: self
			localPosition: positionInAMorph ].
	^ super
		sentTo: aMorph
		localPosition: positionInAMorph! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 23:05'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	type == #mouseOver ifTrue: [
		^aMorph handleMouseOver: self localPosition: positionInAMorph ].
	type == #mouseEnter ifTrue: [
		^ aMorph handleMouseEnter: self localPosition: positionInAMorph ].
	type == #mouseLeave ifTrue: [
		^aMorph handleMouseLeave: self localPosition: positionInAMorph ].
	^ super sentTo: aMorph localPosition: positionInAMorph! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 22:32'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| globalPt aMorphHandlesIt handledByInner lastHandler answer eventPositionInChild |

	"Only for MouseDown"
	self isMouseDown
		ifFalse: [ ^super dispatchWith: aMorph localPosition: positionInAMorph ].

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
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: self eventPosition event: self ])
		ifTrue:[
			"aMorph is in the top-most unlocked, visible morph in the chain."
			aMorphHandlesIt ifTrue: [ self sentTo: aMorph localPosition: positionInAMorph ].
			answer _ self ]
		ifFalse: [
			"Mouse was not on aMorph nor any of its children"
			answer _ #rejected ].

	eventHandler _ lastHandler.
	^answer! !

!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 23:10'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	type == #mouseDown ifTrue: [
		^aMorph handleMouseDown: self localPosition: positionInAMorph ].
	type == #mouseUp ifTrue: [
		^aMorph handleMouseUp: self localPosition: positionInAMorph ].
	^super sentTo: aMorph localPosition: positionInAMorph! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 23:04'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	type == #mouseMove ifTrue: [
		^aMorph handleMouseMove: self localPosition: positionInAMorph ].
	^ super sentTo: aMorph localPosition: positionInAMorph! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 8/19/2012 22:40'!
processMouseOver: aMouseEvent 
	"Re-establish the z-order for all morphs wrt the given event"

	| hand focus evt p |
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
					p _ m internalizeFromWorld: evt eventPosition.
					evt sentTo: m localPosition: p ]
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
					p _ m internalizeFromWorld: evt eventPosition.
					evt sentTo: m localPosition: p ]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			^leftMorphs := enteredMorphs := overMorphs := nil].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 22:31'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Host window events do not have a position and are only dispatched to the World"

	aMorph isWorldMorph ifFalse: [ ^#rejected ].
	self wasHandled ifTrue: [ ^self ].
	^ self sentTo: aMorph localPosition: positionInAMorph! !

!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 8/19/2012 23:12'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	^ aMorph handleWindowEvent: self localPosition: positionInAMorph! !

!methodRemoval: PluggableListMorph #handleMouseMove:!
PluggableListMorph removeSelector: #handleMouseMove:!
!methodRemoval: MenuItemMorph #handleMouseEnter:!
MenuItemMorph removeSelector: #handleMouseEnter:!
!methodRemoval: MenuItemMorph #handleMouseLeave:!
MenuItemMorph removeSelector: #handleMouseLeave:!
!methodRemoval: MenuItemMorph #handleMouseUp:!
MenuItemMorph removeSelector: #handleMouseUp:!
!methodRemoval: InnerTextMorph #handleKeystroke:!
InnerTextMorph removeSelector: #handleKeystroke:!
!methodRemoval: InnerTextMorph #handleMouseMove:!
InnerTextMorph removeSelector: #handleMouseMove:!
!methodRemoval: HierarchicalListMorph #handleMouseMove:!
HierarchicalListMorph removeSelector: #handleMouseMove:!
!methodRemoval: HaloHandleMorph #handleMouseDown:!
HaloHandleMorph removeSelector: #handleMouseDown:!
!methodRemoval: HaloHandleMorph #handleMouseMove:!
HaloHandleMorph removeSelector: #handleMouseMove:!
!methodRemoval: HaloHandleMorph #handleMouseUp:!
HaloHandleMorph removeSelector: #handleMouseUp:!
!methodRemoval: Morph #handleDropMorph:!
Morph removeSelector: #handleDropMorph:!
!methodRemoval: Morph #handleKeyDown:!
Morph removeSelector: #handleKeyDown:!
!methodRemoval: Morph #handleKeyUp:!
Morph removeSelector: #handleKeyUp:!
!methodRemoval: Morph #handleKeystroke:!
Morph removeSelector: #handleKeystroke:!
!methodRemoval: Morph #handleMouseDown:!
Morph removeSelector: #handleMouseDown:!
!methodRemoval: Morph #handleMouseEnter:!
Morph removeSelector: #handleMouseEnter:!
!methodRemoval: Morph #handleMouseLeave:!
Morph removeSelector: #handleMouseLeave:!
!methodRemoval: Morph #handleMouseMove:!
Morph removeSelector: #handleMouseMove:!
!methodRemoval: Morph #handleMouseOver:!
Morph removeSelector: #handleMouseOver:!
!methodRemoval: Morph #handleMouseStillDown:!
Morph removeSelector: #handleMouseStillDown:!
!methodRemoval: Morph #handleMouseUp:!
Morph removeSelector: #handleMouseUp:!
!methodRemoval: Morph #handleUnknownEvent:!
Morph removeSelector: #handleUnknownEvent:!
!methodRemoval: Morph #handleWindowEvent:!
Morph removeSelector: #handleWindowEvent:!
