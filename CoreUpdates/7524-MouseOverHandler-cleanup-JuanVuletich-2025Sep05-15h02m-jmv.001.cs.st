'From Cuis7.5 [latest update: #7523] on 5 September 2025 at 3:03:52 pm'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/5/2025 13:23:41'!
dispatchMouseOverEvent: aMouseEvent
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	| hand focus evt |
	hand := aMouseEvent hand.

	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	
	"Now go looking for eventual mouse overs.
	If this MouseOver gets delivered to some morph, it will end calling #noticeMouseOver:event:,
	updating our instance variables."
	aMouseEvent asMouseOver startDispatchFromHand: hand.
	
	"Get out early if there's no change"
	(leftMorphs isNil or: [			"Should never happen, but it could if you halt during layout."
		(leftMorphs isEmpty and: [enteredMorphs position = 0])])
		ifTrue: [
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self].
	
	focus := hand mouseFocus.
	"Send #mouseLeave as appropriate"
	evt := aMouseEvent asMouseLeave.
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m |
		m isInWorld and: [
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [evt sendEventTo: m ]
				ifFalse: [overMorphs nextPut: m]]].
	"Send #mouseEnter as appropriate"
	evt := aMouseEvent asMouseEnter.
	enteredMorphs ifNil: [
		"inform: was called in handleEvent:"
		leftMorphs := enteredMorphs := overMorphs := nil.
		^self].
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :m |
		m isInWorld and: [
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [evt sendEventTo: m ]]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/5/2025 13:05:53'!
dispatchMouseOverEvent
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	self mouseOverHandler dispatchMouseOverEvent: lastMouseEvent.! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/5/2025 12:53:41'!
startMouseButtonDispatch: aMouseEvent

	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			aMouseEvent wasHandled: true.
			^self ]].

	"Issue a synthetic move event if we're not at the position of the event"
	aMouseEvent eventPosition = self morphPosition ifFalse: [
		"Issue a mouse move event to make the receiver appear at the given position"
		(MouseMoveEvent new
			setPosition: aMouseEvent eventPosition
			buttons: aMouseEvent buttons
			hand: self
			stamp: aMouseEvent timeStamp) startDispatchFromHand: self ].

	"Drop submorphs on button events"
	self hasSubmorphs
		ifTrue: [
			"Not if we are grabbing them"
			mouseClickState ifNil: [
				"Want to drop on mouseUp, NOT mouseDown"
				aMouseEvent isMouseUp ifTrue: [
					self dropMorphs: aMouseEvent ]]]
		ifFalse: [
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]].
		
	self dispatchMouseOverEvent.! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/5/2025 12:53:51'!
startMouseMoveDispatch: aMouseEvent

	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		mouseClickState handleEvent: aMouseEvent from: self ].

	self morphPosition: aMouseEvent eventPosition.
	self mouseFocus
		ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
		ifNil: [ aMouseEvent dispatchIn: owner ].
		
	self dispatchMouseOverEvent.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
MouseOverHandler removeSelector: #processMouseOver:!

