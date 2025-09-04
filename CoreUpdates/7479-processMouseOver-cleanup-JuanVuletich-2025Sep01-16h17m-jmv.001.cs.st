'From Cuis7.5 [latest update: #7476] on 1 September 2025 at 4:21:55 pm'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/1/2025 16:01:03'!
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
				ifTrue: [
					evt sendEventTo: m ]
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
				ifTrue: [
					evt sendEventTo: m ]]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/1/2025 16:04:09'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent ].
		^self].

	"any mouse event but mouseOver"
	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			self mouseOverHandler processMouseOver: lastMouseEvent.
			^self ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			owner dispatchEvent: aMouseEvent
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
					mouseClickState ifNil: [
						"Want to drop on mouseUp, NOT mouseDown"
						aMouseEvent isMouseUp ifTrue: [
							self dropMorphs: aMouseEvent ]
					]
				] ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

