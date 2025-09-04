'From Cuis7.5 [latest update: #7519] on 4 September 2025 at 4:40:30 pm'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/4/2025 16:07:29'!
processMouseOver: aMouseEvent
	"Re-establish the z-order for all morphs wrt the given event"

	| hand focus evt |
	hand := aMouseEvent hand.
	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	"Now go looking for eventual mouse overs"
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


!HandMorph methodsFor: 'event handling' stamp: 'jmv 9/4/2025 16:07:16'!
processEventQueue
	"Process user input events from the local input devices.
	Answer true if any event was handled (but ignore MouseMove)"

	| evt evtBuf type hadAny mcs hadAnyMouseEvent hadAnyMouseMoveEvent |
	mcs := mouseClickState.
	hadAny := false.
	hadAnyMouseEvent := false.
	hadAnyMouseMoveEvent := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type := evtBuf first.
		evt := self createEventFrom: evtBuf ofType: type.
		evt ifNotNil: [
			"Finally, handle it. But process only up to one mouseMove per cycle. Discard the rest."
			(evt isMouseMove and: [ hadAnyMouseMoveEvent ]) ifFalse: [
				evt startDispatchFromHand: self.
				(evt isMouse or: [ evt isKeyboard ]) ifTrue: [
					hadAny := true ].
				evt isMouse ifTrue: [
					hadAnyMouseEvent := true.
					evt isMouseMove ifTrue: [
						hadAnyMouseMoveEvent := true ]]]]].
	hadAnyMouseEvent ifFalse: [
		mcs 
			ifNotNil: [ 
				"No mouse events during this cycle. Make sure click states time out accordingly"
				mcs handleEvent: lastMouseEvent asMouseMove from: self ]].
	^hadAny! !

