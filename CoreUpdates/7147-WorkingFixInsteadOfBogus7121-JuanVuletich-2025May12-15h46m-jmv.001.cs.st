'From Cuis7.3 [latest update: #7146] on 12 May 2025 at 4:19:34 pm'!

!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 5/12/2025 16:16:27'!
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
		m isInWorld and: [
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [
					evt sendEventTo: m ]
				ifFalse: [overMorphs nextPut: m]]].
	"Send #mouseEnter as appropriate"
	evt := aMouseEvent asMouseEnter.
	enteredMorphs ifNil: [
		"inform: was called in handleEvent:"
		^ leftMorphs := enteredMorphs := overMorphs := nil].
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :m |
		m isInWorld and: [
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [
					evt sendEventTo: m ]]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			^leftMorphs := enteredMorphs := overMorphs := nil].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 7/13/2020 16:07:54'!
rejectsEventFully: aMorphicEvent
	"Return true to reject the given event, for thereceiver and all submorphs."

	^ self isLocked or: [ self visible not ]! !

