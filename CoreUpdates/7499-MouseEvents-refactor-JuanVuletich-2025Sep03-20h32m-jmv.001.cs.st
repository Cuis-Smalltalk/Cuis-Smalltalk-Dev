'From Cuis7.5 [latest update: #7498] on 3 September 2025 at 8:34:22 pm'!

!MouseEvent methodsFor: 'comparing' stamp: 'jmv 9/3/2025 16:15:36'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true! !

!MouseEvent methodsFor: 'comparing' stamp: 'jmv 9/3/2025 16:15:56'!
hash
	^ self class name hash bitXor: (position hash bitXor: buttons hash)! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:17:48'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph."

	self isMouseDown ifTrue: [
		aMorph processMouseDown: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
	self isMouseUp ifTrue: [
		aMorph processMouseUp: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].! !


!MouseEnterEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:16:39'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseLeaveEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:16:35'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseMoveEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:16:30'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseOverEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:16:20'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseScrollEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:16:25'!
setPosition: evtPos direction: evtDir buttons: evtButtons hand: evtHand stamp: stamp

	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	direction := evtDir.
	timeStamp := stamp.! !

!methodRemoval: MouseEvent #eventType stamp: 'jmv 9/3/2025 20:33:54'!
MouseEvent removeSelector: #eventType!
