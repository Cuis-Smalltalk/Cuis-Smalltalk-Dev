Class {
	#name : 'MouseOverEvent',
	#superclass : 'MouseEvent',
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'dispatching' }
MouseOverEvent >> dispatchIn: aWorldMorphOrFocusedMorph [
	"Dispatch me.
	The event will be passed to all morphs at event position, in strict front to back order.
	This event is not marked with `wasHandled: true`, so it is passed to multiple recipients
	across the owner chain.."

	| handlerMorph |
	handlerMorph := aWorldMorphOrFocusedMorph topUnlockedMorphAt: position.
	(handlerMorph is: #HaloMorph) ifTrue: [ handlerMorph := handlerMorph target ].
	[ handlerMorph notNil ] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			self sendEventTo: handlerMorph ].
		"If we call #morphBehindAt: instead, MouseEnterEvent and MouseLeaveEvent are sent to morphs
		unrelated to the #topUnlockedMorphAt:, but that happen to be behind it."
		"handlerMorph := handlerMorph morphBehindAt: position."
		handlerMorph := handlerMorph owner ].
]

{ #category : 'testing' }
MouseOverEvent >> isMouseOver [
	^true
]

{ #category : 'dispatching' }
MouseOverEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseOver: self localPosition: (aMorph internalizeFromWorld: position).
]

{ #category : 'private' }
MouseOverEvent >> setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp [

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.
]
