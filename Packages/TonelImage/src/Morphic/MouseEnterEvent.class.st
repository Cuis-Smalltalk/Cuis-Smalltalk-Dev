Class {
	#name : 'MouseEnterEvent',
	#superclass : 'MouseEvent',
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'dispatching' }
MouseEnterEvent >> dispatchIn: neverMind [
	"We are sent directly to recipient via #sendEventTo:"

	self shouldNotHappen
]

{ #category : 'dispatching' }
MouseEnterEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseEnter: self localPosition: (aMorph internalizeFromWorld: position).
]

{ #category : 'private' }
MouseEnterEvent >> setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp [

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.
]
