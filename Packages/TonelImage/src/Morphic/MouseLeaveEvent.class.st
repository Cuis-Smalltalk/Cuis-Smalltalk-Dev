Class {
	#name : 'MouseLeaveEvent',
	#superclass : 'MouseEvent',
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'dispatching' }
MouseLeaveEvent >> dispatchIn: neverMind [
	"We are sent directly to recipient via #sendEventTo:"

	self shouldNotHappen
]

{ #category : 'dispatching' }
MouseLeaveEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseLeave: self localPosition: (aMorph internalizeFromWorld: position).
]

{ #category : 'private' }
MouseLeaveEvent >> setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp [

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.
]
