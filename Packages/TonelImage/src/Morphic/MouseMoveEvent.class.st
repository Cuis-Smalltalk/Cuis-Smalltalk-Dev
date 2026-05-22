Class {
	#name : 'MouseMoveEvent',
	#superclass : 'MouseEvent',
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'comparing' }
MouseMoveEvent >> = aMorphicEvent [

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true
]

{ #category : 'comparing' }
MouseMoveEvent >> hash [
	^ position hash bitXor: buttons hash
]

{ #category : 'testing' }
MouseMoveEvent >> isMouseMove [
	^true
]

{ #category : 'dispatching' }
MouseMoveEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseMove: self localPosition: (aMorph internalizeFromWorld: position).
]

{ #category : 'private' }
MouseMoveEvent >> setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp [

	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.
]

{ #category : 'dispatching' }
MouseMoveEvent >> startDispatchFromHand: aHand [
	"double dispatch the event dispatch"

	aHand startMouseMoveDispatch: self
]
