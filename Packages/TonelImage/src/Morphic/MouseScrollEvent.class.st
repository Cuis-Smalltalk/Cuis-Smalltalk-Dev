"
A MouseScrollEvent can be any type of secondary pointer movement (typically via a scroll wheel on a traditional mouse or a gesture on a trackpad).  Currently, events are extracted from KeyboardEvents (which is how the VM currently communicates things like scroll wheel events via ctl+arrow up/down).

It is also possible to generate these events with a keyboard, pressing ctrl-down or ctrl-up. Given this, we also added ctrl-left and ctrl-right, that can only be generated with a keyboard, to control horizontal scroll.
"
Class {
	#name : 'MouseScrollEvent',
	#superclass : 'MouseEvent',
	#instVars : [
		'direction',
		'delta'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'comparing' }
MouseScrollEvent >> = aMorphicEvent [

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	direction = aMorphicEvent direction ifFalse: [ ^ false ].
	^ true
]

{ #category : 'accessing' }
MouseScrollEvent >> delta [
	^ delta
]

{ #category : 'accessing' }
MouseScrollEvent >> direction [
	^ direction 
]

{ #category : 'comparing' }
MouseScrollEvent >> hash [
	^ position hash bitXor: (buttons hash bitXor: direction hash)
]

{ #category : 'testing' }
MouseScrollEvent >> isMouseScroll [
	^ true
]

{ #category : 'dispatching' }
MouseScrollEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processMouseScroll: self localPosition: (aMorph internalizeFromWorld: position).
]

{ #category : 'private' }
MouseScrollEvent >> setPosition: evtPos direction: evtDir buttons: evtButtons hand: evtHand stamp: stamp [
	self setPosition: evtPos direction: evtDir delta: nil buttons: evtButtons hand: evtHand stamp: stamp
]

{ #category : 'private' }
MouseScrollEvent >> setPosition: evtPos direction: evtDir delta: deltaPoint buttons: evtButtons hand: evtHand stamp: stamp [
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	direction := evtDir.
	delta := deltaPoint.
	timeStamp := stamp.
]
