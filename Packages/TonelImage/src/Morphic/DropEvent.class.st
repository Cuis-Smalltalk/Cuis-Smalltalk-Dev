"
I remember data for drop events so the Morphs involved in the drop don't have to.

"
Class {
	#name : 'DropEvent',
	#superclass : 'MorphicEvent',
	#instVars : [
		'position',
		'contents',
		'wasHandled',
		'formerOwner',
		'formerPosition'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'accessing' }
DropEvent >> contents [
	^contents
]

{ #category : 'dispatching' }
DropEvent >> dispatchIn: aWorldMorph [
	"Drop is done on the innermost target that accepts it.
	If #rejectsEvent:, any party doesn't want the drop,or #wasHandled not by any reason, try with the owner."

	| topUnlockedMorph handlerMorph dropped |
	topUnlockedMorph := aWorldMorph topUnlockedMorphAt: position.
	handlerMorph := topUnlockedMorph.
	[ handlerMorph notNil and: [wasHandled not]] whileTrue: [
		((handlerMorph rejectsEvent: self) not and: [handlerMorph fullIncludesPixel: position]) ifTrue: [
			handlerMorph allowsMorphDrop ifTrue: [
				"Do a symmetric check if both morphs like each other"
				dropped := self contents.
				((handlerMorph wantsDroppedMorph: dropped event: self)		"I want her"
					and: [dropped wantsToBeDroppedInto: handlerMorph])		"she wants me"
						ifTrue: [ self sendEventTo: handlerMorph ]].
		].
		handlerMorph := handlerMorph owner.
	].
	wasHandled
		ifFalse: [
			topUnlockedMorph ifNotNil: [ topUnlockedMorph invalidDrop: self ].
			(self contents is: #DraggingGuideMorph) ifTrue: [
				self wasHandled: true.
				self contents delete ]].
]

{ #category : 'accessing' }
DropEvent >> eventPosition [
	^position
]

{ #category : 'accessing' }
DropEvent >> formerOwner [
	
	^ formerOwner 
]

{ #category : 'accessing' }
DropEvent >> formerPosition [
	
	^ formerPosition 
]

{ #category : 'testing' }
DropEvent >> isDropEvent [
	^true
]

{ #category : 'dispatching' }
DropEvent >> sendEventTo: aMorph [
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	aMorph processDropMorph: self
]

{ #category : 'initialization' }
DropEvent >> setPosition: pos contents: aMorph hand: aHand formerOwner: oldOwner formerPosition: oldPos [
	position := pos.
	contents := aMorph.
	source := aHand.
	formerOwner := oldOwner.
	formerPosition := oldPos.
	wasHandled := false.
]

{ #category : 'accessing' }
DropEvent >> wasHandled [
	^wasHandled
]

{ #category : 'accessing' }
DropEvent >> wasHandled: aBool [

	wasHandled := aBool
]
