'From Cuis 4.1 of 12 December 2012 [latest update: #1569] on 19 January 2013 at 4:15:32 pm'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 16:12'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self."

	^true! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/19/2013 16:09'!
processDropMorph: aDropEvent localPosition: localEventPosition
	"Handle a dropping morph."
	| aMorph posInWorld |
	
	aDropEvent wasHandled ifTrue: [ ^self ]. "Do it just once, for one drop destination"
	
	aMorph _ aDropEvent contents.
	aDropEvent wasHandled: true.
	posInWorld _ aMorph referencePosition.
	aMorph referencePosition: posInWorld.
	self acceptDroppingMorph: aMorph event: aDropEvent.
	aMorph justDroppedInto: self event: aDropEvent! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 1/19/2013 16:15'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsPoint:localEventPosition ]) 
		ifFalse: [ ^self deleteIfPopUp: aMouseButtonEvent ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 1/19/2013 15:55'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner eventPositionInChild |
	"See if we're fully outside aMorphs bounds"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition) ifFalse: [
		^#rejected ]. "outside"

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	"Check for being inside the receiver"
	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 1/19/2013 16:06'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."
	| eventPositionInChild dropped |

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop and: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [
						^ self sentTo: aMorph localPosition: positionInAMorph ]].
	^#rejected! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 15:44'!
acceptDroppingMorph: aMorph event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	self isWorldMorph 
		ifTrue: [	
			"Add the given morph to this world and start stepping it if it wants to be."
			self addMorphFront: aMorph.
			(aMorph morphFullBoundsInWorld intersects: self viewBox) 
				ifFalse: [
					Beeper beep.
					aMorph morphPosition: extent // 2]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m is: #HaloMorph) ifTrue: [ m delete ]].
	self world startSteppingSubmorphsOf: aMorph! !

!methodRemoval: PasteUpMorph #morphToDropFrom:!
PasteUpMorph removeSelector: #morphToDropFrom:!
!methodRemoval: PasteUpMorph #repelsMorph:event:!
PasteUpMorph removeSelector: #repelsMorph:event:!
!methodRemoval: PasteUpMorph #wantsDroppedMorph:event:!
PasteUpMorph removeSelector: #wantsDroppedMorph:event:!
!methodRemoval: Morph #repelsMorph:event:!
Morph removeSelector: #repelsMorph:event:!
