'From Cuis 4.2 of 25 July 2013 [latest update: #2392] on 30 June 2015 at 9:47:53.361103 am'!

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 6/30/2015 09:38'!
popUpFor: aMorph event: aMorphicEvent
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ aMorphicEvent
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastMouseEvent ]
				ifNotNil: [
					hand _ aMorphicEvent hand.
					aMorphicEvent ].
	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ anEvent eventPosition - aMorph morphPositionInWorld! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/30/2015 09:33'!
doDrag: evt with: dragHandle
	| thePoint |
	evt hand obtainHalo: self.
	thePoint _ evt eventPosition - positionOffset.
	target morphPositionInWorld: thePoint.
	self morphPositionInWorld: thePoint.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/30/2015 09:42'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	target _ target duplicateMorph: evt.
	evt hand
		obtainHalo: self;
		grabMorph: target! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/30/2015 09:35'!
doGrab: evt with: grabHandle
	"Ask hand to grab my target."

	evt hand
		obtainHalo: self;
		grabMorph: target! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 6/30/2015 09:47'!
grabMorph: aMorph from: formerOwner
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	| morphPos |
	self releaseMouseFocus. "Break focus"
	morphPos := aMorph morphPositionInWorld.
	grabMorphData
		at: aMorph 
		put: { formerOwner. morphPos. }.
	self addMorphBack: aMorph position: morphPos - self morphPositionInWorld.
	self halo ifNotNil: [:h |
		h target == aMorph ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].
	aMorph justGrabbedFrom: formerOwner! !

!methodRemoval: HaloMorph #stepAt:!
HaloMorph removeSelector: #stepAt:!
!methodRemoval: HaloMorph #stepTime!
HaloMorph removeSelector: #stepTime!
!methodRemoval: HaloMorph #updatePositionAndExtent!
HaloMorph removeSelector: #updatePositionAndExtent!

!HaloMorph reorganize!
('accessing' target target:)
('drawing' drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:event: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' containsPoint:event: rejectsEvent:)
('geometry testing' isOrthoRectangularMorph morphContainsPoint:)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addFontEmphHandle: addFontSizeHandle: addGrabHandle: addGrowHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addRotateHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor initialize)
('testing' is:)
('private' addHandle: addHandles addNameString: basicBox doDebug:with: doDrag:with: doDup:with: doGrab:with: doGrow:with: doMenu:with: doRecolor:with: doRot:with: endInteraction maybeCollapse:with: maybeDismiss:with: maybeDoDup:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startGrow:with: startRot:with:)
('forward to target' chooseEmphasisOrAlignment chooseFont deleteBalloon mouseDownOnHelpHandle:)
!

