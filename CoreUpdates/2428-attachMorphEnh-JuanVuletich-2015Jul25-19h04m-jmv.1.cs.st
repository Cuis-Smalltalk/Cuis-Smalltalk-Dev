'From Cuis 4.2 of 25 July 2013 [latest update: #2424] on 25 July 2015 at 7:27:17.463633 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/25/2015 19:21'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		h target == aMorph ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		grabMorphData
			at: aMorph 
			put: { formerOwner. aMorph morphPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.
	aMorph justGrabbedFrom: formerOwner! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/25/2015 19:27'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed delta |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].

	delta _ (moveUnderHand not and: [grabbed == aMorph or: [ grabbed morphExtent > aMorph morphExtent ]])
		ifTrue: [ aMorph morphPositionInWorld - self morphPositionInWorld ]
		ifFalse: [ (grabbed morphExtent // 2) negated ].
	^ self
		grabMorph: grabbed
		delta: delta! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/25/2015 19:25'!
attachMorph: aMorph
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."

	^self grabMorph: aMorph moveUnderHand: true! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/25/2015 19:25'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	^self grabMorph: aMorph moveUnderHand: false! !

!methodRemoval: HandMorph #grabMorph:from:at:!
HandMorph removeSelector: #grabMorph:from:at:!

!HandMorph reorganize!
('accessing' lastMouseEvent lastMouseEventTime mouseOverHandler)
('balloon help' balloonHelp balloonHelp: deleteBalloonTarget: removePendingBalloonFor: spawnBalloonFor: triggerBalloonFor:after:)
('caching' releaseCachedState)
('classification' is:)
('double click support' dontWaitForMoreClicks waitForClicksOrDrag:event:clkSel:dblClkSel: waitForClicksOrDrag:event:dragSel:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel:)
('drawing' drawOn: fullDrawHandOn: hasChanged needsToBeDrawn restoreSavedPatchOn: savePatchFrom:appendDamageTo:)
('event handling' flushEvents noticeMouseOver:event: processEventQueue)
('events-processing' startDropEventDispatch: startEventDispatch: startKeyboardDispatch: startMouseDispatch: startWindowEventDispatch:)
('focus handling' activateNextWindow activatePreviousWindow keyboardFocus keyboardFocusNext keyboardFocusPrevious mouseFocus newKeyboardFocus: newMouseFocus: nextFocusMorph nextFocusWindow previousFocusMorph previousFocusWindow releaseAllFoci releaseKeyboardFocus releaseKeyboardFocus: releaseMouseFocus releaseMouseFocus:)
('grabbing/dropping' attachMorph: dropMorph:event: dropMorphs: grabMorph: grabMorph:delta: grabMorph:moveUnderHand:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('updating' redrawNeeded)
('private' forgetGrabMorphDataFor: grabMorphDataFor: rememberGrabMorphDataFor:)
('private events' generateKeyboardEvent: generateMouseEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
!

