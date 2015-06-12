'From Cuis 4.2 of 25 July 2013 [latest update: #2343] on 9 June 2015 at 8:16:07.608938 pm'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds grabMorphData '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!HandMorph commentStamp: '<historical>' prior: 0!
The cursor may be thought of as the HandMorph.  The hand's submorphs hold anything being carried by dragging.  

There is some minimal support for multiple hands in the same world.

grabMorphData is a dictionary of Morph->{prevOwner,prevPos} for morphs being carried.!

!classDefinition: #DropEvent category: #'Morphic-Events'!
MorphicEvent subclass: #DropEvent
	instanceVariableNames: 'position contents wasHandled formerOwner formerPosition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!DropEvent commentStamp: '<historical>' prior: 0!
I remember data for drop events so the Morphs involved in the drop don't have to.
!


!Morph methodsFor: 'accessing - properties' stamp: 'KenD 6/6/2015 20:03'!
nameForWorkspace
	"Answer a name suitable for a Workspace variable"
	| displayName |
	displayName := self name.
	^ displayName 
		ifNotNil: [ displayName ] 
		ifNil: [ self class name asLowercase , self identityHash asString ]! !


!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'KenD 6/7/2015 06:47'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ true! !

!WorkspaceWindow methodsFor: 'drag n drop' stamp: 'KenD 6/9/2015 19:58'!
wantsDroppedMorph: aMorph event: evt
	"Dropping a morph on my window means:
  	    display exteral name for aMorph in Workspace
	    add a binding from name to aMorph
	    and rejecting the drop."
	
	| morphName textModelMorph |
	morphName := aMorph nameForWorkspace.
	textModelMorph := self layoutMorph submorphs at: 1.
	
	(self model  bindingOf: morphName) value: aMorph.
	textModelMorph editor afterSelectionInsertAndSelect: morphName , ' '.
	"send aMorph back to original position"
	evt isNil ifFalse: [ evt wasHandled: true ]. 
	"Short circuit"
	aMorph rejectDropMorphEvent: evt.
	
	^ false ! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'KenD 6/9/2015 19:32'!
grabMorph: aMorph from: formerOwner
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	| morphPos |
	self releaseMouseFocus. "Break focus"
	morphPos := aMorph morphPositionInWorld.
	self grabMorphDataDictionary 
		at: aMorph 
		put: { formerOwner. morphPos. }.
	self addMorphBack: aMorph position: morphPos - self morphPositionInWorld.
	aMorph justGrabbedFrom: formerOwner.
! !

!HandMorph methodsFor: 'private' stamp: 'KenD 6/9/2015 14:52'!
forgetGrabMorphDataFor: aMorph

	self grabMorphDataDictionary 
		removeKey: aMorph 
		ifAbsent: [ "no error" ]! !

!HandMorph methodsFor: 'private' stamp: 'KenD 6/9/2015 19:10'!
grabMorphDataDictionary

	grabMorphData ifNil: [ grabMorphData := IdentityDictionary new ].
	
	^ grabMorphData ! !

!HandMorph methodsFor: 'private' stamp: 'KenD 6/9/2015 19:13'!
grabMorphDataFor: aMorph

	^ self grabMorphDataDictionary at: aMorph ifAbsent: [ { nil. nil. } ]! !

!HandMorph methodsFor: 'private' stamp: 'KenD 6/9/2015 19:32'!
rememberGrabMorphDataFor: aMorph

	self grabMorphDataDictionary 
		at: aMorph 
		put: { aMorph owner. aMorph morphPositionInWorld. }! !


!DropEvent methodsFor: 'accessing' stamp: 'KenD 6/9/2015 15:29'!
formerOwner
	
	^ formerOwner ! !

!DropEvent methodsFor: 'accessing' stamp: 'KenD 6/9/2015 15:27'!
formerPosition
	
	^ formerPosition ! !

!DropEvent methodsFor: 'initialization' stamp: 'KenD 6/9/2015 15:29'!
setPosition: pos contents: aMorph hand: aHand formerOwner: oldOwner formerPosition: oldPos
	position _ pos.
	contents _ aMorph.
	source _ aHand.
	formerOwner := oldOwner.
	formerPosition := oldPos.
	wasHandled _ false.! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'KenD 6/9/2015 20:02'!
rejectDropMorphEvent: dropEvent
	"The receiver has been rejected, and must be put back somewhere.
	 If the original owner and position are known, use them, 
	else Just keep it in the hand"

	((dropEvent formerOwner isNil) or: [ dropEvent formerPosition isNil ])
		ifTrue: [ dropEvent hand grabMorph: self.
		]
		ifFalse: [ dropEvent formerOwner addMorph: self.
				   self morphPositionInWorld: dropEvent formerPosition.
		]
		! !


!HaloMorph methodsFor: 'private' stamp: 'KenD 6/9/2015 20:15'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	| h |
	h _ evt hand.
	h obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	target _ target duplicateMorph: evt.
	h grabMorph: target.
	self updatePositionAndExtent.	"update position if necessary"! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'KenD 6/9/2015 14:56'!
attachMorph: aMorph
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self rememberGrabMorphDataFor: aMorph.
	delta _ aMorph morphExtent // 2.
	self addMorphBack: aMorph position: delta negated! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'KenD 6/9/2015 19:36'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent _ DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	owner dispatchEvent: dropEvent localPosition: dropEvent eventPosition.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.
	self mouseOverHandler processMouseOver: aMouseEvent! !

!HandMorph methodsFor: 'initialization' stamp: 'KenD 6/9/2015 14:36'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	grabMorphData := IdentityDictionary new.
	self initForEvents.! !

!HandMorph methodsFor: 'meta-actions' stamp: 'KenD 6/9/2015 20:13'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	| grabbed  |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	^ self
		grabMorph: grabbed
		from: aMorph owner
! !

!HandMorph methodsFor: 'meta-actions' stamp: 'KenD 6/9/2015 19:29'!
grabMorph: aMorph positionInWorld: posOrNil
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand.
	Use posOrNil if specified."
	| grabbed  |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	^ self
		grabMorph: grabbed
		from: aMorph owner
! !

!classDefinition: #DropEvent category: #'Morphic-Events'!
MorphicEvent subclass: #DropEvent
	instanceVariableNames: 'position contents wasHandled formerOwner formerPosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!DropEvent reorganize!
('accessing' contents eventPosition formerOwner formerPosition wasHandled wasHandled:)
('dispatching' dispatchWith:localPosition: sentTo:localPosition: startDispatchFrom:)
('initialization' setPosition:contents:hand: setPosition:contents:hand:formerOwner:formerPosition:)
('testing' isDropEvent)
!

!methodRemoval: HandMorph #grabMorph:from:at:!
HandMorph removeSelector: #grabMorph:from:at:!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds grabMorphData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

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
('grabbing/dropping' attachMorph: dropMorph:event: dropMorphs: grabMorph:from:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('meta-actions' grabMorph: grabMorph:positionInWorld:)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('updating' redrawNeeded)
('private' forgetGrabMorphDataFor: grabMorphDataDictionary grabMorphDataFor: rememberGrabMorphDataFor:)
('private events' generateKeyboardEvent: generateMouseEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
!


!WorkspaceWindow reorganize!
('menu' offerWindowMenu)
('drag n drop' allowsMorphDrop wantsDroppedMorph:event:)
!

