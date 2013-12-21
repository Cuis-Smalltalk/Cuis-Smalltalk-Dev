'From Cuis 4.2 of 25 July 2013 [latest update: #1880] on 21 December 2013 at 9:00:35.237948 am'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds morphInHandOrResizing '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 12/20/2013 20:27'!
reverseRectangleBorder: r borderWidth: borderWidth
	"
	Display getCanvas
		reverseRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20
	"
	| rect |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	port
		sourceForm: nil;
		fillColor: Color gray;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 12/21/2013 08:51'!
isUpdateCycleSlow
	"Answer true if Morphic is getting too slow, and FPS is dropping."

	^worldState isUpdateCycleSlow! !


!HandMorph methodsFor: 'accessing' stamp: 'jmv 12/21/2013 08:22'!
morphInHandOrResizing

	^morphInHandOrResizing! !

!HandMorph methodsFor: 'accessing' stamp: 'jmv 12/21/2013 08:16'!
morphInHandOrResizing: aMorphOrNil

	morphInHandOrResizing _ aMorphOrNil! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 12/21/2013 08:49'!
isUpdateCycleSlow
	"Answer true if Morphic is getting too slow, and FPS is dropping."

	^Time localMillisecondClock-lastCycleTime > (pause*2)! !


!GrafPort methodsFor: 'drawing support' stamp: 'jmv 12/20/2013 20:26'!
frameRect: rect borderWidth: borderWidth
	"Paint a border whose rectangular area is defined by rect. The
	width of the border of each side is borderWidth."

	rect area = 0 ifTrue: [^ self].

	sourceX _ 0.
	sourceY _ 0.
	
	"for top and bottom, the following are the same"
	height _ borderWidth. 
	width _ rect width. 
	destX _ rect left.

	"top"
	destY _ rect top.
	self copyBits.

	"bottom"
	destY _ rect bottom - borderWidth.
	self copyBits.

	"for left & right, the following are the same"
	height _ rect height-borderWidth-borderWidth.
	width _ borderWidth. 
	destY _ rect top+borderWidth.

	"left"
	destX _ rect left.
	self copyBits.

	"right"
	destX _ rect right - borderWidth.
	self copyBits! !


!Morph methodsFor: 'drawing' stamp: 'jmv 12/21/2013 09:00'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"
	"Draw receiver itself"
	| h |
	h _ self world activeHand.
	(h notNil and: [
			h morphInHandOrResizing == self and: [ self world isUpdateCycleSlow ]])
		ifTrue: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
				owner == h
					ifTrue: [
						aCanvas
							reverseRectangleBorder: (0 @ 0 extent: self morphExtent)
							borderWidth: 2 ]
					ifFalse: [ self drawOn: aCanvas ]]]
		ifFalse: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [ self drawOn: aCanvas ].
			self drawSubmorphsOn: aCanvas ]! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/21/2013 08:19'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| dropEvent |
	dropEvent _ DropEvent new setPosition: self morphPosition contents: aMorph hand: self.
	owner dispatchEvent: dropEvent localPosition: dropEvent eventPosition.
	dropEvent wasHandled
		ifTrue: [ self morphInHandOrResizing: nil ]
		ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self mouseOverHandler processMouseOver: aMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/21/2013 08:17'!
grabMorph: aMorph from: formerOwner at: aPoint
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	self releaseMouseFocus. "Break focus"
	self addMorphBack: aMorph.
	self morphInHandOrResizing: aMorph.
	aMorph morphPosition: aPoint.
	aMorph justGrabbedFrom: formerOwner.! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 12/21/2013 08:41'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self cursor show.
	hand _ aMouseButtonEvent hand.
	self startStepping.
	hand morphInHandOrResizing: owner! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 12/21/2013 08:44'!
mouseLeave: anEvent
	super mouseLeave: anEvent.
	
	hand morphInHandOrResizing: nil.
	hand _ nil.
	Cursor normal show! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 12/21/2013 08:42'!
step
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p ]
		ifFalse: [
			hand morphInHandOrResizing: nil.
			owner redrawNeeded.
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal show ]]! !

!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds morphInHandOrResizing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
