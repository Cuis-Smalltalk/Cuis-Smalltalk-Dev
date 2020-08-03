'From Cuis 5.0 [latest update: #4319] on 31 July 2020 at 3:42:14 pm'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds grabMorphData lastPosition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/31/2020 15:24:00'!
displayBoundsOrBogus
	"Never answer nil. A bogus value will trigger a draw operation, and bounds will later be corrected."
	^self displayBounds ifNil: [
		Rectangle center: (self externalizeToWorld: `0@0`) extent: `2@2` ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/31/2020 15:41:33'!
fullDrawHandOn: aCanvas
	"A HandMorph has an unusual drawing requirement:
		The hand itself (i.e., the cursor) appears in front of its submorphs
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"
	lastPosition _ submorphs notEmpty ifTrue: [self morphPosition].! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/31/2020 15:41:15'!
displayFullBounds	
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around)."
	| fullBounds |
	fullBounds _ self displayBounds.
	self submorphsDo: [ :m |
		fullBounds _ fullBounds quickMerge: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around."
			fullBounds _ fullBounds translatedBy: self morphPosition-lastPos.
			fullBounds _ fullBounds quickMerge: self displayBounds ].
	^fullBounds! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 7/31/2020 15:41:42'!
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
	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		lastPosition _ self morphPosition ].! !

!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds grabMorphData lastPosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
