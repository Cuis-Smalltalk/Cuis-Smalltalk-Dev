'From Cuis 5.0 [latest update: #4451] on 17 November 2020 at 9:25:23 am'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 11/16/2020 23:00:04'!
invalidateDisplayRect: damageRect for: aMorph
	"
	If we clip submorphs, then we clip damageRect.
	
	aMorph is the morph that changed and therefore should be redrawn. In some cases, damage reporting is done by no longer occupying some area, and requesting whatever appropriate morph to be drawn there. In such cases, aMorph should be nil. See senders."

	| clippedRect b |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	self clipsSubmorphs ifTrue: [
		b _ self displayBounds.
		b ifNil: [ ^self ].
		clippedRect _ damageRect intersect: b ].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect for: aMorph ]! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 11/16/2020 22:40:52'!
clipsSubmorphs
	"Answer true if we clip the shape of our submorphs to our own.
	Answer true only when clipping by the canvas is needed.
	Morphs that can guarantee that submorphs don't protrude and answer false to 
	#submorphsMightProtrude should answer false for better performance"

	^ false! !


!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 11/16/2020 22:55:54'!
invalidateDisplayRect: damageRect for: aMorph
        "Clip damage reports to my bounds, since drawing is _always_ clipped to my bounds."

	self recordDamagedRect: (damageRect intersect: self morphLocalBounds ) for: aMorph! !


!PluggableScrollPane methodsFor: 'geometry testing' stamp: 'jmv 11/16/2020 22:41:33'!
clipsSubmorphs
	"Answer true if we clip the shape of our submorphs to our own.
	Answer true only when clipping by the canvas is needed.
	Morphs that can guarantee that submorphs don't protrude and answer false to 
	#submorphsMightProtrude should answer false for better performance"

	^ true! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 11/16/2020 23:00:45'!
invalidateLocalRect: localRectangle

	self invalidateDisplayRect: (self externalizeDisplayBounds: localRectangle) for: self! !

!Morph methodsFor: 'geometry' stamp: 'jmv 11/16/2020 23:07:15'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world)."

	| answer |
	answer _ self displayBounds.
	answer ifNil: [ ^nil ].
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsReverseDo: [ :m |
			answer _ answer quickMerge: m displayFullBounds ]].
	^answer! !

!Morph methodsFor: 'updating' stamp: 'jmv 11/16/2020 23:00:32'!
invalidateBounds
	"Report that the area occupied by this morph should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self invalidateDisplayRect: self displayBoundsOrBogus for: nil.! !

!Morph methodsFor: 'updating' stamp: 'jmv 11/16/2020 23:07:38'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self invalidateDisplayRect: self displayBoundsOrBogus for: self.
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 11/16/2020 23:00:22'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."
	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas restorePatch: savedPatch.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self invalidateDisplayRect: (self morphPosition extent: extent) for: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		savedPatch _ nil ].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 11/16/2020 23:04:23'!
drawCurrentAndSubmorphs

	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 11/16/2020 23:03:34'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphs ifFalse: [ ^aBlock value ].

	prevClipRect _ self clipRect.
	prevClippingMorphRect _ clippingMorphDisplayBounds.
	"Might use the fact that currentMorph has just been drawn."
	clippingMorphDisplayBounds _ self boundingRectOfCurrentMorphAfterDraw.
	self setClipRect: (prevClipRect intersect: clippingMorphDisplayBounds).
	self clippingMorph: currentMorph.
	aBlock ensure: [
		self clippingMorph: nil.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds _ prevClippingMorphRect ]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 11/16/2020 23:04:52'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder

	addCurrentBounds ifTrue: [
		currentMorph needsRedraw: false.
		self fullUpdateCurrentBounds.
		aDamageRecorder recordInvalidRect: currentMorph displayBounds for: currentMorph ].
	trySubmorphs ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 11/16/2020 23:05:08'!
updateCurrentDisplayBounds
	"Update displayBounds for if never set.
	Useful for new morph, that are created and attached to the hand."
	
	currentMorph drawOn: self.
	currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw.
	self clippingByCurrentMorphDo: [
		currentMorph submorphsReverseDo: [ :m | self updateDisplayBounds: m ]].! !

!methodRemoval: PluggableScrollPane #clipsLastSubmorph stamp: 'jmv 11/17/2020 09:24:45'!
PluggableScrollPane removeSelector: #clipsLastSubmorph!
!methodRemoval: WorldMorph #invalidateDisplayRect:fromSubmorph:for: stamp: 'jmv 11/17/2020 09:24:45'!
WorldMorph removeSelector: #invalidateDisplayRect:fromSubmorph:for:!
!methodRemoval: Morph #clippedSubmorph stamp: 'jmv 11/17/2020 09:24:45'!
Morph removeSelector: #clippedSubmorph!
!methodRemoval: Morph #unclippedSubmorphsReverseDo: stamp: 'jmv 11/17/2020 09:24:45'!
Morph removeSelector: #unclippedSubmorphsReverseDo:!
!methodRemoval: Morph #clipsLastSubmorph stamp: 'jmv 11/17/2020 09:24:45'!
Morph removeSelector: #clipsLastSubmorph!
!methodRemoval: Morph #submorphsDrawingOutsideReverseDo: stamp: 'jmv 11/17/2020 09:24:45'!
Morph removeSelector: #submorphsDrawingOutsideReverseDo:!
!methodRemoval: Morph #invalidateDisplayRect:fromSubmorph:for: stamp: 'jmv 11/17/2020 09:24:45'!
Morph removeSelector: #invalidateDisplayRect:fromSubmorph:for:!
