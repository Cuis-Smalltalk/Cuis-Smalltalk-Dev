'From Cuis 5.0 [latest update: #4334] on 4 August 2020 at 7:30:06 pm'!

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/4/2020 10:52:52'!
damageReportedFor: aMorph
	^ self pvtAccessProtect critical: [ damageByRoot at: aMorph ifAbsent: nil ]! !

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/4/2020 17:09:22'!
damageReportedNotVisibleMorphs
	| answer |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m visible not or: [m owner isNil]) ifTrue: [
				answer add: r]]].
	^answer! !


!Morph methodsFor: 'updating' stamp: 'jmv 8/4/2020 17:12:13'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self invalidateDisplayRect: self displayBoundsOrBogus fromSubmorph: nil for: self.
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2020 15:13:36'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."
	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas restorePatch: savedPatch.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self
			invalidateDisplayRect: (self morphPosition extent: extent)
			fromSubmorph: nil
			for: self.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		savedPatch _ nil ].! !


!DamageRecorder methodsFor: 'testing' stamp: 'jmv 8/4/2020 15:10:59'!
updateIsNeeded
	"Return true if the display needs to be updated.
	Note: This could give a false positive (i.e. answer true) if invalidRects is not empty but it only contains nils.
	Senders should be aware of this."
	^ totalRepaint or: [ self pvtAccessProtect critical: [damageByRoot notEmpty or: [invalidRects notEmpty]] ].! !

!DamageRecorder methodsFor: 'private' stamp: 'jmv 8/4/2020 15:17:45'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)
	aRootMorph is the top owner of the morph originally reporting requestedRect. It might be nil if irrelevant.
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."
	| newRect mergeRect indexToReuse |
	totalRepaint ifTrue: [ ^ self ].
	"planning full repaint; don't bother collecting damage"
	indexToReuse _ nil.
	newRect _ requestedRect encompassingIntegerRectangle.
"En el futuro no va a poder ser nil"
	aRootMorph ifNotNil: [
		damageByRoot at: aRootMorph put: 
		(damageByRoot at: aRootMorph
			ifPresent: [:r | r quickMerge: newRect]
			ifAbsent:[newRect]) ].
	invalidRects withIndexDo: [ :oldRect :index |
		oldRect
			ifNil: [ indexToReuse ifNil: [ indexToReuse _ index ]]
			ifNotNil: [
				"No two rectangles should intersect"
				(oldRect intersects: newRect) ifTrue: [
					"newRect already in. Nothing to do then."
					(oldRect containsRect: newRect) ifTrue: [ ^ self ].
					"Some oldRect included in newRect. Remove it and continue, as newRect could still intersect others."
					(newRect containsRect: oldRect) ifTrue: [
						invalidRects
							at: index
							put: nil.
						"Effectively like 'invalidRects remove: rect', but without performance penalty."
						indexToReuse ifNil: [ indexToReuse _ index ]].
					"Merge rectangles if they overlap significantly, i.e. if the merge is not much larger than separated rectangles."
					mergeRect _ (oldRect origin min: newRect origin) corner: (oldRect corner max: newRect corner).
					mergeRect area > (newRect area + oldRect area * 2)
						ifTrue: [
							"Avoid intersections!!"
							newRect
								areasOutside: oldRect
								do: [ :nonIntersection |
									"We have brand new newRects. Start all over with each of them."
									self pvtInnerRecordInvalidRect: nonIntersection for: nil ].
							"newRect no longer needed, then."
							^ self ]
						ifFalse: [
							"Merge into newRect, as any overlap with forecoming rectangles in the iteration is detected and treated."
							newRect _ mergeRect.
							invalidRects
								at: index
								put: nil.
							"Effectively like 'invalidRects remove: rect', but without performance penalty."
							indexToReuse ifNil: [ indexToReuse _ index ]]]]].
	invalidRects size >= 10 ifTrue: [
		"if there are too many separate areas, merge them all"
		mergeRect _ Rectangle merging: invalidRects.
		invalidRects removeAll.
		mergeRect ifNotNil: [ invalidRects addLast: mergeRect ].
		indexToReuse _ nil ].
	"Add the given rectangle to the damage list"
	indexToReuse
		ifNil: [ invalidRects addLast: newRect ]
		ifNotNil: [
			invalidRects
				at: indexToReuse
				put: newRect ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/4/2020 15:12:44'!
updateHandsDisplayBounds: aPasteUpMorph addDamageTo: aDamageRecorder
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aPasteUpMorph handsDo: [ :hand |
		self into: hand.
		hand submorphsDo: [ :morph |
			self updateDisplayBounds: morph ].
		aDamageRecorder recordInvalidRect: hand displayFullBounds for: hand.
		self outOfMorph ]! !

