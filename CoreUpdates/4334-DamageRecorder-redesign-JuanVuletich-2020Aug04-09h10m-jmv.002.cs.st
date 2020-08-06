'From Cuis 5.0 [latest update: #4333] on 4 August 2020 at 10:32:23 am'!
!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'invalidRects totalRepaint drSemaphore damageByRoot '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 8/4/2020 10:04:10'!
invalidateDisplayRect: damageRect fromSubmorph: submorphOrNil for: aMorph
	"If we clip aMorph, then we clip damageRect.
	When calling from self, second argument should be nil, i.e. we are not reporting damage for some submorph."

	| clippedRect b |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	submorphOrNil ifNotNil: [
	 	submorphOrNil == self clippedSubmorph
			ifTrue: [
				b _ self displayBounds.
				b ifNil: [ ^self ].
				clippedRect _ damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect fromSubmorph: self for: aMorph ]! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 8/4/2020 10:05:54'!
invalidateDisplayRect: damageRect fromSubmorph: submorphOrNil for: aMorph
        "Clip damage reports to my bounds, since drawing is _always_ clipped to my bounds."

        self == self world 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: self morphLocalBounds ) for: aMorph ]
                ifFalse: [ super invalidateDisplayRect: damageRect fromSubmorph: submorphOrNil for: aMorph ]! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 8/4/2020 10:02:25'!
recordDamagedRect: damageRect for: aMorph

	damageRecorder ifNotNil: [
		damageRecorder recordInvalidRect: damageRect for: aMorph ]! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/4/2020 10:29:51'!
recordInvalidRect: requestedRect for: aMorph
	"aRootMorph should be #root"

	^ self pvtAccessProtect critical: [
		self pvtInnerRecordInvalidRect: requestedRect for: (aMorph ifNotNil: [aMorph root]) ]! !

!DamageRecorder methodsFor: 'private' stamp: 'jmv 8/4/2020 10:30:38'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)
	aRootMorph is the top owner of the morph originally reporting requestedRect. It might be nil if irrelevant.
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."
	| newRect mergeRect indexToReuse oc |
	totalRepaint ifTrue: [ ^ self ].
	"planning full repaint; don't bother collecting damage"
	indexToReuse _ nil.
	newRect _ requestedRect encompassingIntegerRectangle.
	aRootMorph ifNotNil: [
		oc _ damageByRoot at: aRootMorph ifAbsentPut: [OrderedCollection new].
		oc add: newRect ].
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


!Morph methodsFor: 'change reporting' stamp: 'jmv 8/4/2020 10:07:09'!
invalidateLocalRect: localRectangle

	self invalidateDisplayRect: (self externalizeDisplayBounds: localRectangle) fromSubmorph: nil for: self! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/4/2020 10:07:29'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self invalidateDisplayRect: self displayBoundsOrBogus fromSubmorph: nil for: nil.
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/4/2020 10:06:52'!
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
			for: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		savedPatch _ nil ].! !

!HandMorph methodsFor: 'initialization' stamp: 'jmv 8/4/2020 10:15:10'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	extent _ CursorWithMask defaultCursor extent.
	grabMorphData _ IdentityDictionary new.
	self initForEvents.! !


!DamageRecorder methodsFor: 'initialization' stamp: 'jmv 8/4/2020 10:21:05'!
initialize
	super initialize .
	invalidRects _ OrderedCollection new: 15.
	totalRepaint _ false.
	damageByRoot _ IdentityDictionary new! !

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 8/4/2020 09:10:22'!
invalidRectsFullBounds: aRectangle
	"Return a collection of damaged rectangles for the given canvas. Take only intersection with aRectangle.
	If a total repaint has been requested, return the given rectangle.
	Forget about all damage."
	| answer |
	answer _ totalRepaint ifTrue: [ Array with: aRectangle ].
	self pvtAccessProtect critical: [
		answer ifNil: [
			answer _ OrderedCollection new.
			invalidRects do: [ :r |
				r ifNotNil: [
					(aRectangle containsRect: r)
						ifTrue: [ answer add: r ]
						ifFalse: [ answer add: (r intersect: aRectangle) ].
					]]].
		self pvtReset ].
	^ answer.! !

!DamageRecorder methodsFor: 'private' stamp: 'jmv 8/4/2020 10:21:17'!
pvtReset
	"Clear the damage list."
	invalidRects removeAll.
	totalRepaint _ false.
	damageByRoot removeAll.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/4/2020 10:01:36'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder
	| currentMorphBounds |
	addCurrentBounds ifTrue: [
		currentMorph needsRedraw: false.
		currentMorph drawOn: self.
		currentMorphBounds _ self boundingRectOfCurrentMorphAfterDraw.
		currentMorph displayBounds: currentMorphBounds.
		aDamageRecorder recordInvalidRect: currentMorphBounds for: currentMorph.
		currentMorph submorphsDo: [ :m | self fullUpdateBounds: m ].
	].
	trySubmorphs ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullAddRedrawRect: m to: aDamageRecorder ]
	].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/4/2020 09:55:53'!
updateHandsDisplayBounds: aPasteUpMorph addDamageTo: aDamageRecorder
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aPasteUpMorph handsDo: [ :hand |
		self into: hand.
		hand submorphsDo: [ :morph |
			self updateDisplayBounds: morph ].
		aDamageRecorder recordInvalidRect: hand displayFullBounds for: nil.
		self outOfMorph ]! !

!methodRemoval: DamageRecorder #recordInvalidRect:from: stamp: 'jmv 8/4/2020 09:43:57'!
DamageRecorder removeSelector: #recordInvalidRect:from:!
!methodRemoval: DamageRecorder #pvtInnerRecordInvalidRect: stamp: 'jmv 8/4/2020 09:45:01'!
DamageRecorder removeSelector: #pvtInnerRecordInvalidRect:!
!methodRemoval: DamageRecorder #recordInvalidRect: stamp: 'jmv 8/4/2020 10:32:13'!
DamageRecorder removeSelector: #recordInvalidRect:!
!methodRemoval: DamageRecorder #pvtInnerRecordInvalidRect:from: stamp: 'jmv 8/4/2020 09:44:06'!
DamageRecorder removeSelector: #pvtInnerRecordInvalidRect:from:!
!methodRemoval: WorldState #recordDamagedRect: stamp: 'jmv 8/4/2020 10:31:53'!
WorldState removeSelector: #recordDamagedRect:!
!methodRemoval: WorldState #recordDamagedRect:from: stamp: 'jmv 8/4/2020 09:43:51'!
WorldState removeSelector: #recordDamagedRect:from:!
!methodRemoval: PasteUpMorph #invalidateDisplayRect:from: stamp: 'jmv 8/4/2020 09:34:25'!
PasteUpMorph removeSelector: #invalidateDisplayRect:from:!
!methodRemoval: PasteUpMorph #invalidateDisplayRect:fromSubmorph: stamp: 'jmv 8/4/2020 10:07:43'!
PasteUpMorph removeSelector: #invalidateDisplayRect:fromSubmorph:!
!methodRemoval: Morph #invalidateDisplayRect:from: stamp: 'jmv 8/4/2020 09:34:22'!
Morph removeSelector: #invalidateDisplayRect:from:!
!methodRemoval: Morph #invalidateDisplayRect:fromSubmorph: stamp: 'jmv 8/4/2020 10:07:41'!
Morph removeSelector: #invalidateDisplayRect:fromSubmorph:!
!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'invalidRects totalRepaint drSemaphore damageByRoot'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DamageRecorder allSubInstancesDo: [ :dr | dr instVarNamed: 'damageByRoot' put: IdentityDictionary new ]!