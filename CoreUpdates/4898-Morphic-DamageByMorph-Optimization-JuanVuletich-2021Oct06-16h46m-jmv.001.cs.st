'From Cuis 5.0 [latest update: #4897] on 6 October 2021 at 4:54:10 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 10/4/2021 16:05:27'!
fullyCovers: aRectangle
	"Answer whether our drawing completely covers aRectangle. Answer true only if we are certain"

	| answer |
	answer _ true.
	self on: aRectangle
		ifCovered: []
		uncoveredPartsDo: [ :r | answer _ false ]
		else: [ answer _ false ].
	^answer! !

!Morph methodsFor: 'drawing' stamp: 'jmv 10/4/2021 11:17:41'!
on: aRectangle ifCovered: partiallyOrTotallyCoveredBlock uncoveredPartsDo: uncoveredPartBlock else: notCoveredAtAllBlock
	"Find rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included.
	
	If aRectangle is not covered at all, or for whatever reason we don't know, evaluate notCoveredAtAllBlock.
	Othewise evaluate partiallyOrTotallyCoveredBlock once, and also evaluate uncoveredPartBlock for each part in aRectangle we don't cover."

	^ notCoveredAtAllBlock value! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 10/4/2021 14:34:32'!
on: aRectangle ifCovered: partiallyOrTotallyCoveredBlock uncoveredPartsDo: uncoveredPartBlock else: notCoveredAtAllBlock
	"Find rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included.
	
	If aRectangle is not covered at all, or for whatever reason we don't know, evaluate notCoveredAtAllBlock.
	Othewise evaluate partiallyOrTotallyCoveredBlock once, and also evaluate uncoveredPartBlock for each part in aRectangle we don't cover."

"
ver si hacer como
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
que ademas vuela."

	 | bounds r e r2 |
	self isOrAnyOwnerIsRotated ifTrue: [
		^ notCoveredAtAllBlock value ].

	color mightBeTranslucent ifTrue: [
		^ notCoveredAtAllBlock value ].

	bounds _ self displayBounds.
	bounds ifNil: [
		^ notCoveredAtAllBlock value ].

	(aRectangle intersects: bounds) ifFalse: [
		^ notCoveredAtAllBlock value ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	partiallyOrTotallyCoveredBlock value.
	aRectangle areasOutside: bounds do: [ :rect | uncoveredPartBlock value: rect ].

	Theme current roundWindowCorners ifTrue: [
		r _ Theme current roundedWindowRadius.
		e _ r@r.
		r2 _ aRectangle intersect: (bounds topLeft extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds topRight - (r@0) extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds bottomLeft - (0@r)extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds bottomRight - (r@r) extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/6/2021 16:50:48'!
drawWorld: aWorldMorph rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage backgroundDamage: newDamageFromMorphsBelow
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| morph morphDamage allDamage |
	"Iterate from back to front."
	allDamage _ Rectangle merging: newDamageFromMorphsBelow.
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph _ rootMorphs at: i.
		morphDamage _ rootMorphsDamage at: i.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection _ r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage _  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			self newClipRect: morphDamage.
			self fullDraw: morph.
			newDamageFromMorphsBelow add: morphDamage.
			allDamage _ allDamage
				ifNil: [ morphDamage ]
				ifNotNil: [ morphDamage updateMerging: allDamage ]]].
	^allDamage! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/4/2021 19:56:07'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds |
	handBounds _ self displayBounds.
	fullBounds _ handBounds copy.
	self submorphsDo: [ :m |
		fullBounds updateMerging: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ (fullBounds translatedBy: self morphPosition-lastPos) updateMerging: handBounds ].
	^fullBounds encompassingIntegerRectangle outsetBy: 1! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/4/2021 16:25:37'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	Preferences cacheDisplayContentWhenMovingMorphs ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"repair world's damage on canvas"
	allDamage _ canvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		Preferences cacheDisplayContentWhenMovingMorphs
			ifTrue: [ allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage _ h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 10/4/2021 15:29:27'!
handsToDrawForDamage: aRectangle do: aBlock
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	hands do: [: hand |
		hand isDrawnBySoftware ifTrue: [
			hand isRedrawNeeded
				ifTrue: [aBlock value: hand]
				ifFalse: [
					aRectangle ifNotNil: [
						hand displayFullBounds ifNotNil: [ :handBounds |
							(aRectangle intersects: handBounds) ifTrue: [
								aBlock value: hand ]]]]]].! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 10/4/2021 10:15:45'!
damageReportedOther
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand.
	Answer might include nils. Skip them."
	| answer possiblyMoreRectsToMerge |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m visible not or: [m owner isNil or: [m owner is: #HandMorph]]) ifTrue: [
				answer add: r]].
		otherDamage do: [ :r | answer add: r ].
	].
	possiblyMoreRectsToMerge _ true.
	[possiblyMoreRectsToMerge] whileTrue: [
		possiblyMoreRectsToMerge _ false.
		answer withIndexDo: [ :r1 :i |
			r1 ifNotNil: [
				i+1 to: answer size do: [ :j | | r2 |
					r2 _ answer at: j.
					r2 ifNotNil: [
						(r1 intersects: r2) ifTrue: [
							r1 updateMerging: r2.
							answer at: j put: nil.
							possiblyMoreRectsToMerge _ true ]]]]]].
	^answer! !

!DamageRecorder methodsFor: 'private' stamp: 'jmv 10/6/2021 16:36:18'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."
	| newRect |
	newRect _ requestedRect encompassingIntegerRectangle.
	newRect == requestedRect ifTrue: [ newRect _ newRect copy ].
	aRootMorph ifNotNil: [
		(damageByRoot at: aRootMorph
			ifPresent: [ :r | r updateMerging: newRect]
			ifAbsent: [ damageByRoot at: aRootMorph put: newRect ]) ]
		ifNil: [otherDamage add: newRect].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/6/2021 16:52:49'!
computeDamage: aWorldMorph repair: aDamageRecorder rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage
	"Iterate front to back while computing damage to actually repair for each morph, and for world background.
	Answer uncoveredDamage, the areas where World background needs to be redrawn."

	| uncoveredDamage additionalUncoveredDamage morph morphDamage morphFullBounds |
	uncoveredDamage _ aDamageRecorder damageReportedOther.
	additionalUncoveredDamage _ OrderedCollection new.

	"Iterate from front to back"
	1 to: rootMorphs size do: [ :i |
		morph _ rootMorphs at: i.
		morphFullBounds _ morph displayFullBounds.
		morphDamage _ nil.
		"Reported damage can be ignored if the area fully covered by another morph above us."
		(aDamageRecorder damageReportedFor: morph) ifNotNil: [ :r | | wasFullyCovered |
			wasFullyCovered _ false.
			1 to: i-1 do: [ :j |
				wasFullyCovered _ wasFullyCovered or: [ (rootMorphs at: j) fullyCovers: r ]].
			wasFullyCovered ifFalse: [
				uncoveredDamage add: r ]].
		"Any uncovered damage this morph may overlap must be redrawn by it."
		uncoveredDamage do: [ :r | | intersection |
			r ifNotNil: [
				intersection _ r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage _ intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		rootMorphsDamage at: i put: morphDamage.
		"Whatever we cover completely is no longer uncoveredDamage"
		uncoveredDamage withIndexDo: [ :r :ri |
			r ifNotNil: [
				morph
					on: r
					ifCovered: [uncoveredDamage at: ri put: nil]
						uncoveredPartsDo: [ :r2 | additionalUncoveredDamage add: r2 ]
					else: []]].
		uncoveredDamage addAll: additionalUncoveredDamage.
		additionalUncoveredDamage removeAll.
	].

	"Remove redundant rectangles"
	1 to: uncoveredDamage size do: [ :i1 |
		(uncoveredDamage at: i1) ifNotNil: [ :r1 |
			1 to: uncoveredDamage size do: [ :i2 |
				i1 = i2 ifFalse: [
					(uncoveredDamage at: i2) ifNotNil: [ :r2 |
						(r1 containsRect: r2)
							ifTrue: [ uncoveredDamage at: i2 put: nil ]]]]]].

	"Answer any yet uncovered areas. World background should be drawn on them."
	^ uncoveredDamage select: [ :r | r notNil ].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/4/2021 16:29:10'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage worldBackgroundRects |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self boundsFinderCanvas updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder.

	visibleRootMorphs _ aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage _ Array new: visibleRootMorphs size.

	worldBackgroundRects _ self computeDamage: aWorldMorph repair: aDamageRecorder
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage.
	aDamageRecorder reset.

	self drawWorldBackground: aWorldMorph rects: worldBackgroundRects.
	"Debugging aids."
	"
	worldBackgroundRects do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"

	^ self drawWorld: aWorldMorph
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage
		backgroundDamage: worldBackgroundRects.! !

!methodRemoval: MorphicCanvas #drawWorld:rootMorphs:rootMorphsDamage:allDamage: stamp: 'jmv 10/6/2021 16:47:32'!
MorphicCanvas removeSelector: #drawWorld:rootMorphs:rootMorphsDamage:allDamage:!
!methodRemoval: SystemWindow #addPossiblyUncoveredAreasIn:to: stamp: 'jmv 10/6/2021 16:47:32'!
SystemWindow removeSelector: #addPossiblyUncoveredAreasIn:to:!
!methodRemoval: Morph #addPossiblyUncoveredAreasIn:to: stamp: 'jmv 10/6/2021 16:47:32'!
Morph removeSelector: #addPossiblyUncoveredAreasIn:to:!
