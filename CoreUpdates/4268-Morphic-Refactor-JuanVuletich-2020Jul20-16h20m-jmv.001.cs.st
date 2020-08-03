'From Cuis 5.0 [latest update: #4264] on 20 July 2020 at 4:24:39 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/20/2020 16:14:37'!
drawWorld: aPasteUpMorph submorphs: worldSubmorphs repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount morphBounds |

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ aDamageRecorder invalidRectsFullBounds: aPasteUpMorph viewBox.

	rectsToRepair size > 1 ifTrue: [									"Or there is no point in attempting to merge them!!"
		previousRectsToRepair _ OrderedCollection new.

		"Until no more rectangles are merged, i.e. each morph intersects at most one rectangle."
		previousCount _ 0.
		[rectsToRepair size = previousCount] whileFalse: [
			previousCount _ rectsToRepair size.
			worldSubmorphs reverseDo: [ :morph | | thisMorphRectangle thisMorphBounds |
				morph visible ifTrue: [
					reuse _ previousRectsToRepair.
					previousRectsToRepair _ rectsToRepair.
					rectsToRepair _ reuse removeAll.
					thisMorphRectangle _ nil.
					thisMorphBounds _ morph morphFullBoundsInWorld.
					previousRectsToRepair do: [ :r |
						(thisMorphBounds intersects: r)
							ifTrue: [ thisMorphRectangle _ r quickMerge: thisMorphRectangle ]
							ifFalse: [ rectsToRepair add: r ]].
					thisMorphRectangle
						ifNotNil: [rectsToRepair add: thisMorphRectangle]]]].
	].

	"Draw World"
	rectsToRepair do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	"Draw morphs"
	worldSubmorphs reverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(self newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !


!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 7/20/2020 12:44:18'!
merge: aRectangle 
	"Answer a Rectangle that contains both the receiver and aRectangle.
	See #quickMerge:"

	^Rectangle 
		origin: (origin min: aRectangle origin)
		corner: (corner max: aRectangle corner)! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 7/20/2020 12:44:59'!
quickMerge: aRectangle 
	"Answer the receiver if it encloses the given rectangle or the merge of the two rectangles if it doesn't.
	This method is an optimization to reduce extra rectangle creations.
	Accept nil as argument"

	| useRcvr rOrigin rCorner minX maxX minY maxY |
	aRectangle ifNil: [ ^self ].

	useRcvr _ true.
	rOrigin _ aRectangle topLeft.
	rCorner _ aRectangle bottomRight.
	minX _ rOrigin x < origin x ifTrue: [ useRcvr _ false. rOrigin x ] ifFalse: [ origin x ].
	maxX _ rCorner x > corner x ifTrue: [ useRcvr _ false. rCorner x ] ifFalse: [ corner x ].
	minY _ rOrigin y < origin y ifTrue: [ useRcvr _ false. rOrigin y ] ifFalse: [ origin y ].
	maxY _ rCorner y > corner y ifTrue:  [useRcvr _ false. rCorner y ] ifFalse: [ corner y ].

	^useRcvr
		ifTrue: [ self ]
		ifFalse: [ Rectangle origin: minX@minY corner: maxX@maxY ].! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2020 12:43:39'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self morphFullBoundsInWorld.
	fullBounds ifNil: [ ^self ].

	fullBounds _ fullBounds intersect: (`0@0` extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: fullBounds
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds quickMerge: prevFullBounds)].
	prevFullBounds _ fullBounds! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 7/20/2020 16:15:29'!
displayWorldAndSubmorphs: submorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"repair world's damage on canvas"
	worldDamageRects _ canvas drawWorld: world submorphs: submorphs repair: damageRecorder.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	canvas newClipRect: nil.
	handsToDraw reverseDo: [ :h | canvas fullDrawHand: h ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		"Drawing was done to off-Display canvas. Copy content to Display"
		canvas showAt: world viewBox origin invalidRects: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	Display forceDamageToScreen: allDamage.

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 7/20/2020 15:55:32'!
invalidRectsFullBounds: aRectangle
	"Return a collection of damaged rectangles for the given canvas.
	If a total repaint has been requested, return the given rectangle.
	Forget about all damage."
	| answer |
	answer _ totalRepaint ifTrue: [ Array with: aRectangle ].
	self pvtAccessProtect critical: [
		answer ifNil: [
			answer _ invalidRects reject: [ :r |
				r isNil ]].
		self pvtReset ].
	^ answer.! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/20/2020 12:43:25'!
displayFullBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Might be larger than strictly required. See comment at #displayBoundsInWorldOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"

	self flag: #jmvVer2.

	r _ self displayBoundsInWorldOf: aMorph.
	aMorph submorphsDrawingOutsideReverseDo: [ :m |
		m visible ifTrue: [
			r _ r quickMerge: (self displayFullBoundsInWorldOf: m) ]].

	^r! !

!methodRemoval: WorldState class #experiment1 stamp: 'jmv 7/20/2020 16:23:20'!
WorldState class removeSelector: #experiment1!
!methodRemoval: WorldState #drawInvalidAreasSubmorphs: stamp: 'jmv 7/20/2020 16:23:29'!
WorldState removeSelector: #drawInvalidAreasSubmorphs:!
!methodRemoval: WorldState #simpleDrawInvalidAreasSubmorphs: stamp: 'jmv 7/20/2020 16:23:41'!
WorldState removeSelector: #simpleDrawInvalidAreasSubmorphs:!
!methodRemoval: SystemWindow #addPossiblyUncoveredAreasIn:to: stamp: 'jmv 7/20/2020 16:24:10'!
SystemWindow removeSelector: #addPossiblyUncoveredAreasIn:to:!
!methodRemoval: RectangleLikeMorph #addPossiblyUncoveredAreasIn:to: stamp: 'jmv 7/20/2020 16:24:11'!
RectangleLikeMorph removeSelector: #addPossiblyUncoveredAreasIn:to:!
!methodRemoval: Morph #addPossiblyUncoveredAreasIn:to: stamp: 'jmv 7/20/2020 16:24:11'!
Morph removeSelector: #addPossiblyUncoveredAreasIn:to:!
