'From Cuis 5.0 [latest update: #4267] on 21 July 2020 at 11:16:42 am'!

!Morph methodsFor: 'updating' stamp: 'jmv 7/20/2020 17:26:13'!
isRedrawNeeded
	"Report that the area occupied by this morph should be redrawn."
"Creo que es mejor transformarlo en una coleccion. Ver."

	self visible ifFalse: [ ^false ].
	^ self valueOfProperty: #isRedrawNeeded ifAbsent: [ false ]! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 7/20/2020 17:30:42'!
displayWorldAndSubmorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"repair world's damage on canvas"
	worldDamageRects _ canvas drawWorld: world repair: damageRecorder.

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


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/20/2020 17:52:34'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount morphBounds |

	"Add dirty rectangles for all dirty morphs"
	aPasteUpMorph allMorphsDo: [ :morph |
		morph isRedrawNeeded ifTrue: [
			morph setProperty: #isRedrawNeeded toValue: false.
			morph displayFullBounds ifNotNil: [ :r |
				aDamageRecorder recordInvalidRect: r ]]].

	"Aca agregar el seoudodibujado para actualizar los bounds que sean necesarios"

	"Add dirty rectangles for all dirty morphs"
	aPasteUpMorph allMorphsDo: [ :morph |
		morph isRedrawNeeded ifTrue: [
			morph displayFullBounds ifNotNil: [ :r |
				aDamageRecorder recordInvalidRect: r ]]].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ aDamageRecorder invalidRectsFullBounds: aPasteUpMorph viewBox.

	rectsToRepair size > 1 ifTrue: [									"Or there is no point in attempting to merge them!!"
		previousRectsToRepair _ OrderedCollection new.

		"Until no more rectangles are merged, i.e. each morph intersects at most one rectangle."
		previousCount _ 0.
		[rectsToRepair size = previousCount] whileFalse: [
			previousCount _ rectsToRepair size.
			aPasteUpMorph submorphsReverseDo: [ :morph | | thisMorphRectangle thisMorphBounds |
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
	aPasteUpMorph submorphsReverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(self newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !


!Morph methodsFor: 'updating' stamp: 'jmv 7/21/2020 10:48:14'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	self isRedrawNeeded ifFalse: [
		self setProperty: #isRedrawNeeded toValue: true ].
"Dummy, so #updateIsNeeded answers true if some morph dirty"
self invalidateDisplayRect: (10@10 extent: 2@2) from: nil.
false ifTrue: [
	self morphBoundsInWorld ifNotNil: [ :r |
		self invalidateDisplayRect: r from: nil.
		"Expensive in many cases..."
		self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]]
]! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 7/20/2020 17:30:57'!
privateOuterDisplayWorld

	worldState displayWorldAndSubmorphs! !

!methodRemoval: MorphicCanvas #drawWorld:submorphs:repair: stamp: 'jmv 7/21/2020 10:43:06'!
MorphicCanvas removeSelector: #drawWorld:submorphs:repair:!
!methodRemoval: WorldState #displayWorldAndSubmorphs: stamp: 'jmv 7/21/2020 10:43:03'!
WorldState removeSelector: #displayWorldAndSubmorphs:!
