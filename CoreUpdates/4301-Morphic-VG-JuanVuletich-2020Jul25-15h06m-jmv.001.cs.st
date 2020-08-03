'From Cuis 5.0 [latest update: #4297] on 25 July 2020 at 3:14:40 pm'!

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 7/25/2020 15:10:59'!
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld 
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: `0 @ 0`).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := `0 @ 0` extent: initialExtent.
	"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400"
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w visible and: [w isCollapsed not]]) 
						collect: [:w | w displayBounds].
	otherFrames _ otherFrames reject: [ :f | f isNil ].
	0 to: maxLevel do: [ :level | 
		1 to: 4 do: [ :ci | 
			cornerSel := #(#topLeft #topRight #bottomRight #bottomLeft) at: ci.
			corner := allowedArea perform: cornerSel.
			"The extra grid//2 in delta helps to keep title tabs distinct"
			delta := ((maxLevel - level) * grid + (grid // 2)) @ (level * grid).
			1 to: ci - 1 do: [ :i | delta _ delta y negated @ delta x ].	"slow way"
			putativeCorner := corner + delta.
			free := true.
			otherFrames do: [ :w |
				free := free & ((w perform: cornerSel) ~= putativeCorner)].
			free 
				ifTrue: [
					^(initialFrame aligned: (initialFrame perform: cornerSel)
						with: putativeCorner) translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^(self scrollBarSetback @ self screenTopSetback 
		extent: initialFrame extent) translatedAndSquishedToBeWithin: allowedArea! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/25/2020 15:09:48'!
displayBounds: aRectangle

	self displayBounds ~= aRectangle ifTrue: [
		self setProperty: #displayBounds toValue: aRectangle ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/25/2020 15:06:54'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder."
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aPasteUpMorph ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ aDamageRecorder invalidRectsFullBounds: aPasteUpMorph viewBox.

	rectsToRepair size = 0 ifTrue: [
		^rectsToRepair].

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
					thisMorphBounds _ morph displayFullBounds.
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
	aPasteUpMorph submorphsReverseDo: [ :morph | | morphBounds |
		morph visible ifTrue: [
			morphBounds _ morph displayFullBounds.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(self newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/25/2020 15:08:35'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc

	engine ifNil: [ ^nil ].
	^super textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc! !

!methodRemoval: BitBltCanvas #clippingByCurrentMorphDo: stamp: 'jmv 7/25/2020 15:10:07'!
BitBltCanvas removeSelector: #clippingByCurrentMorphDo:!
