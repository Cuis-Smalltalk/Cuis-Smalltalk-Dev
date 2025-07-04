'From Cuis7.5 [latest update: #7302] on 30 June 2025 at 11:08:05 am'!

!LinearLayout methodsFor: 'accessing' stamp: 'jmv 6/30/2025 11:01:21'!
axisEdgeWeight
	^axisEdgeWeight! !

!LinearLayout methodsFor: 'accessing' stamp: 'jmv 6/30/2025 10:55:12'!
axisEdgeWeight: aNumberOrNil
	axisEdgeWeight := aNumberOrNil.! !

!LinearLayout methodsFor: 'accessing' stamp: 'jmv 6/30/2025 11:05:20'!
gap
	^gap! !

!LinearLayout methodsFor: 'accessing' stamp: 'jmv 6/30/2025 10:46:07'!
gap: aNumber
	gap := aNumber! !


!ColumnLayout methodsFor: 'initialization' stamp: 'jmv 6/30/2025 10:56:22'!
initialize
	gap := 0.
	axisEdgeWeight := 0.5. "#center"! !


!RowLayout methodsFor: 'initialization' stamp: 'jmv 6/30/2025 10:56:34'!
initialize
	gap := 0.
	axisEdgeWeight := 0.0. "#rowLeft"! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 6/30/2025 11:04:39'!
axisEdgeWeight: aSymbolOrNumber
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the 	 	 LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

	If a Row, one can specify this weight symbolically as
	  { #rowLeft (0.0), #center (0.5), #rowRight (1.0)}

	If a Column, one can specify the weight symbolically as:
	  { #columnTop (0.0), #center (0.5), #columnBottom (1.0) }"
"	
	self axisEdgeWeight: #rowLeft.	-- axis must be horizontal 
	self axisEdgeWeight: #columnTop.	-- axis must be vertical
"
	| edgeWeight |
	(aSymbolOrNumber is: #Number)
		ifTrue: [ (aSymbolOrNumber between: 0.0 and: 1.0)
			ifTrue: [edgeWeight := aSymbolOrNumber ] 
			ifFalse: [self error: 'axisEdgeWeight ', aSymbolOrNumber printString, ' is out of range 0.0 to 1.0']
		]
		ifFalse: [
			(self direction = #horizontal) 
				ifTrue: [ "Row"
					edgeWeight := aSymbolOrNumber
						caseOf: {
							[ #rowLeft ] -> [ 0.0 ].
							[ #center ] -> [ 0.5 ].
							[ #rowRight ] -> [ 1.0 ]
						}
						otherwise: [ self error: 'bad axisEdgeWeight for Row: ', aSymbolOrNumber printString ].
				]
				ifFalse: [ "vertical => Column"
					edgeWeight := aSymbolOrNumber
						caseOf: {
							[ #columnTop ] -> [ 0.0 ].
							[ #center ] -> [ 0.5 ].
							[ #columnBottom ] -> [ 1.0 ]
						}
						otherwise: [ self error: 'bad axisEdgeWeight for Column: ', aSymbolOrNumber printString ].
				]
		].
	layout axisEdgeWeight: edgeWeight.
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 6/30/2025 11:06:53'!
gap: aNumber

	layout gap = aNumber ifFalse: [
		self adjustExtentFor: [
			layout gap: aNumber ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 6/30/2025 11:06:05'!
minimumExtent: cachedValues
	"Answer size sufficient to frame my submorphs."
	
	| width height mle |
	width  := 0.
	height := 0.
	(self direction = #vertical)
		ifTrue: [ "Column"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"use maximum width across submorphs"
				width := width max: mle x.
				"sum up submorph heights, including separation between them"
				height := height + mle y + layout gap.
			].
			height := height - layout gap. "An extra gap was just added"
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"sum up submorphs width, include gap"
				width := width + mle x + layout gap.
				"use maximum height across submorph"
				height := height max: mle y.
			].
			width := width - layout gap. "An extra gap was just added"
		].
	^ (width @ height) + (2 * self interiorOrigin)! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 11:04:49'!
beColumn
	"Establish the major layout axis, with default edge weight" 
	direction := #vertical.
	layout := ColumnLayout new.
	layout gap: self defaultGap.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 11:04:45'!
beRow
	"Establish the major layout axis, with default edge weight" 
	direction := #horizontal.
	layout := RowLayout new.
	layout gap: self defaultGap.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 11:06:24'!
initializeColumn

	self initialize.

	doAdoptWidgetsColor := false.
	self beColumn.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 11:06:20'!
initializeRow

	self initialize.

	doAdoptWidgetsColor := false.
	self beRow.! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:05:29'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount widths widthToAllocate leftOver x cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	widthToAllocate := boundsForLayout width - (gapCount * layout gap).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left + (leftOver * layout axisEdgeWeight).

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		| height y |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + layout gap.
	].! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:05:36'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount heights heightToAllocate leftOver y cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	heightToAllocate := boundsForLayout height - (gapCount * layout gap).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top + (leftOver * layout axisEdgeWeight).

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		| width x |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + layout gap.
	].! !

!methodRemoval: LayoutMorph #gap stamp: 'jmv 6/30/2025 11:00:01'!
LayoutMorph removeSelector: #gap!
!methodRemoval: LayoutMorph #axisEdgeWeight stamp: 'jmv 6/30/2025 11:02:02'!
LayoutMorph removeSelector: #axisEdgeWeight!
