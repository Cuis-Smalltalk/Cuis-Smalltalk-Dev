'From Cuis7.1 [latest update: #6693] on 11 September 2024 at 9:30:13 am'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 09:20:56'!
gap: aNumberOrPoint

	gap = aNumberOrPoint ifFalse: [
		self adjustExtentFor: [ gap := aNumberOrPoint ]].
	self layoutSubmorphs ! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 09:23:49'!
xGap
	^gap isNumber
		ifTrue: [ gap ]
		ifFalse: [ gap x ]! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 09:23:54'!
yGap
	^gap isNumber
		ifTrue: [ gap ]
		ifFalse: [ gap y ]! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 09:22:55'!
separation: aNumberOrPoint

	self padding: aNumberOrPoint.
	self gap: aNumberOrPoint.! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 9/11/2024 09:25:35'!
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
				height := height + mle y + self yGap.
			].
			height := height - self yGap. "An extra gap was just added"
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"sum up submorphs width, include gap"
				width := width + mle x + self xGap.
				"use maximum height across submorph"
				height := height max: mle y.
			].
			width := width - self xGap. "An extra gap was just added"
		].
	^ (width @ height) + (2 * self interiorOrigin)! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/11/2024 09:25:24'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs xGap gapCount widths widthToAllocate leftOver x height y cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].
	
	xGap := self xGap.
	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	widthToAllocate := boundsForLayout width - (gapCount * xGap).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left + (leftOver * self axisEdgeWeight).

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + xGap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/11/2024 09:24:02'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs yGap gapCount heights heightToAllocate leftOver y width x cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	yGap := self yGap.
	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	heightToAllocate := boundsForLayout height - (gapCount * yGap).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top + (leftOver * self axisEdgeWeight).

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + yGap.
	]! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 9/11/2024 09:28:10'!
exampleEdgesColumn
	"Show 2 cases :1 useEdgeSpace true and 1 false"
	" self exampleEdgesColumn "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newColumn :: padding: (120 @ 40);  gap: 20.
	noEdge := self newColumn ::  padding: (0 @ 0);  gap: 20.
	upSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 0.
	upSpec proportionalWidth: 0.8 minimum: 200; proportionalHeight: 0.8 minimum: 200.
	downSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 1.
	downSpec proportionalWidth: 0.5 minimum: 200; proportionalHeight: 0.5 minimum: 200.
	
	withEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	noEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	withEdge addMorph: (LabelMorph new :: contents: 'WITH Padding').
	noEdge addMorph: (LabelMorph new :: contents: 'withOUT Padding').
	withEdge addMorph: (BoxedMorph new :: layoutSpec: downSpec).
	noEdge addMorph: (BoxedMorph new ::  layoutSpec: downSpec).
	
	withEdge openInWorld.
	noEdge openInWorld.! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 9/11/2024 09:28:39'!
exampleEdgesRow
	"Show 2 cases: 1 useEdgeSpace true and 1 false"
	" self exampleEdgesRow "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newRow :: padding: (120 @ 40);  gap: 20.
	noEdge := self newRow :: padding: (0 @ 0);  gap: 20.
	upSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 0.
	upSpec proportionalWidth: 0.8 minimum: 100; proportionalHeight: 0.8 minimum: 100.
	downSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 1.
	downSpec proportionalWidth: 0.5 minimum: 100; proportionalHeight: 0.5 minimum: 100.
	
	withEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	noEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	withEdge addMorph: (LabelMorph new :: contents: 'WITH Padding').
	noEdge addMorph: (LabelMorph new :: contents: 'withOUT Padding').
	withEdge addMorph: (BoxedMorph new :: layoutSpec: downSpec).
	noEdge addMorph: (BoxedMorph new ::  layoutSpec: downSpec).
	
	withEdge openInWorld.
	noEdge openInWorld.! !

!methodRemoval: LayoutMorph #ySeparation stamp: 'jmv 9/11/2024 09:25:12'!
LayoutMorph removeSelector: #ySeparation!
!methodRemoval: LayoutMorph #xSeparation stamp: 'jmv 9/11/2024 09:25:39'!
LayoutMorph removeSelector: #xSeparation!
