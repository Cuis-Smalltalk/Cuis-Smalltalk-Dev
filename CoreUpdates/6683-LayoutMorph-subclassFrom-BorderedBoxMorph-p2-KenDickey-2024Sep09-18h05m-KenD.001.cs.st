'From Cuis7.1 [latest update: #6682] on 9 September 2024 at 6:07:39 pm'!
!LayoutMorph methodsFor: 'accessing' stamp: 'KenD 9/9/2024 16:54:03'!
padding: numberOrPoint

	super padding: numberOrPoint.
	self layoutSubmorphs! !


!LayoutMorph methodsFor: 'geometry' stamp: 'KenD 9/9/2024 17:24:46'!
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
				"sum up submorph heights, including separation"
				height := height + mle y + self ySeparation.
			].
			height := height - self ySeparation. "An extra separation was added"
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"sum up submorphs width"
				width := width + mle x + self xSeparation.
				"use maximum height across submorph"
				height := height max: mle y.
			].
			width := width - self xSeparation. "An extra separation was added"
		].
	^ (width @ height) + (2 * self interiorOrigin)! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 9/9/2024 16:59:48'!
defaultPadding
	"answer the default inner padding for the receiver"
	^ `0@0`! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 9/9/2024 17:00:08'!
initialize
	super initialize.
	separation := 0.
	axisEdgeWeight := 0.0.
	doAdoptWidgetsColor := false.! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 9/8/2024 13:13:06'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^ self interiorOrigin extent: self interiorExtent ! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 9/3/2024 18:40:24'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap gapCount widths widthToAllocate leftOver x height y cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].
	
	gap := self xSeparation.
	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	widthToAllocate := boundsForLayout width - (gapCount * gap).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left + (leftOver * self axisEdgeWeight).

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + gap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 9/3/2024 18:52:11'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap gapCount heights heightToAllocate leftOver y width x cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gap := self ySeparation.
	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	heightToAllocate := boundsForLayout height - (gapCount * gap).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top + (leftOver * self axisEdgeWeight).

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + gap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 9/3/2024 18:45:27'!
offAxisHeightFor: aMorph within: availableHeight minLayoutExtentCache: cache
	"Answer height for a single morph -- offAxis calculation for a Row"
	
	^(availableHeight * aMorph layoutSpec proportionalLayoutHeight)
							 max: (aMorph minimumLayoutExtent: cache) y.! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 9/3/2024 18:45:59'!
offAxisWidthFor: aMorph within: availableWidth minLayoutExtentCache: cache
	"Answer width for a single morph -- offAxis calculation for a Column"

	^ (availableWidth * aMorph layoutSpec proportionalLayoutWidth)
						 	max: (aMorph minimumLayoutExtent: cache) x.
! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 9/3/2024 18:48:57'!
offAxisXOffsetFor: aMorph within: availableWidth
	"Answer x offset for a single morph -- offAxis calculation for a Column"

	^ availableWidth * aMorph layoutSpec offAxisEdgeWeight. ! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 9/3/2024 18:48:36'!
offAxisYOffsetFor: aMorph within: availableHeight
	"Answer y offset for a single morph -- offAxis calculation for a Row"
	
	^ availableHeight * aMorph layoutSpec offAxisEdgeWeight! !


!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 9/9/2024 17:07:05'!
exampleEdgesColumn
	"Show 2 cases :1 useEdgeSpace true and 1 false"
	" self exampleEdgesColumn "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newColumn :: padding: (120 @ 40);  separation: 20.
	noEdge := self newColumn ::  padding: (0 @ 0);  separation: 20.
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

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 9/9/2024 17:07:12'!
exampleEdgesRow
	"Show 2 cases: 1 useEdgeSpace true and 1 false"
	" self exampleEdgesRow "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newRow :: padding: (120 @ 40);  separation: 20.
	noEdge := self newRow :: padding: (0 @ 0);  separation: 20.
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

!methodRemoval: LayoutMorph #useEdgeSpace: stamp: 'jmv 9/9/2024 18:06:27'!
LayoutMorph removeSelector: #useEdgeSpace:!
!methodRemoval: LayoutMorph #useEdgeSpace stamp: 'jmv 9/9/2024 18:06:27'!
LayoutMorph removeSelector: #useEdgeSpace!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation axisEdgeWeight doAdoptWidgetsColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LayoutMorph allSubInstances do: [ :m | m instVarNamed: 'padding' put: m defaultPadding ].
!
