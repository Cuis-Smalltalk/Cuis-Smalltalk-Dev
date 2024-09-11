'From Cuis7.1 [latest update: #6706] on 11 September 2024 at 5:13:52 pm'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 17:11:02'!
gap
	^gap! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 17:07:27'!
gap: aNumber

	gap = aNumber ifFalse: [
		self adjustExtentFor: [ gap := aNumber ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 17:09:49'!
separation: aNumberOrPoint

	| g |
	self padding: aNumberOrPoint.
	g := aNumberOrPoint.
	g isPoint ifTrue: [
		g := direction == #horizontal ifTrue: [ g x ] ifFalse: [ g y ]].
	self gap: g.! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 9/11/2024 17:12:38'!
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
				height := height + mle y + gap.
			].
			height := height - gap. "An extra gap was just added"
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"sum up submorphs width, include gap"
				width := width + mle x + gap.
				"use maximum height across submorph"
				height := height max: mle y.
			].
			width := width - gap. "An extra gap was just added"
		].
	^ (width @ height) + (2 * self interiorOrigin)! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/11/2024 17:11:24'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount widths widthToAllocate leftOver x height y cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

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
	].! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/11/2024 17:12:07'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount heights heightToAllocate leftOver y width x cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

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
	].! !

!methodRemoval: LayoutMorph #yGap stamp: 'jmv 9/11/2024 17:12:44'!
LayoutMorph removeSelector: #yGap!
!methodRemoval: LayoutMorph #xGap stamp: 'jmv 9/11/2024 17:12:42'!
LayoutMorph removeSelector: #xGap!
