'From Cuis7.1 [latest update: #6693] on 11 September 2024 at 9:20:03 am'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/11/2024 09:19:37'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs xGap gapCount widths widthToAllocate leftOver x height y cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].
	
	xGap := self xSeparation.
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

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/11/2024 09:19:56'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs yGap gapCount heights heightToAllocate leftOver y width x cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	yGap := self ySeparation.
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

