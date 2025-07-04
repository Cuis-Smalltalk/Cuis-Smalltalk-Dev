'From Cuis7.5 [latest update: #7313] on 3 July 2025 at 3:37:48 pm'!

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 7/3/2025 15:37:29'!
minimumExtent: cachedValues
	"Answer size sufficient to frame my submorphs."
	
	| width height mle |
	width  := 0.
	height := 0.
	layout isColumn
		ifTrue: [ "Column"
			submorphs do: [ :sm |
				sm visible ifTrue: [
					mle := sm minimumLayoutExtent: cachedValues.
					"use maximum width across submorphs"
					width := width max: mle x.
					"sum up submorph heights, including separation between them"
					height := height + mle y + layout gap.
				]
			].
			height := height - layout gap. "An extra gap was just added"
		]
		ifFalse: [ "Row"
			submorphs do: [ :sm |
				sm visible ifTrue: [
					mle := sm minimumLayoutExtent: cachedValues.
					"sum up submorphs width, include gap"
					width := width + mle x + layout gap.
					"use maximum height across submorph"
					height := height max: mle y.
				]
			].
			width := width - layout gap. "An extra gap was just added"
		].
	^ (width @ height) + (2 * self interiorOrigin)! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/3/2025 15:36:28'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self ].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self adjustExtentFor: nil.

	layout layoutMorphs: submorphs in: self layoutBounds.
	self layoutNeeded: false.! !


!ColumnLayout methodsFor: 'layout' stamp: 'jmv 7/3/2025 15:36:07'!
layoutMorphs: morphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount heights heightToAllocate leftOver y cache |

	visibleSubmorphs := (morphs select: [ :m | m visible ]) reversed. "Display Order"
	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	heightToAllocate := boundsForLayout height - (gapCount * gap).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top + (leftOver * axisEdgeWeight).

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		| width x |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + gap.
	].! !


!RowLayout methodsFor: 'layout' stamp: 'jmv 7/3/2025 15:36:04'!
layoutMorphs: morphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gapCount widths widthToAllocate leftOver x cache |

	visibleSubmorphs := (morphs select: [ :m | m visible ]) reversed. "Display Order"
	gapCount := visibleSubmorphs size - 1. "gap between each contained Morph"
	widthToAllocate := boundsForLayout width - (gapCount * gap).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left + (leftOver * axisEdgeWeight).

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		| height y |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + gap.
	].! !

!methodRemoval: LayoutMorph #submorphsToLayout stamp: 'jmv 7/3/2025 15:37:33'!
LayoutMorph removeSelector: #submorphsToLayout!
