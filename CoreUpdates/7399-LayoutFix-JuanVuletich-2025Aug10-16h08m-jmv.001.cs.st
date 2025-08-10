'From Cuis7.5 [latest update: #7398] on 10 August 2025 at 4:09:45 pm'!

!ColumnLayout methodsFor: 'layout' stamp: 'jmv 8/10/2025 16:09:31'!
layoutMorphs: morphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleMorphs gapCount heights heightToAllocate leftOver y cache |

	visibleMorphs := (morphs select: [ :m | m visible ]) reversed. "Display Order"
	visibleMorphs isEmpty ifTrue: [ ^self ].
	gapCount := visibleMorphs size - 1. "gap between each contained Morph"
	heightToAllocate := boundsForLayout height - (gapCount * gap).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleMorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top + (leftOver * axisEdgeWeight).

	visibleMorphs with: heights do: [ :sm :smHeight |
		| width x |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + gap.
	].! !


!RowLayout methodsFor: 'layout' stamp: 'jmv 8/10/2025 16:09:35'!
layoutMorphs: morphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleMorphs gapCount widths widthToAllocate leftOver x cache |

	visibleMorphs := (morphs select: [ :m | m visible ]) reversed. "Display Order"
	visibleMorphs isEmpty ifTrue: [ ^self ].
	gapCount := visibleMorphs size - 1. "gap between each contained Morph"
	widthToAllocate := boundsForLayout width - (gapCount * gap).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleMorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left + (leftOver * axisEdgeWeight).

	visibleMorphs with: widths do: [ :sm :smWidth |
		| height y |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + gap.
	].! !

