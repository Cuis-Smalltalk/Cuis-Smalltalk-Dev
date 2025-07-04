'From Cuis7.5 [latest update: #7313] on 3 July 2025 at 3:46:51 pm'!

!ColumnLayout methodsFor: 'services' stamp: 'jmv 7/3/2025 15:45:26'!
minimumExtentFor: morphs cachedValues: cachedValues
	| width height mle |
	width  := 0.
	height := 0.
	morphs do: [ :sm |
		sm visible ifTrue: [
			mle := sm minimumLayoutExtent: cachedValues.
			"use maximum width across submorphs"
			width := width max: mle x.
			"sum up submorph heights, including separation between them"
			height := height + mle y + gap.
		]
	].
	height := height - gap. "An extra gap was just added"
	^width@height! !


!RowLayout methodsFor: 'services' stamp: 'jmv 7/3/2025 15:45:25'!
minimumExtentFor: morphs cachedValues: cachedValues
	| width height mle |
	width  := 0.
	height := 0.
	morphs do: [ :sm |
		sm visible ifTrue: [
			mle := sm minimumLayoutExtent: cachedValues.
			"sum up submorphs width, include gap"
			width := width + mle x + gap.
			"use maximum height across submorph"
			height := height max: mle y.
		]
	].
	width := width - gap. "An extra gap was just added"
	^width@height! !


!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 7/3/2025 15:46:32'!
minimumExtent
	"Answer size sufficient to frame my submorphs."
	
	^ (layout minimumExtentFor: submorphs cachedValues: IdentityDictionary new) + (2 * self interiorOrigin)! !


!ColumnLayout methodsFor: 'layout' stamp: 'jmv 7/3/2025 15:41:48'!
heightsFor: visibleMorphs within: overallHeight minLayoutExtentCache: cache
	"Answer array of morphHeights for visibleSubs."
	"Preconditions: 
		ySepararations already subtracted from overallHeight.
		overallHeight is large enough to contain minimumLayoutHeights of visibleSubs."

	| n scaleFactor heightToAllocate
	  sumOfPropMin sumOfPropDesired sumOfPropOriginallyDesired sumOfFixedMin
	  allocatedHeights propIndices desiredProps
	  maxOfMinAllocHeight |
	n := visibleMorphs size.
	sumOfFixedMin := 0.  "pixels"
	sumOfPropMin := 0.  "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	sumOfPropOriginallyDesired := 0. "Different from sumOfPropDesired if some #maximumLayoutHeight"
	allocatedHeights := Array new: n.
	desiredProps := Array new: n.
	propIndices := Set new. "keep set of proportional morph indices"
	1 to: n do: [ :index | | sm layoutHeightMin |
		sm := visibleMorphs at: index.
		layoutHeightMin := (sm minimumLayoutExtent: cache) y.
		allocatedHeights at: index put: layoutHeightMin.
		(sm isProportionalHeight)
			ifTrue:  [ | propDesired maxHeight |
				propDesired := sm layoutSpec privateProportionalHeight.
				sumOfPropOriginallyDesired := sumOfPropOriginallyDesired + propDesired.
				maxHeight := sm maximumLayoutHeight.
				(maxHeight notNil and: [ maxHeight < (propDesired * overallHeight)])
					ifTrue: [
						allocatedHeights at: index put: maxHeight.
						sumOfFixedMin := sumOfFixedMin + maxHeight ]
					ifFalse: [
						propIndices add: index.
						desiredProps at: index put: propDesired.
						sumOfPropDesired := sumOfPropDesired + propDesired.
						sumOfPropMin := sumOfPropMin + layoutHeightMin ]]
			ifFalse: [ "Allocate height for non-proportional-height morphs"
				sumOfFixedMin := sumOfFixedMin + layoutHeightMin ]].

	((propIndices size = 0) "already finished"
	  or: [(overallHeight - (sumOfFixedMin + sumOfPropMin)) < 2]) "close enough"
		ifTrue: [ ^ allocatedHeights ]. 
			
	"All fixed heights allocated; rest is for proportional + leftOver"
	heightToAllocate := (overallHeight - sumOfFixedMin) max: 0.
	scaleFactor := 1.0.
	"If sum of requested proportional heights is more than 1.0,
	scale to make it all fit."
	sumOfPropOriginallyDesired > 0.999 ifTrue: [
		scaleFactor := 1.0 / sumOfPropDesired ].

	"Do simple default scaling"
	propIndices do: [ :morphIndex |
		allocatedHeights at: morphIndex 
			put:  ((allocatedHeights at: morphIndex) max:
				(heightToAllocate 
					* scaleFactor 
						* (desiredProps at: morphIndex))) ].

	"Find the min alloc size at which adding height is above max of minHeights
	 Keep the proportions over the minHeights at this allocation."
	maxOfMinAllocHeight := propIndices max: [ :morphIndex | 
		(visibleMorphs at: morphIndex) allocHeightForFactor: scaleFactor minLayoutExtentCache: cache ].

	"Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(heightToAllocate <= maxOfMinAllocHeight) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ heightToAllocate := heightToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array new: visibleMorphs size.
		propIndices do: [ :morphIndex | | heightAtBalance | 
			heightAtBalance := maxOfMinAllocHeight * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
				put: heightAtBalance "delta above min height"
			 	  	- (((visibleMorphs at: morphIndex) minimumLayoutExtent: cache) y)].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex | 
				allocatedHeights at: morphIndex
					put: (((visibleMorphs at: morphIndex) minimumLayoutExtent: cache) y)
						+ (((heightToAllocate - sumOfPropMin ) max: 0.0)
							* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)]]].
	
	^ allocatedHeights! !

!ColumnLayout methodsFor: 'layout' stamp: 'jmv 7/3/2025 15:40:22'!
layoutMorphs: morphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleMorphs gapCount heights heightToAllocate leftOver y cache |

	visibleMorphs := (morphs select: [ :m | m visible ]) reversed. "Display Order"
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


!RowLayout methodsFor: 'layout' stamp: 'jmv 7/3/2025 15:40:56'!
layoutMorphs: morphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleMorphs gapCount widths widthToAllocate leftOver x cache |

	visibleMorphs := (morphs select: [ :m | m visible ]) reversed. "Display Order"
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

!RowLayout methodsFor: 'layout' stamp: 'jmv 7/3/2025 15:41:34'!
widthsFor: visibleMorphs within: overallWidth minLayoutExtentCache: cache
	"Answer array of morphWidths for visibleSubs."
	"Preconditions: 
		xSepararations already subtracted from overallWidth.
		overallWidth is large enough to contain minimumLayoutWidths of visibleSubs."
	
	| n scaleFactor widthToAllocate
	  sumOfPropMin sumOfPropDesired sumOfFixedMin
	  allocatedWidths propIndices desiredProps
	  maxOfMinAllocWidth |
	n := visibleMorphs size.
	sumOfFixedMin := 0. "pixels"
	sumOfPropMin := 0. "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	allocatedWidths := Array new: n.
	desiredProps := Array new: n.
	propIndices := Set new. "keep set of proportional morph indices"
	1 to: n do: [ :index | | sm layoutWidthMin |
		sm := visibleMorphs at: index.
		layoutWidthMin := (sm minimumLayoutExtent: cache) x.
		allocatedWidths at: index put: layoutWidthMin.
		(sm isProportionalWidth)
			ifTrue:  [ | propDesired |
				propIndices add: index.
				propDesired := sm layoutSpec privateProportionalWidth.
				desiredProps at: index put: propDesired.
				sumOfPropDesired := sumOfPropDesired + propDesired.
				sumOfPropMin := sumOfPropMin + layoutWidthMin ]
			ifFalse: [ "Allocate width for non-proportional-width morphs"
				sumOfFixedMin := sumOfFixedMin + layoutWidthMin ]].

	((propIndices size = 0) "already finished"
	  or: [(overallWidth - (sumOfFixedMin + sumOfPropMin)) < 2]) "close enough"
		ifTrue: [ ^ allocatedWidths ]. 
			
	"All fixed widths allocated; rest is for proportional + leftOver"
	widthToAllocate := (overallWidth - sumOfFixedMin) max: 0.
	scaleFactor := 1.0.
	"If sum of requested proportional widths is more than 1.0,
	scale to make it all fit."
	sumOfPropDesired > 1.0 ifTrue: [
		scaleFactor := 1.0 / sumOfPropDesired ].
	
	"Do simple default scaling"
	propIndices do: [ :morphIndex |
		allocatedWidths at: morphIndex 
			put:  ((allocatedWidths at: morphIndex) max:
				(widthToAllocate 
					* scaleFactor 
						* (desiredProps at: morphIndex))) ].

	"Find the min alloc size at which adding width is above max of minWidths
	 Keep the proportions over the minWidths at this allocation."
	maxOfMinAllocWidth := propIndices max: [ :morphIndex | 
		(visibleMorphs at: morphIndex) allocWidthForFactor: scaleFactor minLayoutExtentCache: cache ].

     "Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(widthToAllocate <= maxOfMinAllocWidth) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ widthToAllocate := widthToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array new: visibleMorphs size.
		propIndices do: [ :morphIndex | | widthAtBalance | 
			widthAtBalance := maxOfMinAllocWidth * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
				put: widthAtBalance "delta above min width"
					- (((visibleMorphs at: morphIndex) minimumLayoutExtent: cache) x)].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex |
				allocatedWidths at: morphIndex
					put: (((visibleMorphs at: morphIndex) minimumLayoutExtent: cache) x) 
						+ (((widthToAllocate - sumOfPropMin ) max: 0.0)
							* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)]]].
	
	^ allocatedWidths! !

!methodRemoval: LayoutMorph #minimumExtent: stamp: 'jmv 7/3/2025 15:46:37'!
LayoutMorph removeSelector: #minimumExtent:!

!ColumnLayout reorganize!
('accessing' axisEdgeWeight:)
('initialization' initialize)
('layout' heightsFor:within:minLayoutExtentCache: layoutMorphs:in: offAxisWidthFor:within:minLayoutExtentCache: offAxisXOffsetFor:within:)
('adjust' adjustMorphBefore:morphAfter:adjusterRefPos:at:)
('services' minimumExtentFor:cachedValues:)
('testing' isColumn)
!


!RowLayout reorganize!
('accessing' axisEdgeWeight:)
('initialization' initialize)
('layout' layoutMorphs:in: offAxisHeightFor:within:minLayoutExtentCache: offAxisYOffsetFor:within: widthsFor:within:minLayoutExtentCache:)
('adjust' adjustMorphBefore:morphAfter:adjusterRefPos:at:)
('services' minimumExtentFor:cachedValues:)
('testing' isRow)
!

