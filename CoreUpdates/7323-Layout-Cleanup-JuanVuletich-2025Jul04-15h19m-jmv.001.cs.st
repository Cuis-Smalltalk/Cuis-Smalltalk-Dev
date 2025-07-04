'From Cuis7.5 [latest update: #7322] on 4 July 2025 at 3:22:02 pm'!

!Layout methodsFor: 'services' stamp: 'jmv 7/4/2025 14:56:57'!
minimumExtentFor: morphs cachedValues: cachedValues
	self subclassResponsibility ! !


!LayoutSpec methodsFor: 'private' stamp: 'jmv 7/4/2025 14:23:06'!
updateContainingLayout
	| owningLayout |
	morph ifNotNil: [
		owningLayout := morph owner.
		(owningLayout is: #LayoutMorph) ifTrue:
			[ owningLayout layoutSubmorphs ]].! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 7/4/2025 14:31:20'!
allocHeightForFactor: aFactor minLayoutExtentCache: cachedValues

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minHeight
	so breakeven is when
	  amountToAlloc = minHeight / proportion * factor"
	| minHeight |
	minHeight := (self minimumLayoutExtent: cachedValues) y.
	^(self isProportionalHeight)
		ifFalse:  [ minHeight ]
		ifTrue: [ minHeight / (aFactor * self layoutSpec proportionalLayoutHeight) ]! !

!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 7/4/2025 14:24:45'!
allocWidthForFactor: aFactor minLayoutExtentCache: cachedValues

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minWidth
	so breakeven is when
	  amountToAlloc = minWidth / proportion * factor"
	| minWidth |
	minWidth := (self minimumLayoutExtent: cachedValues) x.
	^(self isProportionalWidth)
		ifFalse:  [ minWidth ]
		ifTrue: [ minWidth / (aFactor * self layoutSpec proportionalLayoutWidth) ]! !


!ColumnLayout methodsFor: 'layout' stamp: 'jmv 7/4/2025 14:25:25'!
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
				propDesired := sm layoutSpec proportionalLayoutHeight.
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


!RowLayout methodsFor: 'layout' stamp: 'jmv 7/4/2025 14:32:17'!
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
				propDesired := sm layoutSpec proportionalLayoutWidth.
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

!methodRemoval: LayoutSizeSpec #updateContainingLayout stamp: 'jmv 7/4/2025 15:19:37'!
LayoutSizeSpec removeSelector: #updateContainingLayout!
!methodRemoval: LayoutSizeSpec #proportionaLayoutlHeight stamp: 'jmv 7/4/2025 15:21:24'!
LayoutSizeSpec removeSelector: #proportionaLayoutlHeight!
