'From Cuis6.3 [latest update: #6205] on 2 February 2024 at 4:00:12 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 2/2/2024 15:44:51'!
heightsFor: visibleSubs within: overallHeight minLayoutExtentCache: cache
	"Answer array of morphHeights for visibleSubs."
	"Preconditions: 
		ySepararations already subtracted from overallHeight.
		overallHeight is large enough to contain minimumLayoutHeights of visibleSubs."

	| numSubs scaleFactor heightToAllocate
	  sumOfPropMin sumOfPropDesired sumOfPropOriginallyDesired sumOfFixedMin
	  allocatedHeights propIndices desiredProps
	  maxOfMinAllocHeight |
	numSubs := visibleSubs size.
	sumOfFixedMin := 0.  "pixels"
	sumOfPropMin := 0.  "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	sumOfPropOriginallyDesired := 0. "Different from sumOfPropDesired if some #maximumLayoutHeight"
	allocatedHeights := Array new: numSubs.
	desiredProps := Array new: numSubs.
	propIndices := Set new. "keep set of proportional morph indices"
	1 to: numSubs do: [ :index | | sm layoutHeightMin |
		sm := visibleSubs at: index.
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
		(visibleSubs at: morphIndex) allocHeightForFactor: scaleFactor minLayoutExtentCache: cache ].

	"Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(heightToAllocate <= maxOfMinAllocHeight) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ heightToAllocate := heightToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array new: visibleSubs size.
		propIndices do: [ :morphIndex | | heightAtBalance | 
			heightAtBalance := maxOfMinAllocHeight * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
				put: heightAtBalance "delta above min height"
			 	  	- (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) y)].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex | 
				allocatedHeights at: morphIndex
					put: (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) y)
						+ (((heightToAllocate - sumOfPropMin ) max: 0.0)
							* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)]]].
	
	^ allocatedHeights! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 2/2/2024 15:44:56'!
widthsFor: visibleSubs within: overallWidth minLayoutExtentCache: cache
	"Answer array of morphWidths for visibleSubs."
	"Preconditions: 
		xSepararations already subtracted from overallWidth.
		overallWidth is large enough to contain minimumLayoutWidths of visibleSubs."
	
	| numSubs scaleFactor widthToAllocate
	  sumOfPropMin sumOfPropDesired sumOfFixedMin
	  allocatedWidths propIndices desiredProps
	  maxOfMinAllocWidth |
	numSubs := visibleSubs size.
	sumOfFixedMin := 0. "pixels"
	sumOfPropMin := 0. "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	allocatedWidths := Array new: numSubs.
	desiredProps := Array new: numSubs.
	propIndices := Set new. "keep set of proportional morph indices"
	1 to: numSubs do: [ :index | | sm layoutWidthMin |
		sm := visibleSubs at: index.
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
		(visibleSubs at: morphIndex) allocWidthForFactor: scaleFactor minLayoutExtentCache: cache ].

     "Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(widthToAllocate <= maxOfMinAllocWidth) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ widthToAllocate := widthToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array new: visibleSubs size.
		propIndices do: [ :morphIndex | | widthAtBalance | 
			widthAtBalance := maxOfMinAllocWidth * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
				put: widthAtBalance "delta above min width"
					- (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) x)].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex |
				allocatedWidths at: morphIndex
					put: (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) x) 
						+ (((widthToAllocate - sumOfPropMin ) max: 0.0)
							* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)]]].
	
	^ allocatedWidths! !

