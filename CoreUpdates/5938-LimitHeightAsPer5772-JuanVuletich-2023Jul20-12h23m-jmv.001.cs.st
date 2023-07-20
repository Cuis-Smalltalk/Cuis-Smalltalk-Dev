'From Cuis 6.0 [latest update: #5937] on 20 July 2023 at 12:25:09 pm'!

!Morph methodsFor: 'layout' stamp: 'jmv 7/20/2023 11:52:18'!
maximumLayoutHeight
	"Some morphs may be more specific."

	^nil! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2023 11:18:00'!
desiredHeight
	^ scroller desiredHeight + (self xtraBorder + borderWidth + 1 * 2)! !

!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2023 11:53:09'!
maximumLayoutHeight
	^self desiredHeight! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/20/2023 12:15:02'!
heightsFor: visibleSubs within: overallHeight
	"Answer array of morphHeights for visibleSubs."
	"Preconditions: 
		ySepararations already subtracted from overallHeight.
		overallHeight is large enough to contain minimumLayoutHeights of visibleSubs."

	| numSubs scaleFactor heightToAllocate
	  sumOfPropMin sumOfPropDesired sumOfFixedMin
	  allocatedHeights propIndices desiredProps
	  maxOfMinAllocHeight
	 |
	numSubs := visibleSubs size.
	sumOfFixedMin := 0.  "pixels"
	sumOfPropMin  := 0.  "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	allocatedHeights := Array ofSize: numSubs.
	desiredProps       := Array ofSize: numSubs.
	propIndices          := Set new. "keep set of proportional morph indices"
	1 to: numSubs do: [ :index | | sm layoutHeightMin |
		sm := visibleSubs at: index.
		layoutHeightMin := sm minimumLayoutHeight.
		allocatedHeights at: index put: layoutHeightMin.
		(sm isProportionalHeight)
			ifTrue:  [ | propDesired maxHeight |
				propDesired := sm layoutSpec privateProportionalHeight.
				maxHeight := sm maximumLayoutHeight.
				(maxHeight notNil and: [ maxHeight < (propDesired * overallHeight)])
					ifTrue: [
						allocatedHeights at: index put: maxHeight.
						sumOfFixedMin := sumOfFixedMin + maxHeight ]
					ifFalse: [
						propIndices add: index.
						desiredProps at: index put: propDesired.
						sumOfPropDesired := sumOfPropDesired + propDesired.
						sumOfPropMin := sumOfPropMin + layoutHeightMin ].
			]
			ifFalse: [ "Allocate height for non-proportional-height morphs"
				sumOfFixedMin := sumOfFixedMin + layoutHeightMin.
			]
	 ].

	((propIndices size = 0) "already finished"
	  or: [(overallHeight - (sumOfFixedMin + sumOfPropMin)) < 2]) "close enough"
		ifTrue: [ ^ allocatedHeights ]. 
			
	"All fixed heights allocated; rest is for proportional + leftOver"
	heightToAllocate := (overallHeight - sumOfFixedMin) max: 0.
	scaleFactor := 1.0 / sumOfPropDesired.

	"Do simple default scaling"
	propIndices do: [ :morphIndex |
			allocatedHeights at: morphIndex 
							put:  ((allocatedHeights at: morphIndex) max:
									(heightToAllocate 
										* scaleFactor 
											* (desiredProps at: morphIndex)))
	].

	"Find the min alloc size at which adding height is above max of minHeights
	 Keep the proportions over the minHeights at this allocation."
	maxOfMinAllocHeight := propIndices max: [ :morphIndex | 
		(visibleSubs at: morphIndex) allocHeightForFactor: scaleFactor 
	].

    "Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(heightToAllocate <= maxOfMinAllocHeight) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ heightToAllocate := heightToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array ofSize: visibleSubs size.
		propIndices do: [ :morphIndex | | heightAtBalance | 
			heightAtBalance := maxOfMinAllocHeight * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
								   put: heightAtBalance "delta above min height"
							   	- ((visibleSubs at: morphIndex) minimumLayoutHeight)
		].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex | 
				allocatedHeights at: morphIndex
								put: ((visibleSubs at: morphIndex) minimumLayoutHeight)
									+ (((heightToAllocate - sumOfPropMin ) max: 0.0)
										* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)
			]
		]
	].
	
	^ allocatedHeights! !

