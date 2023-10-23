'From Cuis 6.0 [latest update: #6053] on 23 October 2023 at 3:54:25 pm'!

!Morph methodsFor: 'layout' stamp: 'jmv 10/23/2023 11:11:45'!
minimumLayoutExtent: cachedValues
	"I combine information from a Morph and its optional LayoutSpec"

	^cachedValues at: self ifAbsentPut: [
		self minimumExtent ].! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2023 15:47:10'!
allocHeightForFactor: aFactor minLayoutExtentCache: cachedValues

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minHeight
	so breakeven is when
	  amountToAlloc = minHeight / proportion * factor"
	| minHeight |
	minHeight := (self minimumLayoutExtent: cachedValues) y.
	^(self isProportionalHeight)
		ifFalse:  [ minHeight ]
		ifTrue: [ minHeight / (aFactor * self layoutSpec privateProportionalHeight) ]! !

!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2023 15:47:14'!
allocWidthForFactor: aFactor minLayoutExtentCache: cachedValues

	"Morphs with LayoutSpecs may specify propottional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minWidth
	so breakeven is when
	  amountToAlloc = minWidth / proportion * factor"
	| minWidth |
	minWidth := (self minimumLayoutExtent: cachedValues) x.
	^(self isProportionalWidth)
		ifFalse:  [ minWidth ]
		ifTrue: [ minWidth / (aFactor * self layoutSpec privateProportionalWidth) ]! !

!PlacedMorph methodsFor: 'layout' stamp: 'jmv 10/23/2023 11:18:04'!
minimumLayoutExtent: cachedValues
	"I combine information from a Morph and its optional LayoutSpec"
	| minExtent |

	^cachedValues at: self ifAbsentPut: [
		minExtent :=  self minimumExtent. "from morph (me)."
		layoutSpec
			ifNil: [ minExtent ] 
			ifNotNil: [ :ls | minExtent max: ls minimumSpecExtent ]]! !


!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2023 11:26:45'!
minimumExtent: cachedValues
	"Answer size sufficient to frame my submorphs."
	
	| width height mle |
	width  := 0.
	height := 0.
	(self direction =  #vertical)
		ifTrue: [ "Column"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"use maximum width across submorphs"
				width := width max: mle x.
				"sum up submorph heights, including separation"
				height := height + mle y + self ySeparation.
			].
			width  := width + (2 * self xSeparation). "separation on each side"
			height := height + self ySeparation. "one side already separated"
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"sum up submorphs width"
				width := width + mle x + self xSeparation.
				"use maximum height across submorph"
				height := height max: mle y.
			].
			height := height + (2 * self ySeparation). "separation on each side"
			width  := width + self xSeparation.       "one side already separated"
		].

	^ (width @ height) + self extentBorder! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/23/2023 15:47:48'!
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
	sumOfPropMin  := 0.  "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	sumOfPropOriginallyDesired := 0. "Different from sumOfPropDesired if some #maximumLayoutHeight"
	allocatedHeights := Array ofSize: numSubs.
	desiredProps       := Array ofSize: numSubs.
	propIndices          := Set new. "keep set of proportional morph indices"
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
											* (desiredProps at: morphIndex)))
	].

	"Find the min alloc size at which adding height is above max of minHeights
	 Keep the proportions over the minHeights at this allocation."
	maxOfMinAllocHeight := propIndices max: [ :morphIndex | 
		(visibleSubs at: morphIndex) allocHeightForFactor: scaleFactor minLayoutExtentCache: cache
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
							   	- (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) y)
		].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex | 
				allocatedHeights at: morphIndex
								put: (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) y)
									+ (((heightToAllocate - sumOfPropMin ) max: 0.0)
										* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)
			]
		]
	].
	
	^ allocatedHeights! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/23/2023 15:48:15'!
offAxisHeightFor: aMorph within: availableHeight minLayoutExtentCache: cache
	"Answer height for a single morph -- offAxis calculation for a Row"
	
	| availableForPropHeight actualPropHeight |
	availableForPropHeight := availableHeight  - (2 * self ySeparation).
	actualPropHeight := (availableForPropHeight * aMorph layoutSpec proportionalLayoutHeight)
							 max: (aMorph minimumLayoutExtent: cache) y.
	^ actualPropHeight! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/23/2023 15:48:25'!
offAxisWidthFor: aMorph within: availableWidth minLayoutExtentCache: cache
	"Answer width for a single morph -- offAxis calculation for a Column"

	| availableForPropWidth actualPropWidth |
	availableForPropWidth := availableWidth - (2 * self xSeparation).
	actualPropWidth := (availableForPropWidth * aMorph layoutSpec proportionalLayoutWidth)
						 	max: (aMorph minimumLayoutExtent: cache) x.
	^ actualPropWidth! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/23/2023 15:47:52'!
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
	sumOfPropMin   := 0. "pixels"
	sumOfPropDesired := 0. "sum of percentage fractions; may be more than 100%"
	allocatedWidths := Array ofSize: numSubs.
	desiredProps      := Array ofSize: numSubs.
	propIndices         := Set new. "keep set of proportional morph indices"
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
				sumOfPropMin := sumOfPropMin + layoutWidthMin.
			]
			ifFalse: [ "Allocate width for non-proportional-width morphs"
				sumOfFixedMin := sumOfFixedMin + layoutWidthMin.
			]
	 ].

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
											* (desiredProps at: morphIndex)))
	].

	"Find the min alloc size at which adding width is above max of minWidths
	 Keep the proportions over the minWidths at this allocation."
	maxOfMinAllocWidth := propIndices max: [ :morphIndex | 
		(visibleSubs at: morphIndex) allocWidthForFactor: scaleFactor minLayoutExtentCache: cache
	].

     "Below the balance point, calculate proportions from belowBalanceDeltas,
	above, use desiredProps"
	(widthToAllocate <= maxOfMinAllocWidth) ifTrue: [ | belowBalanceDeltas sumOfDeltas | 
		(sumOfPropDesired < 1.0) "Trim off space to save"
			ifTrue: [ widthToAllocate := widthToAllocate * sumOfPropDesired ].
		belowBalanceDeltas := Array ofSize: visibleSubs size.
		propIndices do: [ :morphIndex | | widthAtBalance | 
			widthAtBalance := maxOfMinAllocWidth * scaleFactor * (desiredProps at: morphIndex).
			belowBalanceDeltas at: morphIndex
								   put: widthAtBalance "delta above min width"
							   	- (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) x)
		].
		sumOfDeltas := belowBalanceDeltas sum: [ :b | b ifNil: [0]].
		(sumOfDeltas > propIndices size) ifTrue: [ "space desired < 100%"
			propIndices do: [ :morphIndex |
				allocatedWidths at: morphIndex
								put: (((visibleSubs at: morphIndex) minimumLayoutExtent: cache) x) 
									+ (((widthToAllocate - sumOfPropMin ) max: 0.0)
										* (belowBalanceDeltas at: morphIndex) / sumOfDeltas)
			]
		]
	].
	
	^ allocatedWidths! !


!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 10/23/2023 11:24:40'!
minimumExtent
	"Answer size sufficient to frame my submorphs."
	
	^self minimumExtent: IdentityDictionary new! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/23/2023 15:48:30'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap widths
	widthToAllocate leftOver x height y 
	cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].
	
	gap := self xSeparation.
	widthToAllocate := (boundsForLayout width - ((visibleSubmorphs size + 1) * gap)).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left
				+ gap 
				+ (leftOver * self axisEdgeWeight). "first X, edge shifted"

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + gap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/23/2023 15:48:37'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap heights
	heightToAllocate leftOver y width x 
	cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gap := self ySeparation.
	heightToAllocate := (boundsForLayout height - ((visibleSubmorphs size + 1) * gap)).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top
				+ gap 
				+ (leftOver * self axisEdgeWeight). "first Y, edge shifted"

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + gap.
	]! !


!LayoutSpec methodsFor: 'layout' stamp: 'jmv 10/23/2023 11:15:58'!
minimumSpecExtent
	"If fixedHeight is not nil, use it.  
	 If fixdHeight and propostionlHeight are nil, use morphHeight"

	| meo h w |
	meo := morph morphExtentInOwner.
	w := fixedWidth ifNil: [ proportionalWidth ifNotNil: [ 0 ] ifNil: [  meo x ] ].
	h := fixedHeight ifNil: [ proportionalHeight ifNotNil: [ 0 ] ifNil: [ meo y ] ].
	^w@h! !

!methodRemoval: LayoutSpec #minimumSpecWidth stamp: 'jmv 10/23/2023 15:53:20'!
LayoutSpec removeSelector: #minimumSpecWidth!
!methodRemoval: LayoutSpec #minimumSpecHeight stamp: 'jmv 10/23/2023 15:53:20'!
LayoutSpec removeSelector: #minimumSpecHeight!
!methodRemoval: LayoutMorph #offAxisHeightFor:within: stamp: 'jmv 10/23/2023 15:52:07'!
LayoutMorph removeSelector: #offAxisHeightFor:within:!
!methodRemoval: LayoutMorph #offAxisWidthFor:within: stamp: 'jmv 10/23/2023 15:52:08'!
LayoutMorph removeSelector: #offAxisWidthFor:within:!
!methodRemoval: LayoutMorph #widthsFor:within: stamp: 'jmv 10/23/2023 15:52:10'!
LayoutMorph removeSelector: #widthsFor:within:!
!methodRemoval: LayoutMorph #heightsFor:within: stamp: 'jmv 10/23/2023 15:52:12'!
LayoutMorph removeSelector: #heightsFor:within:!
!methodRemoval: PlacedMorph #minimumLayoutHeight stamp: 'jmv 10/23/2023 15:52:25'!
PlacedMorph removeSelector: #minimumLayoutHeight!
!methodRemoval: PlacedMorph #allocWidthForFactor: stamp: 'jmv 10/23/2023 15:52:38'!
PlacedMorph removeSelector: #allocWidthForFactor:!
!methodRemoval: PlacedMorph #minimumLayoutWidth stamp: 'jmv 10/23/2023 15:53:16'!
PlacedMorph removeSelector: #minimumLayoutWidth!
!methodRemoval: PlacedMorph #minimumLayoutExtent stamp: 'jmv 10/23/2023 15:52:18'!
PlacedMorph removeSelector: #minimumLayoutExtent!
!methodRemoval: PlacedMorph #allocHeightForFactor: stamp: 'jmv 10/23/2023 15:52:16'!
PlacedMorph removeSelector: #allocHeightForFactor:!
!methodRemoval: Morph #minimumLayoutHeight stamp: 'jmv 10/23/2023 15:52:26'!
Morph removeSelector: #minimumLayoutHeight!
!methodRemoval: Morph #minimumLayoutWidth stamp: 'jmv 10/23/2023 15:53:10'!
Morph removeSelector: #minimumLayoutWidth!
