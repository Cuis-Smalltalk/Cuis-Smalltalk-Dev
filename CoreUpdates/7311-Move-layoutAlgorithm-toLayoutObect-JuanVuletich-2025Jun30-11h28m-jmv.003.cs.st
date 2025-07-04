'From Cuis7.5 [latest update: #7303] on 30 June 2025 at 11:44:48 am'!

!ColumnLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:24:40'!
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

!ColumnLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:23:10'!
layoutMorphs: visibleSubmorphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| gapCount heights heightToAllocate leftOver y cache |

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

!ColumnLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:23:46'!
offAxisWidthFor: aMorph within: availableWidth minLayoutExtentCache: cache
	"Answer width for a single morph -- offAxis calculation for a Column"

	^ (availableWidth * aMorph layoutSpec proportionalLayoutWidth)
						 	max: (aMorph minimumLayoutExtent: cache) x.
! !

!ColumnLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:24:03'!
offAxisXOffsetFor: aMorph within: availableWidth
	"Answer x offset for a single morph -- offAxis calculation for a Column"

	^ availableWidth * aMorph layoutSpec offAxisEdgeWeight. ! !


!RowLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:17:05'!
layoutMorphs: visibleSubmorphs in: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| gapCount widths widthToAllocate leftOver x cache |

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

!RowLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:19:33'!
offAxisHeightFor: aMorph within: availableHeight minLayoutExtentCache: cache
	"Answer height for a single morph -- offAxis calculation for a Row"
	
	^(availableHeight * aMorph layoutSpec proportionalLayoutHeight)
							 max: (aMorph minimumLayoutExtent: cache) y.! !

!RowLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:19:58'!
offAxisYOffsetFor: aMorph within: availableHeight
	"Answer y offset for a single morph -- offAxis calculation for a Row"
	
	^ availableHeight * aMorph layoutSpec offAxisEdgeWeight! !

!RowLayout methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:18:40'!
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


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 6/30/2025 11:39:09'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs |
	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self ].

	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order") ifEmpty: [
		self layoutNeeded: false.
		^self ].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self adjustExtentFor: nil.

	layout layoutMorphs: visibleSubmorphs in: self layoutBounds.
	self layoutNeeded: false.! !

!methodRemoval: LayoutMorph #widthsFor:within:minLayoutExtentCache: stamp: 'jmv 6/30/2025 11:30:03'!
LayoutMorph removeSelector: #widthsFor:within:minLayoutExtentCache:!
!methodRemoval: LayoutMorph #offAxisYOffsetFor:within: stamp: 'jmv 6/30/2025 11:30:32'!
LayoutMorph removeSelector: #offAxisYOffsetFor:within:!
!methodRemoval: LayoutMorph #offAxisWidthFor:within:minLayoutExtentCache: stamp: 'jmv 6/30/2025 11:30:29'!
LayoutMorph removeSelector: #offAxisWidthFor:within:minLayoutExtentCache:!
!methodRemoval: LayoutMorph #offAxisHeightFor:within:minLayoutExtentCache: stamp: 'jmv 6/30/2025 11:30:26'!
LayoutMorph removeSelector: #offAxisHeightFor:within:minLayoutExtentCache:!
!methodRemoval: LayoutMorph #offAxisXOffsetFor:within: stamp: 'jmv 6/30/2025 11:30:31'!
LayoutMorph removeSelector: #offAxisXOffsetFor:within:!
!methodRemoval: LayoutMorph #layoutSubmorphsHorizontallyIn: stamp: 'jmv 6/30/2025 11:34:01'!
LayoutMorph removeSelector: #layoutSubmorphsHorizontallyIn:!
!methodRemoval: LayoutMorph #layoutSubmorphsVerticallyIn: stamp: 'jmv 6/30/2025 11:34:01'!
LayoutMorph removeSelector: #layoutSubmorphsVerticallyIn:!
!methodRemoval: LayoutMorph #heightsFor:within:minLayoutExtentCache: stamp: 'jmv 6/30/2025 11:29:55'!
LayoutMorph removeSelector: #heightsFor:within:minLayoutExtentCache:!
