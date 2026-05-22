Class {
	#name : 'ColumnLayout',
	#superclass : 'LinearLayout',
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'adjust' }
ColumnLayout >> adjustMorphBefore: t morphAfter: b adjusterRefPos: adjusterRefPos at: localPoint [
	| tCurrentHeight bCurrentHeight delta tNewHeight bNewHeight ts bs |

	tCurrentHeight := t extentInOwner y max: 1.	"avoid division by zero"
	bCurrentHeight := b extentInOwner y max: 1.	"avoid division by zero"
	delta := localPoint y - adjusterRefPos y.
	delta := delta max: t minimumShrinkHeight - tCurrentHeight.
	delta := delta min: bCurrentHeight - b minimumShrinkHeight.
	delta = 0 ifTrue: [ ^false ].
	tNewHeight := tCurrentHeight + delta.
	bNewHeight := bCurrentHeight - delta.
	ts := t layoutSpec.
	bs := b layoutSpec.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion := tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion := 1.0 - topNewProportion.
			toDistribute := ts proportionalLayoutHeight + bs proportionalLayoutHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	^true
]

{ #category : 'accessing' }
ColumnLayout >> axisEdgeWeight: aSymbolOrNumber [
	"Along the Major or Layout Axis, subMorphs may be attracted toward one side (0.0) or the other (1.0).  This is the LayoutMorph's axisEdgeWeight.  A numeric value is between 0.0 and 1.0

	If a Row, one can specify this weight symbolically as
	  { #rowLeft (0.0), #center (0.5), #rowRight (1.0)}

	If a Column, one can specify the weight symbolically as:
	  { #columnTop (0.0), #center (0.5), #columnBottom (1.0) }"
"	
	self axisEdgeWeight: #rowLeft.	-- axis must be horizontal 
	self axisEdgeWeight: #columnTop.	-- axis must be vertical
"
	| edgeWeight |
	(aSymbolOrNumber is: #Number)
		ifTrue: [ (aSymbolOrNumber between: 0.0 and: 1.0)
			ifTrue: [edgeWeight := aSymbolOrNumber ] 
			ifFalse: [self error: 'axisEdgeWeight ', aSymbolOrNumber printString, ' is out of range 0.0 to 1.0']
		]
		ifFalse: [
			"vertical => Column"
			edgeWeight := aSymbolOrNumber
				caseOf: {
					[ #columnTop ] -> [ 0.0 ].
					[ #center ] -> [ 0.5 ].
					[ #columnBottom ] -> [ 1.0 ]
				}
				otherwise: [ self error: 'bad axisEdgeWeight for Column: ', aSymbolOrNumber printString ].
		].
	super axisEdgeWeight: edgeWeight.
]

{ #category : 'layout' }
ColumnLayout >> heightsFor: visibleMorphs within: overallHeight minLayoutExtentCache: cache [
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
	
	^ allocatedHeights
]

{ #category : 'initialization' }
ColumnLayout >> initialize [
	gap := 0.
	axisEdgeWeight := 0.0. "#columnTop"
]

{ #category : 'testing' }
ColumnLayout >> isColumn [
	^true
]

{ #category : 'layout' }
ColumnLayout >> layoutMorphs: morphs in: boundsForLayout [
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
	].
]

{ #category : 'services' }
ColumnLayout >> minimumExtentFor: morphs cachedValues: cachedValues [
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
	^width@height
]

{ #category : 'layout' }
ColumnLayout >> offAxisWidthFor: aMorph within: availableWidth minLayoutExtentCache: cache [
	"Answer width for a single morph -- offAxis calculation for a Column"

	^ (availableWidth * aMorph layoutSpec proportionalLayoutWidth)
						 	max: (aMorph minimumLayoutExtent: cache) x.

]

{ #category : 'layout' }
ColumnLayout >> offAxisXOffsetFor: aMorph within: availableWidth [
	"Answer x offset for a single morph -- offAxis calculation for a Column"

	^ availableWidth * aMorph layoutSpec offAxisEdgeWeight. 
]
