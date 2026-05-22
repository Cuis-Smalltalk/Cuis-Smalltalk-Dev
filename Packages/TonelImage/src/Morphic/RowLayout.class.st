Class {
	#name : 'RowLayout',
	#superclass : 'LinearLayout',
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'adjust' }
RowLayout >> adjustMorphBefore: l morphAfter: r adjusterRefPos: adjusterRefPos at: localPoint [
	| lCurrentWidth rCurrentWidth delta lNewWidth rNewWidth ls rs |

	lCurrentWidth := l extentInOwner x max: 1.	"avoid division by zero"
	rCurrentWidth := r extentInOwner x max: 1.	"avoid division by zero"
	delta := localPoint x - adjusterRefPos x.
	delta := delta max: l minimumShrinkWidth - lCurrentWidth.
	delta := delta min: rCurrentWidth - r minimumShrinkWidth.
	delta = 0 ifTrue: [ ^false ].
	rNewWidth := rCurrentWidth - delta.
	lNewWidth := lCurrentWidth + delta.
	ls := l layoutSpec.
	rs := r layoutSpec.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion := lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion := 1.0 - leftNewProportion.
			toDistribute := ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	^true
]

{ #category : 'accessing' }
RowLayout >> axisEdgeWeight: aSymbolOrNumber [
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
			"Row"
			edgeWeight := aSymbolOrNumber
				caseOf: {
					[ #rowLeft ] 	-> [ 0.0 ].
					[ #center ] 		-> [ 0.5 ].
					[ #rowRight ] 	-> [ 1.0 ]
				}
				otherwise: [ self error: 'bad axisEdgeWeight for Row: ', aSymbolOrNumber printString ].
		].
	super axisEdgeWeight: edgeWeight.
]

{ #category : 'initialization' }
RowLayout >> initialize [
	gap := 0.
	axisEdgeWeight := 0.0. "#rowLeft"
]

{ #category : 'testing' }
RowLayout >> isRow [
	^true
]

{ #category : 'layout' }
RowLayout >> layoutMorphs: morphs in: boundsForLayout [
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
	].
]

{ #category : 'services' }
RowLayout >> minimumExtentFor: morphs cachedValues: cachedValues [
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
	^width@height
]

{ #category : 'layout' }
RowLayout >> offAxisHeightFor: aMorph within: availableHeight minLayoutExtentCache: cache [
	"Answer height for a single morph -- offAxis calculation for a Row"
	
	^(availableHeight * aMorph layoutSpec proportionalLayoutHeight)
							 max: (aMorph minimumLayoutExtent: cache) y.
]

{ #category : 'layout' }
RowLayout >> offAxisYOffsetFor: aMorph within: availableHeight [
	"Answer y offset for a single morph -- offAxis calculation for a Row"
	
	^ availableHeight * aMorph layoutSpec offAxisEdgeWeight
]

{ #category : 'layout' }
RowLayout >> widthsFor: visibleMorphs within: overallWidth minLayoutExtentCache: cache [
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
	
	^ allocatedWidths
]
