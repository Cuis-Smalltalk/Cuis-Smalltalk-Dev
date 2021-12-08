'From Cuis 5.0 [latest update: #4890] on 28 September 2021 at 2:40:47 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 12:35:45'!
offAxisHeightFor: aMorph within: availableHeight
	"Answer height for a single morph -- offAxis calculation for a Row"
	
	| availableForPropHeight actualPropHeight |
	availableForPropHeight := availableHeight  - (2 * self ySeparation).
	actualPropHeight := (availableForPropHeight * aMorph layoutSpec proportionalLayoutHeight)
							 max: aMorph minimumLayoutHeight.
	^ actualPropHeight! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 12:35:50'!
offAxisWidthFor: aMorph within: availableWidth
	"Answer width for a single morph -- offAxis calculation for a Column"

	| availableForPropWidth actualPropWidth |
	availableForPropWidth := availableWidth - (2 * self xSeparation).
	actualPropWidth := (availableForPropWidth * aMorph layoutSpec proportionalLayoutWidth)
						 	max: aMorph minimumLayoutWidth.
	^ actualPropWidth! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 12:37:54'!
offAxisXOffsetFor: aMorph within: availableWidth
	"Answer x offset for a single morph -- offAxis calculation for a Column"

	| leftOver |
	leftOver := availableWidth - (2 * self xSeparation).
	^ self xSeparation
			+ (leftOver * aMorph layoutSpec offAxisEdgeWeight). "first X, edge shifted"! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 12:37:57'!
offAxisYOffsetFor: aMorph within: availableHeight
	"Answer y offset for a single morph -- offAxis calculation for a Row"
	
	| leftOver |
	leftOver := availableHeight - (2 * self ySeparation).
	^ self ySeparation
			+ (leftOver * aMorph layoutSpec offAxisEdgeWeight). "first Y, edge shifted"! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 9/28/2021 12:30:37'!
axisEdgeWeight

	^ axisEdgeWeight ifNil: [
		direction == #horizontal
			ifTrue: [0.0]
			ifFalse: [0.5]]! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 9/28/2021 12:29:22'!
beColumn
	"Establish the major layout axis, with default edge weight" 

	direction _ #vertical.
	axisEdgeWeight ifNil: [self axisEdgeWeight: #center].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 9/28/2021 12:29:27'!
beRow
	"Establish the major layout axis, with default edge weight" 

	direction _ #horizontal.
	axisEdgeWeight ifNil: [self axisEdgeWeight: #rowLeft].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 12:34:38'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap widths
	widthToAllocate leftOver x height y |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].
	
	gap := self xSeparation.
	widthToAllocate := (boundsForLayout width - ((visibleSubmorphs size + 1) * gap)).
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left
				+ gap 
				+ (leftOver * self axisEdgeWeight). "first X, edge shifted"

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		height := self offAxisHeightFor: sm within: boundsForLayout height.
		y _ self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm morphPosition: x @ (boundsForLayout top + y).
		sm morphExtent: smWidth @ height.
		x := x + smWidth + gap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 12:34:53'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap heights
	heightToAllocate leftOver y width x |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gap := self ySeparation.
	heightToAllocate := (boundsForLayout height - ((visibleSubmorphs size + 1) * gap)).
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top
				+ gap 
				+ (leftOver * self axisEdgeWeight). "first Y, edge shifted"

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		width := self offAxisWidthFor: sm within: boundsForLayout width.
		x _ self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm morphPosition: boundsForLayout left + x @ y.
		sm morphExtent: width @ smHeight.
		y := y + smHeight + gap.
	]! !


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 9/28/2021 12:37:14'!
offAxisEdgeWeight
	^offAxisEdgeWeight ifNil: [ 0.5 ]! !

!methodRemoval: LayoutMorph #offVerticalMetricFor:withinExtent: stamp: 'jmv 9/28/2021 14:40:24'!
LayoutMorph removeSelector: #offVerticalMetricFor:withinExtent:!
!methodRemoval: LayoutMorph #offHorizontalMetricFor:withinExtent: stamp: 'jmv 9/28/2021 14:40:24'!
LayoutMorph removeSelector: #offHorizontalMetricFor:withinExtent:!
