'From Cuis 5.0 [latest update: #4891] on 28 September 2021 at 3:55:27 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:52:47'!
morphExtentInOwner

	^self fullBoundsInOwner extent! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:31:59'!
fitInto: aRectangle
	"Change the position and extent of this morph. Arguments are owner's coordinates.
	See inheritance: For general, non resizeable morphs, adjust position and scale."

	self morphPosition: aRectangle origin.
	self morphExtentInOwner: aRectangle extent.! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:17:52'!
morphExtentInOwner

	^self externalizeDistance: extent! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:17:17'!
morphExtentInOwner: newExtent

	self morphExtent: (self internalizeDistance: newExtent).! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:21:10'!
morphHeightInOwner: newHeight

	self morphExtentInOwner: self morphExtentInOwner x @ newHeight! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:21:31'!
morphWidthInOwner: newWidth

	self morphExtentInOwner: newWidth @ self morphExtentInOwner y! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:32:02'!
fitInto: aRectangle
	"Change the position and extent of this morph. Arguments are owner's coordinates.
	See inheritance: For general, non resizeable morphs, adjust position and scale."

	self morphPosition: aRectangle origin.
	self morphExtentInOwner: aRectangle extent.! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:25:52'!
morphExtentInOwner

	^self externalizeDistance: extent! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:25:58'!
morphExtentInOwner: newExtent

	self morphExtent: (self internalizeDistance: newExtent).! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:26:21'!
morphHeightInOwner: newHeight

	self morphExtentInOwner: self morphExtentInOwner x @ newHeight! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:26:31'!
morphWidthInOwner: newWidth

	self morphExtentInOwner: newWidth @ self morphExtentInOwner y! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:27:35'!
morphPosition: newPos extent: newExtent
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self morphPosition: newPos.
	self morphExtentInOwner: newExtent.! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 9/28/2021 15:27:28'!
morphPosition: newPos extent: newExtent
	"Change the position and extent of this morph. Arguments are owner's coordinates."

	self morphPosition: newPos.
	self morphExtentInOwner: newExtent.! !


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 9/28/2021 15:23:25'!
adjustHorizontallyBy: aLayoutAdjustMorph at: localPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
			 self inform: 'LayoutAdjustingMorphs require morphs before and after'.
			^self 
	].
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphExtentInOwner x max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphExtentInOwner x max: 1.	"avoid division by zero"
	delta _ localPoint x - aLayoutAdjustMorph referencePositionInOwner x.
	delta _ delta max: l minimumShrinkWidth - lCurrentWidth.
	delta _ delta min: rCurrentWidth - r minimumShrinkWidth.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion _ lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion _ 1.0 - leftNewProportion.
			toDistribute _ ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 9/28/2021 15:23:58'!
adjustVerticallyBy: aLayoutAdjustMorph at: localPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
			 self inform: 'LayoutAdjustingMorphs require morphs above and below'.
			^self 
	].	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphExtentInOwner y max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphExtentInOwner y max: 1.	"avoid division by zero"
	delta _ localPoint y - aLayoutAdjustMorph referencePositionInOwner y.
	delta _ delta max: t minimumShrinkHeight - tCurrentHeight.
	delta _ delta min: bCurrentHeight - b minimumShrinkHeight.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion _ tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion _ 1.0 - topNewProportion.
			toDistribute _ ts proportionalLayoutHeight + bs proportionalLayoutHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 15:43:44'!
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
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + gap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/28/2021 15:41:41'!
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
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + gap.
	]! !


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 9/28/2021 15:19:43'!
fixedHeight
	^proportionalHeight isNil ifTrue: [fixedHeight ifNil: [morph morphExtentInOwner y]] ifFalse: [ 0 ]! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 9/28/2021 15:22:19'!
fixedOrMorphHeight: aNumber
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight
		ifNotNil: [ fixedHeight _ aNumber ]
		ifNil: [ fixedHeight _ aNumber.
			  morph morphHeightInOwner: aNumber 
		].
	proportionalHeight _ nil! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 9/28/2021 15:22:31'!
fixedOrMorphWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth
		ifNotNil: [ fixedWidth _ aNumber ]
		ifNil: [ fixedWidth _ aNumber.
			morph morphWidthInOwner: aNumber ].
	proportionalWidth _ nil! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 9/28/2021 15:20:04'!
fixedWidth
	^proportionalWidth isNil ifTrue: [fixedWidth ifNil: [morph morphExtentInOwner x]] ifFalse: [ 0 ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 9/28/2021 15:19:48'!
fixedOrMinimumLayoutHeight
	^fixedHeight ifNil: [ morph morphExtentInOwner y ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 9/28/2021 15:20:00'!
fixedOrMinimumLayoutWidth
	^fixedWidth ifNil: [ morph morphExtentInOwner x ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 9/28/2021 15:19:55'!
minimumSpecHeight
	"If fixedHeight is not nil, use it.  
	 If fixdHeight and propostionlHeight are nil, use morphHeight"

	^ fixedHeight ifNil: [ proportionalHeight ifNotNil: [ 0 ] ifNil: [ morph morphExtentInOwner y ] ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 9/28/2021 15:20:08'!
minimumSpecWidth
	"If fixedWidth is not nil, use it.  
	 If fixdWidth and propostionlWidth are nil, use morphWidth"

	^ fixedWidth ifNil: [ proportionalWidth ifNotNil: [ 0 ] ifNil: [ morph morphExtentInOwner x ] ]! !

