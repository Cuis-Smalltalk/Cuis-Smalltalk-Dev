'From Cuis7.1 [latest update: #6614] on 21 August 2024 at 12:50:32 pm'!

!LayoutMorph methodsFor: 'geometry' stamp: 'RMV 8/14/2024 19:47:20'!
minimumExtent: cachedValues
	"Answer size sufficient to frame my submorphs."
	
	| width height mle |
	width  := 0.
	height := 0.
	(self direction = #vertical)
		ifTrue: [ "Column"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"use maximum width across submorphs"
				width := width max: mle x.
				"sum up submorph heights, including separation"
				height := height + mle y + self ySeparation.
			].
			useEdgeSpace
				ifTrue: [
					height := height + self ySeparation. "one side already separated"
					width := width + (2 * self xSeparation). "separation on each side"
				]
				ifFalse: [
					height := height - self ySeparation. "one side already separated"
				].
		]
		ifFalse: [ "Row"
			self submorphsToLayout do: [ :sm |
				mle := sm minimumLayoutExtent: cachedValues.
				"sum up submorphs width"
				width := width + mle x + self xSeparation.
				"use maximum height across submorph"
				height := height max: mle y.
			].
			useEdgeSpace
				ifTrue: [
					width := width + self xSeparation. "one side already separated"
					height := height + (2 * self ySeparation). "separation on each side"
				]
				ifFalse: [
					width := width - self xSeparation. "one side already separated"
				].
		].

	^ (width @ height) + self extentBorder! !

!LayoutMorph methodsFor: 'layout' stamp: 'RMV 8/14/2024 18:32:47'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap gapCount widths widthToAllocate leftOver x height y cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].
	
	gap := self xSeparation.
	gapCount := useEdgeSpace ifTrue: [ visibleSubmorphs size + 1 ] ifFalse: [ visibleSubmorphs size - 1].
	widthToAllocate := (boundsForLayout width - (gapCount * gap)).
	cache := IdentityDictionary new.
	widths := self widthsFor: visibleSubmorphs within: widthToAllocate minLayoutExtentCache: cache.
	leftOver := widthToAllocate - widths sum.
	x := boundsForLayout left + (leftOver * self axisEdgeWeight).
	useEdgeSpace ifTrue: [ x := x + gap ].

	visibleSubmorphs with: widths do: [ :sm :smWidth |
		height := self offAxisHeightFor: sm within: boundsForLayout height minLayoutExtentCache: cache.
		y := self offAxisYOffsetFor: sm within: boundsForLayout height - height.
		sm fitInto: (x @ (boundsForLayout top + y) extent: smWidth @ height).
		x := x + smWidth + gap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 8/15/2024 05:24:55'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs gap gapCount heights heightToAllocate leftOver y width x cache |
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	gap := self ySeparation.
	gapCount := useEdgeSpace ifTrue: [ visibleSubmorphs size + 1 ] ifFalse: [ visibleSubmorphs size - 1].
	heightToAllocate := (boundsForLayout height - (gapCount * gap)).
	cache := IdentityDictionary new.
	heights := self heightsFor: visibleSubmorphs within: heightToAllocate minLayoutExtentCache: cache.
	leftOver := heightToAllocate - heights sum.
	y := boundsForLayout top + (leftOver * self axisEdgeWeight).
	useEdgeSpace ifTrue: [ y := y + gap ].

	visibleSubmorphs with: heights do: [ :sm :smHeight |
		width := self offAxisWidthFor: sm within: boundsForLayout width minLayoutExtentCache: cache.
		x := self offAxisXOffsetFor: sm within: boundsForLayout width - width.
		sm fitInto: (boundsForLayout left + x @ y extent: width @ smHeight).
		y := y + smHeight + gap.
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 8/15/2024 05:41:15'!
offAxisHeightFor: aMorph within: availableHeight minLayoutExtentCache: cache
	"Answer height for a single morph -- offAxis calculation for a Row"
	
	| availableForPropHeight actualPropHeight |
	availableForPropHeight := useEdgeSpace 
		ifTrue: [ availableHeight  - (2 * self ySeparation) ]
		ifFalse: [ availableHeight ].
	actualPropHeight := (availableForPropHeight * aMorph layoutSpec proportionalLayoutHeight)
							 max: (aMorph minimumLayoutExtent: cache) y.
	^ actualPropHeight! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 8/15/2024 05:42:45'!
offAxisWidthFor: aMorph within: availableWidth minLayoutExtentCache: cache
	"Answer width for a single morph -- offAxis calculation for a Column"

	| availableForPropWidth actualPropWidth |
	availableForPropWidth := useEdgeSpace 
		ifTrue: [ 	availableWidth - (2 * self xSeparation) ]
		 ifFalse: [ availableWidth ].
	actualPropWidth := (availableForPropWidth * aMorph layoutSpec proportionalLayoutWidth)
						 	max: (aMorph minimumLayoutExtent: cache) x.
	^ actualPropWidth! !

!LayoutMorph methodsFor: 'layout' stamp: 'RMV 8/14/2024 19:30:46'!
offAxisXOffsetFor: aMorph within: availableWidth
	"Answer x offset for a single morph -- offAxis calculation for a Column"

	| leftOver offset |
	leftOver := useEdgeSpace
		ifTrue: [availableWidth - (2 * self xSeparation)] 
		ifFalse: [availableWidth].
	offset := leftOver * aMorph layoutSpec offAxisEdgeWeight. 
	useEdgeSpace ifTrue: [ ^ self xSeparation + offset ].
	^ offset! !

!LayoutMorph methodsFor: 'layout' stamp: 'RMV 8/14/2024 19:30:50'!
offAxisYOffsetFor: aMorph within: availableHeight
	"Answer y offset for a single morph -- offAxis calculation for a Row"
	
	| leftOver offset |
	leftOver := useEdgeSpace
		ifTrue: [availableHeight - (2 * self ySeparation)] 
		ifFalse: [availableHeight].
	offset := leftOver * aMorph layoutSpec offAxisEdgeWeight.
	useEdgeSpace ifTrue: [ ^ self ySeparation + offset ].
	^offset! !

