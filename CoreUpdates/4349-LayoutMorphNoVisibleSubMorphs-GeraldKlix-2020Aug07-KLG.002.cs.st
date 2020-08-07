'From Cuis 5.0 [latest update: #4348] on 7 August 2020 at 4:23:03 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'KLG 8/7/2020 16:22:34'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs sumOfWidths theSeparation widthsArray 
	widthToAllocate leftOver nextX | 
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. " Nothing to layout, `sum` would fai" ].
	
	theSeparation := self xSeparation.

	widthToAllocate := (boundsForLayout width - ((visibleSubmorphs size + 1) * theSeparation)).
	widthsArray := self widthsFor: visibleSubmorphs within: widthToAllocate.
	sumOfWidths := widthsArray sum: [ :w | w ] .
	leftOver := widthToAllocate - sumOfWidths.
	nextX := boundsForLayout origin x
				+ theSeparation 
				+ (leftOver * (self axisEdgeWeight ifNil: [0.0])). "first X, edge shifted"
"Transcript log: 'first X=', nextX asString, ' leftOver=', leftOver asString; cr.
"	1 to: visibleSubmorphs size do: [ :index | | sm smX smWidth heightAndY newExtent |
		sm := visibleSubmorphs at: index.
		smX := nextX.
		smWidth := widthsArray at: index.
		nextX := smX + smWidth + theSeparation.
		heightAndY := self offHorizontalMetricFor: sm withinExtent: boundsForLayout.
		sm morphPosition: smX @ (heightAndY y).
		newExtent :=  smWidth @ (heightAndY x).
		(sm morphExtent = newExtent) ifFalse: [ sm morphExtent: newExtent ].
	]
! !

!LayoutMorph methodsFor: 'layout' stamp: 'KLG 8/7/2020 16:17:20'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."

	| visibleSubmorphs sumOfHeights theSeparation heightsArray 
	heightToAllocate leftOver nextY | 
	"Preconditions: self isRow & morphExtent >= minimumLayoutExtent"
	(visibleSubmorphs := self submorphsToLayout reversed  "Display Order")
		ifEmpty: [ ^ self. "Nothing to layout, besides `sum` below, would fail" ].

	theSeparation := self ySeparation.
	heightToAllocate := (boundsForLayout height - ((visibleSubmorphs size + 1) * theSeparation)).
	heightsArray := self heightsFor: visibleSubmorphs within: heightToAllocate.
	sumOfHeights := heightsArray sum: [ :w | w ] .
	leftOver := heightToAllocate - sumOfHeights.
	nextY := boundsForLayout origin y
				+ theSeparation 
				+ (leftOver * (self axisEdgeWeight ifNil: [0.5])). "first Y, edge shifted"
"Transcript log: 'first Y=', nextY asString, ' leftOver=', leftOver asString; cr.
"	1 to: visibleSubmorphs size do: [ :index | | sm smY smHeight xAndWidth newExtent |
		sm := visibleSubmorphs at: index.
		smY := nextY.
		smHeight := heightsArray at: index.
		nextY := smY + smHeight + theSeparation.
		xAndWidth := self offVerticalMetricFor: sm withinExtent: boundsForLayout.
		sm morphPosition: (xAndWidth x) @ smY.
		newExtent :=  (xAndWidth y) @ smHeight.
		(sm morphExtent = newExtent) ifFalse: [ sm morphExtent: newExtent ].
	]
! !

