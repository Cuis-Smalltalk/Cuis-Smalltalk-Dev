'From Cuis 4.2 of 25 July 2013 [latest update: #2737] on 19 April 2016 at 11:35:05.709328 am'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 4/19/2016 11:21'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		fractionalWidths integerWidths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls theRight boundsBottom theBottom alternativeWidths count diff i |
	
	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size + 1) * xSep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := usableWidth - sumOfFixed max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	fractionalWidths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerWidths _ fractionalWidths collect: [ :w | w rounded ].
	diff _ integerWidths sum - fractionalWidths sum rounded.
	alternativeWidths _ diff > 0 ifTrue: [ fractionalWidths collect: [ :w | w floor ]] ifFalse: [ fractionalWidths collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerWidths at: i) = (alternativeWidths at: i) ifFalse: [
			integerWidths at: i put: (alternativeWidths at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minWidth := integerWidths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) max: 0) + boundsForLayout left + xSep.
	usableHeight := boundsForLayout height - (ySep * 2) max: 0.
	boundsTop    := boundsForLayout top + ySep.
	boundsRight  := boundsForLayout right - xSep.
	boundsBottom := boundsForLayout bottom - ySep.

	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := integerWidths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := ((usableHeight - ht) * ls minorDirectionPadding) floor + boundsTop.
		theBottom := (theTop + ht) ceiling min: boundsBottom.
		theRight := (theLeft + (wd min: minWidth)) "ceiling "min: boundsRight.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theLeft := theRight + xSep
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 4/19/2016 11:24'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		fractionalHeights integerHeights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight alternativeHeights count diff i |

	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size + 1) * ySep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight := usableHeight - sumOfFixed max: 0.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	fractionalHeights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerHeights _ fractionalHeights collect: [ :w | w rounded ].
	diff _ integerHeights sum - fractionalHeights sum rounded.
	alternativeHeights _ diff > 0 ifTrue: [ fractionalHeights collect: [ :w | w floor ]] ifFalse: [ fractionalHeights collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerHeights at: i) = (alternativeHeights at: i) ifFalse: [
			integerHeights at: i put: (alternativeHeights at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minHeight := integerHeights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) max: 0) + boundsForLayout top + ySep.
	usableWidth     := boundsForLayout width - (xSep * 2) max: 0.
	boundsLeft      := boundsForLayout left + xSep.
	boundsBottom := boundsForLayout bottom - ySep.
	boundsRight := boundsForLayout right - xSep.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := integerHeights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := ((usableWidth - wd) * ls minorDirectionPadding) floor + boundsLeft.
		theRight := (theLeft + wd) ceiling min: boundsRight.
		theBottom := (theTop + (ht min: minHeight)) "ceiling" min: boundsBottom.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theTop := theBottom + ySep
	]! !

