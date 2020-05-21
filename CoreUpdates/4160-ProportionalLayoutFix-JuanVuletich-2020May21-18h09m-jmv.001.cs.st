'From Cuis 5.0 [latest update: #4159] on 21 May 2020 at 6:36:14 pm'!

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 5/21/2020 18:18:34'!
fixedHeight
	^proportionalHeight isNil ifTrue: [fixedHeight ifNil: [morph morphHeight]] ifFalse: [ 0 ]! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 5/21/2020 18:13:55'!
fixedWidth
	^proportionalWidth isNil ifTrue: [ fixedWidth ] ifFalse: [ 0 ]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 5/21/2020 18:35:44'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight normalizationFactor 
		fractionalHeights integerHeights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight alternativeHeights count diff i desiredLayoutHeight |

	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size + 1) * ySep).

	desiredLayoutHeight _ self desiredLayoutHeight.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	usableHeight > desiredLayoutHeight
		ifTrue: [ | sumOfFixed |
			sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedHeight ].
			{usableHeight*normalizationFactor - sumOfFixed. usableHeight*normalizationFactor. usableHeight.normalizationFactor. sumOfFixed} print.
			fractionalHeights := submorphsToLayout collect: [ :m | m layoutSpec heightForComfortable: usableHeight - sumOfFixed * normalizationFactor].
			fractionalHeights print.
			]
		ifFalse: [ | sumOfFixedOrMinimum availableForPropHeight |
			sumOfFixedOrMinimum := submorphsToLayout sum: [ :m | m minimumLayoutExtent y max: m layoutSpec fixedOrMinimumLayoutHeight ].
			availableForPropHeight := usableHeight - sumOfFixedOrMinimum max: 0.
			fractionalHeights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight * normalizationFactor ]].

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

