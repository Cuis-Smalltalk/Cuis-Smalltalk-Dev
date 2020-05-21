'From Cuis 5.0 [latest update: #4154] on 21 May 2020 at 3:21:23 pm'!

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 5/21/2020 15:12:52'!
desiredLayoutHeight

	| fixed proportional |
	proportional _ 0.
	fixed _ 0.
	self submorphsToLayout do: [ :m | | ls |
		ls _ m layoutSpec.
		ls isProportionalHeight ifTrue: [
			proportional _ proportional max: ls fixedOrMinimumLayoutHeight / ls proportionalLayoutHeight ]
		ifFalse: [
			fixed _ fixed + ls fixedOrMinimumLayoutHeight ]].
	^fixed + proportional! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 4/30/2020 17:13:46'!
desiredLayoutWidth

	| fixed proportional |
	proportional _ 0.
	fixed _ 0.
	self submorphsToLayout do: [ :m | | ls |
		ls _ m layoutSpec.
		ls isProportionalWidth ifTrue: [
			proportional _ proportional max: ls fixedOrMinimumLayoutWidth / ls proportionalLayoutWidth ]
		ifFalse: [
			fixed _ fixed + ls fixedOrMinimumLayoutWidth ]].
	^fixed + proportional! !


!LayoutSpec methodsFor: 'layout' stamp: 'jmv 5/21/2020 15:11:39'!
heightForComfortable: availableSpace
	"Similar to #heightFor:. To be used when we know availableSpace is more than the minimum required to apply proportional layout and still get more than our minimum. See senders."

	^proportionalHeight isNil
		ifTrue: [fixedHeight ifNil: [morph morphHeight]]
		ifFalse: [proportionalHeight * availableSpace]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 5/21/2020 15:12:30'!
proportionalLayoutHeight

	^ proportionalHeight ifNil: [ 0 ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 4/30/2020 17:01:00'!
widthForComfortable: availableSpace
	"Similar to #widthFor:. To be used when we know availableSpace is more than the minimum required to apply proportional layout and still get more than our minimum. See senders."

	^proportionalWidth isNil
		ifTrue: [fixedWidth ifNil: [morph morphWidth]]
		ifFalse: [proportionalWidth * availableSpace]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 4/30/2020 17:13:33'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth normalizationFactor 
		fractionalWidths integerWidths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls theRight boundsBottom theBottom alternativeWidths count diff i desiredLayoutWidth |
	
	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size + 1) * xSep).

	desiredLayoutWidth _ self desiredLayoutWidth.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	usableWidth > desiredLayoutWidth
		ifTrue: [
			fractionalWidths := submorphsToLayout collect: [ :m | m layoutSpec widthForComfortable: usableWidth*normalizationFactor ]]
		ifFalse: [ | sumOfFixedOrMinimum availableForPropWidth |
			sumOfFixedOrMinimum := submorphsToLayout sum: [ :m | m minimumLayoutExtent x max: m layoutSpec fixedOrMinimumLayoutWidth ].
			availableForPropWidth := usableWidth - sumOfFixedOrMinimum max: 0.
			fractionalWidths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth * normalizationFactor ]].
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

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 5/21/2020 15:19:28'!
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
		ifTrue: [
			fractionalHeights := submorphsToLayout collect: [ :m | m layoutSpec heightForComfortable: usableHeight*normalizationFactor ]]
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

