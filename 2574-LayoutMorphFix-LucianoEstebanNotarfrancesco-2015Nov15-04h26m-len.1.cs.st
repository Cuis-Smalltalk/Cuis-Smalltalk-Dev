'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 15 November 2015 at 5:25:05.447294 am'!

!LayoutMorph methodsFor: 'layout' stamp: 'len 11/14/2015 03:50'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		widths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls theRight boundsBottom theBottom |
	
	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size - 1) * xSep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := (usableWidth - sumOfFixed) max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	widths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	minWidth := widths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) max: 0)  +  boundsForLayout left.
	usableHeight := boundsForLayout height  max: 0.
	boundsTop    := boundsForLayout top.	
	boundsRight  := boundsForLayout right.
	boundsBottom := boundsForLayout bottom.
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := widths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := ((usableHeight - ht) * ls minorDirectionPadding + boundsTop) floor min: boundsTop.
		theBottom := (theTop + ht) ceiling min: boundsBottom.
		theRight := index == 1 ifTrue: [boundsRight] ifFalse: [(theLeft + (wd  min: minWidth)) ceiling min: boundsRight].
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft floor @ theTop floor extent:  theRight - theLeft  @ (theBottom - theTop).
		
		theLeft := theRight  + xSep
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'len 11/14/2015 03:59'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight |

	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. 	"Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size - 1) * ySep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight := usableHeight - sumOfFixed.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	heights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	minHeight := heights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) max: 0) + boundsForLayout top.
	usableWidth     := boundsForLayout width max: 0.
	boundsLeft      := boundsForLayout left.
	boundsBottom := boundsForLayout bottom.
	boundsRight := boundsForLayout right.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := heights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := ((usableWidth - wd) * ls minorDirectionPadding) floor + boundsLeft.
		theRight := theLeft + wd ceiling min: boundsRight.
		theBottom := index == 1 ifTrue: [boundsBottom] ifFalse: [(theTop + ht min: minHeight) ceiling min: boundsBottom].
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"	
		nextMorph morphPosition: theLeft @ theTop extent: theRight - theLeft  @ (theBottom - theTop).
		theTop := theBottom + ySep
	]! !

