'From Cuis 4.2 of 25 July 2013 [latest update: #2592] on 18 November 2015 at 10:11:16.725485 pm'!

!LayoutSpec methodsFor: 'layout' stamp: 'len 11/18/2015 22:08'!
heightFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."

	^proportionalHeight isNil
		ifTrue: [fixedHeight ifNil: [morph morphHeight]]
		ifFalse: [fixedHeight max: proportionalHeight * availableSpace]! !

!LayoutSpec methodsFor: 'layout' stamp: 'len 11/18/2015 21:55'!
widthFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."

	^proportionalWidth isNil
		ifTrue: [fixedWidth ifNil: [morph morphWidth]]
		ifFalse: [fixedWidth max: proportionalWidth * availableSpace]! !


!LayoutMorph methodsFor: 'layout' stamp: 'len 11/18/2015 22:06'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		widths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls theRight boundsBottom theBottom |
	
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
	
	widths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	minWidth := widths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) max: 0) + boundsForLayout left + xSep.
	usableHeight := boundsForLayout height - (ySep * 2) max: 0.
	boundsTop    := boundsForLayout top + ySep.
	boundsRight  := boundsForLayout right - xSep.
	boundsBottom := boundsForLayout bottom - ySep.

	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := widths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := ((usableHeight - ht) * ls minorDirectionPadding) floor + boundsTop.
		theBottom := (theTop + ht) ceiling min: boundsBottom.
		theRight := (theLeft + (wd min: minWidth)) ceiling min: boundsRight.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop).
		theLeft := theRight + xSep
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'len 11/18/2015 22:07'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight |

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
	
	heights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	minHeight := heights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) max: 0) + boundsForLayout top + ySep.
	usableWidth     := boundsForLayout width - (xSep * 2) max: 0.
	boundsLeft      := boundsForLayout left + xSep.
	boundsBottom := boundsForLayout bottom - ySep.
	boundsRight := boundsForLayout right - xSep.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := heights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := ((usableWidth - wd) * ls minorDirectionPadding) floor + boundsLeft.
		theRight := (theLeft + wd) ceiling min: boundsRight.
		theBottom := (theTop + (ht min: minHeight)) ceiling min: boundsBottom.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"	
		nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop).
		theTop := theBottom + ySep
	]! !

