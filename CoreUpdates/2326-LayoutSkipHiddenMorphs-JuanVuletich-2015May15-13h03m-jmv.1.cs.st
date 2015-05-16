'From Cuis 4.2 of 25 July 2013 [latest update: #2325] on 15 May 2015 at 1:11:00.675764 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 5/15/2015 13:09'!
submorphsToLayout
	"Select those that will be layout"

	^submorphs select: [ :m | m visible ]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 5/15/2015 13:09'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		widths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout |
	
	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size + 1) * xSep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := (usableWidth - sumOfFixed) max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	widths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	minWidth := widths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) + xSep max: 0) 
			+  boundsForLayout left.
	usableHeight := boundsForLayout height - (2*ySep) max: 0.
	boundsTop    := boundsForLayout top.	
	boundsRight  := boundsForLayout right.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index | | nextMorph ht wd ls |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := widths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := (usableHeight - ht) * ls minorDirectionPadding + ySep + boundsTop.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft rounded @ theTop rounded extent: (wd rounded min: minWidth)@ ht rounded.
			
		wd > 0 ifTrue: [ theLeft := theLeft + wd + xSep min: boundsRight ]
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 5/15/2015 13:09'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout |

	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. 	"Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size + 1) * ySep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight := usableHeight - sumOfFixed.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	heights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	minHeight := heights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) + ySep max: 0) 
				+  boundsForLayout top.
	usableWidth     := boundsForLayout width - (2*xSep) max: 0.
	boundsLeft      := boundsForLayout left.	
	boundsBottom := boundsForLayout bottom.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index | | nextMorph ht wd ls |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := heights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := (usableWidth - wd) * ls minorDirectionPadding + xSep + boundsLeft.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft rounded @ theTop rounded extent: wd rounded @ (ht rounded min: minHeight).
			
		ht > 0 ifTrue: [ theTop := theTop + ht + ySep min: boundsBottom ]
	]! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 5/15/2015 13:09'!
proportionalHeightNormalizationFactor

	| sumOfProportional |
	sumOfProportional _ self submorphsToLayout sum: [ :m | m layoutSpec proportionalHeight ].
	^1.0 / (sumOfProportional max: 1.0).! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 5/15/2015 13:10'!
proportionalWidthNormalizationFactor

	| sumOfProportional |
	sumOfProportional _ self submorphsToLayout sum: [ :m | m layoutSpec proportionalWidth ].
	^1.0 / (sumOfProportional max: 1.0).! !

