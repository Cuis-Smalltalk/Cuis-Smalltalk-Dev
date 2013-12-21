'From Cuis 4.2 of 25 July 2013 [latest update: #1889] on 21 December 2013 at 9:13:16.914422 am'!

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 12/20/2013 13:51'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		widths theLeft usableHeight boundsTop boundsRight theTop |
	
	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"
		
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed := submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := usableWidth - sumOfFixed.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	widths := submorphs collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	theLeft := ((usableWidth - widths sum) * (padding ifNil: [0]) + xSep max: 0) 
			+  boundsForLayout left.
	usableHeight := boundsForLayout height - (2*ySep) max: 0.
	boundsTop    := boundsForLayout top.	
	boundsRight  := boundsForLayout right.
	
	submorphs size to: 1 by: -1 do: [ :index | | nextMorph ht wd ls |
		nextMorph := submorphs at: index.
		"major direction"
		wd := widths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := (usableHeight - ht) * ls minorDirectionPadding + ySep + boundsTop.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph
			morphPosition: theLeft rounded @ theTop rounded;
			morphExtent: (wd rounded min: boundsForLayout width)@ ht rounded.
			
		wd > 0 ifTrue: [ theLeft := theLeft + wd + xSep min: boundsRight ]
	]
	! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 12/20/2013 13:46'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft |

	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. 	"Too small. Don't bother!!"

	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphs size + 1) * ySep).
	sumOfFixed := submorphs sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight := usableHeight - sumOfFixed.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	heights := submorphs collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	theTop := ((usableHeight - heights sum) * (padding ifNil: [0]) + ySep max: 0) 
				+  boundsForLayout top.
	usableWidth     := boundsForLayout width - (2*xSep) max: 0.
	boundsLeft      := boundsForLayout left.	
	boundsBottom := boundsForLayout bottom.
	
	submorphs size to: 1 by: -1 do: [ :index | | nextMorph ht wd ls |
		nextMorph := submorphs at: index.
		"major direction"
		ht := heights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := (usableWidth - wd) * ls minorDirectionPadding + xSep + boundsLeft.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph
			morphPosition: theLeft rounded @ theTop rounded;
			morphExtent: wd rounded @ (ht rounded min: boundsForLayout height).
			
		ht > 0 ifTrue: [ theTop := theTop + ht + ySep min: boundsBottom ]
	]
! !

