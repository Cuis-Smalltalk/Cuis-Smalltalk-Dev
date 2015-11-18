'From Cuis 4.2 of 25 July 2013 [latest update: #2590] on 18 November 2015 at 9:52:29.619657 am'!

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 11/18/2015 09:45'!
example20
"
	self example20
"
| column |

column _ (LayoutMorph newColumn separation: 5) name: #Column.
column
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 20);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec fixedWidth: 40 proportionalHeight: 0.5);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 30).
column morphPosition: 150@130 extent: 400@300.
column openInWorld! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 11/18/2015 09:37'!
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
	availableForPropWidth := (usableWidth - sumOfFixed) max: 0.
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
		theRight := (theLeft + (wd  min: minWidth)) ceiling min: boundsRight.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft floor @ theTop floor extent:  theRight - theLeft  @ (theBottom - theTop).
		theLeft := theRight  + xSep
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 11/18/2015 09:47'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight |

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
		theRight := theLeft + wd ceiling min: boundsRight.
		theBottom := (theTop + (ht min: minHeight)) ceiling min: boundsBottom.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"	
		nextMorph morphPosition: theLeft @ theTop extent: theRight - theLeft  @ (theBottom - theTop).
		theTop := theBottom + ySep
	]! !

