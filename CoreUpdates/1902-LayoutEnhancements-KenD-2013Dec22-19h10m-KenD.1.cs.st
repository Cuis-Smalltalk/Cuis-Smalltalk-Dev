'From Cuis 4.2 of 25 July 2013 [latest update: #1898] on 22 December 2013 at 8:39:42.285559 pm'!

!StringMorph methodsFor: 'geometry' stamp: 'KenD 12/22/2013 20:34'!
minimumExtent

	^ self measureContents ! !

!StringMorph methodsFor: 'geometry' stamp: 'KenD 12/22/2013 20:34'!
morphExtent: aPoint 
	"Set the receiver's extent to value provided. Honor my minimumExtent."

	super morphExtent: (aPoint max: self minimumExtent)! !


!LayoutSpec methodsFor: 'layout' stamp: 'KenD 12/22/2013 19:39'!
fixedHeight
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined. (no proportional extent is computed)
	Otherwise, we do proportional layout, and the stored extent is a minimum extent, so we don't  really a fixed extent."
	proportionalHeight ifNil: [ ^ fixedHeight ifNil: [ morph morphHeight ] ].
	^ 0! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 12/22/2013 19:39'!
fixedWidth
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined. (no proportional extent is computed)
	Otherwise, we do proportional layout, and the stored extent is a minimum extent, so we don't  really a fixed extent."
	proportionalWidth ifNil: [ ^ fixedWidth ifNil: [ morph morphWidth ] ].
	^ 0! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 12/22/2013 20:07'!
heightFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."

	proportionalHeight ifNotNil: [
		^ fixedHeight max: (proportionalHeight * availableSpace) rounded 
	].
	^ fixedHeight ifNil: [ morph morphHeight ] ! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 12/22/2013 19:41'!
proportionalHeight

	^ proportionalHeight ifNil: [ 0 ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 12/22/2013 19:41'!
proportionalWidth

	^ proportionalWidth ifNil: [ 0 ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'KenD 12/22/2013 20:07'!
widthFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."
	
	proportionalWidth ifNotNil: [
		^ fixedWidth max: (proportionalWidth * availableSpace) rounded 
	].
	^ fixedWidth ifNil: [ morph morphWidth ]! !

!LayoutSpec methodsFor: 'testing' stamp: 'KenD 12/22/2013 19:42'!
isProportionalHeight

	^ proportionalHeight notNil! !

!LayoutSpec methodsFor: 'testing' stamp: 'KenD 12/22/2013 19:42'!
isProportionalWidth

	^ proportionalWidth notNil! !


!Morph methodsFor: 'layout-properties' stamp: 'KenD 12/22/2013 20:39'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec ! !


!LayoutMorph methodsFor: 'layout' stamp: 'KenD 12/22/2013 20:16'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		widths theLeft usableHeight boundsTop boundsRight theTop minWidth |
	
	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"
		
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed := submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := (usableWidth - sumOfFixed) max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	widths := submorphs collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	minWidth := widths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) + xSep max: 0) 
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
			morphExtent: (wd rounded min: minWidth)@ ht rounded.
			
		wd > 0 ifTrue: [ theLeft := theLeft + wd + xSep min: boundsRight ]
	]
	! !

!LayoutMorph methodsFor: 'layout' stamp: 'KenD 12/22/2013 20:20'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft minHeight |

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
	minHeight := heights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) + ySep max: 0) 
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
			morphExtent: wd rounded @ (ht rounded min: minHeight).
			
		ht > 0 ifTrue: [ theTop := theTop + ht + ySep min: boundsBottom ]
	]
! !


!StringMorph reorganize!
('accessing' contents contents: fitContents font font:emphasis: fontToUse measureContents)
('drawing' drawOn:)
('font' emphasis:)
('halos and balloon help' addOptionalHandlesTo:box:)
('initialization' defaultColor initWithContents:font:emphasis: initialize)
('menu' addCustomMenuItems:hand: changeEmphasis changeFont)
('printing' font: printOn:)
('geometry' minimumExtent morphExtent:)
!

