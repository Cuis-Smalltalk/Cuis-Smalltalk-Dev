'From Cuis 4.2 of 25 July 2013 [latest update: #1853] on 24 October 2013 at 5:25:32.621183 pm'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/21/2013 19:36'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth widths l usableHeight boundsTop boundsRight t |
	"Don't even bother!!"
	boundsForLayout extent > (2@2)"self minimumExtent" ifFalse: [
		^self ].
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableWidth _ boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth _ usableWidth - sumOfFixed.
	normalizationFactor _ self proportionalWidthNormalizationFactor.
	availableForPropWidth _ availableForPropWidth * normalizationFactor.
	widths _ submorphs collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	l _ ((usableWidth - widths sum) * (padding ifNil: [0]) + xSep max: 0) +  boundsForLayout left.
	usableHeight _ boundsForLayout height - (2*ySep) max: 0.
	boundsTop _ boundsForLayout top.	
	boundsRight _ boundsForLayout right.
	submorphs size to: 1 by: -1 do: [ :index | | m w h ls |
		m _ submorphs at: index.
		"major direction"
		w _ widths at: index.
		"minor direction"
		ls _ m layoutSpec.
		h _ (ls heightFor: usableHeight) min: usableHeight.
		t _ (usableHeight - h) * ls minorDirectionPadding + ySep + boundsTop.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		m
			morphPosition: l rounded @ t rounded;
			morphExtent: (w rounded min: boundsForLayout width)@ h rounded.
		w > 0 ifTrue: [
			l _ l + w + xSep min: boundsRight ]]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/21/2013 19:36'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight heights t usableWidth boundsLeft boundsBottom l |
	"Don't even bother!!"
	boundsForLayout extent > (2@2)"self minimumExtent" ifFalse: [
		^self ].
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableHeight _ boundsForLayout height - ((submorphs size + 1) * ySep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight _ usableHeight - sumOfFixed.
	normalizationFactor _ self proportionalHeightNormalizationFactor.
	availableForPropHeight _ availableForPropHeight * normalizationFactor.
	heights _ submorphs collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	t _ ((usableHeight - heights sum) * (padding ifNil: [0]) + ySep max: 0) +  boundsForLayout top.
	usableWidth _ boundsForLayout width - (2*xSep) max: 0.
	boundsLeft _ boundsForLayout left.	
	boundsBottom _ boundsForLayout bottom.
	submorphs size to: 1 by: -1 do: [ :index | | m h w ls |
		m _ submorphs at: index.
		"major direction"
		h _ heights at: index.
		"minor direction"
		ls _ m layoutSpec.
		w _ (ls widthFor: usableWidth) min: usableWidth.
		l _ (usableWidth - w) * ls minorDirectionPadding + xSep + boundsLeft.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		m
			morphPosition: l rounded @ t rounded;
			morphExtent: w rounded @ (h rounded min: boundsForLayout height).
		h > 0 ifTrue: [
			t _ t + h + ySep min: boundsBottom ]]! !

