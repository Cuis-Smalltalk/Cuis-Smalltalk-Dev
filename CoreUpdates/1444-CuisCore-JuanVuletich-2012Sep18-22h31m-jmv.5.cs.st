'From Cuis 4.0 of 21 April 2012 [latest update: #1443] on 18 September 2012 at 11:01:18 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 9/18/2012 22:59'!
clippingBoundsInWorld
	"Return the bounds to which any submorphs should be clipped if the property is set"
	"Should be a region, like our shadow"
	self flag: #jmvVer2.
	^ self morphBoundsInWorld! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/18/2012 22:37'!
layoutBounds
	"Return the bounds for laying out children of the receiver"
	self flag: #jmvVer2.
	^ 0@0 extent: self morphExtent! !


!BorderedRectMorph methodsFor: 'geometry' stamp: 'jmv 9/18/2012 22:59'!
clippingBoundsInWorld
	"Return the bounds to which any submorphs should be clipped if the property is set"
	"Should be a region, like our shadow"
	self flag: #jmvVer2.
	^ super clippingBoundsInWorld insetBy: borderWidth! !

!BorderedRectMorph methodsFor: 'geometry' stamp: 'jmv 9/18/2012 22:38'!
layoutBounds
	"Return the bounds for laying out children of the receiver"
	self flag: #jmvVer2.
	^ super layoutBounds insetBy: borderWidth! !


!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 9/18/2012 23:00'!
initialIndicatorBoundsInWorld
	^self morphBoundsInWorld outsetBy: 1! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/18/2012 22:40'!
layoutBounds
	"Return the bounds for laying out children of the receiver"
	"Exclude the label area"

	^ super layoutBounds insetBy: (0 @ (self labelHeight) corner: 0 @ 0)! !


!WindowEdgeAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 9/18/2012 23:00'!
initialIndicatorBoundsInWorld
	^owner morphBoundsInWorld! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/18/2012 22:59'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"
	submorphs isEmpty ifTrue: [ ^ self ].
	self clipsSubmorphs
		ifTrue: [
			aCanvas
				clipBy: self clippingBoundsInWorld
				during: [ :clippedCanvas | 
					submorphs reverseDo:
						[ :m |  clippedCanvas fullDraw: m ] ] ]
		ifFalse: [
			submorphs reverseDo:
				[ :m |  aCanvas fullDraw: m ] ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/18/2012 22:45'!
morphPositionInOwner: newPositionInOwner
	"Change the position of this morph."

	self flag: #jmvVer2.

	location position = newPositionInOwner ifTrue: [
		^ self ].		"Null change"

	self redrawNeeded.

	"Maybe we don't really need an owner to run this method..."
	self validateOwnerNotNil.

	location setPosition: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'layout' stamp: 'jmv 9/18/2012 22:34'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."
	"Only specific subclasses do layout. They redefine this method."

	layoutNeeded _ false! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 9/18/2012 22:45'!
morphExtent: aPoint
	"assume it is always in our coordinates!!"
	self flag: #jmvVer2.
	self basicExtent: aPoint! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 9/18/2012 23:00'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	self cursor show.
	hand _ aMouseButtonEvent hand.
	self startStepping.
	Preferences fastDragWindowForMorphic ifTrue: [
		indicator _ RectangleIndicatorMorph new.
		indicator morphBoundsInWorld: self initialIndicatorBoundsInWorld.
		indicator openInWorld ]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/18/2012 22:53'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		layoutNeeded _ false.
		^self].

	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	layoutNeeded _ false! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/18/2012 22:56'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth widths l usableHeight boundsTop boundsRight t |
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
			morphPositionInOwner: l rounded @ t rounded;
			morphExtent: (w rounded min: boundsForLayout width)@ h rounded.
		w > 0 ifTrue: [
			l _ l + w + xSep min: boundsRight ]]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/18/2012 22:56'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight heights t usableWidth boundsLeft boundsBottom l |
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
			morphPositionInOwner: l rounded @ t rounded;
			morphExtent: w rounded @ (h rounded min: boundsForLayout height).
		h > 0 ifTrue: [
			t _ t + h + ySep min: boundsBottom ]]! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 9/18/2012 22:59'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			"self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage multipliedBy: color at: bounds topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage multipliedBy: color at: bounds topLeft ]"
			self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBoundsInWorld
					during: [ :canvas | canvas image: backgroundImage at: 0@0 ]]
				ifFalse: [ aCanvas image: backgroundImage at: 0@0 ]]

		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [aCanvas drawsOnDisplay] and: [color class == TranslucentColor])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Squeak Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt current toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !


!SystemWindow methodsFor: 'layout' stamp: 'jmv 9/18/2012 22:46'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww b |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster)
		morphPositionInOwner: cornerExtent@0;
		morphExtent: w@thickness.
	(adjusters at: #bottomAdjuster)
		morphPositionInOwner: cornerExtent@(wh-thickness);
		morphExtent: w@thickness.
	(adjusters at: #leftAdjuster)
		morphPositionInOwner: 0@cornerExtent;
		morphExtent: thickness@h.
	(adjusters at: #rightAdjuster)
		morphPositionInOwner: ww-thickness@cornerExtent;
		morphExtent: thickness@h.
	(adjusters at: #topLeftAdjuster)
		morphPositionInOwner: 0@0;
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster)
		morphPositionInOwner: 0@(wh-cornerExtent);
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster)
		morphPositionInOwner: ww-cornerExtent@0;
		morphExtent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster)
		morphPositionInOwner: ww@wh-cornerExtent;
		morphExtent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		b _ self layoutBounds.
		layoutMorph
			morphPositionInOwner: b origin;
			morphExtent: b extent ].
	
	layoutNeeded _ false! !

!methodRemoval: WindowEdgeAdjustingMorph #zzinitialIndicatorBounds!
WindowEdgeAdjustingMorph removeSelector: #zzinitialIndicatorBounds!
!methodRemoval: SystemWindow #zzlayoutBounds!
SystemWindow removeSelector: #zzlayoutBounds!
!methodRemoval: LayoutMorph #zzlayoutSubmorphsHorizontallyIn:!
LayoutMorph removeSelector: #zzlayoutSubmorphsHorizontallyIn:!
!methodRemoval: LayoutMorph #zzlayoutSubmorphsVerticallyIn:!
LayoutMorph removeSelector: #zzlayoutSubmorphsVerticallyIn:!
!methodRemoval: LayoutAdjustingMorph #zzinitialIndicatorBounds!
LayoutAdjustingMorph removeSelector: #zzinitialIndicatorBounds!
!methodRemoval: BorderedRectMorph #zzclippingBounds!
BorderedRectMorph removeSelector: #zzclippingBounds!
!methodRemoval: BorderedRectMorph #zzlayoutBounds!
BorderedRectMorph removeSelector: #zzlayoutBounds!
!methodRemoval: Morph #zzclippingBounds!
Morph removeSelector: #zzclippingBounds!
!methodRemoval: Morph #zzlayoutBounds!
Morph removeSelector: #zzlayoutBounds!
