'From Cuis 4.0 of 21 April 2012 [latest update: #1433] on 8 September 2012 at 8:43:08 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 20:40'!
zzclippingBounds
	"Return the bounds to which any submorphs should be clipped if the property is set"
	"Maybe shouldn't exist"
	self flag: #jmvVer2.
	^self zzinnerBounds! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/8/2012 20:37'!
zzinnerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

"would be better In own coordinates!!"
	self flag: #jmvVer2.
	^ self morphBoundsInWorld! !

!Morph methodsFor: 'layout' stamp: 'jmv 9/8/2012 20:39'!
zzlayoutBounds
	"Return the bounds for laying out children of the receiver"
	^self zzinnerBounds! !


!BorderedRectMorph methodsFor: 'geometry' stamp: 'jmv 9/8/2012 20:37'!
zzinnerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

	^ self morphBoundsInWorld insetBy: borderWidth! !


!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 9/8/2012 20:40'!
zzinitialIndicatorBounds
	^self morphBoundsInWorld outsetBy: 1! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/8/2012 20:37'!
zzinnerBounds
	"Exclude the label area"

	^ self morphBoundsInWorld insetBy: (borderWidth @ (self labelHeight+borderWidth) corner: borderWidth @ borderWidth)! !


!WindowEdgeAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 9/8/2012 20:41'!
zzinitialIndicatorBounds
	^owner morphBoundsInWorld! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 20:40'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"
	submorphs isEmpty ifTrue: [ ^ self ].
	self clipsSubmorphs
		ifTrue: [
			aCanvas
				clipBy: self zzclippingBounds
				during: [ :clippedCanvas | 
					submorphs reverseDo:
						[ :m |  clippedCanvas fullDraw: m ] ] ]
		ifFalse: [
			submorphs reverseDo:
				[ :m |  aCanvas fullDraw: m ] ]! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 9/8/2012 20:41'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	self cursor show.
	hand _ aMouseButtonEvent hand.
	self startStepping.
	Preferences fastDragWindowForMorphic ifTrue: [
		indicator _ RectangleIndicatorMorph new.
		indicator morphBoundsInWorld: self zzinitialIndicatorBounds.
		indicator openInWorld ]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 9/8/2012 20:39'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		layoutNeeded _ false.
		^self].

	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self zzlayoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self zzlayoutBounds ].

	layoutNeeded _ false! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 20:33'!
drawOn: aCanvas 
	| stringColor stringBounds leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas zzfillRectangle: (0@0 extent: extent) colorOrInfiniteForm: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas zzimage: iconForm at: leftEdge+1 @ (extent y - iconForm height // 2).
			leftEdge _ leftEdge + iconForm width + self iconSeparation].

	stringBounds _  leftEdge @ 1 extent: extent.

	aCanvas
		zzdrawString: contents
		in: stringBounds
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			zzimage: SubMenuMarker
			at: extent x - 8 @ (extent y - SubMenuMarker height // 2) ]! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 9/8/2012 20:30'!
minItemWidth
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse _ self fontToUse.
	subMenuWidth _ self hasSubMenu
				ifFalse: [0]
				ifTrue: [10].
	iconWidth _ self hasIcon
				ifTrue: [self icon width + self iconSeparation]
				ifFalse: [0].
	markerWidth _ self hasMarker
		ifTrue: [ submorphs first morphWidth + 8 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 9/8/2012 20:26'!
addBlankIconsIfNecessary: anIcon
	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"

	| withIcons withoutIcons |
	withIcons _ Set new.
	withoutIcons _ Set new.
	self items do: [ :item |
		item hasIcon | item hasMarker
			ifTrue: [ withIcons add: item ]
			ifFalse: [ withoutIcons add: item ].
		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary: anIcon ]].
	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ].
	withoutIcons do: [ :item | item icon: anIcon ].! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 20:40'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			"self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage multipliedBy: color at: bounds topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage multipliedBy: color at: bounds topLeft ]"
			self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self zzclippingBounds
					during: [ :canvas | canvas zzimage: backgroundImage at: 0@0 ]]
				ifFalse: [ aCanvas zzimage: backgroundImage at: 0@0 ]]

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


!ProgressBarMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 20:38'!
drawOn: aCanvas
	| width inner |
	super drawOn: aCanvas.
	inner _ self zzinnerBounds.
	width _ (inner width * value) truncated min: inner width.
	aCanvas fillRectangle: (inner origin extent: width @ inner height) colorOrInfiniteForm: progressColor.! !


!SystemWindow methodsFor: 'layout' stamp: 'jmv 9/8/2012 20:39'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww |
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
		layoutMorph morphBoundsInWorld: self zzlayoutBounds ].
	
	layoutNeeded _ false! !

!methodRemoval: WindowEdgeAdjustingMorph #initialIndicatorBounds!
WindowEdgeAdjustingMorph removeSelector: #initialIndicatorBounds!
!methodRemoval: SystemWindow #innerBounds!
SystemWindow removeSelector: #innerBounds!
!methodRemoval: MenuItemMorph #hasIconOrMarker!
MenuItemMorph removeSelector: #hasIconOrMarker!
!methodRemoval: LayoutAdjustingMorph #initialIndicatorBounds!
LayoutAdjustingMorph removeSelector: #initialIndicatorBounds!
!methodRemoval: BorderedRectMorph #innerBounds!
BorderedRectMorph removeSelector: #innerBounds!
!methodRemoval: Morph #clippingBounds!
Morph removeSelector: #clippingBounds!
!methodRemoval: Morph #innerBounds!
Morph removeSelector: #innerBounds!
!methodRemoval: Morph #layoutBounds!
Morph removeSelector: #layoutBounds!
