'From Cuis 4.0 of 21 April 2012 [latest update: #1393] on 21 August 2012 at 7:04:28 pm'!
"Change Set:		1396-CuisCore-JuanVuletich-2012Aug21-18h54m
Date:			21 August 2012
Author:			Juan Vuletich

<your descriptive text goes here>"
Taskbar reset.
"Taskbar show."
!


!RectangleLikeMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:54'!
color

	^ ccolor! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:55'!
drawOn: aCanvas

	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: ccolor borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: borderColor! !

!BorderedRectMorph methodsFor: 'testing' stamp: 'jmv 8/21/2012 18:55'!
isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #containsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	ccolor mightBeTranslucent ifTrue: [
		^false ].
	borderWidth > 0 ifTrue: [
		borderColor mightBeTranslucent ifTrue: [
			^false ]].
	^true! !


!EllipseMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:55'!
drawOn: aCanvas 

	| r |
	r _ self morphBoundsInWorld.
	aCanvas isShadowDrawing
		ifTrue: [^ aCanvas fillOval: r color:  ccolor borderWidth: 0 borderColor: nil].
	aCanvas fillOval: r color: ccolor borderWidth: borderWidth borderColor: borderColor.
! !


!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				roundRect: self morphBoundsInWorld
				color: ccolor
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		fillRectangle: textPane morphBoundsInWorld
		colorOrInfiniteForm: (Theme current paneBackgroundFrom: ccolor)! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:59'!
drawOn: aCanvas

	aCanvas
		image: (self class circleForm: xtent)
		multipliedBy: (ccolor alpha: 0.57)
		at: self morphPositionInWorld! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 8/21/2012 18:59'!
step
	| newBounds |
	target
		ifNil: [^ self].
	newBounds _ target isWorldMorph
				ifTrue: [target morphBoundsInWorld]
				ifFalse: [target worldBoundsForHalo truncated].
	newBounds = self morphBoundsInWorld
		ifTrue: [^ self].
	newBounds extent = xtent
		ifTrue: [^ self morphPosition: newBounds origin].
	growingOrRotating ifFalse: [
		submorphs size > 1
			ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self morphBoundsInWorld: newBounds! !

!HaloMorph methodsFor: 'updating' stamp: 'jmv 8/21/2012 18:59'!
redrawNeeded
	"Quicker to invalidate handles individually if target is large (especially the world)"

	xtent > (200@200)
		ifTrue: [(target notNil and: [target ~~ self world]) ifTrue: [
					"Invalidate 4 outer strips first, thus subsuming separate damage."
					(self morphFullBoundsInWorld areasOutside: target morphBoundsInWorld) do:
						[ :r | self invalidRect: r ]].
				self submorphsDo: [:m | m redrawNeeded]]
		ifFalse: [ super redrawNeeded ]! !


!ImageMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:59'!
borderWidth: bw

	| newExtent |
	newExtent _ 2 * bw + image extent.
	xtent = newExtent ifFalse: [
		self basicExtent: newExtent ]! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:56'!
textColor

	^ ccolor! !

!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		paragraph: self paragraph
		bounds: self morphBoundsInWorld
		color: ccolor
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 18:59'!
morphExtent: aPoint
	| newExtent |
	"Resist changing the extent if no wordwrap.. this should be checked."
	wrapFlag ifFalse: [ ^ self ].
	newExtent _ aPoint truncated max: self minimumExtent.
	
	"No change of wrap width"
	newExtent x = xtent x ifTrue: [ ^ self ].

	super morphExtent: newExtent.
	
	self resetParagraph.
	self editor recomputeSelection.	
	self updateFromParagraph.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/21/2012 18:59'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	newExtent _ (self paragraph extent max: 9 @ StrikeFont default height) + (0 @ 2).
	xtent = newExtent ifFalse: [
		self basicExtent: newExtent ].

	self redrawNeeded.	"Too conservative: only paragraph composition
							should cause invalidation."
	owner innerHeight: newExtent y! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawOn: aCanvas
	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: ccolor borderWidth: 2 borderStyleSymbol: #raised! !

!LayoutAdjustingMorph methodsFor: 'testing' stamp: 'jmv 8/21/2012 18:56'!
isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #containsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	ccolor mightBeTranslucent ifTrue: [
		^false ].
	^true! !


!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 8/21/2012 18:58'!
chooseMagnification
	| result |
	result _ (SelectionMenu selections: #(1.5 2 4 8))
		startUpWithCaption: 'Choose magnification
(currently ', magnification printString, ')'.
	(result == nil or: [ result = magnification ]) ifTrue: [ ^ self ].
	magnification _ result.
	self morphExtent: xtent. "round to new magnification"
	self redrawNeeded. "redraw even if extent wasn't changed"! !


!MenuMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:55'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self morphBoundsInWorld color: ccolor radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: ccolor borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: ccolor ]! !


!MinimalStringMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawOn: aCanvas

	aCanvas drawString: contents in: self morphBoundsInWorld font: self fontToUse color: ccolor! !

!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:59'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	xtent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:59'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	xtent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:59'!
characterIndexAtPoint: aPoint

	| line block f |
	f _ self fontToUse.
	
	line _ TextLine 
		start: 1
		stop: contents size
		internalSpaces: 0
		paddingWidth: 0.
	line
		rectangle: (0@0 extent: xtent);
		lineHeight: f height baseline: f ascent.
		
	block _ (CharacterBlockScanner new text: 
			(contents asText font: font))
		characterBlockAtPoint: aPoint index: nil
		in: line.

	^ block stringIndex! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawOn: aCanvas
	self hasSelection ifTrue: [
		self drawSelectionOn: aCanvas ].
	self hasVisibleCaret ifTrue: [
		self drawCaretOn: aCanvas].
	aCanvas drawString: contents in: self morphBoundsInWorld font: self fontToUse color: ccolor! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:57'!
drawOn: aCanvas

	"draw background image."
	| b |
	b _ self morphBoundsInWorld.
	backgroundImage
		ifNotNil: [
			"self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage multipliedBy: color at: bounds topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage multipliedBy: color at: bounds topLeft ]"
			self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage at: b topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage at: b topLeft ]]

		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [aCanvas drawsOnDisplay] and: [ccolor class == TranslucentColor])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Squeak Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt current toForm: Display)
						clipRect: aCanvas clipRect;
						copy: b
						from: 0@0 in: nil
						fillColor: ccolor rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 8/21/2012 18:58'!
buildMagnifiedBackgroundImage
	| image old |
	old _ backgroundImage.
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [ 
			image _ Form fromBinaryStream: backgroundImageData readStream.
			backgroundImage _ image magnifyTo: xtent.
			self canvas ifNotNil: [ :c |
				(backgroundImage depth = 32 and: [ c depth < 32 ]) ifTrue: [
					backgroundImage _ backgroundImage orderedDither32To16 ]]
		].
	old == backgroundImage ifFalse: [
		self redrawNeeded ]! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:57'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ ccolor.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: self morphBoundsInWorld
		colorOrInfiniteForm: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:58'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y b |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		b _ self morphBoundsInWorld.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ b center.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ b width-labelMargin-labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ b left + labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f height // 2).
		aCanvas
			drawStringEmbossed: l
			in: (x@y extent: xtent - (labelMargin*2-2@4))
			font: f
			color: colorForLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:58'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin b |

	f _ self fontToUse.
	b _ self morphBoundsInWorld.
	center _ b center.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ b width-labelMargin-labelMargin-1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ b left + labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			in: (x@y extent: xtent - (labelMargin*2-2@4))
			font: f
			color: Theme current buttonLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:57'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	rect _ self morphBoundsInWorld insetBy: 1@3.
	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: ccolor hue s: ccolor saturation * 1.3 v: ccolor brightness * 0.9 ]
				ifFalse: [
					colorForButton _ ccolor ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ ccolor adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'jmv 8/21/2012 18:58'!
magnifiedIcon
	| b |
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		self isRoundButton
			ifFalse: [ ^ magnifiedIcon ].
		b _ xtent x max: xtent y.
		b < icon extent x ifTrue: [
			magnifiedIcon _ icon magnifyTo: b@b ].
		b /  icon extent x > 1.7
			ifTrue: [	
				b _ b * 3 // 4.
				magnifiedIcon _ icon magnifyTo: b @ b]].
	^magnifiedIcon! !


!ScrollBar methodsFor: 'access' stamp: 'jmv 8/21/2012 18:55'!
color: aColor
	"Change the color of the scrollbar to go with aColor."
	| buttonColor |
	super color: aColor.
	buttonColor _ ccolor alphaMixed: 0.7 with: (Color gray: 0.95).
	upButton color: buttonColor.
	downButton color: buttonColor.
	slider color: buttonColor slightlyLighter.
	sliderShadow color: (ccolor alphaMixed: 0.45 with: Color white)! !

!ScrollBar methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:55'!
drawOn: aCanvas

	aCanvas
		fillRectangle: self morphBoundsInWorld
		colorOrInfiniteForm: (ccolor alphaMixed: 0.3 with: Color white)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 8/21/2012 18:58'!
morphExtent: newExtent

	| newExtentToUse |
	newExtent = xtent ifTrue: [^ self].
	newExtentToUse _ self isHorizontal
		ifTrue: [ (newExtent x max: 14) @ newExtent y ]
		ifFalse: [ newExtent x @ (newExtent y max: 14) ].
	newExtentToUse = xtent ifTrue: [^ self].
	super morphExtent: newExtentToUse.
		
	self flag: #jmv.
	"Most times it is not necessary to recreate the buttons"
	self recreateSubmorphs! !

!ScrollBar methodsFor: 'initialize' stamp: 'jmv 8/21/2012 18:56'!
recreateSubmorphs
	self removeAllMorphs.
	self
		initializeUpButton;
		initializeDownButton;
		initializeSlider.
	"Set color for submorphs"
	self color: ccolor.! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 18:59'!
fitContents

	| newExtent |
	newExtent _ self measureContents.
	xtent = newExtent ifFalse: [
		self morphExtent: newExtent.
		self redrawNeeded ]! !

!StringMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawOn: aCanvas

	aCanvas drawString: contents in: self morphBoundsInWorld font: self fontToUse color: ccolor.! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawOn: aCanvas

	| tRect sRect columnRect columnScanner columnData columnLeft colorToUse |

	tRect := self toggleRectangle translatedBy: self morphPositionInWorld.
	sRect := self morphBoundsInWorld withLeft: tRect right + 4.
	self drawToggleOn: aCanvas in: tRect.
	colorToUse _ complexContents preferredColor ifNil: [ccolor].
	(container columns isNil or: [(contents asString indexOf: Character tab) = 0]) ifTrue: [
		aCanvas drawString: contents asString in: sRect font: self fontToUse color: colorToUse.
	] ifFalse: [
		columnLeft _ sRect left.
		columnScanner _ ReadStream on: contents asString.
		container columns do: [ :width |
			columnRect _ columnLeft @ sRect top extent: width @ sRect height.
			columnData _ columnScanner upTo: Character tab.
			columnData isEmpty ifFalse: [
				aCanvas drawString: columnData in: columnRect font: self fontToUse color: colorToUse.
			].
			columnLeft _ columnRect right + 5.
		].
	]
! !

!IndentingListItemMorph methodsFor: 'private' stamp: 'jmv 8/21/2012 18:59'!
toggleRectangle

	^(12*indentLevel @ 0) extent: 12@xtent y! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:57'!
drawOn: aCanvas 
	| stringColor stringBounds leftEdge b |

	b _ self morphBoundsInWorld.
	stringColor _ ccolor.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: b colorOrInfiniteForm: Theme current menuHighlight].
	leftEdge := 0.
	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas image: iconForm at: b left+1 @ (b top + (b height - iconForm height // 2)).
			leftEdge _ iconForm width + self iconSeparation].

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + self submorphBounds width + 8 ].

	stringBounds _ b left + leftEdge @ (b top + 1) corner: b corner.

	aCanvas
		drawString: contents
		in: stringBounds
		font: self fontToUse
		color: stringColor.
	subMenu
		ifNotNil: [ aCanvas image: SubMenuMarker at: b right - 8 @ (b top + b bottom - SubMenuMarker height // 2) ]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."
	 | r |
	ccolor mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: self morphBoundsInWorld do: [ :rr |  aCollection add: rr ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report top and bottom rows as uncovered areas"
	r _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (self morphBoundsInWorld insetBy: 0@r) do: [ :rr |  aCollection add: rr ]! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:55'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: ccolor borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	aCanvas fillRectangle: self titleAreaInnerRect colorOrInfiniteForm: titleColor! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:56'!
drawRoundedFrameOn: aCanvas color: widgetsColor
	"Title area is not inside window borders"
	| bottomFactor topFactor |
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor _ Theme current titleGradientTopFactor.
			bottomFactor _ Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor _ 1.
			bottomFactor _ 1 ].
	aCanvas
		windowFrame: self morphBoundsInWorld 
		color: widgetsColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: self labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: ccolor! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/21/2012 18:58'!
makeMeVisible 

	self world morphExtent > (0@0) ifFalse: [^ self].

	(self morphPosition >= (0@0) and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: xtent world: self world) topLeft! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/21/2012 18:58'!
morphExtent: aPoint 
	"Set the receiver's extent to value provided. Respect my minimumExtent."

	| newExtent |
	newExtent _ self isCollapsed
		ifTrue: [aPoint]
		ifFalse: [aPoint max: self minimumExtent].
	newExtent = xtent ifTrue: [^ self].

	isCollapsed
		ifTrue: [super morphExtent: newExtent x @ (self labelHeight + 2)]
		ifFalse: [super morphExtent: newExtent]! !

!SystemWindow methodsFor: 'testing' stamp: 'jmv 8/21/2012 18:56'!
isOpaqueMorph
	"Not really used, as we also reimplement #addPossiblyUncoveredAreasIn:to:"
	^(Theme current roundWindowCorners or: [ ccolor mightBeTranslucent ]) not! !

!methodRemoval: RectangleLikeMorph #morphExtent!
RectangleLikeMorph removeSelector: #morphExtent!
