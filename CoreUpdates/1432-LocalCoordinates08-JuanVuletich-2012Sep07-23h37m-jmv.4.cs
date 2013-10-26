'From Cuis 4.0 of 21 April 2012 [latest update: #1431] on 8 September 2012 at 12:18:42 am'!

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/7/2012 23:39'!
zzroundRect: aRectangle color: aColor radius: radious gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	FormCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor displayRectangle r |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	r _ (currentTransformation externalizeScalar: radious) rounded.
	"top stripe"
	self
		image: (FormCanvas topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) colorOrInfiniteForm: bottomColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) colorOrInfiniteForm: bottomColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/7/2012 23:45'!
zzwindowFrame: aRectangle color: aColor radius: radious border: borderWidth labelHeight: labelHeight gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green
	"
	"top stripe"
	| bottomColor he tl tr displayRectangle r bw lh |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	r _ (currentTransformation externalizeScalar: radious) rounded.
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	lh _ (currentTransformation externalizeScalar: labelHeight) rounded.
	self
		image: (FormCanvas topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: lh) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ displayRectangle topLeft + (0@lh).
	tr _ displayRectangle topRight + (bw negated@lh).
	he _ bw@(displayRectangle height - lh - r).
	self fillRectangle: (tl extent: he) colorOrInfiniteForm: bottomColor.
	self fillRectangle: (tr extent: he) colorOrInfiniteForm: bottomColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@bw negated)) extent: (displayRectangle width - r - r@bw)) colorOrInfiniteForm: bottomColor.

	"inside"
	self fillRectangle: (displayRectangle insetBy: (bw@lh corner: bw@bw)) colorOrInfiniteForm: insideColor! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/8/2012 00:03'!
zzdrawString: s at: pt font: aFont color: aColor

	^ self zzdrawString: s from: 1 to: s size at: pt font: aFont color: aColor! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/8/2012 00:03'!
zzdrawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c
	| font p1 |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [StrikeFont default].
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (origin + p1)
		strikeFont: font
		kern: font baseKern negated! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/8/2012 00:17'!
zzdrawStringEmbossed: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor
	| font portRect insideColor bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	insideColor _ shadowColor ifNil: [ aColor ].
	insideColor = Color black ifFalse: [ | topColor |
		topColor _ insideColor alphaMixed: 0.25 with: Color black.
		port installStrikeFont: font foregroundColor: topColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: (bounds topLeft + origin)
			strikeFont: font
			kern: font baseKern negated ].
	insideColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ insideColor alphaMixed: 0.22 with: Color white.
		port installStrikeFont: font foregroundColor: bottomColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: (bounds topLeft + origin + (0@2))
			strikeFont: font
			kern: font baseKern negated ].
	port installStrikeFont: font foregroundColor: insideColor.
	port
		displayString: aString asString
		from: firstIndex
		to: lastIndex
		at: (bounds topLeft + origin + (0@1))
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/8/2012 00:17'!
zzdrawStringEmbossed: s in: boundsRect font: fontOrNil color: c
	^self zzdrawStringEmbossed: s from: 1 to: s size in: boundsRect font: fontOrNil color: c! !


!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/7/2012 23:48'!
fillRectangle: displayRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: bw borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	"
	| if |
	if _ InfiniteForm verticalGradient: 24 topColor: Color green bottomColor:Color red.
	Display getCanvas
		fillRectangle: (10@10 extent: 300@200)
		colorOrInfiniteForm: if
		borderWidth: 5
		borderStyleSymbol: #raised
		baseColorForBorder: if asColor
	"

	"
Pretty ugly.
#fillRectangle:color::borderWidth:borderStyleSymbol:  is much better but has trouble with silly transparent morphs
	"
	
	self fillRectangle: (displayRectangle insetBy: bw) colorOrInfiniteForm: aColorOrInfiniteForm.
	self frameRectangle: displayRectangle color: baseColorForBorder borderWidth: bw borderStyleSymbol: aSymbol! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/7/2012 23:48'!
zzfillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	| displayRectangle bw |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	self fillRectangle: (displayRectangle insetBy: bw) colorOrInfiniteForm: aColorOrInfiniteForm.
	self frameRectangle: displayRectangle color: baseColorForBorder borderWidth: bw borderStyleSymbol: aSymbol! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:57'!
drawDropHighlightOn: aCanvas

	self highlightedForDrop ifTrue: [
		aCanvas zzframeRectangle: (0@0 extent: self morphExtent) borderWidth: 1 color: self dropHighlightColor ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:57'!
drawMouseDownHighlightOn: aCanvas
	self highlightedForMouseDown ifTrue: [
		aCanvas zzframeRectangle: (0@0 extent: self morphExtent) borderWidth: 1 color: self color darker darker ]! !


!FrameRateMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:04'!
drawOn: aCanvas
	super drawOn: aCanvas.
	meanStepDelta ifNotNil: [
		aCanvas zzdrawString: lastStepDelta rounded printString at: 0@0 font: StrikeFont default color: Color black.
		aCanvas zzdrawString: meanStepDelta rounded printString at: 0@14 font: StrikeFont default color: Color black.
		"aCanvas drawString: lastStepStamp printString at: bounds topLeft + (0@28) font: StrikeFont default color: Color black "
		]! !


!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:31'!
drawOn: aCanvas

	| b |
	aCanvas zzroundRect: (0@0 extent: extent) color: self color radius: 4.
	b _ self morphBoundsInWorld.
	aCanvas
		paragraph: paragraph
		bounds: (b insetBy: 4)
		color: Color black
		selectionColor: (Theme current textHighlightFocused: false)! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:56'!
debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"

	self paragraph lines do: [ :line |
		aCanvas
			zzframeRectangle: line rectangle
			borderWidth: 1
			color: Color brown ]
! !

!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:57'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		paragraph: self paragraph
		bounds: self morphBoundsInWorld
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:49'!
drawOn: aCanvas

	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		colorOrInfiniteForm: color
		borderWidth: 2
		borderStyleSymbol: #raised
		baseColorForBorder: color! !


!MinimalStringMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:12'!
drawOn: aCanvas

	aCanvas zzdrawString: contents in: (0@0 extent: extent) font: self fontToUse color: color! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:12'!
drawOn: aCanvas
	self hasSelection ifTrue: [ self drawSelectionOn: aCanvas ].
	self hasVisibleCaret ifTrue: [ self drawCaretOn: aCanvas ].
	aCanvas
		zzdrawString: contents
		in: (0@0 extent: extent)
		font: self fontToUse
		color: color! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:50'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		colorOrInfiniteForm: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:51'!
drawOn: aCanvas

	self isRoundButton
		ifTrue: [
			icon ifNil: [
				self drawRoundGradientLookOn: aCanvas ]]
		ifFalse: [
			 self draw3DLookOn: aCanvas ].

	icon ifNotNil: [
		self drawInconOn: aCanvas ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:40'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas
					zzroundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas zzroundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !


!RectangleIndicatorMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:58'!
drawOn: aCanvas
	| bw b |
	bw _ self defaultBorderWidth.
	b _ 0@0 extent: extent.
	aCanvas zzframeRectangle: b borderWidth: bw color: Color black.
	aCanvas zzframeRectangle: (b insetBy: bw) borderWidth: bw color: Color white! !


!ScrollBar methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:52'!
drawOn: aCanvas

	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		colorOrInfiniteForm: (color alphaMixed: 0.3 with: Color white)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!StringMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:13'!
drawOn: aCanvas
	aCanvas
		zzdrawString: contents
		in: (0@0 extent: extent)
		font: self fontToUse
		color: color! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 9/7/2012 23:55'!
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker.
	marker morphPosition: self morphPositionInWorld + (0@2)! !

!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:11'!
drawOn: aCanvas 
	| stringColor stringBounds leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas zzfillRectangle: (0@0 extent: extent) colorOrInfiniteForm: Theme current menuHighlight].
	leftEdge _ 0.
	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas zzimage: iconForm at: 1 @ (extent y - iconForm height // 2).
			leftEdge _ iconForm width + self iconSeparation].

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + self submorphBounds width + 8 ].

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

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/7/2012 23:55'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.9) 
			borderWidth: 1 borderColor: Color black.
	^form! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/7/2012 23:55'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		fillRectangle: (form boundingBox insetBy: 2) colorOrInfiniteForm: Color black.
	^form! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/8/2012 00:17'!
drawLabelOn: aCanvas

	Theme current embossedTitles
		ifFalse: [
			aCanvas
				zzdrawString: labelString
				in: self labelRectangle
				font: Preferences windowTitleFont
				color: Theme current windowLabel ]
		ifTrue: [
			aCanvas
				zzdrawStringEmbossed: labelString
				in: self labelRectangleForEmbossed
				font: Preferences windowTitleFont
				color: Theme current windowLabel ]! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/7/2012 23:46'!
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
		zzwindowFrame: (0@0 extent: extent)
		color: widgetsColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: self labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: color! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/8/2012 00:15'!
labelRectangle
	"Actually the whole label area"

	| e x0 y0 x1 y1|
	e _ self boxExtent.
	x0 _  e x * 4 + 14.
	y0 _ 2.
	x1 _ extent x - 1.
	y1 _ e y + 1.
	^x0@y0 corner: x1@y1
	
	
	
! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/8/2012 00:18'!
labelRectangleForEmbossed
	"Actually the whole label area"

	| e x0 y0 x1 y1 |
	e _ self boxExtent.
	x0 _ e x * 4 + 14.
	y0 _ 1.
	x1 _ extent x - 1.
	y1 _ e y + 2.
	^x0@y0 corner: x1@y1
	
	
	
! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 9/4/2012 20:14'!
drawHand: aHandMorph
	"Draw a hand that carries morphs, or needs to be drawn by us, because of not being the hardware mouse pointer."
	| bw r |
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth // 2.
		r _ aHandMorph morphFullBoundsInWorld.
		canvas frameRectangle: r borderWidth: bw color: Color black.
		canvas frameRectangle: (r insetBy: bw) borderWidth: bw color: Color white.
		canvas clipBy: aHandMorph morphBoundsInWorld during: [ :c | aHandMorph drawOn: c ]]
	ifFalse: [
		 canvas fullDraw: aHandMorph]! !

