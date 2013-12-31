'From Cuis 4.0 of 21 April 2012 [latest update: #1436] on 9 September 2012 at 12:34:30 am'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 00:29'!
zzfillRectangle: aRectangle color: aColor
	"Fill the given rectangle."

	| color |

	color _ aColor.
	self isShadowDrawing ifTrue: [
		color _ shadowColor ].

	^self 
		zzframeAndFillRectangle: aRectangle
		fillColor: color
		borderWidth: 0
		borderColor: Color transparent! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 00:30'!
zzfillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	self zzfillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self zzframeRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol! !


!GrafPort methodsFor: 'drawing support' stamp: 'jmv 9/9/2012 00:33'!
fillRect: rect

	destX _ rect left.
	destY _ rect top.
	sourceX _ 0.
	sourceY _ 0.
	width _ rect width.
	height _ rect height.
	self copyBits! !


!Color class methodsFor: 'examples' stamp: 'jmv 9/9/2012 00:30'!
experimentsTowarsANewColorPalette
"
self experimentsTowarsANewColorPalette
"
| selectedHue selectedSaturation selectedV selectedColor h s v color width height selectedChroma selectedLuminance |
width _ 300.
height _ 120.
selectedColor _ Color fromUser.
selectedHue _ selectedColor hue.
selectedSaturation _ selectedColor saturation.
selectedChroma _ selectedColor chroma.
selectedV _ selectedColor brightness.
selectedLuminance _ selectedColor luminance.
Display getCanvas zzfillRectangle: (0@0 extent: height@height) color: selectedColor.
0 to: height do: [ :y |
	v _ 1.0 - (y / height).
	0 to: height do: [ :x |
		s _ x / height.
		color _ Color basicNew setHue: selectedHue saturation: s brightness: v.
		Display colorAt: x@(y+height) put: color
	].
	Display forceToScreen
].
0 to: height do: [ :y | | c |
	v _ 1.0 - (y / height).
	s _ 1.0 - (y / height).
	c _ s.
	0 to: width do: [ :x |
		h _ x / width * 360.
		
		color _ Color basicNew setHue: h chroma: c luminance: selectedLuminance.
"		color _ Color basicNew setHue: h chroma: c brightness: selectedV."
		color ifNil: [ color _ Color black ].
"		color _ Color basicNew setHue: h saturation: s brightness: selectedV."
		Display colorAt: x+height@y put: color.
		
		color _ Color basicNew setHue: h chroma: selectedChroma luminance: v.
"		color _ Color basicNew setHue: h chroma: selectedChroma brightness: v."
		color ifNil: [ color _ Color black ].
"		color _ Color basicNew setHue: h saturation: selectedSaturation brightness: v."
		Display colorAt: x+height@(y+height) put: color.
		
	].
	Display forceToScreen
].! !


!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 00:33'!
fillRectangle: r color: fillColor
	| rect |
	rect _ r translatedBy: origin.
	"draw the border of the rectangle"

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: rect ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 00:33'!
zzframeAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |
	rect _ (currentTransformation displayBoundsOfTransformOf: r) translatedBy: origin.
	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: borderWidth ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) ]! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/9/2012 00:30'!
zzroundRect: aRectangle color: aColor radius: r
	"
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10
	"
	"radious is not scaled properly..."
	"top stripe"
	self
		zzimage: (FormCanvas topLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		zzimage: (FormCanvas topRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self zzfillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self zzfillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		zzimage: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		zzimage: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self zzfillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/9/2012 00:30'!
zzroundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	FormCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor |
	"top stripe"
	self
		zzimage: (FormCanvas topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		zzimage: (FormCanvas topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		zzfillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self zzfillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) color: bottomColor.
	
	"bottom stripe"
	self
		zzimage: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		zzimage: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self zzfillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) color: bottomColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/9/2012 00:31'!
zzwindowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green
	"
	"top stripe"
	| bottomColor he tl tr |
	self
		zzimage: (FormCanvas topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		zzimage: (FormCanvas topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		zzfillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ aRectangle topLeft + (0@lh).
	tr _ aRectangle topRight + (bw negated@lh).
	he _ bw@(aRectangle height - lh - r).
	self zzfillRectangle: (tl extent: he) color: bottomColor.
	self zzfillRectangle: (tr extent: he) color: bottomColor.
	
	"bottom stripe"
	self
		zzimage: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		zzimage: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self zzfillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self zzfillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/9/2012 00:22'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor screen patternWord |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent].
	port sourceForm: nil.
	(paintColor isOpaque) ifTrue: [
		port fillPattern: paintColor.
		port combinationRule: Form paint.
		self depth = 8 ifTrue:[
			"In 8 bit depth it's usually a good idea to use a stipple pattern"
			port fillColor: (form balancedPatternFor: paintColor)].
		^self].

	self depth > 8 ifTrue:[
		"BitBlt setup for alpha mapped transfer"
		port fillPattern: paintColor.
		self depth = 16
			ifTrue:[port alphaBits: paintColor privateAlpha; combinationRule: 31]
			ifFalse:[port combinationRule: Form blend].
		^self].

	"Can't represent actual transparency -- use stipple pattern"
	screen _ Color translucentMaskFor: paintColor alpha depth: self depth.
	patternWord _ form pixelWordFor: paintColor.
	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).
	port combinationRule: Form paint! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		zzfillRectangle: (0@0 extent: self morphExtent)
		color: self color! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:26'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:30'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness |
	aCanvas zzframeAndFillRectangle: (0@0 extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ extent x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			zzframeRectangle: (extent x - scrollbarThickness@0
				extent: scrollbarThickness @ extent y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			zzimage: (FormCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: extent x - scrollbarThickness@0.
		aCanvas
			zzimage: (FormCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: 0@0 + extent - scrollbarThickness.
		h _ extent y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			zzfillRectangle: (extent x - scrollbarThickness+2@y1 corner:  extent x-2 @ y2)
			color: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas zzfillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].
			aCanvas
				zzdrawString: (completer entries at: index) asString
				in: rectangle
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !


!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:30'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				zzroundRect: (0@0 extent: extent)
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		zzfillRectangle: (14@25 extent: extent-(28@62))
		color: (Theme current paneBackgroundFrom: color)! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawOn: aCanvas

	| tRect sRect colorToUse sLeft aForm centeringOffset |
	isSelected ifTrue: [
		aCanvas
			zzfillRectangle: (0@0 extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	complexContents hasContents ifTrue: [
		tRect _ self toggleRectangle.
		aForm _ isExpanded 
			ifTrue: [ container expandedForm ]
			ifFalse: [ container notExpandedForm ].
		centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.
		aCanvas 
			zzimage: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	sLeft _ indentLevel * 12 + 16.
	sRect _ sLeft@0 extent: extent - (sLeft@0).
	colorToUse _ complexContents preferredColor ifNil: [ color ].
	aCanvas
		zzdrawString: contents asString
		in: sRect
		font: self fontToUse
		color: colorToUse! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas zzfillRectangle: selectionDrawBounds color: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	aCanvas
		zzfillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !


!LayoutAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:26'!
drawOn: aCanvas

	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		color: color
		borderWidth: 2
		borderStyleSymbol: #raised
		baseColorForBorder: color! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawOn: aCanvas 
	| stringColor stringBounds leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas zzfillRectangle: (0@0 extent: extent) color: Theme current menuHighlight].
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

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 00:31'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		zzframeAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		zzfillRectangle: (form boundingBox insetBy: 2) color: Color black.
	^form! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawOn: aCanvas 
	| baseColor r |
	baseColor _ owner color.
	r _ self morphBoundsInWorld.
	aCanvas
		zzfillRectangle: (r topLeft corner: r rightCenter)
		color: baseColor twiceDarker.
			
	aCanvas
		zzfillRectangle: (r leftCenter corner: r bottomRight)
		color: baseColor twiceLighter! !


!MenuMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:26'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas zzroundRect: (0@0 extent: extent) color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas zzfillRectangle: (0@0 extent: extent) color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawSelectionOn: aCanvas
	| rightX leftX bottom |

	bottom _ self baseFont height.
	leftX _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
	rightX _ self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1.

	aCanvas
		zzfillRectangle: (leftX @ 0 corner: rightX @ bottom)
		color: (Theme current textHighlightFocused: self hasKeyboardFocus)! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:26'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !


!ProgressBarMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawOn: aCanvas

	super drawOn: aCanvas.
	aCanvas
		zzfillRectangle: (borderWidth@borderWidth extent: extent x - borderWidth - borderWidth * value @ extent y - borderWidth-borderWidth)
		color: progressColor! !


!ScrollBar methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:27'!
drawOn: aCanvas

	aCanvas
		zzfillRectangle: (0@0 extent: extent)
		color: (color alphaMixed: 0.3 with: Color white)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:31'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas zzfillRectangle: (0@0 extent: extent) color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas zzfillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: titleColor! !

!methodRemoval: GrafPort #fillRect:offset:!
GrafPort removeSelector: #fillRect:offset:!
!methodRemoval: FormCanvas #zzfillRectangle:colorOrInfiniteForm:!
FormCanvas removeSelector: #zzfillRectangle:colorOrInfiniteForm:!
!methodRemoval: FormCanvas #zzfillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:baseColorForBorder:!
FormCanvas removeSelector: #zzfillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:baseColorForBorder:!
