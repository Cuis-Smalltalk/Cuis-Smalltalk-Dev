'From Cuis 4.0 of 21 April 2012 [latest update: #1436] on 9 September 2012 at 12:18:12 am'!

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/8/2012 23:05'!
zzimage: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ currentTransformation transform: aPoint.
	self isShadowDrawing ifTrue: [
		^self stencil: aForm at: p rounded sourceRect: sourceRect color: shadowColor ].
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint]
		ifFalse: [ Form blend ].
	self image: aForm
		at: p rounded
		sourceRect: sourceRect
		rule: r! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/8/2012 23:07'!
zzimage: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	Display getCanvas image: (SystemWindow roundedCornerTR: 20)multipliedBy: Color red at: 20@20
	"
	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		self zzimage: AuxForm at: aPoint sourceRect: aForm boundingBox ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 00:16'!
fillRectangle: r color: fillColor
	| rect |
	rect _ r translatedBy: origin.
	"draw the border of the rectangle"

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: rect offset: origin ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/8/2012 23:49'!
zzfillRectangle: aRectangle infiniteForm: anInfiniteForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of anInfiniteForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) infiniteForm: (SystemWindow titleGradient: 12) multipliedBy: Color red.
	"

	| f displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	f _ anInfiniteForm form.
	AccessProtect critical: [
		self buildAuxWith: f multipliedWith: aColor.
		self fillRectangle: displayRectangle tilingWith: AuxForm sourceRect: f boundingBox rule: Form paint ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 00:09'!
zzframeRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	Display getCanvas zzfillRectangle: (10@10 extent: 300@200) colorOrInfiniteForm: Color white.
	Display getCanvas
		zzframeRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 2
		borderStyleSymbol: #raised.
	"

	| displayRectangle bw |
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	aSymbol == #raised ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteWhiter
			bottomRightColor: aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteBlacker
			bottomRightColor: aColor quiteWhiter ].
	
	"Unrecognized border style. Draw some border..."
	self zzframeRectangle: r borderWidth: bw color: aColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/8/2012 23:49'!
zzroundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	"
	Display restore.
	FormCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35
	"
	| h2 |
	"top stripe"
	self
		zzimage: (FormCanvas topLeftCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		zzimage: (FormCanvas topRightCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		zzfillRectangle: ((aRectangle withHeight: h1) insetBy: r@0)
		infiniteForm: (FormCanvas verticalGrayGradient: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor.
	
	"bottom stripe"
	h2 _ aRectangle height - h1.
	self
		zzimage: (FormCanvas bottomLeftCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft + (0@h1).
	self
		zzimage: (FormCanvas bottomRightCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight + (r negated@h1).
	self
		zzfillRectangle: ((aRectangle topLeft + (r@h1)) extent: (aRectangle width-r-r@h2))
		infiniteForm: (FormCanvas verticalGrayGradient: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.! !


!Color class methodsFor: 'examples' stamp: 'jmv 9/8/2012 23:19'!
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
Display getCanvas zzfillRectangle: (0@0 extent: height@height) colorOrInfiniteForm: selectedColor.
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


!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/8/2012 23:05'!
zzimage: aForm at: aPoint
	"Draw a translucent image using the best available way of representing translucency."

	self zzimage: aForm
		at: aPoint
		sourceRect: aForm boundingBox! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/8/2012 23:23'!
zzfillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm
	"Fill the given rectangle."

	| f colorOrInfiniteForm displayRectangle |

	colorOrInfiniteForm _ aColorOrInfiniteForm.
	self isShadowDrawing
		ifTrue: [
			colorOrInfiniteForm _ shadowColor ]
		ifFalse: [
			(aColorOrInfiniteForm isKindOf: InfiniteForm) ifTrue: [
				displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
				f _ aColorOrInfiniteForm form.
				^self fillRectangle: displayRectangle tilingWith: f sourceRect: f boundingBox rule: Form paint ]].

	^self 
		zzframeAndFillRectangle: aRectangle
		fillColor: colorOrInfiniteForm
		borderWidth: 0
		borderColor: Color transparent! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/9/2012 00:09'!
zzfillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	self zzfillRectangle: (aRectangle insetBy: borderWidth) colorOrInfiniteForm: aColorOrInfiniteForm.
	self zzframeRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/8/2012 23:18'!
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
	self zzfillRectangle: ((aRectangle withHeight: r) insetBy: r@0) colorOrInfiniteForm: aColor.

	"center stripe"
	self zzfillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) colorOrInfiniteForm: aColor.
	
	"bottom stripe"
	self
		zzimage: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		zzimage: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self zzfillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) colorOrInfiniteForm: aColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/8/2012 23:51'!
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
	self zzfillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) colorOrInfiniteForm: bottomColor.
	
	"bottom stripe"
	self
		zzimage: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		zzimage: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self zzfillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) colorOrInfiniteForm: bottomColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 9/8/2012 23:52'!
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
	self zzfillRectangle: (tl extent: he) colorOrInfiniteForm: bottomColor.
	self zzfillRectangle: (tr extent: he) colorOrInfiniteForm: bottomColor.
	
	"bottom stripe"
	self
		zzimage: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		zzimage: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self zzfillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) colorOrInfiniteForm: bottomColor.

	"inside"
	self zzfillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) colorOrInfiniteForm: insideColor! !


!FormCanvas class methodsFor: 'cached forms' stamp: 'jmv 9/8/2012 23:49'!
steButtonForm: extent
	^CachedForms
		at: { #steButton . extent }
		ifAbsentPut: [
			| form canvas |
			form _ Form extent: extent depth: 32.
			canvas _ form getCanvas.
			canvas
				zzroundRect: (0@0 extent: extent)
				color: (Color gray: 0.4)
				radius: 4.
			canvas
				zzroundRect: (1@1 extent: extent-2)
				color: Color white
				radius: 4
				gradientTop: 1.0
				gradientCenter: 0.73
				gradientBottom: 0.94
				gradient1Height: (extent y-8+1 max: extent y//2).
			form]! !


!GrafPort methodsFor: 'drawing support' stamp: 'jmv 9/8/2012 23:03'!
image: aForm at: aPoint sourceRect: sourceRect rule: rule
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."

	sourceForm _ aForm.
	combinationRule _ rule.
	self sourceRect: sourceRect.
	self destOrigin: aPoint.
	self copyBits! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 23:45'!
drawOn: aCanvas

	aCanvas
		zzimage: (self class circleForm: extent)
		multipliedBy: (color alpha: 0.57)
		at: 0@0! !


!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 00:10'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		zzframeAndFillRectangle: form boundingBox fillColor: (Color gray: 0.9) 
			borderWidth: 1 borderColor: Color black.
	^form! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/8/2012 23:24'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		zzframeAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		zzfillRectangle: (form boundingBox insetBy: 2) colorOrInfiniteForm: Color black.
	^form! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 23:25'!
drawOn: aCanvas 
	| baseColor r |
	baseColor _ owner color.
	r _ self morphBoundsInWorld.
	aCanvas
		zzfillRectangle: (r topLeft corner: r rightCenter)
		colorOrInfiniteForm: baseColor twiceDarker.
			
	aCanvas
		zzfillRectangle: (r leftCenter corner: r bottomRight)
		colorOrInfiniteForm: baseColor twiceLighter! !


!Paragraph methodsFor: 'display' stamp: 'jmv 9/9/2012 00:16'!
displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas paragraphTopLeft: paragraphTopLeft  selectionColor: sc
	| leftX rightX idx caretFont t b caretAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show caret on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			leftX _ paragraphTopLeft x + startBlock left.
			idx _ startBlock stringIndex.
			caretAttributes _ editor ifNotNil: [ editor currentAttributes ].
			caretFont _ caretAttributes
				ifNil: [ model actualContents fontAt: idx ]
				ifNotNil: [ model actualContents fontIfApplying: caretAttributes ].
			b _ paragraphTopLeft y + line top + line baseline + caretFont descent-1.
			t _ paragraphTopLeft y + line top + line baseline - caretFont ascent.
			showCaret ifTrue: [
				self
					displayInsertionMarkAtX: leftX
					top: t
					bottom: b
					emphasis: caretFont emphasis
					on: aCanvas
					paragraphTopLeft: paragraphTopLeft ]]
		ifFalse: [
			"Test entire selection before or after here"
			(stopBlock stringIndex < line first 
				or: [startBlock stringIndex > (line last + 1)])
					ifTrue: [^self].	"No selection on this line"
			(stopBlock stringIndex = line first 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].	"Selection ends on line above"
			(startBlock stringIndex = (line last + 1) 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].
			lastCaretRect _ nil.
			leftX _  paragraphTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  paragraphTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  paragraphTopLeft y) corner: rightX @ (line bottom +  paragraphTopLeft y))
				color: sc ].	"Selection begins on line below"! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 23:46'!
drawInconOn: aCanvas

	| theIcon |
	theIcon _ self magnifiedIcon.
	aCanvas
		zzimage: theIcon
		multipliedBy: self iconColor
		at: (extent - theIcon extent //2)! !


!ProgressBarMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:13'!
drawOn: aCanvas

	super drawOn: aCanvas.
	aCanvas
		zzfillRectangle: (borderWidth@borderWidth extent: extent x - borderWidth - borderWidth * value @ extent y - borderWidth-borderWidth)
		colorOrInfiniteForm: progressColor! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 9/9/2012 00:17'!
drawHand: aHandMorph
	"Draw a hand that carries morphs, or needs to be drawn by us, because of not being the hardware mouse pointer."
	| bw r |
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth // 2.
		r _ aHandMorph morphFullBoundsInWorld.
		canvas zzframeRectangle: r borderWidth: bw color: Color black.
		canvas zzframeRectangle: (r insetBy: bw) borderWidth: bw color: Color white.
		canvas clipBy: aHandMorph morphBoundsInWorld during: [ :c | aHandMorph drawOn: c ]]
	ifFalse: [
		 canvas fullDraw: aHandMorph]! !

!methodRemoval: ScrollBar #zzzdrawOn:!
ScrollBar removeSelector: #zzzdrawOn:!
!methodRemoval: FormCanvas #fillRectangle:colorOrInfiniteForm:!
FormCanvas removeSelector: #fillRectangle:colorOrInfiniteForm:!
!methodRemoval: FormCanvas #fillRectangle:fillColor:!
FormCanvas removeSelector: #fillRectangle:fillColor:!
!methodRemoval: FormCanvas #fillRectangle:infiniteForm:multipliedBy:!
FormCanvas removeSelector: #fillRectangle:infiniteForm:multipliedBy:!
!methodRemoval: FormCanvas #frameAndFillRectangle:fillColor:borderWidth:borderColor:!
FormCanvas removeSelector: #frameAndFillRectangle:fillColor:borderWidth:borderColor:!
!methodRemoval: FormCanvas #frameRectangle:borderWidth:color:!
FormCanvas removeSelector: #frameRectangle:borderWidth:color:!
!methodRemoval: FormCanvas #frameRectangle:color:borderWidth:borderStyleSymbol:!
FormCanvas removeSelector: #frameRectangle:color:borderWidth:borderStyleSymbol:!
!methodRemoval: FormCanvas #image:at:sourceRect:!
FormCanvas removeSelector: #image:at:sourceRect:!
!methodRemoval: FormCanvas #image:multipliedBy:at:!
FormCanvas removeSelector: #image:multipliedBy:at:!
!methodRemoval: FormCanvas #roundRect:color:radius:!
FormCanvas removeSelector: #roundRect:color:radius:!
!methodRemoval: FormCanvas #roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height:!
FormCanvas removeSelector: #roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height:!
!methodRemoval: FormCanvas #zzzfillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:baseColorForBorder:!
FormCanvas removeSelector: #zzzfillRectangle:colorOrInfiniteForm:borderWidth:borderStyleSymbol:baseColorForBorder:!
