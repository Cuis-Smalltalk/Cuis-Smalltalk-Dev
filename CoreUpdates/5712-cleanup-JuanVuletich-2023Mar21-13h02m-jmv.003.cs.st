'From Cuis 6.0 [latest update: #5710] on 21 March 2023 at 3:08:57 pm'!

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/21/2023 14:56:02'!
bottomLeftCorner: r height: height
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw l d width dy |
	width := r.
	aaw := 1.3.
	f := Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			0
				to: width - 1
				do: [ :x |
					dy := height - r - y.
					dy > 0
						ifTrue: [ l := 1.0 ]
						ifFalse: [
							d := (width - 1 - x @ dy) r.
							l := ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/21/2023 14:56:25'!
bottomRightCorner: r height: height
	"Create and answer a Form for the bottomRight corner of a rounded rectangle"
	| f aaw l d width dy |
	width := r.
	aaw := 1.3.
	f := Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			0
				to: width - 1
				do: [ :x |
					dy := height - r - y.
					dy > 0
						ifTrue: [ l := 1.0 ]
						ifFalse: [
							d := (x @ dy) r.
							l := ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/21/2023 14:57:45'!
topLeftCorner: r height: height
	"Create and answer a Form for the topLeft corner of a rounded rectangle"
	| f bw l d w |
	w := r.
	bw := 1.3.
	f := Form
		extent: w @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			0
				to: w - 1
				do: [ :x |
					l := 1.0.
					y < r ifTrue: [
						d := (w - 1 - x @ (w - 1 - y)) r.
						l := ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 3/21/2023 14:57:37'!
topRightCorner: r height: height
	"Create and answer a Form for the topRight corner of a rounded rectangle"
	| f bw l d w |
	w := r.
	bw := 1.3.
	f := Form
		extent: w @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			0
				to: w - 1
				do: [ :x |
					l := 1.0.
					y < r ifTrue: [
						d := (x @ (w - y - 1)) r.
						l := ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (`Color white` alpha: l) ]].
	^ f! !


!MorphicCanvas methodsFor: 'drawing-windows' stamp: 'jmv 3/21/2023 14:24:36'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight

	self subclassResponsibility.! !


!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 3/21/2023 14:41:23'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight
	"
	BitBltCanvas onForm: Display :: drawRoundedFrame: (10@10 extent: 200@100) border: 5 color: Color red topColor: Color orange insideColor: Color green labelHeight: 25.
	VectorCanvas onForm: Display :: drawRoundedFrame: (210@10 extent: 200@100) border: 5 color: Color red topColor: Color orange insideColor: Color green labelHeight: 25.
	Display forceToScreen
	"

	| d lh r |
	d := borderWidth*0.5.
	lh := labelHeight + borderWidth.
	self strokeWidth: borderWidth color: borderColor fillColor: insideColor do: [
		self rectangleMinX: aRectangle left+d maxX: aRectangle right-d minY: aRectangle top+lh-d maxY: aRectangle bottom-d ].
	r := Theme current roundedWindowRadius.
	self strokeWidth: borderWidth color: titleAreaColor fillColor: titleAreaColor do: [
		self roundTopRectangleMinX: aRectangle left+d maxX: aRectangle right-d minY: aRectangle top+d maxY: aRectangle top+lh-d rx: r ry: r ].! !


!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 3/21/2023 15:04:31'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight

	self
		windowFrame: aRectangle
		color: borderColor
		radius: Theme current roundedWindowRadius
		border: borderWidth
		topColor: titleAreaColor
		labelHeight: labelHeight + borderWidth
		insideColor: insideColor! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 3/21/2023 15:01:56'!
windowFrame: aRectangle color: aColor radius: r border: bw topColor: titleAreaColor labelHeight: labelHeight insideColor: insideColor

	| lh he tl tr |
	"top stripe"
	lh := labelHeight roundedHAFZ.
	self
		image: (self class topLeftCorner: r height: lh)
		multipliedBy: titleAreaColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: lh)
		multipliedBy: titleAreaColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		color: titleAreaColor.

	"left and right borders"
	tl := aRectangle topLeft + (0@lh).
	tr := aRectangle topRight + (bw negated@lh).
	he := bw@(aRectangle height - lh).
	self fillRectangle: (tl extent: he) color: aColor.
	self fillRectangle: (tr extent: he) color: aColor.
	
	"bottom stripe"
	self fillRectangle: ((aRectangle bottomLeft + (0@bw negated)) extent: (aRectangle width - 0@bw)) color: aColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !


!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 3/21/2023 14:49:50'!
bottomLeftCorner: r height: height
	^self cachedForms
		at: { #bottomLeft. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				bottomLeftCorner: r
				height: height ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 3/21/2023 14:58:34'!
bottomRightCorner: r height: height
	^self cachedForms
		at: { #bottomRight. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				bottomRightCorner: r
				height: height ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 3/21/2023 14:58:20'!
topLeftCorner: r height: height
	^self cachedForms
		at: { #topLeft. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				topLeftCorner: r
				height: height ]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 3/21/2023 14:58:50'!
topRightCorner: r height: height
	^self cachedForms
		at: { #topRight. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				topRightCorner: r
				height: height ]! !


!HybridCanvas methodsFor: 'drawing-windows' stamp: 'jmv 3/21/2023 14:31:45'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight

	self
		windowFrame: aRectangle
		color: borderColor
		radius: Theme current roundedWindowRadius
		border: borderWidth
		topColor: titleAreaColor
		labelHeight: labelHeight + borderWidth
		insideColor: insideColor! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/21/2023 15:05:45'!
drawClassicFrameOn: aCanvas color: windowFrameColor
	"Window border encompasses title area. No round corners."

	aCanvas fillRectangle: (self morphLocalBounds insetBy: 1.5) color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self borderColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: windowFrameColor! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/21/2023 15:06:05'!
drawOn: aCanvas
	| windowFrameColor roundCorners |
	windowFrameColor := self windowFrameColor.
	roundCorners := Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			self drawRoundedFrameOn: aCanvas color: windowFrameColor ]
		ifFalse: [
			self drawClassicFrameOn: aCanvas color: windowFrameColor ].
	labelString ifNotNil: [ self drawLabelOn: aCanvas ].! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/21/2023 14:26:27'!
drawRoundedFrameOn: aCanvas color: windowFrameColor

	aCanvas
		drawRoundedFrame: self morphLocalBounds
		border: borderWidth
		color: self borderColor
		topColor: windowFrameColor
		insideColor: color
		labelHeight: self labelHeight.! !

!SystemWindow methodsFor: 'panes' stamp: 'jmv 3/21/2023 14:42:10'!
windowFrameColor

	| windowFrameColor |
	windowFrameColor := self borderColor.
	self isTopWindow
		ifTrue: [ windowFrameColor := windowFrameColor lighter ].
	^windowFrameColor! !

!SystemWindow methodsFor: 'top window' stamp: 'jmv 3/21/2023 14:46:29'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	
	self isTopWindow ifTrue: [
		self comeToFront.
		^self ].

	oldTop := TopWindow.
	TopWindow := self.
	self invalidateTitleArea.

	oldTop ifNotNil: [
		oldTop invalidateTitleArea.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow := oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	self comeToFront.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !


!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 3/21/2023 14:59:34'!
roundRect: aRectangle color: aColor radius: aNumber
	"
	(BitBltCanvas onForm: Display) roundRect: (10@10 extent: 200@100) color: Color red radius: 10.5. Display forceToScreen.
	"
	| r |
	r := (currentTransformation externalizeScalar: aNumber) roundedHAFZ.

	"top stripe"
	self
		image: (self class topLeftCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self fillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self fillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor! !

!methodRemoval: Theme #titleGradientTopFactor stamp: 'jmv 3/21/2023 15:03:14'!
Theme removeSelector: #titleGradientTopFactor!
!methodRemoval: Theme #titleGradientBottomFactor stamp: 'jmv 3/21/2023 15:03:17'!
Theme removeSelector: #titleGradientBottomFactor!
!methodRemoval: Theme #titleGradientExtraLightness stamp: 'jmv 3/21/2023 15:04:45'!
Theme removeSelector: #titleGradientExtraLightness!
!methodRemoval: Theme #useWindowTitleGradient stamp: 'jmv 3/21/2023 15:04:54'!
Theme removeSelector: #useWindowTitleGradient!
!methodRemoval: HybridCanvas #drawRoundedFrame:border:color:insideColor:labelHeight: stamp: 'jmv 3/21/2023 14:26:47'!
HybridCanvas removeSelector: #drawRoundedFrame:border:color:insideColor:labelHeight:!
!methodRemoval: BitBltCanvas class #verticalGrayGradient:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 15:02:04'!
BitBltCanvas class removeSelector: #verticalGrayGradient:gradientTop:gradientBottom:!
!methodRemoval: BitBltCanvas class #bottomRightCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 14:54:40'!
BitBltCanvas class removeSelector: #bottomRightCorner:height:gradientTop:gradientBottom:!
!methodRemoval: BitBltCanvas class #bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth: stamp: 'jmv 3/21/2023 14:52:15'!
BitBltCanvas class removeSelector: #bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth:!
!methodRemoval: BitBltCanvas class #topLeftCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 15:00:00'!
BitBltCanvas class removeSelector: #topLeftCorner:height:gradientTop:gradientBottom:!
!methodRemoval: BitBltCanvas class #topRightCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 15:00:33'!
BitBltCanvas class removeSelector: #topRightCorner:height:gradientTop:gradientBottom:!
!methodRemoval: BitBltCanvas class #bottomRightCorner:height:gradientTop:gradientBottom:borderWidth: stamp: 'jmv 3/21/2023 14:52:33'!
BitBltCanvas class removeSelector: #bottomRightCorner:height:gradientTop:gradientBottom:borderWidth:!
!methodRemoval: BitBltCanvas class #bottomLeftCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 14:50:44'!
BitBltCanvas class removeSelector: #bottomLeftCorner:height:gradientTop:gradientBottom:!
!methodRemoval: BitBltCanvas #drawRoundedFrame:border:color:insideColor:labelHeight: stamp: 'jmv 3/21/2023 14:26:46'!
BitBltCanvas removeSelector: #drawRoundedFrame:border:color:insideColor:labelHeight:!
!methodRemoval: BitBltCanvas #windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor: stamp: 'jmv 3/21/2023 14:33:03'!
BitBltCanvas removeSelector: #windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:!
!methodRemoval: AbstractVectorCanvas #drawRoundedFrame:border:color:insideColor:labelHeight: stamp: 'jmv 3/21/2023 14:26:45'!
AbstractVectorCanvas removeSelector: #drawRoundedFrame:border:color:insideColor:labelHeight:!
!methodRemoval: MorphicCanvas #drawRoundedFrame:border:color:insideColor:labelHeight: stamp: 'jmv 3/21/2023 14:26:45'!
MorphicCanvas removeSelector: #drawRoundedFrame:border:color:insideColor:labelHeight:!
!methodRemoval: Form class #bottomRightCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 14:54:44'!
Form class removeSelector: #bottomRightCorner:height:gradientTop:gradientBottom:!
!methodRemoval: Form class #bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth: stamp: 'jmv 3/21/2023 14:52:16'!
Form class removeSelector: #bottomLeftCorner:height:gradientTop:gradientBottom:borderWidth:!
!methodRemoval: Form class #topLeftCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 15:00:08'!
Form class removeSelector: #topLeftCorner:height:gradientTop:gradientBottom:!
!methodRemoval: Form class #verticalGradient:topColor:bottomColor: stamp: 'jmv 3/21/2023 15:02:53'!
Form class removeSelector: #verticalGradient:topColor:bottomColor:!
!methodRemoval: Form class #topRightCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 15:00:41'!
Form class removeSelector: #topRightCorner:height:gradientTop:gradientBottom:!
!methodRemoval: Form class #bottomRightCorner:height:gradientTop:gradientBottom:borderWidth: stamp: 'jmv 3/21/2023 14:52:38'!
Form class removeSelector: #bottomRightCorner:height:gradientTop:gradientBottom:borderWidth:!
!methodRemoval: Form class #bottomLeftCorner:height:gradientTop:gradientBottom: stamp: 'jmv 3/21/2023 14:50:48'!
Form class removeSelector: #bottomLeftCorner:height:gradientTop:gradientBottom:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BitBltCanvas releaseClassState!

