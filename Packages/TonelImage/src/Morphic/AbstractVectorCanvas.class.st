"
A VectorGraphics canvas specially built for Morphic.

self runningWorld backgroundImage displayAt: 0@0. (VectorCanvas onForm: Display) fullDraw: self runningWorld. Display copy inspect.
Display writePNGfileNamed: 'm3z.png'

On any Morph:
| c |
c := VectorCanvas onForm: Display.
self allOwnersReverseDo: [ :m | c into: m ].
c fullDraw: self.
Display forceToScreen
"
Class {
	#name : 'AbstractVectorCanvas',
	#superclass : 'MorphicCanvas',
	#instVars : [
		'firstPoint',
		'lastControlPoint',
		'currentPoint'
	],
	#classVars : [
		'IconPathData'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'samples' }
AbstractVectorCanvas class >> experiment1 [
	"
	Not perfect, but can be compared with
	http://www.hpl.hp.com/research/mmsl/projects/graphics/antialiasing/figure21.htm
	BTW, this is so much faster...
	VectorCanvas experiment1
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 e f |
canvas := self onForm: Display.
h := 300.
h := 490.
h := 280.
bottom := 200.
bottom := 0.
centerX := 400.
centerX := 250.
w0 := 120.0.
hf := 1.0.
hf := 1.73.
hf := 1.71.
y := nil.
delta := nil.
	e := 500@500.
	f := Form extent: e depth: 32.
	f fillColor: Color transparent..
"	canvas := self on: f.
	canvas useComponentAlphas."
1 to: "400" 150 do: [ :i |
	i print.
	prevDelta := delta.
	delta := h / (w0 * i + h).
	prevY := y.
	y := bottom + (delta * hf * h).
	prevY ifNotNil: [
"		-40 to: 40 do: [ :ii |"
		-3 // delta to: 3 // delta do: [ :ii |
			x1p := centerX - (prevDelta * w0 * (ii-1)).
			x2p := centerX - (prevDelta * w0 * ii).
			x1 := centerX - (delta * w0 * (ii-1)).
			x2 := centerX - (delta * w0 * ii).
			color := i + ii \\ 2 = 0 ifTrue: [ Color black ] ifFalse: [ Color white ].
"			color := color alpha: (((prevY-y) min: (x1-x2)) min: 1)."
"			color := color alpha: 0.5."
			canvas fillColor: color do: [
				canvas
					moveTo: x1 @ y;
					lineTo: x2 @ y;
					lineTo: x2p @ prevY;
					lineTo: x1p @ prevY;
					closePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.
"
	canvas removeSpuriousTranslucencyFloatAndBlendOver: Color red.
	f writePNGfileNamed: 'pp17.png'
"
]

{ #category : 'samples' }
AbstractVectorCanvas class >> experiment2 [
	"
	VectorCanvas experiment2
	"
| canvas h bottom centerX w0 hf y delta prevDelta prevY color x1p x2p x1 x2 |
canvas := self onForm: Display.
h := 300.
h := 600.
bottom := 200.
centerX := 400.
w0 := 60.0.
hf := 1.0.
y := nil.
delta := nil.
1 to: 400 do: [ :i |
	i print.
	prevDelta := delta.
	delta := h / (w0 * i + h).
	prevY := y.
	y := bottom + (delta * hf * h).
	prevY ifNotNil: [
"		-40 to: 40 do: [ :ii |"
		-6 // delta to: 6 // delta do: [ :ii |
			x1p := centerX - (prevDelta * w0 * (ii-1)).
			x2p := centerX - (prevDelta * w0 * ii).
			x1 := centerX - (delta * w0 * (ii-1)).
			x2 := centerX - (delta * w0 * ii).
			color := i + ii \\ 2 = 0 ifTrue: [ Color white ] ifFalse: [ Color black ].
			color := color alpha: (((prevY-y) min: (x1-x2)) "* 0.3" min: 1).
"			color := color alpha: 0.5."
			canvas fillColor: color do: [
				canvas
					moveTo: x1 @ y;
					lineTo: x2 @ y;
					lineTo: x2p @ prevY;
					lineTo: x1p @ prevY;
					closePath ]
			].
		].
	Display forceToScreen.
	].
Display forceToScreen.

]

{ #category : 'samples' }
AbstractVectorCanvas class >> experiment3 [
	"
	VectorCanvas experiment3
	"
	| canvas w r |
	canvas := self onForm: Display.
	w := 10.
	r := 15.
	100 timesRepeat: [
		canvas strokeWidth: w color: Color black do: [
			canvas circleCenter: 300 @ 300 radius: r ].
		r := r + w.
		w := w * 0.8.
		r := r + w.

	Display forceToScreen.
	].
Display forceToScreen
]

{ #category : 'samples' }
AbstractVectorCanvas class >> experiment4 [
	"
	VectorCanvas experiment4
	"
	| canvas w r |
	canvas := self onForm: Display.
	w := 10.
	r := 15.
	1 to: 100 do: [ :i |
		r := r + w.
		w := 20.0 / i.
		r := r + w.
		canvas strokeWidth: w color: Color black do: [
			canvas circleCenter: 300 @ 300 radius: r ].
	Display forceToScreen.
	].
Display forceToScreen
]

{ #category : 'class initialization' }
AbstractVectorCanvas class >> initialize [
	"
	AbstractVectorCanvas initialize
	"
	IconPathData := Dictionary new.
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> pushPinIconData: aSize [
	| scale |
	(IconPathData at: #pushPinIconSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #pushPinIcon ifPresent: [ :found | ^found ]].
	IconPathData at: #pushPinIconSize put: aSize.
	scale := aSize / 19.0.
	^IconPathData at: #pushPinIcon put: (
		self scalePathData: `#[
			0 0 21
			1 10 13
			1 15 18
			1 14 13
			1 21 6
			1 15 0
			1 8 7
			1 3 6
			1 8 11
			1 0 21
		] asFloat32Array` scale: scale)
]

{ #category : 'private' }
AbstractVectorCanvas class >> scalePathData: aFloat32Array scale: scale [
	"aFloatArray is a #pathSequence: argument. It encodes a sequence of path commands in a Float32Array.
	See #pathSequence:"

	^Float32Array streamContents: [ :scaledStrm | | unscaledStrm kind |
		unscaledStrm := aFloat32Array readStream.
		[ unscaledStrm atEnd ] whileFalse: [
			kind := unscaledStrm next.
			scaledStrm nextPut: kind.
			(kind asInteger *2 max: 2) timesRepeat: [
				scaledStrm nextPut: unscaledStrm next * scale ]]
		].
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> scrollDownPathData: aSize [
	| scale |
	(IconPathData at: #scrollDownSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollDown ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollDownSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollDown put: (
		self scalePathData: `#[
			0 4 7
			1 9 13
			1 14 7
		] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> scrollLeftPathData: aSize [
	| scale |
	(IconPathData at: #scrollLeftSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollLeft ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollLeftSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollLeft put: (
		self scalePathData: `#[
			0 12 4
			1 6 9
			1 12 14
		] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> scrollRightPathData: aSize [
	| scale |
	(IconPathData at: #scrollRightSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollRight ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollRightSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollRight put: (
		self scalePathData: `#[
			0 7 4
			1 13 9
			1 7 14
			] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> scrollUpPathData: aSize [
	| scale |
	(IconPathData at: #scrollUpSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollUp ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollUpSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollUp put: (
		self scalePathData: `#[
			0 4 12
			1 9 6
			1 14 12
			] asFloat32Array` scale: scale)
]

{ #category : 'samples' }
AbstractVectorCanvas class >> siemensStar [
	"
	VectorCanvas siemensStar display
	"
	| angle backColor canvas count extent foreColor form location offsetFromCenter |
	count := 32.
	angle := Float twoPi / count / 2.
	extent := 512.
	offsetFromCenter := 0.
	backColor := `Color gray: 50/255`.
	foreColor := `Color gray: 200/255`.
	form := Form extent: extent asPoint depth: 32.
	form fillColor: backColor.
	canvas := VectorCanvas onForm: form.
	1 to: count do: [ :i |
		location := AffineTransformation withRadians: i * angle * 2+0.02 scale: extent*0.45 position: extent-1/2.0 + offsetFromCenter.
		canvas geometryTransformation: location.
		canvas fillColor: foreColor do: [
			canvas
				moveTo: 0 @ 0;
				lineTo: 1 @ 0;
				lineTo: angle cos @ angle sin;
				lineTo: 0 @ 0.
		].
	].
	^form
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> windowButtonClosePathData: aSize [
	| scale |
	(IconPathData at: #windowButtonCloseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonClose ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCloseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonClose put: (
		self scalePathData: `#[
			0 6 14
			1 14 6
			0 6 6
			1 14 14
		] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> windowButtonCollapsePathData: aSize [
	| scale |
	(IconPathData at: #windowButtonCollapseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonCollapse ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCollapseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonCollapse put: (
		self scalePathData: `#[
			0 5 10
			1 15 10
		] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> windowButtonExpandPathData: aSize [
	| scale |
	(IconPathData at: #windowButtonExpandSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonExpand ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonExpandSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonExpand put: (
		self scalePathData: `#[
			0 5 10
			1 15 10
			0 10 5
			1 10 15
		] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> windowButtonMenuPathData: aSize [
	| scale |
	(IconPathData at: #windowButtonMenuSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonMenu ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonMenuSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonMenu put: (
		self scalePathData: `#[
			0 4 8
			1 16 8
			1 10 16
			1 4 8
		] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> windowButtonPathData: aSize [
	| scale |
	(IconPathData at: #windowButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButton put: (
		self scalePathData: `#[
			0.0 0 10
			2    10 20 0.5 19.5
			2    20 10 19.5 19.5
			2    10 0 19.5 0.5
			2    0 10 0.5 0.5
		] asFloat32Array` scale: scale)
]

{ #category : 'literal path data' }
AbstractVectorCanvas class >> windowSquareButtonPathData: aSize [
	| scale |
	(IconPathData at: #windowSquareButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowSquareButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowSquareButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowSquareButton put: (
		self scalePathData: `#[
			0.0 0 0
			1    0 20
			1    20 20
			1    20 0
			1    0 0
		] asFloat32Array` scale: scale)
]

{ #category : 'paths' }
AbstractVectorCanvas >> arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle [
	engine arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle.
	currentPoint := aPoint.
]

{ #category : 'paths' }
AbstractVectorCanvas >> arcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles [
"
(VectorCanvas onForm: Display)
	strokeWidth: 1.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.2 position: 200@100);
	strokeWidth: 1.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
Display forceToScreen
"

	engine arcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles.
	currentPoint := aPoint.
]

{ #category : 'morphic world' }
AbstractVectorCanvas >> boundingRectOfCurrentIfTrivial [
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	currentMorph knowsOwnLocalBounds ifTrue: [
		"Include an extra pixel to cover possible anti aliasing."
		^(currentTransformation externalBoundingRectOf: currentMorph localBounds)
			encompassingIntegerRectangle outsetBy: `0@0 corner: 1@1` ].
	^nil.
]

{ #category : 'morphic world' }
AbstractVectorCanvas >> boundingRectOfCurrentMorphAfterDraw [
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid _after_ drawing currentMorph.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used only as an optimization of the area to be redrawn."

	| boundingRect |
	"A good optimization in many cases.
	But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
	for example) this is essential."
	self boundingRectOfCurrentIfTrivial ifNotNil: [ :br |
		^ clippingMorphDisplayBounds
			ifNotNil: [ :ownerClips | br intersect: ownerClips ]
			ifNil: [ br ]].

	"Use the bounds bound by the engine during #drawOn.
	This method must therefore be called after calling #drawOn:."
	boundingRect := engine morphBoundsAfterDraw.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]
]

{ #category : 'paths' }
AbstractVectorCanvas >> circleCenter: aPoint radius: mr [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			circleCenter: 200 @ 150 radius: 80].
Display forceToScreen
"

	engine circleCenter: aPoint radius: mr
]

{ #category : 'paths' }
AbstractVectorCanvas >> closePath [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200;
			cubicBezierTo: 300@120 control1: 310@110 control2: 200@200;
			closePath;
			lineTo: 150@300 ].
Display forceToScreen
"

	firstPoint notNil ifTrue: [
		self lineTo: firstPoint ].
]

{ #category : 'paths' }
AbstractVectorCanvas >> closePathAndMoveBack [
	"Close the current path, but don't change the current position"
"
(VectorCanvas onForm: Display)
	strokeWidth: 12.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200;
			cubicBezierTo: 300@120 control1: 310@110 control2: 200@200;
			closePath;
			lineTo: 150@300 ].
(VectorCanvas onForm: Display)
	strokeWidth: 3.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200;
			cubicBezierTo: 300@120 control1: 310@110 control2: 200@200;
			closePathAndMoveBack;
			lineTo: 150@300 ].
Display forceToScreen

"

	| cp |
	(firstPoint notNil and: [ firstPoint ~= currentPoint]) ifTrue: [
		cp := currentPoint.
		self lineTo: firstPoint.
		self moveTo: cp ].
]

{ #category : 'paths' }
AbstractVectorCanvas >> cubicBezier: points [
	"points must be of size 4
	http://en.wikipedia.org/wiki/Bezier_curve"

	self moveTo: points first.
	self
		cubicBezierTo: points fourth
		control1: points second
		control2: points third
]

{ #category : 'paths' }
AbstractVectorCanvas >> cubicBezierSmoothTo: aPoint control2: controlPoint [
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 150@300 radius: 10.
		canvas circleCenter: 600@400 radius: 6.
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400
			].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 600 @ -200 radius: 10.
		canvas circleCenter: 100 @ -100 radius: 6.
		canvas moveTo: 600 @ 100.
		canvas
			cubicBezierTo: 300@100 control1: 600 @ -200 control2: 100 @ -100
			].
Display forceToScreen.
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400;
			cubicBezierSmoothTo: 300@100 control2: 100 @ -100
			].
Display forceToScreen
	"
	| mirroredLastControlPoint |
	mirroredLastControlPoint := currentPoint + (currentPoint - lastControlPoint).
	self cubicBezierTo: aPoint control1: mirroredLastControlPoint control2: controlPoint
]

{ #category : 'paths' }
AbstractVectorCanvas >> cubicBezierTo: aPoint control1: controlPoint1 control2: controlPoint2 [
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ].
Display forceToScreen
	"
	engine cubicBezierTo: aPoint control1: controlPoint1 control2: controlPoint2.
	currentPoint := aPoint.
	lastControlPoint := controlPoint2.
]

{ #category : 'paths' }
AbstractVectorCanvas >> curveTo: aPoint control1: control1Point control2: control2Point [

	self cubicBezierTo: aPoint control1: control1Point control2: control2Point
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawCloseIcon [

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.996 g: 0.329 b: 0.329`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonClosePathData: size) ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawCollapseIcon [

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.988 g: 0.557 b: 0.102`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.988 g: 0.557 b: 0.102) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonCollapsePathData: size) ].
]

{ #category : 'morphic' }
AbstractVectorCanvas >> drawCurrentAsOutline [

	^false
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawDownIcon [
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollDownPathData: size) ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawExpandIcon [

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.337 g: 0.604 b: 0.161`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.337 g: 0.604 b: 0.161) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonExpandPathData: size) ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawExpandedAt: aPoint height: h [
	"For Tree View (IndentingListItemMorph)"

	| halfWidth width |
	width := h * 3/4.
	halfWidth := width / 2.
	self fillColor: `Color lightGray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: (h/8) @ (h/4) negated;
			rel_LineTo: halfWidth @ width;
			rel_LineTo: halfWidth @ width negated;
			rel_LineTo: width negated @ 0 ].
		
]

{ #category : 'morphic' }
AbstractVectorCanvas >> drawHand [

	self drawsHandLarge
		ifTrue: [self drawHandLarge]
		ifFalse: [self drawHandSmall].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawHandLarge [

	self strokeWidth: 2.2 color: `Color black` fillColor: `Color white` do: [ :canvas |
		canvas pathSequence:
			`#[
				0.0    10.97    35.70
				1.0    -3.17    26.78
				2.0    -7.18    20.17    -8.01    23.69
				2.0    -3.16    18.50    -6.55    18.00
				1.0    4.84    20.92
				1.0    5.97    23.63
				1.0    -2.81    2.44
				2.0    -1.35    -3.26    -4.61    -1.91
				2.0    3.71    -0.26    1.91    -4.61
				1.0    9.56    13.87
				1.0    6.41    6.26
				1.0    9.67    4.91
				2.0    13.20    7.27    11.85    4.01
				1.0    15.45    12.70
				1.0    12.75    6.18
				1.0    16.01    4.83
				2.0    19.53    7.19    18.18    3.93
				1.0    21.33    11.54
				1.0    19.08    6.10
				1.0    22.34    4.75
				2.0    25.87    7.11    24.52    3.85
				1.0    27.67    11.46
				1.0    30.82    19.07
				2.0    30.79    34.36    33.52    25.59
				1.0    17.74    39.77
				2.0    11.42    36.78    14.48    41.12
				1.0    10.97    35.70
			] asFloat32Array` ].

]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawHandSmall [

	self strokeWidth: 1.1 color: `Color black` fillColor: `Color white` do: [ :canvas |
		canvas pathSequence:
			`#[
				0.0    5.33    17.34
				1.0    -1.54    13.01
				2.0    -3.49    9.79    -3.89    11.51
				2.0    -1.54    8.99    -3.18    8.74
				1.0    2.35    10.16
				1.0    2.90    11.48
				1.0    -1.37    1.18
				2.0    -0.66    -1.58    -2.24    -0.93
				2.0    1.80    -0.13    0.93    -2.24
				1.0    4.65    6.74
				1.0    3.11    3.04
				1.0    4.70    2.38
				2.0    6.41    3.53    5.75    1.95
				1.0    7.50    6.17
				1.0    6.19    3.00
				1.0    7.78    2.35
				2.0    9.49    3.49    8.83    1.91
				1.0    10.36    5.60
				1.0    9.27    2.96
				1.0    10.85    2.31
				2.0    12.56    3.45    11.91    1.87
				1.0    13.44    5.57
				1.0    14.97    9.26
				2.0    14.95    16.69    16.28    12.43
				1.0    8.62    19.31
				2.0    5.55    17.87    7.04    19.97
				1.0    5.33    17.34
			] asFloat32Array` ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawLeftIcon [
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollLeftPathData: size) ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawMenuIcon [

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.212 g: 0.439 b: 0.784`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.212 g: 0.439 b: 0.784) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self fillColor: s do: [
		self pathSequence: (self class windowButtonMenuPathData: size) ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawNotExpandedAt: aPoint height: h [
	"For Tree View (IndentingListItemMorph)"

	| halfWidth width |
	width := h * 3/4.
	halfWidth := width / 2.
	self fillColor: `Color lightGray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: (h/4) @ halfWidth negated;
			rel_LineTo: width @ halfWidth;
			rel_LineTo: width negated @ halfWidth;
			rel_LineTo: 0 @ width negated.
		].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawPushPinIcon [
	"For Menu stay-up button"
	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329)`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s := `Color black` ].
	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: s fillColor: c do: [
		self pathSequence: (self class pushPinIconData: size) ]
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawRightIcon [
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollRightPathData: size) ].
]

{ #category : 'drawing - windows' }
AbstractVectorCanvas >> drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight [
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
		self roundTopRectangleMinX: aRectangle left+d maxX: aRectangle right-d minY: aRectangle top+d maxY: aRectangle top+lh-d rx: r ry: r ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawSquareCloseIcon [
"make all colors be parameters!!!!!"

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.996 g: 0.329 b: 0.329`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/20.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowSquareButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonClosePathData: size) ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawSquareExpandIcon [

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.337 g: 0.604 b: 0.161`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.337 g: 0.604 b: 0.161) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/20.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowSquareButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonExpandPathData: size) ].
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawSquareMenuIcon [

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.212 g: 0.439 b: 0.784`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.212 g: 0.439 b: 0.784) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/20.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowSquareButtonPathData: size) ].

	self fillColor: s do: [
		self pathSequence: (self class windowButtonMenuPathData: size) ].
]

{ #category : 'drawing - text' }
AbstractVectorCanvas >> drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor [
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	^ aString asUnicodeString drawOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.
]

{ #category : 'drawing - text' }
AbstractVectorCanvas >> drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc [
	"Answer position to place next glyph"
	"
| s c f |
c := VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f := FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s := 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
(c drawString: s from: 1 to: s size atBaseline: 100@180 font: f color: Color black borderWidth: 3 borderColor: Color red) print.
c strokeWidth: 1 color: Color black do: [ c moveTo: 100@180; lineTo: 1000@180 ].
Display forceToScreen
	"
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	^ aString asUnicodeString drawOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor borderWidth: sw borderColor: sc.
]

{ #category : 'drawing - icons' }
AbstractVectorCanvas >> drawUpIcon [
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollUpPathData: size) ].
]

{ #category : 'morphic' }
AbstractVectorCanvas >> drawsHandLarge [

	^true
]

{ #category : 'paths' }
AbstractVectorCanvas >> ellipseCenter: aPoint radius: radiusPoint [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50].
Display forceToScreen
"
	engine ellipseCenter: aPoint radius: radiusPoint 
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> ellipseCenter: center radius: radiusPointOrNumber borderWidth: morphStrokeWidth borderColor: strokeColor fillColor: fillColor [

	| radiusPoint |
	radiusPoint := radiusPointOrNumber asPoint.
	self strokeWidth: morphStrokeWidth color: strokeColor fillColor: fillColor do: [
		self ellipseCenter: center radius: radiusPoint ]
]

{ #category : 'paths' }
AbstractVectorCanvas >> ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
Display forceToScreen
"

	engine ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta
]

{ #category : 'paths' }
AbstractVectorCanvas >> ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle [
	"Currently draws a line from pathCurrentX@pathCurrentY to start (if startAngle ~= 0.0 or sweepAngle ~= Float twoPi).
	This could be useful for proper fills. But might not be what is desired in some cases.
	Implement desired behavior when clear."
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6 ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.0 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6].
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			ellipseCenter: 200@350 radius: 100@50 rotationAngle: 0.5 start: 0.1 sweep: 6 ].
Display forceToScreen.
"

	engine ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle
]

{ #category : 'accessing' }
AbstractVectorCanvas >> engine [
	^engine
]

{ #category : 'stroke & fill' }
AbstractVectorCanvas >> fillColor: aFillColor do: pathCommandsBlock [
	"Prepare parameters for drawing stroke, fill, or both."

	self initForPath.
	engine fillColor: aFillColor do: pathCommandsBlock.
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> fillRectangle: aRectangle color: aColor [

	self fillColor: aColor do: [
		self
			rectangleMinX: aRectangle left maxX: aRectangle right
			minY: aRectangle top maxY: aRectangle bottom ].
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder [
	aSymbol = #simple ifTrue: [
		^ self frameAndFillRectangle: aRectangle fillColor: aColor borderWidth: borderWidth borderColor: baseColorForBorder ].
	
	super fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> frameAndFillRectangle: aRectangle fillColor: aColor borderWidth: borderWidth borderColor: borderColor [
	| d |
	d := borderWidth * 0.5.
	self strokeWidth: borderWidth color: borderColor fillColor: aColor do: [
		self
			rectangleMinX: aRectangle left+d maxX: aRectangle right-d
			minY: aRectangle top+d maxY: aRectangle bottom-d ]
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> frameRectangle: aRectangle borderWidth: borderWidth color: borderColor [
	| d |
	d := borderWidth*0.5.
	self strokeWidth: borderWidth color: borderColor do: [
		self
			rectangleMinX: aRectangle left+d maxX: aRectangle right-d
			minY: aRectangle top+d maxY: aRectangle bottom-d ]
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: morphStrokeWidth [
	"
	(VectorCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| d |
	d := morphStrokeWidth*0.5.
	self strokeWidth: morphStrokeWidth color: brColor do: [
		self moveTo: (aRectangle left + d) @ (aRectangle bottom - d).
		self lineTo: (aRectangle right - d) @ (aRectangle bottom - d).
		self lineTo: (aRectangle right - d) @ (aRectangle top + d) ].

	self strokeWidth: morphStrokeWidth color: tlColor do: [
		self moveTo: (aRectangle left + d) @ (aRectangle bottom - d).
		self lineTo: (aRectangle left + d) @ (aRectangle top + d).
		self lineTo: (aRectangle right - d) @ (aRectangle top + d) ].
]

{ #category : 'accessing' }
AbstractVectorCanvas >> geometryTransformation: aGeometryTransformation [
	"Only used for testing. Usually, transformations are handled as we traverse the Morphs tree."

	currentTransformation := aGeometryTransformation.
	engine geometryTransformation: aGeometryTransformation.
]

{ #category : 'stroke & fill' }
AbstractVectorCanvas >> initForPath [
	"Perform any initialization needed before starting a Path"

	firstPoint := nil.
	currentPoint := `0.0@0.0`.
	lastControlPoint := currentPoint.
]

{ #category : 'morphic' }
AbstractVectorCanvas >> into: aMorph [
	super into: aMorph.
	engine
		currentMorph: currentMorph;
		resetMorphBounds;
		geometryTransformation: currentTransformation.
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> line: pt1 to: pt2 width: morphStrokeWidth color: aStrokeColor [

	self strokeWidth: morphStrokeWidth color: aStrokeColor do: [
		self moveTo: pt1.
		self lineTo: pt2 ]
]

{ #category : 'paths' }
AbstractVectorCanvas >> lineTo: aPoint [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150 ].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineTo: 15@30.
		canvas lineTo: 60@10 ].
Display forceToScreen
"
	engine lineTo: aPoint.
	currentPoint := aPoint.
	lastControlPoint := currentPoint.
]

{ #category : 'paths' }
AbstractVectorCanvas >> lineToX: xTo [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineToX: 200 ].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineToX: 50.
		canvas lineTo: 60@10 ].
Display forceToScreen
"
	self lineTo: xTo @ currentPoint y
]

{ #category : 'paths' }
AbstractVectorCanvas >> lineToY: yTo [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineToY: 200 ].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineToY: 50.
		canvas lineTo: 60@10 ].
Display forceToScreen
"
	self lineTo: currentPoint x @ yTo.
]

{ #category : 'accessing' }
AbstractVectorCanvas >> morphIds [
	^engine morphIds
]

{ #category : 'paths' }
AbstractVectorCanvas >> moveTo: aPoint [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150 ].
Display forceToScreen
"
	engine moveTo: aPoint.
	firstPoint := aPoint.
	currentPoint := aPoint.
	lastControlPoint := currentPoint.
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiArc: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngles: useIncreasingAngles [

	| to r |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				r := rs at: i.
				self arcTo: to radius: r angleOfXAxis: (xAxisRotations at: i) largeFlag: (doLargerArcs at: i) sweepFlag: (useIncreasingAngles at: i) ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				r := rs at: i.
				self rel_ArcTo: to radius: r angleOfXAxis: (xAxisRotations at: i) largeFlag: (doLargerArcs at: i) sweepFlag: (useIncreasingAngles at: i) ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiCurveTo: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points [

	| c1 c2 to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				c1 := control1Points at: i.
				c2 := control2Points at: i.
				self cubicBezierTo: to control1: c1 control2: c2 ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				c1 := control1Points at: i.
				c2 := control2Points at: i.
				self rel_CubicBezierTo: to control1: c1 control2: c2 ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiCurveToSmooth: coordinatesAreRelative toPoints: toPoints control2Points: control2Points [

	| c2 to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				c2 := control2Points at: i.
				self cubicBezierSmoothTo: to control2: c2 ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				c2 := control2Points at: i.
				self rel_CubicBezierSmoothTo: to control2: c2 ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiLineTo: coordinatesAreRelative points: toPoints [

	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				self lineTo: (toPoints at: i) ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				self rel_LineTo:(toPoints at: i) ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiLineTo: coordinatesAreRelative xs: xs [

	coordinatesAreRelative
		ifFalse: [
			xs do: [ :x |
				self lineToX: x ]]
		ifTrue: [
			xs do: [ :x |
				self rel_LineTo: x @ 0 ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiLineTo: coordinatesAreRelative ys: ys [

	coordinatesAreRelative
		ifFalse: [
			ys do: [ :y |
				self lineToY: y ]]
		ifTrue: [
			ys do: [ :y |
				self rel_LineTo: 0 @ y ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiMoveTo: coordinatesAreRelative points: points [

	| isFirst |
	isFirst := true.
	coordinatesAreRelative
		ifFalse: [
			points do: [ :p |
				isFirst
					ifTrue: [ self moveTo: p ]
					ifFalse: [ self lineTo: p ].
				isFirst := false ]]
		ifTrue: [
			points do: [ :p |
				isFirst
					ifTrue: [ self rel_MoveTo: p ]
					ifFalse: [ self rel_LineTo: p ].
				isFirst := false ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiQuadraticCurveTo: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints [

	| c to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				c := controlPoints at: i.
				self quadraticBezierTo: to control: c ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				c := controlPoints at: i.
				self rel_QuadraticBezierTo: to control: c ]]
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> multiQuadraticCurveToSmooth: coordinatesAreRelative toPoints: toPoints [

	| to |
	coordinatesAreRelative
		ifFalse: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				self quadraticBezierSmoothTo: to ]]
		ifTrue: [
			1 to: toPoints size do: [ :i |
				to := toPoints at: i.
				self rel_QuadraticBezierSmoothTo: to ]]
]

{ #category : 'morphic' }
AbstractVectorCanvas >> outOfMorph [
	super outOfMorph.
	engine
		currentMorph: currentMorph;
		resetMorphBounds;
		geometryTransformation: currentTransformation.
]

{ #category : 'paths - sequences' }
AbstractVectorCanvas >> pathSequence: aFloat32Array [
	"
	'Circle approximation by bezier curves
	https://spencermortensen.com/articles/bezier-circle/'.
	(VectorCanvas onForm: Display)
		geometryTransformation: (AffineTransformation withScale: 14 position: 100@100);
		strokeWidth: 3 color: Color random do: [ :engine |
			engine
				pathSequence: `#[
					0.0 15.0 15.0
					3.0 30.0 30.0 15.0 23.3 21.7 30.0
					3.0 45.0 15.0 38.3 30.0 45.0 23.3
					3.0 30.0 0.0 45.0 6.7 38.3 0.0
					3.0 15.0 15.0 21.7 0.0 15.0 6.7
					0.0 22.0 19.0
					2.0 30.0 24.0 24.5 24.0
					2.0 38.0 19.0 35.5 24.0
					0.0 25.0 10.0
					1.0 25.0 12.0
					0.0 35.0 10.0
					1.0 35.0 12.0
					] asFloat32Array`
				].
	Display forceToScreen
	"
	engine pathSequence: aFloat32Array
]

{ #category : 'paths' }
AbstractVectorCanvas >> polyLine: points [

	| p |
	p := points first.
	self moveTo: p.
	2 to: points size do: [ :i |
		p := points at: i.	
		self lineTo: p]
]

{ #category : 'paths' }
AbstractVectorCanvas >> quadraticBezier: points [
	"points must be of size 3
	http://en.wikipedia.org/wiki/Bezier_curve"

	self moveTo: points first.
	self quadraticBezierTo: points third control: points second.
]

{ #category : 'paths' }
AbstractVectorCanvas >> quadraticBezierSmoothTo: aPoint [
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 450@300 radius: 10.
		canvas moveTo: 100@50.
		canvas
			quadraticBezierTo: 600@100 control: 450@300
			].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 750 @ -100 radius: 10.
		canvas moveTo: 600@100.
		canvas
			quadraticBezierTo: 300@100 control: 750 @ -100
			].
Display forceToScreen.
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			quadraticBezierTo: 600@100 control: 450@300;
			quadraticBezierSmoothTo: 300@100
			].
Display forceToScreen
"
	| mirroredLastControlPoint |
	mirroredLastControlPoint := currentPoint + (currentPoint - lastControlPoint).
	self quadraticBezierTo: aPoint control: mirroredLastControlPoint
]

{ #category : 'paths' }
AbstractVectorCanvas >> quadraticBezierTo: aPoint control: controlPoint [
	"
(VectorCanvas onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			quadraticBezierTo: 300@200 control: 100@210 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas quadraticBezierTo: 60@10 control: 15@30 ].
Display forceToScreen
	"
	engine quadraticBezierTo: aPoint control: controlPoint.
	currentPoint := aPoint.
	lastControlPoint := controlPoint.
]

{ #category : 'paths' }
AbstractVectorCanvas >> rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax [

	self
		moveTo: xMin @ yMin;
		lineTo: xMax @ yMin;
		lineTo: xMax @ yMax;
		lineTo: xMin @ yMax;
		closePath
]

{ #category : 'paths - relative coordinates' }
AbstractVectorCanvas >> rel_ArcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles [
"
(VectorCanvas onForm: Display)
	strokeWidth: 5.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			rel_ArcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false;
			closePath ].
Display forceToScreen
"
	self arcTo: currentPoint + aPoint radius: radiusPointOrNumber angleOfXAxis: theta largeFlag: doLargerArc sweepFlag: useIncreasingAngles
]

{ #category : 'paths - relative coordinates' }
AbstractVectorCanvas >> rel_CubicBezierSmoothTo: aPoint control2: controlPoint [
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve
	Smooth squence of curves: use as control1 point, last used control2 point."
"Check that behavior is correct!"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400;
			rel_CubicBezierTo: 300@300 control1: 0@0 control2: 100@100
			].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100 control1: 150@300 control2: 600@400;
			rel_CubicBezierSmoothTo: 300@300 control2: 100@100
			].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 0.2 position: 200@100);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierSmoothTo: 600@100 control2: 600@400;
			cubicBezierSmoothTo: 300@300 control2: 100@100
			].
Display forceToScreen
	"

	self
		cubicBezierSmoothTo: currentPoint + aPoint
		control2: currentPoint + controlPoint
]

{ #category : 'paths - relative coordinates' }
AbstractVectorCanvas >> rel_CubicBezierTo: aPoint control1: control1Point control2: control2Point [
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 1.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ];
	strokeWidth: 1.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_CubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			cubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ];
	strokeWidth: 10 color: (Color red alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			rel_CubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ].
Display forceToScreen
	"

	self
		cubicBezierTo: currentPoint+aPoint
		control1: currentPoint+control1Point
		control2: currentPoint+control2Point
]

{ #category : 'paths - relative coordinates' }
AbstractVectorCanvas >> rel_LineTo: aPoint [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150];
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_LineTo: 200@150].
Display forceToScreen
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas lineTo: 15@30.
		canvas lineTo: 60@10 ];
	geometryTransformation: (AffineTransformation withScale: 10);
	strokeWidth: 2 color: (Color red alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas rel_LineTo: 15@30.
		canvas rel_LineTo: 60@10 ].
Display forceToScreen
"
	self lineTo: currentPoint + aPoint
]

{ #category : 'paths - relative coordinates' }
AbstractVectorCanvas >> rel_MoveTo: aPoint [
"
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			lineTo: 200@150];
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_MoveTo: 10@20;
			lineTo: 200@150].
Display forceToScreen
"
	self moveTo: currentPoint + aPoint
]

{ #category : 'paths - relative coordinates' }
AbstractVectorCanvas >> rel_QuadraticBezierSmoothTo: aPoint [
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color green alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 550@350 radius: 10.
		canvas moveTo: 100@50.
		canvas
			rel_QuadraticBezierTo: 600@100 control: 450@300
			].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color red alpha: 0.7) do: [ :canvas |
		canvas circleCenter: 850 @ -50 radius: 10.
		canvas moveTo: 700@150.
		canvas
			rel_QuadraticBezierTo: -300 @ -50 control: 150 @ -200
			].
Display forceToScreen.
"
"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 0 scale: 1.0 position: 100@300);
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :canvas |
		canvas moveTo: 100@50.
		canvas
			rel_QuadraticBezierTo: 600@100 control: 450@300;
			rel_QuadraticBezierSmoothTo: -300 @ -50
			].
Display forceToScreen
"
	self
		quadraticBezierSmoothTo: currentPoint + aPoint
]

{ #category : 'paths - relative coordinates' }
AbstractVectorCanvas >> rel_QuadraticBezierTo: aPoint control: controlPoint [
	"
(VectorCanvas onForm: Display)
	strokeWidth: 3.0 color: Color black do: [ :canvas |
		canvas
			moveTo: 100@100;
			quadraticBezierTo: 300@200 control: 100@210 ];
	strokeWidth: 3.0 color: Color red do: [ :canvas |
		canvas
			moveTo: 100@100;
			rel_QuadraticBezierTo: 300@200 control: 100@210 ].
Display forceToScreen
	"
	"
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 3 position: 200@100);
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas quadraticBezierTo: 60@10 control: 15@30 ];
	strokeWidth: 2 color: (Color red alpha: 0.7) fillColor: (Color green alpha: 0.4) do: [ :canvas |
		canvas moveTo: 10@5.
		canvas rel_QuadraticBezierTo: 60@10 control: 15@30 ].
Display forceToScreen
	"
	self
		quadraticBezierTo: currentPoint + aPoint
		control: currentPoint + controlPoint
]

{ #category : 'drawing - windows' }
AbstractVectorCanvas >> roundBottomLeftCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor [

	| d1 d2 r corner p1 p2 p3 p4 |
	d1 := borderWidth*0.5.
	d2 := borderWidth*0.5.
	r := Theme current roundedWindowRadius.
	corner := (rectOriginX + d1)@(rectOriginY + l - d2).
	p1 := corner - (0@l).
	p2 := corner - (0@r).
	p3 := corner + (r@0).
	p4 := corner + (l@0).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].
]

{ #category : 'paths' }
AbstractVectorCanvas >> roundBottomRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry [

	| corner1 corner2 corner3 corner4 dx dy p5 p6 p7 p8 |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

	corner1 := xMin@yMin.
	corner2 := xMax@yMin.
	corner3 := xMax@yMax.
	corner4 := xMin@yMax.
	dx := mrx@0.
	dy := 0@mry.
	p5 := corner3 - dy.
	p6 := corner3 - dx.
	p7 := corner4 + dx.
	p8 := corner4 - dy.
	self
		moveTo: corner1;
		lineTo: corner2;
		lineTo: p5;
		quadraticBezierTo: p6 control: corner3;
		lineTo: p7;
		quadraticBezierTo: p8 control: corner4;
		closePath
]

{ #category : 'drawing - windows' }
AbstractVectorCanvas >> roundBottomRightCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor [

	| d2 r corner p1 p2 p3 p4 |
	d2 := borderWidth*0.5.
	r := Theme current roundedWindowRadius.
	corner := (rectOriginX + l - d2)@(rectOriginY + l - d2).
	p1 := corner - (l@0).
	p2 := corner - (r@0).
	p3 := corner - (0@r).
	p4 := corner - (0@l).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].
]

{ #category : 'drawing - windows' }
AbstractVectorCanvas >> roundEdge: aRectangle border: borderWidth color: borderColor [

	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: aRectangle origin;
			lineTo: aRectangle corner ].
]

{ #category : 'drawing - MorphicCanvas protocol' }
AbstractVectorCanvas >> roundRect: aRectangle color: aColor radius: r [

	self fillColor: aColor do: [
		self
			roundRectangleMinX: aRectangle left maxX: aRectangle right
			minY: aRectangle top maxY: aRectangle bottom rx: r ry: r ]
]

{ #category : 'paths' }
AbstractVectorCanvas >> roundRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry [

	| corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 p5 p6 p7 p8 |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^self
			rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

	corner1 := xMin@yMin.
	corner2 := xMax@yMin.
	corner3 := xMax@yMax.
	corner4 := xMin@yMax.
	dx := mrx@0.
	dy := 0@mry.
	p1 := corner1 + dy.
	p2 := corner1 + dx.
	p3 := corner2 - dx.
	p4 := corner2 + dy.
	p5 := corner3 - dy.
	p6 := corner3 - dx.
	p7 := corner4 + dx.
	p8 := corner4 - dy.
	self
		moveTo: p1;
		quadraticBezierTo: p2 control: corner1;
		lineTo: p3;
		quadraticBezierTo: p4 control: corner2;
		lineTo: p5;
		quadraticBezierTo: p6 control: corner3;
		lineTo: p7;
		quadraticBezierTo: p8 control: corner4;
		closePath
]

{ #category : 'drawing - windows' }
AbstractVectorCanvas >> roundTopLeftCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor [

	| d1 r corner p1 p2 p3 p4 |
	d1 := borderWidth*0.5.
	r := Theme current roundedWindowRadius.
	corner := rectOriginX@rectOriginY + d1.
	p1 := corner + (0@l).
	p2 := corner + (0@r).
	p3 := corner + (r@0).
	p4 := corner + (l@0).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].
]

{ #category : 'paths' }
AbstractVectorCanvas >> roundTopRectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax rx: mrx ry: mry [

	| corner1 corner2 corner3 corner4 dx dy p1 p2 p3 p4 |
	(mrx = 0.0 or: [ mry = 0.0 ]) ifTrue: [
		^ self rectangleMinX: xMin maxX: xMax minY: yMin maxY: yMax ].

	corner1 := xMin@yMin.
	corner2 := xMax@yMin.
	corner3 := xMax@yMax.
	corner4 := xMin@yMax.
	dx := mrx@0.
	dy := 0@mry.
	p1 := corner1 + dy.
	p2 := corner1 + dx.
	p3 := corner2 - dx.
	p4 := corner2 + dy.
	self
		moveTo: p1;
		quadraticBezierTo: p2 control: corner1;
		lineTo: p3;
		quadraticBezierTo: p4 control: corner2;
		lineTo: corner3;
		lineTo: corner4;
		closePath
]

{ #category : 'drawing - windows' }
AbstractVectorCanvas >> roundTopRightCornerX: rectOriginX y: rectOriginY length: l border: borderWidth color: borderColor [

	| d1 d2 r corner p1 p2 p3 p4 |
	d1 := borderWidth*0.5.
	d2 := borderWidth*0.5.
	r := Theme current roundedWindowRadius.
	corner := (rectOriginX + l - d2)@(rectOriginY + d1).
	p1 := corner - (l@0).
	p2 := corner - (r@0).
	p3 := corner + (0@r).
	p4 := corner + (0@l).
	self strokeWidth: borderWidth color: borderColor do: [
		self
			moveTo: p1;
			lineTo: p2;
			quadraticBezierTo: p3 control: corner;
			lineTo: p4 ].
]

{ #category : 'paths' }
AbstractVectorCanvas >> stadium: aRectangle [
	"
| r |
r := 500@150 extent: 100@250.
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
			canvas stadium: r ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			stadium: r ].
Display forceToScreen
"
	| height p1 p2 p3 p4 radius width x1 x2 y1 y2 |
	width := aRectangle width.
	height := aRectangle height.
	radius := (width min: height) / 2.
	height < width
		ifTrue: [
			x1 := aRectangle left + radius.
			x2 := aRectangle right - radius.
			y1 := aRectangle top.
			y2 := aRectangle bottom.
			p1 := x1 @ y1.
			p2 := x2 @ y1.
			p3 := x2 @ y2.
			p4 := x1 @ y2 ]
		ifFalse: [
			x1 := aRectangle left.
			x2 := aRectangle right.
			y1 := aRectangle top + radius.
			y2 := aRectangle bottom - radius.
			p1 := x1 @ y2.
			p2 := x1 @ y1.
			p3 := x2 @ y1.
			p4 := x2 @ y2].
	self
		moveTo: p1;
		lineTo: p2;
		arcTo: p3 radius: radius angleOfXAxis: 0 largeFlag: false sweepFlag: true;
		lineTo: p4;
		arcTo: p1 radius: radius angleOfXAxis: 0 largeFlag: false sweepFlag: true.
]

{ #category : 'stroke & fill' }
AbstractVectorCanvas >> strokeWidth: strokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: lengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock [
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		- onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
		- lengthOfEachBit is exactly that, the actual length of stroke for each bit
		- offsetOrNil can be used to do 'walking dashes' effects. It is an integer (nil defaults to 0).
	+ onOffBitSequence is usually stated in base-2.
	A possible example is 2r1110000100000. This draws '---    -     '
	"

	self initForPath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		dashedStrokeBits: onOffBitSequence dashBitLength: lengthOfEachBit dashBitOffset: offsetOrNil
		do: pathCommandsBlock.
]

{ #category : 'stroke & fill' }
AbstractVectorCanvas >> strokeWidth: strokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence do: pathCommandsBlock [
	"Prepare parameters for drawing stroke.
	Do dashed strokes. This spec is periodic, the pattern is repeated as needed.
		- onOffBitSequence is an integer number, a sequence of bits specifying on/off parts
			A possible example is 2r1110000100000. This draws '---    -     '
	"

	self strokeWidth: strokeWidth
		color: aStrokeColor
		dashedStrokeBits: onOffBitSequence
		dashBitLength: nil
		dashBitOffset: nil
		do: pathCommandsBlock
]

{ #category : 'stroke & fill' }
AbstractVectorCanvas >> strokeWidth: strokeWidth color: aStrokeColor do: pathCommandsBlock [
	"Prepare parameters for drawing stroke."

	self initForPath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		do: pathCommandsBlock.
]

{ #category : 'stroke & fill' }
AbstractVectorCanvas >> strokeWidth: strokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock [
	"Prepare parameters for drawing stroke, fill, or both."

	self initForPath.
	engine
		strokeWidth: strokeWidth
		color: aStrokeColor
		fillColor: aFillColor
		do: pathCommandsBlock.
]
