'From Cuis7.5 [latest update: #7282] on 26 June 2025 at 5:35:08 pm'!

!Form class methodsFor: 'instance creation' stamp: 'jmv 6/26/2025 16:52:37'!
fakeSphereAt: aPoint diameter: diameter color: aColor
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb := BitBltCanvasEngine toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect := aPoint extent: diameter.
	bb fillOval: rect.
	p := (diameter * 2 // 5) asPoint + aPoint.
	#(2 3 4 5 6 ) do: [ :i |
		"simulate facade by circles of gray"
		bb fillColor: `Color white alpha: 0.3`.
		d := i * diameter // 10.
		rect := Rectangle
			center: p
			extent: d roundedToEven.
		bb fillOval: rect ].
	Display forceToScreen.
"
	Form fakeSphereAt: 70@70 diameter: 40 color:  (Color red alpha: 0.5)
"! !


!BitBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:40:00'!
alphaBlendDemo
	"To run this demo, use...
		Display restoreAfter: [BitBlt alphaBlendDemo]
	Displays 10 alphas, then lets you paint.  Option-Click to stop painting."

	"This code exhibits alpha blending in any display depth by performing
	the blend in an off-screen buffer with 32-bit pixels, and then copying
	the result back onto the screen with an appropriate color map. - tk 3/10/97"
	
	"This version uses a sliding buffer for painting that keeps pixels in 32 bits
	as long as they are in the buffer, so as not to lose info by converting down
	to display resolution and back up to 32 bits at each operation. - di 3/15/97"

	| brush buff dispToBuff buffToDisplay mapDto32 map32toD prevP p brushToBuff theta buffRect buffSize buffToBuff brushRect delta newBuffRect updateRect |  

	"compute color maps if needed"
	Display depth <= 8 ifTrue: [
		mapDto32 := Color cachedColormapFrom: Display depth to: 32.
		map32toD := Color cachedColormapFrom: 32 to: Display depth].

	"display 10 different alphas, across top of screen"
	buff := Form extent: 500@50 depth: 32.
	dispToBuff := BitBlt toForm: buff.
	dispToBuff colorMap: mapDto32.
	dispToBuff copyFrom: (50@10 extent: 500@50) in: Display to: 0@0.
	1 to: 10 do: [:i | dispToBuff fill: (50*(i-1)@0 extent: 50@50)
						fillColor: (Color red alpha: i/10)
						rule: Form blend].
	buffToDisplay := BitBlt toForm: Display.
	buffToDisplay colorMap: map32toD.
	buffToDisplay copyFrom: buff boundingBox in: buff to: 50@10.
	Display forceToScreen.

	"Create a brush with radially varying alpha"
	brush := Form extent: 30@30 depth: 32.
	1 to: 5 do: 
		[:i | brush fillShape: (Form dotOfSize: brush width*(6-i)//5)
				fillColor: (Color red alpha: 0.02 * i - 0.01)
				at: brush extent // 2].

	"Now paint with the brush using alpha blending."
	buffSize := 100.
	buff := Form extent: brush extent + buffSize depth: 32.  "Travelling 32-bit buffer"
	dispToBuff := BitBlt toForm: buff.  "This is from Display to buff"
	dispToBuff colorMap: mapDto32.
	brushToBuff := BitBlt toForm: buff.  "This is from brush to buff"
	brushToBuff sourceForm: brush; sourceOrigin: 0@0.
	brushToBuff combinationRule: Form blend.
	buffToBuff := BitBlt toForm: buff.  "This is for slewing the buffer"

	[Sensor isMouseButton2Pressed] whileFalse:
		[prevP := nil.
		buffRect := Sensor mousePoint - (buffSize // 2) extent: buff extent.
		dispToBuff copyFrom: buffRect in: Display to: 0@0.
		[Sensor isMouseButton1Pressed] whileTrue:
			["Here is the painting loop"
			p := Sensor mousePoint - (brush extent // 2).
			(prevP == nil or: [prevP ~= p]) ifTrue:
				[prevP == nil ifTrue: [prevP := p].
				(p dist: prevP) > buffSize ifTrue:
					["Stroke too long to fit in buffer -- clip to buffer,
						and next time through will do more of it"
					theta := (p-prevP) theta.
					p := ((theta cos@theta sin) * buffSize asFloat + prevP) truncated].
				brushRect := p extent: brush extent.
				(buffRect containsRect: brushRect) ifFalse:
					["Brush is out of buffer region.  Scroll the buffer,
						and fill vacated regions from the display"
					delta := brushRect amountToTranslateWithin: buffRect.
					buffToBuff copyFrom: buff boundingBox in: buff to: delta.
					newBuffRect := buffRect translatedBy: delta negated.
					newBuffRect
						areasOutside: buffRect
						do: [ :r | dispToBuff copyFrom: r in: Display to: r origin - newBuffRect origin ].
					buffRect := newBuffRect].

				"Interpolate from prevP to p..."
				brushToBuff drawFrom: prevP - buffRect origin
									to: p - buffRect origin
									withFirstPoint: false.

				"Update (only) the altered pixels of the destination"
				updateRect := (p min: prevP) corner: (p max: prevP) + brush extent.
				buffToDisplay copy: updateRect from: updateRect origin - buffRect origin in: buff.
				Display forceToScreen: updateRect.
				prevP := p]]]! !

!BitBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:40:23'!
exampleColorMap	"BitBlt exampleColorMap"
	"This example shows what one can do with the fixed part of a color map. The color map, as setup below, rotates the bits of a pixel all the way around. Thus you'll get a (sometime strange looking ;-) animation of colors which will end up exactly the way it looked at the beginning. The example is given to make you understand that the masks and shifts can be used for a lot more than simply color converting pixels. In this example, for instance, we use only two of the four independent shifters."
	| cc bb |
	cc := ColorMap masks: {
		1 << (Display depth-1). "mask out high bit of color component"
		1 << (Display depth-1) - 1. "mask all other bits"
		0.
		0}
		shifts: {
			1 - Display depth. "shift right to bottom most position"
			1. "shift all other pixels one bit left"
			0.
			0}.
	bb := BitBlt toForm: Display.
	bb 
		sourceForm: Display;
		combinationRule: 3;
		colorMap: cc.
	1 to: Display depth do: [ :i |
		bb copyBits.
		Display forceToScreen.
	]! !


!WarpBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:55:19'!
test1
	"
	Display restoreAfter: [WarpBlt test1]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p ext box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 := Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 := Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp := (self toForm: Display)
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p := Sensor mousePoint.
		pts := {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext := (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		box := (r1 extent*5-ext//2 extent: ext).
		warp copyQuad: pts toRect: box.
		Display forceToScreen: box ]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:55:34'!
test12
	"
	Display restoreAfter: [WarpBlt test12]
	"
	"Just like test1, but comparing smooth to non-smooth warps"
	| warp pts r1 p0 p ext warp2 box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 := Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 := Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp := (self toForm: Display)
		cellSize: 2;  "installs a colormap"
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	warp2 := (self toForm: Display)
		clipRect: ((0@0 extent: r1 extent*5) translatedBy: 250@0);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p := Sensor mousePoint.
		pts := {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext := (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		box := (r1 extent*5-ext//2 extent: ext).
		warp copyQuad: pts toRect: box.
		Display forceToScreen: box.
		box := ((r1 extent*5-ext//2 extent: ext) translatedBy: 250@0).
		warp2 copyQuad: pts toRect: box.
		Display forceToScreen: box.
		]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:55:51'!
test1Mirrored
	"
	Display restoreAfter: [WarpBlt test1Mirrored]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p ext box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 := Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 := Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp := (self toForm: Display)
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p := Sensor mousePoint.
		pts := {r1 topLeft. r1 topRight. r1 bottomRight. r1 bottomLeft}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext := (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		box := (r1 extent*5-ext//2 extent: ext).
		warp copyQuad: pts toRect: box.
		Display forceToScreen: box ]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:56:09'!
test3
	"
	Display restoreAfter: [WarpBlt test3]
	"
	"The Squeak Release Mandala - 9/23/96 di"

	"Move the mouse near the center of the square.
	Up and down affects shrink/grow
	Left and right affect rotation angle"
	| warp pts p0 p box map d t |
	box := 100@100 extent: 300@300.
	Display border: (box expandBy: 2) width: 2.

	"Make a color map that steps through the color space"
	map := (Display depth > 8
		ifTrue: ["RGB is a bit messy..."
				d := Display depth = 16 ifTrue: [5] ifFalse: [8].
				(1 to: 512) collect: [:i | t := i bitAnd: 511.
					((t bitAnd: 16r7) bitShift: d-3)
					+ ((t bitAnd: 16r38) bitShift: d-3*2)
					+ ((t bitAnd: 16r1C0) bitShift: d-3*3)]]
		ifFalse: ["otherwise simple"
				1 to: (1 bitShift: Display depth)])
			as: Bitmap.
	warp := (WarpBlt toForm: Display)
		clipRect: box;
		sourceForm: Display;
		colorMap: map;
		combinationRule: Form over.
	p0 := box center.
	[Sensor isAnyButtonPressed] whileFalse:
		[p := Sensor mousePoint.
		pts := (box insetBy: p y - p0 y) innerCorners
			collect: [:pt | pt inverseRotatedBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box.
		Display forceToScreen: box ]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:56:29'!
test4
	"
	Display restoreAfter: [WarpBlt test4]
	"
	"The Squeak Release Mandala - 9/23/96 di
	This version does smoothing"

	"Move the mouse near the center ofhe square.
	Up and dn affects shrink/grow
	Left and right affect rotation angle"
	| warp pts p0 p box |
	box := 100@100 extent: 300@300.
	Display border: (box expandBy: 2) width: 2.

	warp := (WarpBlt toForm: Display)
		clipRect: box;
		sourceForm: Display;
		cellSize: 2;  "installs a colormap"
		combinationRule: Form over.
	p0 := box center.
	[Sensor isAnyButtonPressed] whileFalse:
		[p := Sensor mousePoint.
		pts := (box insetBy: p y - p0 y) innerCorners
			collect: [:pt | pt inverseRotatedBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box.
		Display forceToScreen: box ]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:56:44'!
test5
	"
	Display restoreAfter: [WarpBlt test5]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 := Rectangle fromUser.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 := Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp := (self toForm: Display)
		cellSize: 1;
		sourceForm: Display;
		cellSize: 2;  "installs a colormap"
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p := Sensor mousePoint.
		pts := {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		box := (r1 translatedBy: r1 width@0).
		warp copyQuad: pts toRect: box.
		Display forceToScreen: box ]
! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:57:02'!
testSourceDestOverlap
	"Show that if source and destination are the same, then source quad and dest rect should not overlap,
	or data will be lost.
	WarpBlt testSourceDestOverlap
	"
	| warp box | 
	warp := (WarpBlt toForm: Display)
		sourceForm: Display;
		combinationRule: Form over.
	box := (0@0 extent: 160@160).
	warp copyQuad: {0@0. 160@0. 160@160. 0@160} toRect: box.
	Display forceToScreen: box.! !


!CharacterSequence methodsFor: 'displaying' stamp: 'jmv 6/26/2025 16:46:27'!
displayAt: aPoint 
	"Display the receiver as a DisplayText at aPoint on the display screen."

	self displayOn: Display at: aPoint.
	Display forceToScreen.! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 6/26/2025 16:33:22'!
endEntry
	| c d cb |
	c := self contents.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame := frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d := textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb := textComposition characterBlockAtPoint:
			`0@0` + (0@(d+FontFamily defaultLineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit := position := collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		drawTextComposition: textComposition
		at: `4@4` + frame topLeft
		extent: Display extent
		color: `Color black`
		selectionColor: `Color blue`.
	Display forceToScreen.! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/26/2025 16:35:38'!
displayUnfinishedEntry
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayUnfinishedEntryOnDisplay ifNotNil: [ :damage |
			Display forceToScreen: damage ]]! !

