'From Cuis 4.2 of 25 July 2013 [latest update: #2435] on 28 July 2015 at 8:16:52.647533 am'!

!BitBlt class methodsFor: 'examples' stamp: 'jmv 7/27/2015 17:07'!
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
		mapDto32 _ Color cachedColormapFrom: Display depth to: 32.
		map32toD _ Color cachedColormapFrom: 32 to: Display depth].

	"display 10 different alphas, across top of screen"
	buff _ Form extent: 500@50 depth: 32.
	dispToBuff _ BitBlt toForm: buff.
	dispToBuff colorMap: mapDto32.
	dispToBuff copyFrom: (50@10 extent: 500@50) in: Display to: 0@0.
	1 to: 10 do: [:i | dispToBuff fill: (50*(i-1)@0 extent: 50@50)
						fillColor: (Color red alpha: i/10)
						rule: Form blend].
	buffToDisplay _ BitBlt toForm: Display.
	buffToDisplay colorMap: map32toD.
	buffToDisplay copyFrom: buff boundingBox in: buff to: 50@10.
	DisplayScreen screenUpdateRequired: nil.

	"Create a brush with radially varying alpha"
	brush _ Form extent: 30@30 depth: 32.
	1 to: 5 do: 
		[:i | brush fillShape: (Form dotOfSize: brush width*(6-i)//5)
				fillColor: (Color red alpha: 0.02 * i - 0.01)
				at: brush extent // 2].

	"Now paint with the brush using alpha blending."
	buffSize _ 100.
	buff _ Form extent: brush extent + buffSize depth: 32.  "Travelling 32-bit buffer"
	dispToBuff _ BitBlt toForm: buff.  "This is from Display to buff"
	dispToBuff colorMap: mapDto32.
	brushToBuff _ BitBlt toForm: buff.  "This is from brush to buff"
	brushToBuff sourceForm: brush; sourceOrigin: 0@0.
	brushToBuff combinationRule: Form blend.
	buffToBuff _ BitBlt toForm: buff.  "This is for slewing the buffer"

	[Sensor isMouseButton2Pressed] whileFalse:
		[prevP _ nil.
		buffRect _ Sensor mousePoint - (buffSize // 2) extent: buff extent.
		dispToBuff copyFrom: buffRect in: Display to: 0@0.
		[Sensor isMouseButton1Pressed] whileTrue:
			["Here is the painting loop"
			p _ Sensor mousePoint - (brush extent // 2).
			(prevP == nil or: [prevP ~= p]) ifTrue:
				[prevP == nil ifTrue: [prevP _ p].
				(p dist: prevP) > buffSize ifTrue:
					["Stroke too long to fit in buffer -- clip to buffer,
						and next time through will do more of it"
					theta _ (p-prevP) theta.
					p _ ((theta cos@theta sin) * buffSize asFloat + prevP) truncated].
				brushRect _ p extent: brush extent.
				(buffRect containsRect: brushRect) ifFalse:
					["Brush is out of buffer region.  Scroll the buffer,
						and fill vacated regions from the display"
					delta _ brushRect amountToTranslateWithin: buffRect.
					buffToBuff copyFrom: buff boundingBox in: buff to: delta.
					newBuffRect _ buffRect translatedBy: delta negated.
					newBuffRect
						areasOutside: buffRect
						do: [ :r | dispToBuff copyFrom: r in: Display to: r origin - newBuffRect origin ].
					buffRect _ newBuffRect].

				"Interpolate from prevP to p..."
				brushToBuff drawFrom: prevP - buffRect origin
									to: p - buffRect origin
									withFirstPoint: false.

				"Update (only) the altered pixels of the destination"
				updateRect _ (p min: prevP) corner: (p max: prevP) + brush extent.
				buffToDisplay copy: updateRect from: updateRect origin - buffRect origin in: buff.
				DisplayScreen screenUpdateRequired: updateRect.
				prevP _ p]]]! !


!WarpBlt class methodsFor: 'examples' stamp: 'jmv 7/28/2015 08:07'!
test1
	"
	Display restoreAfter: [WarpBlt test1]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p ext box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		box _ (r1 extent*5-ext//2 extent: ext).
		warp copyQuad: pts toRect: box.
		DisplayScreen screenUpdateRequired: box ]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 7/28/2015 08:08'!
test12
	"
	Display restoreAfter: [WarpBlt test12]
	"
	"Just like test1, but comparing smooth to non-smooth warps"
	| warp pts r1 p0 p ext warp2 box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		cellSize: 2;  "installs a colormap"
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	warp2 _ (self toForm: Display)
		clipRect: ((0@0 extent: r1 extent*5) translatedBy: 250@0);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		box _ (r1 extent*5-ext//2 extent: ext).
		warp copyQuad: pts toRect: box.
		DisplayScreen screenUpdateRequired: box.
		box _ ((r1 extent*5-ext//2 extent: ext) translatedBy: 250@0).
		warp2 copyQuad: pts toRect: box.
		DisplayScreen screenUpdateRequired: box.
		]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 7/28/2015 08:09'!
test1Mirrored
	"
	Display restoreAfter: [WarpBlt test1Mirrored]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p ext box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 topRight. r1 bottomRight. r1 bottomLeft}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		box _ (r1 extent*5-ext//2 extent: ext).
		warp copyQuad: pts toRect: box.
		DisplayScreen screenUpdateRequired: box ]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 7/27/2015 17:10'!
test3
	"
	Display restoreAfter: [WarpBlt test3]
	"
	"The Squeak Release Mandala - 9/23/96 di"

	"Move the mouse near the center of the square.
	Up and down affects shrink/grow
	Left and right affect rotation angle"
	| warp pts p0 p box map d t |
	box _ 100@100 extent: 300@300.
	Display border: (box expandBy: 2) width: 2.

	"Make a color map that steps through the color space"
	map _ (Display depth > 8
		ifTrue: ["RGB is a bit messy..."
				d _ Display depth = 16 ifTrue: [5] ifFalse: [8].
				(1 to: 512) collect: [:i | t _ i bitAnd: 511.
					((t bitAnd: 16r7) bitShift: d-3)
					+ ((t bitAnd: 16r38) bitShift: d-3*2)
					+ ((t bitAnd: 16r1C0) bitShift: d-3*3)]]
		ifFalse: ["otherwise simple"
				1 to: (1 bitShift: Display depth)])
			as: Bitmap.
	warp _ (WarpBlt toForm: Display)
		clipRect: box;
		sourceForm: Display;
		colorMap: map;
		combinationRule: Form over.
	p0 _ box center.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ (box insetBy: p y - p0 y) innerCorners
			collect: [:pt | pt inverseRotatedBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box.
		DisplayScreen screenUpdateRequired: box]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 7/27/2015 17:11'!
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
	box _ 100@100 extent: 300@300.
	Display border: (box expandBy: 2) width: 2.

	warp _ (WarpBlt toForm: Display)
		clipRect: box;
		sourceForm: Display;
		cellSize: 2;  "installs a colormap"
		combinationRule: Form over.
	p0 _ box center.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ (box insetBy: p y - p0 y) innerCorners
			collect: [:pt | pt inverseRotatedBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box.
		DisplayScreen screenUpdateRequired: box]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 7/27/2015 17:11'!
test5
	"
	Display restoreAfter: [WarpBlt test5]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p box |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle fromUser.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		cellSize: 1;
		sourceForm: Display;
		cellSize: 2;  "installs a colormap"
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		box _ (r1 translatedBy: r1 width@0).
		warp copyQuad: pts toRect: box.
		DisplayScreen screenUpdateRequired: box ]
! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 7/27/2015 17:12'!
testSourceDestOverlap
	"Show that if source and destination are the same, then source quad and dest rect should not overlap,
	or data will be lost.
	WarpBlt testSourceDestOverlap
	"
	| warp box | 
	warp _ (WarpBlt toForm: Display)
		sourceForm: Display;
		combinationRule: Form over.
	box _ (0@0 extent: 160@160).
	warp copyQuad: {0@0. 160@0. 160@160. 0@160} toRect: box.
	DisplayScreen screenUpdateRequired: box! !


!String methodsFor: 'displaying' stamp: 'jmv 7/27/2015 17:13'!
displayAt: aPoint 
	"Display the receiver as a DisplayText at aPoint on the display screen."

	self displayOn: Display at: aPoint.
	DisplayScreen screenUpdateRequired: nil! !

