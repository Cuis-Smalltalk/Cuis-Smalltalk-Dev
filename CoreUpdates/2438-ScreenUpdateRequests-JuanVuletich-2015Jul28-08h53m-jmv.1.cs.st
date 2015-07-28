'From Cuis 4.2 of 25 July 2013 [latest update: #2437] on 28 July 2015 at 8:53:46.373717 am'!

!BitBlt class methodsFor: 'examples' stamp: 'jmv 7/28/2015 08:29'!
exampleColorMap	"BitBlt exampleColorMap"
	"This example shows what one can do with the fixed part of a color map. The color map, as setup below, rotates the bits of a pixel all the way around. Thus you'll get a (sometime strange looking ;-) animation of colors which will end up exactly the way it looked at the beginning. The example is given to make you understand that the masks and shifts can be used for a lot more than simply color converting pixels. In this example, for instance, we use only two of the four independent shifters."
	| cc bb |
	cc _ ColorMap masks: {
		1 << (Display depth-1). "mask out high bit of color component"
		1 << (Display depth-1) - 1. "mask all other bits"
		0.
		0}
		shifts: {
			1 - Display depth. "shift right to bottom most position"
			1. "shift all other pixels one bit left"
			0.
			0}.
	bb _ BitBlt toForm: Display.
	bb 
		sourceForm: Display;
		combinationRule: 3;
		colorMap: cc.
	1 to: Display depth do: [ :i |
		bb copyBits.
		DisplayScreen screenUpdateRequired: nil.
	]! !


!Color class methodsFor: 'examples' stamp: 'jmv 7/28/2015 08:29'!
experimentsTowarsANewColorPalette
"
self experimentsTowarsANewColorPalette
"
| selectedHue selectedSaturation selectedV selectedColor h s v color width height selectedChroma selectedLuminance |
width _ 300.
height _ 120.
selectedColor _ Color random.
selectedHue _ selectedColor hue.
selectedSaturation _ selectedColor saturation.
selectedChroma _ selectedColor chroma.
selectedV _ selectedColor brightness.
selectedLuminance _ selectedColor luminance.
Display getCanvas fillRectangle: (0@0 extent: height@height) color: selectedColor.
0 to: height do: [ :y |
	v _ 1.0 - (y / height).
	0 to: height do: [ :x |
		s _ x / height.
		color _ Color basicNew setHue: selectedHue saturation: s brightness: v.
		Display colorAt: x@(y+height) put: color
	].
	DisplayScreen screenUpdateRequired: nil
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
	DisplayScreen screenUpdateRequired: nil
].! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 7/28/2015 08:38'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth |
	f _ AbstractFont default.
	h _ f height * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).

	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ 0@0 extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + (0@2).
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ nil.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		(oldFilledWidth isNil or: [ filledWidth  > oldFilledWidth ]) ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + (2@2) extent: filledWidth@17).
			oldFilledWidth _ filledWidth ]].
	"Not pretty at all!!"
	WorldState addDeferredUIMessage: [
		self runningWorld ifNotNil: [ :w |
			w invalidateDisplayRect: outerFrame from: nil ]].
	self resume: result! !


!Form methodsFor: 'transitions' stamp: 'jmv 7/28/2015 08:32'!
pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d |
	pageRect _ otherImage boundingBox.
	oldPage _ self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: Color black.
	oldBottom _ self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps _ 8.
	buffer _ Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d _ pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect _ pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad _ Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp _ (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceToScreen.
		].

	buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceToScreen.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"
! !

!Form methodsFor: 'transitions' stamp: 'jmv 7/28/2015 08:32'!
slideImage: otherImage at: topLeft delta: delta
	"
	Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 3@-4
	"
	| bb nSteps clipRect |
	bb _ otherImage boundingBox.
	clipRect _ topLeft extent: otherImage extent.
	nSteps _ 1.
	delta x = 0 ifFalse: [nSteps _ nSteps max: (bb width//delta x abs) + 1].
	delta y = 0 ifFalse: [nSteps _ nSteps max: (bb height//delta y abs) + 1].
	1 to: nSteps do: [ :i | 
			self 
				copyBits: bb from: otherImage
				at: delta*(i-nSteps) + topLeft
				clippingBox: clipRect rule: Form paint.
			Display forceToScreen.
		]! !

!Form methodsFor: 'transitions' stamp: 'jmv 7/28/2015 08:32'!
wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectsOrNil waitTime |
	i _ 0.
	clipRect _ topLeft extent: otherImage extent.
	clipBox ifNotNil: [ clipRect _ clipRect intersect: clipBox ].
	[ rectsOrNil _ rectForIndexBlock value: (i _ i + 1) ]
		whileNotNil: [
			t _ Time localMillisecondClock.
			rectsOrNil do: [ :r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over ].
			Display forceToScreen.
			waitTime _ 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]]! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 7/28/2015 08:23'!
fakeSphereAt: aPoint diameter: diameter color: aColor
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb _ GrafPort toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect _ aPoint extent: diameter.
	bb fillOval: rect.
	p _ (diameter*2//5) asPoint + aPoint..
	#(2 3 4 5 6) do: [ :i |  "simulate facade by circles of gray"
		bb fillColor: (Color white alpha: 0.3).
		d _ i*diameter//10.
		rect _ Rectangle center: p extent: d.
		bb fillOval: rect ].
	DisplayScreen screenUpdateRequired: nil
"
	Form fakeSphereAt: 50@50 diameter: 30 color:  (Color red alpha: 0.5)
"! !


!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 7/28/2015 08:35'!
flash: aRectangle 
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle.
	self forceToScreen: aRectangle! !


!BMPReadWriter class methodsFor: 'testing' stamp: 'jmv 7/28/2015 08:28'!
displayAllFrom: fd
	"BMPReadWriter displayAllFrom: FileDirectory smalltalkImageDirectory"
	fd fileNames do: [ :fName |
		(fName endsWith: '.bmp') ifTrue:[
			[
				(Form fromBinaryStream: (fd readOnlyFileNamed: fName) binary) display.
			] on: Error do:[:nix|].
		].
	].
	fd directoryNames do: [ :fdName |
		self displayAllFrom: (fd directoryNamed: fdName)
	]! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 7/28/2015 08:25'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: 0@0 ]
		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [ aCanvas drawsOnDisplay ] and: [ color class == TranslucentColor ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 7/28/2015 08:33'!
fillRects: rectangleList color: aColor
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		fillColor: aColor;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 150) wait! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 7/28/2015 08:34'!
flashRects: rectangleList color: aColor
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."
	"Details: Uses two reverses so that the display is restored to its original state. This is necessary when in deferred update mode."

	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		fillColor: aColor;
		combinationRule: Form reverse.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 250) wait.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ]! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 7/28/2015 08:26'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			0@0 + (0@(d+AbstractFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue.
	DisplayScreen screenUpdateRequired: nil! !

