'From Cuis 4.1 of 12 December 2012 [latest update: #1555] on 14 January 2013 at 9:21:03 pm'!

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:11'!
isAnyButtonPressed
	"Answer whether at least one mouse button is currently being pressed."

	^ self primMouseButtons anyMask: InputSensor anyMouseButton
! !

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:15'!
isMouseButton1Pressed
	"Answer true if only the mouseButton1 is being pressed.
	This is the first mouse button, usually the one at the left."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton1! !

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:15'!
isMouseButton2Pressed
	"Answer whether only the mouseButton2 is being pressed. 
	This is the usually the right mouse button or option+click on the Mac."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton2! !

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:16'!
isMouseButton3Pressed
	"Answer whether only the mouseButton3 is being pressed. 
	This is the third mouse button, usually the wheel or button at the center, or cmd+click on the Mac."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton3! !


!AbstractSound class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:11'!
testFMInteractively
	"Experiment with different settings of the FM modulation and multiplier settings interactively by moving the mouse. The top-left corner of the screen is 0 for both parameters. Stop when the mouse is pressed."
	"AbstractSound testFMInteractively"

	| s mousePt lastVal status mod ratio |
	SoundPlayer startPlayerProcessBufferSize: 1100 rate: 11025 stereo: false.
	s _ FMSound pitch: 440.0 dur: 200.0 loudness: 0.2.

	SoundPlayer playSound: s.
	lastVal _ nil.
	[Sensor isAnyButtonPressed] whileFalse: [
		mousePt _ Sensor mousePoint.
		mousePt ~= lastVal ifTrue: [
			mod _ mousePt x asFloat / 20.0.
			ratio _ mousePt y asFloat / 20.0.
			s modulation: mod ratio: ratio.
			lastVal _ mousePt.
			status _
'mod: ', mod printString, '
ratio: ', ratio printString.
			status displayOn: Display at: 10@10]].

	SoundPlayer shutDown.
! !


!BitBlt class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:17'!
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
				prevP _ p]]]! !

!BitBlt class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:11'!
exampleTwo
	"This is to test painting with a gray tone. It also tests that the seaming with gray patterns is correct in the microcode. Lets you paint for a while and then automatically stops. This only works at Depth of 1."
	| f aBitBlt displayDepth |
	"create a small black Form source as a brush. "
	displayDepth _ Display depth.
	Display newDepth: 1.
	f _ Form extent: 20 @ 20.
	f fillBlack.
	"create a BitBlt which will OR gray into the display. "
	aBitBlt _ BitBlt
		destForm: Display
		sourceForm: f
		fillColor: Color gray
		combinationRule: Form over
		destOrigin: Sensor mousePoint
		sourceOrigin: 0 @ 0
		extent: f extent
		clipRect: Display computeBoundingBox.
	"paint the gray Form on the screen for a while. "
	[Sensor isAnyButtonPressed] whileFalse: 
		[aBitBlt destOrigin: Sensor mousePoint.
		aBitBlt copyBits].
	Display newDepth: displayDepth.
	"BitBlt exampleTwo"! !


!Color class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:12'!
showHuesInteractively
	"Shows a palette of hues at a (saturation, brightness) point determined by the mouse position. Click the mouse button to exit and return the selected (saturation, brightness) point."
	"Color showHuesInteractively"

	| p s v |
	[Sensor isAnyButtonPressed] whileFalse: [
		p _ Sensor mousePoint.
		s _ p x asFloat / 300.0.
		v _ p y asFloat / 300.0.
		self showColors: (self wheel: 12 saturation: s brightness: v)].
	^ (s min: 1.0) @ (v min: 1.0)! !

!Color class methodsFor: 'color from user' stamp: 'jmv 1/14/2013 21:11'!
fromUser
	"Displays a color palette of colors, waits for a mouse click, and returns the selected color. Any pixel on the Display can be chosen, not just those in the color palette."
	"Note: Since the color chart is cached, you may need to do 'ColorChart _ nil' after changing the oldColorPaletteForDepth:extent: method."
	"Color fromUser"

	| d startPt save tr oldColor c here s |
	d _ Display depth.
	((ColorChart == nil) or: [ColorChart depth ~= Display depth]) 
		ifTrue: [ColorChart _ self oldColorPaletteForDepth: d extent: (2 * 144)@80].
	Sensor mousePoint y < Display center y 
		ifTrue: [startPt _ 0@(Display boundingBox bottom - ColorChart height)]
		ifFalse: [startPt _ 0@0].

	save _ Form fromDisplay: (startPt extent: ColorChart extent).
	ColorChart displayAt: startPt.
	tr _ ColorChart extent - (50@19) corner: ColorChart extent.
	tr _ tr translatedBy: startPt.

	oldColor _ nil.
	[Sensor isAnyButtonPressed] whileFalse: [
		c _ Display colorAt: (here _ Sensor mousePoint).
		(tr containsPoint: here)
			ifFalse: [Display fill: (0@61+startPt extent: 20@19) fillColor: c]
			ifTrue: [
				c _ Color transparent.
				Display fill: (0@61+startPt extent: 20@19) fillColor: Color white].
		c = oldColor ifFalse: [
			Display fillWhite: (20@61 + startPt extent: 135@19).
			c isTransparent
				ifTrue: [s _ 'transparent']
				ifFalse: [s _ c shortPrintString.
						s _ s copyFrom: 7 to: s size - 1].
			s displayAt: 20@61 + startPt.
			oldColor _ c]].
	save displayAt: startPt.
	Sensor waitNoButton.
	^ c
! !


!ContextPart class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:12'!
trace: aBlock on: aStream		"ContextPart trace: [3 factorial]"
	"This method uses the simulator to print calls to a file."
	| prev |
	prev _ aBlock.
	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :current |
			Sensor isAnyButtonPressed ifTrue: [^ nil].
			current == prev
				ifFalse: [
					prev sender ifNil: [
						aStream space; nextPut: $^.
						self carefullyPrint: current top on: aStream].
					aStream newLine.
					(current depthBelow: aBlock) timesRepeat: [aStream space].
					self carefullyPrint: current receiver on: aStream.
					aStream space; nextPutAll: current selector.
					prev _ current]]! !


!FWT methodsFor: 'testing' stamp: 'jmv 1/14/2013 21:12'!
viewPhiAndPsi  "(FWT new nSamples: 256 nLevels: 6) viewPhiAndPsi"
	"View the scaling function and mother wavelets for this transform"
	| p |
	Display fillWhite: (0@0 extent: 300@300).
	Display border: (0@0 extent: 300@300) width: 2.
	[Sensor isAnyButtonPressed] whileFalse:
		["Move mouse around in the outer rectangle to explore"
		p _ Sensor mousePoint min: 300@300.
		self setAlpha: (p x - 150) / 150.0 * Float pi
				beta: (p y - 150) / 150.0 * Float pi.
		'alpha=', (alpha roundTo: 0.01) printString, '   ',
			'beta=', (beta roundTo: 0.01) printString, '    ' displayAt: 50@5.
		transform do: [:w | w atAllPut: 0.0].
		(transform at: transform size - 1) at: (nSamples>>nLevels) put: 1.0.
		self transformForward: false.
		FFT new plot: (samples copyFrom: 1 to: nSamples) in: (20@30 extent: nSamples@100).

		transform do: [:w | w atAllPut: 0.0].
		(transform at: transform size) at: (nSamples>>nLevels) put: 1.0.
		self transformForward: false.
		FFT new plot: (samples copyFrom: 1 to: nSamples) in: (20@170 extent: nSamples@100)].
	Sensor waitNoButton! !


!Form class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:17'!
exampleSketch
	"This is a simple drawing algorithm to get a sketch on the display screen.
	Draws whenever mouse button down.  Ends with option-click."
	| aPen color |
	aPen _ Pen new.
	color _ 0.
	[Sensor isMouseButton2Pressed]
		whileFalse:
		[aPen place: Sensor mousePoint; color: (color _ color + 1).
		[Sensor isMouseButton1Pressed]
			whileTrue: [aPen goto: Sensor mousePoint]].
	Sensor waitNoButton.

	"Form exampleSketch"! !

!Form class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:18'!
toothpaste: diam		"Display restoreAfter: [Form toothpaste: 30]"
	"Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| facade ball filter point queue port color q colors colr colr2 |
	colors _ Display depth = 1
		ifTrue: [Array with: Color black]
		ifFalse: [Color red wheel: 12].
	facade _ Form extent: diam@diam offset: (diam//-2) asPoint.
	(Form dotOfSize: diam) displayOn: facade
			at: (diam//2) asPoint clippingBox: facade boundingBox
			rule: Form under fillColor: Color white.
	#(1 2 3) do:
		[:x |  "simulate facade by circles of gray"
		(Form dotOfSize: x*diam//5) displayOn: facade
			at: (diam*2//5) asPoint clippingBox: facade boundingBox
			rule: Form under
			fillColor: (Color perform: 
					(#(black gray lightGray) at: x)).
		"facade displayAt: 50*x@50"].
	ball _ Form dotOfSize: diam.
	color _ 8.
	[ true ] whileTrue:
		[port _ BitBlt toForm: Display.
		"Expand 1-bit forms to any pixel depth"
		port colorMap: (Bitmap with: 0 with: 16rFFFFFFFF).
		queue _ OrderedCollection new: 32.
		16 timesRepeat: [queue addLast: -20@-20].
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		filter _ Sensor mousePoint.
		colr _ colors atWrap: (color _ color + 5).  "choose increment relatively prime to colors size"
		colr2 _ colr alphaMixed: 0.3 with: Color white.
		[Sensor isMouseButton1Pressed or: [queue size > 0]] whileTrue:
			[filter _ filter * 4 + Sensor mousePoint // 5.
			point _ Sensor isMouseButton1Pressed
				ifTrue: [filter] ifFalse: [-20@-20].
			port copyForm: ball to: point rule: Form paint fillColor: colr.
			(q _ queue removeFirst) ifNil: [^ self].	"exit"
			Display depth = 1
				ifTrue: [port copyForm: facade to: q rule: Form erase]
				ifFalse: [port copyForm: facade to: q rule: Form paint fillColor: colr2].
			Sensor isMouseButton1Pressed ifTrue: [queue addLast: point]]].
! !

!Form class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:17'!
xorHack: size  "Display restoreAfter: [Form xorHack: 256]"
	"Draw a smiley face or stick figure, and end with option-click.
	Thereafter image gets 'processed' as long as you have button down.
	If you stop at just the right time, you'll see you figure upside down,
	and at the end of a full cycle, you'll see it perfectly restored.
	Dude -- this works in color too!!"
	| rect form i bb |
	rect _ 5@5 extent: size@size.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	Display border: (rect topRight - (0@2) extent: rect extent*2 + 4) width: 2.
	Form exampleSketch.
	form _ Form fromDisplay: rect.
	bb _ form boundingBox.
	i _ 0.
	[Sensor isMouseButton2Pressed] whileFalse:
		[[Sensor isMouseButton1Pressed] whileTrue:
			[i _ i + 1.
			(Array with: 0@1 with: 0@-1 with: 1@0 with: -1@0) do:
				[:d | form copyBits: bb from: form at: d
					clippingBox: bb rule: Form reverse fillColor: nil].
			form displayAt: rect topLeft.
			i+2\\size < 4 ifTrue: [(Delay forMilliseconds: 300) wait]].
		(form magnify: form boundingBox by: 2@2) displayAt: rect topRight + (2@0).
		Sensor waitButton].! !


!InputSensor methodsFor: 'joystick' stamp: 'jmv 1/14/2013 21:12'!
testJoystick: index
	"Sensor testJoystick: 3"

	| f pt buttons status |
	f _ Form extent: 110@50.
	[Sensor isAnyButtonPressed] whileFalse: [
		pt _ Sensor joystickXY: index.
		buttons _ Sensor joystickButtons: index.
		status _
'xy: ', pt printString, '
buttons: ', buttons hex.
		f fillWhite.
		status displayOn: f at: 10@10.
		f displayOn: Display at: 10@10.
	].
! !

!InputSensor methodsFor: 'keyboard' stamp: 'jmv 1/14/2013 21:19'!
kbdTest
	"
	Sensor kbdTest
	"
	"This test routine will print the unmodified character, its keycode,
	and the OR of all its modifier bits, until the character x is typed

Also useful:
	Sensor test
	"
	| char |
	char _ nil.
	[ char = $x ] whileFalse: [
		[ self keyboardPressed ] whileFalse.
		char _ self characterForKeycode: self keyboard.
		Display fill: (5@5 extent: 400@20) fillColor: Color white.
		(String streamContents: [ :s |
			s
				nextPut: $-;
				nextPut: char;
				nextPut: $-;
				space;
				print: char asciiValue;
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ]])
		displayAt: 10 @ 10 ].! !

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:12'!
noButtonPressed
	"Answer whether any mouse button is not being pressed."

	^self isAnyButtonPressed not
! !

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:12'!
waitButton
	"Wait for the user to press any mouse button and then answer with the 
	current location of the cursor."

	| delay |
	delay _ Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ] whileFalse: [ delay wait ].
	^self mousePoint
! !

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:12'!
waitButtonOrKeyboard
	"Wait for the user to press either any mouse button or any key. 
	Answer the current cursor location or nil if a keypress occured."

	| delay |
	delay := Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ]
		whileFalse: [
			delay wait.
			self keyboardPressed
				ifTrue: [^ nil]].
	^ self mousePoint
! !

!InputSensor methodsFor: 'mouse' stamp: 'jmv 1/14/2013 21:13'!
waitNoButton
	"Wait for the user to release any mouse button and then answer the current location of the cursor."

	| delay |
	delay _ Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ] whileTrue: [ delay wait].
	^self mousePoint
! !


!MIDIInputParser methodsFor: 'real-time processing' stamp: 'jmv 1/14/2013 21:13'!
midiDoUntilMouseDown: midiActionBlock
	"Process the incoming MIDI stream in real time by calling midiActionBlock for each MIDI event. This block takes three arguments: the MIDI command byte and two argument bytes. One or both argument bytes may be nil, depending on the MIDI command. If not nil, evaluatue idleBlock regularly whether MIDI data is available or not. Pressing any mouse button terminates the interaction."

	| time cmd arg1 arg2 |
	self clearBuffers.
	[ Sensor isAnyButtonPressed ] whileFalse: [
		self midiDo: [ :item |
			time _ item at: 1.
			cmd _ item at: 2.
			arg1 _ arg2 _ nil.
			item size > 2 ifTrue: [
				arg1 _ item at: 3.
				item size > 3 ifTrue: [arg2 _ item at: 4]].
				midiActionBlock value: cmd value: arg1 value: arg2]].
! !


!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jmv 1/14/2013 21:13'!
processMIDIUntilMouseDown
	"Used for debugging. Do MIDI processing until the mouse is pressed."

	midiParser clearBuffers.
	[ Sensor isAnyButtonPressed ] whileFalse: [ self processMIDI ]! !


!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 1/14/2013 21:09'!
mouseButton1Changed
	"Answer true if the mouseButton1 has changed.
	This is usually the left mouse button.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	"The Mac VM converts command-button1 to command-button3.
	If the preference is not set, convert this back to command-button1.
	In this way, it is the image and not the VM who's on control of the meaning of command-button1.
	(This logic is repeated in #mouseButton1Pressed, #mouseButton3Pressed, #mouseButton1Changed, #mouseButton3Changed."
	
	(self commandAltKeyPressed
		and: [ Preferences commandClickOpensHalo not
			and: [ Smalltalk platformName = 'Mac OS' ]])
		ifTrue: [ ^ whichButton anyMask: InputSensor mouseButton3 ].
	^ whichButton anyMask: InputSensor mouseButton1! !

!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 1/14/2013 21:09'!
mouseButton2Changed
	"Answer true if the mouseButton2 has changed.
	This is the usually the right mouse button or option+click on the Mac.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	^ whichButton anyMask: InputSensor mouseButton2! !

!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 1/14/2013 21:09'!
mouseButton3Changed
	"Answer true if the mouseButton3 has changed.
	This is usually the center (wheel) mouse button or cmd+click on the Mac.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	"The Mac VM converts command-button1 to command-button3.
	If the preference is not set, convert this back to command-button1.
	In this way, it is the image and not the VM who's on control of the meaning of command-button1.
	(This logic is repeated in #mouseButton1Pressed, #mouseButton3Pressed, #mouseButton1Changed, #mouseButton3Changed."

	(self commandAltKeyPressed
		and: [ Preferences commandClickOpensHalo not
			and: [ Smalltalk platformName = 'Mac OS' ]])
		ifTrue: [ ^false ].
	^ whichButton anyMask: InputSensor mouseButton3! !


!Pen methodsFor: 'geometric designs' stamp: 'jmv 1/14/2013 21:17'!
web   "Display restoreAfter: [Pen new web]"
	"Draw pretty web-like patterns from the mouse movement on the screen.
	Press the mouse button to draw, option-click to exit.
	By Dan Ingalls and Mark Lentczner. "
	| history newPoint ancientPoint lastPoint filter color |
	"self erase."
	color _ 1.
	[ true ] whileTrue:
		[ history _ OrderedCollection new.
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		filter _ lastPoint _ Sensor mousePoint.
		20 timesRepeat: [ history addLast: lastPoint ].
		self color: (color _ color + 1).
		[ Sensor isMouseButton1Pressed ] whileTrue: 
			[ newPoint _ Sensor mousePoint.
			(newPoint = lastPoint) ifFalse:
				[ ancientPoint _ history removeFirst.
				filter _ filter * 4 + newPoint // 5.
				self place: filter.
				self goto: ancientPoint.
				lastPoint _ newPoint.
				history addLast: filter ] ] ]! !


!Pen class methodsFor: 'tablet drawing examples' stamp: 'jmv 1/14/2013 21:14'!
inkBrush
	"Similar to simplePressurePen, but this example uses the average of the recent pen pressure values. The effect is that of a Japanese ink brush that comes up gradually off the paper as the brush is lifted, causing end (and beginning) of each stroke to taper. Requires the optional tablet support primitives which may not be supported on all platforms. Works best in full screen mode. Shift-mouse to exit." 
	"Pen inkBrush"

	| tabletScale historyMSecs pressureHistory pen now currentPressure sum averagePressure p |
	tabletScale _ self tabletScaleFactor.
	historyMSecs _ 120.
	pressureHistory _ OrderedCollection new.
	pen _ Pen newOnForm: Display.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [
		[Sensor shiftPressed and: [Sensor isAnyButtonPressed]] whileFalse: [
			"compute the average pressure over last historyMSecs milliseconds"
			now _ Time millisecondClockValue.
			currentPressure _ (20.0 * Sensor tabletPressure) rounded.
			pressureHistory addLast: (Array with: now with: currentPressure).
			[pressureHistory size > 0 and:
			 [(pressureHistory first first + historyMSecs) < now]]
				whileTrue: [pressureHistory removeFirst].  "prune old entries"
			sum _ pressureHistory inject: 0 into: [:t :e | t + e last].
			averagePressure _ sum // pressureHistory size.

			p _ (Sensor tabletPoint * tabletScale) rounded.
		     averagePressure > 0
				ifTrue: [
					pen roundNib: averagePressure.
					pen goto: p]
				ifFalse: [
					pen place: p]]].
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'jmv 1/14/2013 21:14'!
simplePressurePen
	"An example of using a pressure sensitive pen to control the thickness of the pen. This requires the optional tablet support primitives which may not be supported on all platforms. Works best in full screen mode. Shift-mouse to exit." 
	"Pen simplePressurePen"

	| tabletScale pen pressure p |
	tabletScale _ self tabletScaleFactor.
	pen _ Pen newOnForm: Display.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [
		[Sensor shiftPressed and: [Sensor isAnyButtonPressed]] whileFalse: [
			p _ (Sensor tabletPoint * tabletScale) rounded.
			pressure _ (15.0 * Sensor tabletPressure) rounded.
		     pressure > 0
				ifTrue: [
					pen roundNib: pressure.
					pen goto: p]
				ifFalse: [
					pen place: p]]].
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'jmv 1/14/2013 21:14'!
testMouseTracking
	"A very simple example of drawing using the mouse. Compare the tracking speed of this example with that of testTabletTracking. Mouse down to draw a stroke, shift-mouse to exit." 
	"Pen testMouseTracking"

	| pen p |
	pen _ Pen newOnForm: Display.
	pen roundNib: 8.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [
		[Sensor shiftPressed and: [Sensor isAnyButtonPressed]] whileFalse: [
			p _ Sensor mousePoint.
		     Sensor isAnyButtonPressed
				ifTrue: [pen goto: p]
				ifFalse: [
					pen color: Color random.
					pen place: p]]].
! !

!Pen class methodsFor: 'tablet drawing examples' stamp: 'jmv 1/14/2013 21:14'!
testTabletTracking
	"A very simple example of drawing using the pen of a digitizing tablet such as a Wacom ArtZ tablet. This requires the optional tablet support primitives which may not be supported on all platforms. Compare the tracking speed of this example with that of testMouseTracking. On a Macintosh, the tablet primitives provide roughly 120 samples/second versus only 60 mouse samples/second, and the difference is noticable. Works best in full screen mode. Mouse down to draw a stroke, shift-mouse to exit." 
	"Pen testTabletTracking"

	| tabletScale pen p |
	tabletScale _ self tabletScaleFactor.
	pen _ Pen newOnForm: Display.
	pen roundNib: 8.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [
		[Sensor shiftPressed and: [Sensor isAnyButtonPressed]] whileFalse: [
			p _ (Sensor tabletPoint * tabletScale) rounded.
		     Sensor tabletPressure > 0
				ifTrue: [pen goto: p]
				ifFalse: [
					pen color: Color random.
					pen place: p]]].
! !


!Rectangle methodsFor: 'transforming' stamp: 'jmv 1/14/2013 21:14'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay _ Delay forMilliseconds: 10.
	buttonStart _ buttonNow _ Sensor isAnyButtonPressed.
	rect _ self.
	Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow _ Sensor isAnyButtonPressed.
		newRect _ newRectBlock value: rect.
		newRect = rect ifFalse:
			[Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
			Display border: newRect width: 2 rule: Form reverse fillColor: Color gray.
			rect _ newRect]].
	Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
	" pay the price for reading the sensor directly ; get this party started "
	aHand _ self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent.
	^ rect! !


!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 1/14/2013 21:14'!
awaitMouseUpIn: box whileMouseDownDo: doBlock1 whileMouseDownInsideDo: doBlock2 ifSucceed: succBlock
	"The mouse has gone down in box; track the mouse, inverting the box while it's within, and if, on mouse up, the cursor was still within the box, execute succBlock.  While waiting for the mouse to come up, repeatedly execute doBlock1, and also, if the cursor is within the box, execute doBlock2.  6/10/96 sw
3/31/00 JMM added logic to stop multiple redraws"

	| p inside lightForm darkForm isLight |

	p _ Sensor mousePoint.
	inside _ box insetBy: 1.
	isLight _ true.
	lightForm _ Form fromDisplay: inside.
	darkForm _ lightForm copy reverse.
	[Sensor isAnyButtonPressed] whileTrue:
		[doBlock1 value.
		(box containsPoint: (p _ Sensor mousePoint))
			ifTrue: [doBlock2 value.
					isLight ifTrue: 
						[isLight _ false.
						darkForm displayAt: inside origin]]
			ifFalse: [isLight ifFalse:
						[isLight _ true.
						lightForm displayAt: inside origin]]].
	(box containsPoint: p)
		ifTrue: [lightForm displayAt: inside origin.
				^ succBlock value]
! !


!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:14'!
test1   "Display restoreAfter: [WarpBlt test1]"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p ext |
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
			collect: [:pt | pt inverseRotateBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		warp copyQuad: pts toRect: (r1 extent*5-ext//2 extent: ext)]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:14'!
test12   "Display restoreAfter: [WarpBlt test12]"
	"Just like test1, but comparing smooth to non-smooth warps"
	| warp pts r1 p0 p ext warp2 |
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
			collect: [:pt | pt inverseRotateBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		warp copyQuad: pts toRect: (r1 extent*5-ext//2 extent: ext).
		warp2 copyQuad: pts toRect: ((r1 extent*5-ext//2 extent: ext) translatedBy: 250@0).
		]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:14'!
test3   "Display restoreAfter: [WarpBlt test3]"

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
			collect: [:pt | pt inverseRotateBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:14'!
test4   "Display restoreAfter: [WarpBlt test4]"

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
			collect: [:pt | pt inverseRotateBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:14'!
test5   "Display restoreAfter: [WarpBlt test5]"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p |
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
			collect: [:pt | pt inverseRotateBy: (p-p0) theta about: r1 center].
		warp copyQuad: pts toRect: (r1 translatedBy: r1 width@0)]! !

!methodRemoval: InputSensor #anyButtonPressed!
InputSensor removeSelector: #anyButtonPressed!
!methodRemoval: InputSensor #mouseButton1Pressed!
InputSensor removeSelector: #mouseButton1Pressed!
!methodRemoval: InputSensor #mouseButton2Pressed!
InputSensor removeSelector: #mouseButton2Pressed!
!methodRemoval: InputSensor #mouseButton3Pressed!
InputSensor removeSelector: #mouseButton3Pressed!
