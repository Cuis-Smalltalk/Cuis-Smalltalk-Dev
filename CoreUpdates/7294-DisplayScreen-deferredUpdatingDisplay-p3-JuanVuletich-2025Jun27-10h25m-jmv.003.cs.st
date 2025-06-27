'From Cuis7.5 [latest update: #7289] on 27 June 2025 at 10:54:54 am'!

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 6/27/2025 10:36:57'!
copyToDisableImmediateUpdates
	"Answer an instance that shares our pixel storage.
	This is useful to prevent the VM from immediately updating the host platform pixels the user actually sees,
	especially on platforms where #deferUpdates: answers ni, meaning it doesn't really support doing this.
	Flicking is possible. For this reason Morphic doesn't use this technique.
	Preferred technique is to use #deferredUpdatingDisplay and related methods.
	"
	^self shallowCopy! !


!Rectangle methodsFor: 'transforming' stamp: 'jmv 6/27/2025 10:33:59'!
newRect2From: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect aHand delay |
	delay := Delay forMilliseconds: 10.
	rect := self.
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	[Sensor isAnyButtonPressed] whileTrue: 
		[delay wait.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen deferredUpdatingDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen forceDeferredToScreen: (rect merge: newRect).
			rect := newRect]].
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !

!Rectangle methodsFor: 'transforming' stamp: 'jmv 6/27/2025 10:34:20'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay := Delay forMilliseconds: 10.
	buttonStart := buttonNow := Sensor isAnyButtonPressed.
	rect := self.
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow := Sensor isAnyButtonPressed.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen deferredUpdatingDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen forceDeferredToScreen: (rect merge: newRect).
			rect := newRect]].
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !


!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 10:34:42'!
experimentsTowardsANewColorPalette
"
For UIs.
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Saturation and brightness.
	TopRight. For selected Luminance, change Hue and Chroma
	BottomRight. For selected Chroma, change Hue and Luminance.
Evaluate multiple times and see results:
	Color experimentsTowardsANewColorPalette
"
| selectedHue selectedSaturation selectedV selectedColor h s v color width height selectedChroma selectedLuminance |
width := 300.
height := 120.
selectedColor := Color random.
selectedHue := selectedColor hue.
selectedSaturation := selectedColor saturation.
selectedChroma := selectedColor chroma.
selectedV := selectedColor brightness.
selectedLuminance := selectedColor luminance.
"TopLeft. Show 'selected' color."
DisplayScreen deferredUpdatingDisplay getCanvas fillRectangle: (0@0 extent: height@height) color: selectedColor.

0 to: height do: [ :y |
	v := 1.0 - (y / height).
	0 to: height do: [ :x |
		s := x / height.
		color := Color new setHue: selectedHue saturation: s brightness: v.
		DisplayScreen deferredUpdatingDisplay colorAt: x@(y+height) put: color
	].
	DisplayScreen forceDeferredToScreen: (0@y extent: height@y).
].
0 to: height do: [ :y | | c |
	v := 1.0 - (y / height).
	s := 1.0 - (y / height).
	c := s.
	0 to: width do: [ :x |
		h := x / width * 360.
		
		color := Color new setHue: h chroma: c luminance: selectedLuminance.
"		color := Color new setHue: h chroma: c brightness: selectedV."
		color ifNil: [ color := Color black ].
"		color := Color new setHue: h saturation: s brightness: selectedV."
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@y put: color.
		
		color := Color new setHue: h chroma: selectedChroma luminance: v.
"		color := Color new setHue: h chroma: selectedChroma brightness: v."
		color ifNil: [ color := Color black ].
"		color := Color new setHue: h saturation: selectedSaturation brightness: v."
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@(y+height) put: color.
		
	].
	DisplayScreen forceDeferredToScreen: (0@y extent: width@y).
].! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 10:35:01'!
showColorCube
	"Show a 12x12x12 color cube."
	"
	Color showColorCube
	"

	0 to: 11 do: [:r |
		0 to: 11 do: [:g |
			0 to: 11 do: [:b |	
				DisplayScreen deferredUpdatingDisplay fill: (((r*60) + (b*5)) @ (g*5) extent: 5@5)
					fillColor: (Color r: r g: g b: b range: 11)]]].
	DisplayScreen forceDeferredToScreen.
! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 10:35:24'!
showColors: colorList
	"Display the given collection of colors across the top of the Display."

	| w r |
	w := DisplayScreen deferredUpdatingDisplay width // colorList size.
	r := 0@0 extent: w@((w min: 30) max: 10).
	colorList do: [:c |
		DisplayScreen deferredUpdatingDisplay fill: r fillColor: c.
		r := r translatedBy: w@0].
	DisplayScreen forceDeferredToScreen.! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 10:35:47'!
showHSVPalettes
	"Shows a palette of hues, varying the saturation and brightness for each one. Best results are with depths 16 and 32."
	"
	Color showHSVPalettes
	"

	| left top c |
	left := top := 0.
	0 to: 179 by: 15 do: [:h |
		0 to: 10 do: [:s |
			left := (h * 4) + (s * 4).
			0 to: 10 do: [:v |
				c := Color h: h s: s asFloat / 10.0 v: v asFloat / 10.0.
				top := (v * 4).
				DisplayScreen deferredUpdatingDisplay fill: (left@top extent: 4@4) fillColor: c.

				c := Color h: h + 180 s: s asFloat / 10.0 v: v asFloat / 10.0.
				top := (v * 4) + 50.
				DisplayScreen deferredUpdatingDisplay fill: (left@top extent: 4@4) fillColor: c]]].
	DisplayScreen forceDeferredToScreen.! !


!ProgressInitiationException methodsFor: 'exceptionDescription' stamp: 'jmv 6/27/2025 10:46:14'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame lastDrawTime |
	f := FontFamily defaultFamilyAndPointSize.
	h := f lineSpacing * 3//2.
	textWidth := (f widthOfString: progressTitle) + h.
	width := 150 max: textWidth.

	textForm := Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame := Rectangle center: aPoint extent: (width@h) roundedToEven.
	textFrame := `0@0` extent: width@h.
	textFrame := textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame := barFrame merge: textFrame.
	delta := outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame := barFrame translatedBy: delta.
	textFrame := textFrame translatedBy: delta.
	outerFrame := outerFrame translatedBy: delta.
	range := maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame := barFrame insetBy: 2.
	textForm displayOn: DisplayScreen deferredUpdatingDisplay at: textFrame topLeft.
	DisplayScreen deferredUpdatingDisplay fillBlack: barFrame.
	DisplayScreen deferredUpdatingDisplay fillWhite: innerBarFrame.
	DisplayScreen forceDeferredToScreen: outerFrame.
	lastDrawTime := Time localMillisecondClock.
	result := workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal := barVal ]
			ifNil: [
				currentVal := currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal := minVal ]].
		filledWidth := ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		Time localMillisecondClock - lastDrawTime > 100 ifTrue: [
			textForm displayOn: DisplayScreen deferredUpdatingDisplay at: textFrame topLeft.
			DisplayScreen deferredUpdatingDisplay fillBlack: barFrame.
			DisplayScreen deferredUpdatingDisplay fillWhite: innerBarFrame.
			DisplayScreen deferredUpdatingDisplay fillGray: (barFrame topLeft + `2@2` extent: filledWidth@(h-4)).
			DisplayScreen forceDeferredToScreen: outerFrame.
			lastDrawTime := Time localMillisecondClock ]].
	UISupervisor ui ifNotNil: [ :ui | ui invalidateDisplayRect: outerFrame for: nil ].
	self resume: result! !


!Transcript class methodsFor: 'private' stamp: 'jmv 6/27/2025 10:28:48'!
displayEngine
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	"
	Transcript releaseClassCachedState
	"
	(displayEngine isNil or: [
		displayEngine class ~= BitBltCanvasEngine]) ifTrue: [
			displayEngine := BitBltCanvasEngine toForm: DisplayScreen deferredUpdatingDisplay ].
	^ displayEngine! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/27/2025 10:44:05'!
display
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayOnDisplay.
		DisplayScreen forceDeferredToScreen: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	UISupervisor whenUIinSafeState: [ self triggerEvent: #redrawTranscriptArea ].! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 6/27/2025 10:44:12'!
displayUnfinishedEntry
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayUnfinishedEntryOnDisplay ifNotNil: [ :damage |
			DisplayScreen forceDeferredToScreen: damage ]]! !

!methodRemoval: MorphicCanvas #showAt:invalidRect: stamp: 'jmv 6/27/2025 10:53:46'!
MorphicCanvas removeSelector: #showAt:invalidRect:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Transcript releaseClassCachedState!

