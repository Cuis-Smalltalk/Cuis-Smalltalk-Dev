'From Cuis7.5 [latest update: #7289] on 27 June 2025 at 9:18:23 am'!

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 6/27/2025 09:15:59'!
copyToDisableImmediateUpdates
	"Answer an instance that shares our pixel storage.
	This is useful to prevent the VM from immediately updating the host platform pixels the user actually sees,
	especially on platforms where #deferUpdates: answers ni, meaning it doesn't really support doing this.
	Flicking is possible. See comment at #beDisplay . For this reason Morphic doesn't use this technique.
	Alternatively, the separate Form used as the Morphic Canvas target on such platforms could be made into
	a service provided by this class, together with the appropriate way to update the real host pixels, removing
	this code from #displayWorldOn:.
	"
	^self shallowCopy! !


!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 09:17:40'!
experimentsTowardsANewColorPalette
"
For UIs.
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Saturation and brightness.
	TopRight. For selected Luminance, change Hue and Chroma
	BottomRight. For selected Chroma, change Hue and Luminance.
Color experimentsTowardsANewColorPalette
"
| selectedHue selectedSaturation selectedV selectedColor h s v color width height selectedChroma selectedLuminance offDisplay |
width := 300.
height := 120.
selectedColor := Color random.
selectedHue := selectedColor hue.
selectedSaturation := selectedColor saturation.
selectedChroma := selectedColor chroma.
selectedV := selectedColor brightness.
selectedLuminance := selectedColor luminance.
"TopLeft. Show 'selected' color."
Display getCanvas fillRectangle: (0@0 extent: height@height) color: selectedColor.
offDisplay := Display copyToDisableImmediateUpdates.

0 to: height do: [ :y |
	v := 1.0 - (y / height).
	0 to: height do: [ :x |
		s := x / height.
		color := Color new setHue: selectedHue saturation: s brightness: v.
		offDisplay colorAt: x@(y+height) put: color
	].
	Display forceToScreen: (0@y extent: height@y).
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
		offDisplay colorAt: x+height@y put: color.
		
		color := Color new setHue: h chroma: selectedChroma luminance: v.
"		color := Color new setHue: h chroma: selectedChroma brightness: v."
		color ifNil: [ color := Color black ].
"		color := Color new setHue: h saturation: selectedSaturation brightness: v."
		offDisplay colorAt: x+height@(y+height) put: color.
		
	].
	Display forceToScreen: (0@y extent: width@y).
].! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 09:17:36'!
showColorCube
	"Show a 12x12x12 color cube."
	"
	Color showColorCube
	"

	| offDisplay |
	offDisplay := Display copyToDisableImmediateUpdates.
	0 to: 11 do: [:r |
		0 to: 11 do: [:g |
			0 to: 11 do: [:b |	
				offDisplay fill: (((r*60) + (b*5)) @ (g*5) extent: 5@5)
					fillColor: (Color r: r g: g b: b range: 11)]]].
	Display forceToScreen.
! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 09:17:32'!
showColors: colorList
	"Display the given collection of colors across the top of the Display."

	| w r offDisplay |
	offDisplay := Display copyToDisableImmediateUpdates.
	w := Display width // colorList size.
	r := 0@0 extent: w@((w min: 30) max: 10).
	colorList do: [:c |
		offDisplay fill: r fillColor: c.
		r := r translatedBy: w@0].
	Display forceToScreen.! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/27/2025 09:17:29'!
showHSVPalettes
	"Shows a palette of hues, varying the saturation and brightness for each one. Best results are with depths 16 and 32."
	"
	Color showHSVPalettes
	"

	| left top c offDisplay |
	offDisplay := Display copyToDisableImmediateUpdates.
	left := top := 0.
	0 to: 179 by: 15 do: [:h |
		0 to: 10 do: [:s |
			left := (h * 4) + (s * 4).
			0 to: 10 do: [:v |
				c := Color h: h s: s asFloat / 10.0 v: v asFloat / 10.0.
				top := (v * 4).
				offDisplay fill: (left@top extent: 4@4) fillColor: c.

				c := Color h: h + 180 s: s asFloat / 10.0 v: v asFloat / 10.0.
				top := (v * 4) + 50.
				offDisplay fill: (left@top extent: 4@4) fillColor: c]]].
	Display forceToScreen.
! !


!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 6/27/2025 09:17:19'!
drawsOnDisplay
	"Answer true if we actually draw on (Display copyToDisableImmediateUpdates)"
	^form == Display | (form bits == Display bits)! !


!Transcript class methodsFor: 'private' stamp: 'jmv 6/27/2025 09:17:24'!
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
			displayEngine := BitBltCanvasEngine toForm: Display copyToDisableImmediateUpdates].
	^ displayEngine! !

