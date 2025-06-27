'From Cuis7.5 [latest update: #7285] on 26 June 2025 at 5:46:45 pm'!

!Color class methodsFor: 'examples' stamp: 'jmv 6/26/2025 17:44:56'!
showColorCube
	"Show a 12x12x12 color cube."
	"
	Color showColorCube
	"

	| offDisplay |
	offDisplay := Display shallowCopy.
	0 to: 11 do: [:r |
		0 to: 11 do: [:g |
			0 to: 11 do: [:b |	
				offDisplay fill: (((r*60) + (b*5)) @ (g*5) extent: 5@5)
					fillColor: (Color r: r g: g b: b range: 11)]]].
	Display forceToScreen.
! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/26/2025 17:45:46'!
showColors: colorList
	"Display the given collection of colors across the top of the Display."

	| w r offDisplay |
	offDisplay := Display shallowCopy.
	w := Display width // colorList size.
	r := 0@0 extent: w@((w min: 30) max: 10).
	colorList do: [:c |
		offDisplay fill: r fillColor: c.
		r := r translatedBy: w@0].
	Display forceToScreen.! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/26/2025 17:46:33'!
showHSVPalettes
	"Shows a palette of hues, varying the saturation and brightness for each one. Best results are with depths 16 and 32."
	"
	Color showHSVPalettes
	"

	| left top c offDisplay |
	offDisplay := Display shallowCopy.
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

!Color class methodsFor: 'examples' stamp: 'jmv 6/26/2025 17:45:16'!
showHuesInteractively
	"Shows a palette of hues at a (saturation, brightness) point determined by the mouse position. Click the mouse button to exit and return the selected (saturation, brightness) point."
	"
	Color showHuesInteractively
	"

	| p s v |
	[Sensor isAnyButtonPressed] whileFalse: [
		p := Sensor mousePoint.
		s := p x asFloat / 300.0.
		v := p y asFloat / 300.0.
		self showColors: (self wheel: 12 saturation: s brightness: v)].
	^ (s min: 1.0) @ (v min: 1.0)! !


!Color class methodsFor: 'examples' stamp: 'jmv 6/26/2025 16:50:37'!
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
offDisplay := Display shallowCopy.

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

