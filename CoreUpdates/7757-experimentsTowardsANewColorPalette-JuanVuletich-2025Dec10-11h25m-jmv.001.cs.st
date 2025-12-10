'From Cuis7.5 [latest update: #7756] on 10 December 2025 at 11:26:20 am'!

!Color class methodsFor: 'examples' stamp: 'jmv 10/Dec/2025 11:23:09'!
experimentsTowardsANewColorPalette: selectedColor
"
For UIs.
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Saturation and brightness. (HSV)
	TopRight. For selected Luminance, change Hue and Chroma (HSL)
	BottomRight. For selected Chroma, change Hue and Luminance. (HSL)
"
| selectedHue selectedChroma selectedLuminance hue chroma saturation luminance value color width height origin |
origin := 0@0.
width := 600.
height := 240.
selectedHue := selectedColor hue.
selectedChroma := selectedColor chroma.
selectedLuminance := selectedColor luminance.
"TopLeft. Show 'selected' color."
DisplayScreen deferredUpdatingDisplay getCanvas fillRectangle: (origin extent: height@height) color: selectedColor.

0 to: height do: [ :y |
	value := 1.0 - (y / height).
	0 to: height do: [ :x |
		saturation := x / height.
		color := Color hue: selectedHue saturation: saturation brightness: value.
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x@(y+height)+origin put: color
	].
].
0 to: height do: [ :y | 
	luminance := 1.0 - (y / height).
	chroma := 1.0 - (y / height).
	0 to: width do: [ :x |
		hue := x / width * 360.
		
		color := Color hue: hue chroma: chroma luminance: selectedLuminance.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@y+origin put: color.
		
		color := Color hue: hue chroma: selectedChroma luminance: luminance.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@(y+height)+origin put: color.
	].
].
DisplayScreen forceDeferredToScreen.! !

!Color class methodsFor: 'examples' stamp: 'jmv 10/Dec/2025 11:23:01'!
experimentsTowardsANewColorPaletteHSL: selectedColor
"
For UIs. HSL model (Hue, Chroma, Luminance):
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Chroma and Luminance.
	TopRight. For selected Luminance, change Hue and Chroma
	BottomRight. For selected Chroma, change Hue and Luminance.
"
| selectedHue selectedChroma selectedLuminance hue chroma luminance color width height origin |
origin := 0@960.
width := 600.
height := 240.
selectedHue := selectedColor hue.
selectedChroma := selectedColor chroma.
selectedLuminance := selectedColor luminance.
"TopLeft. Show 'selected' color."
DisplayScreen deferredUpdatingDisplay getCanvas fillRectangle: (origin extent: height@height) color: selectedColor.

0 to: height do: [ :y |
	luminance := 1.0 - (y / height).
	0 to: height do: [ :x |
		chroma := x / height.
		color := Color hue: selectedHue chroma: chroma luminance: luminance.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x@(y+height)+origin put: color
	].
].
0 to: height do: [ :y | 
	luminance := 1.0 - (y / height).
	chroma := 1.0 - (y / height).
	0 to: width do: [ :x |
		hue := x / width * 360.
		
		color := Color hue: hue chroma: chroma luminance: selectedLuminance.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@y+origin put: color.
		
		color := Color hue: hue chroma: selectedChroma luminance: luminance.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@(y+height)+origin put: color.
		
	].
].
DisplayScreen forceDeferredToScreen.! !

!Color class methodsFor: 'examples' stamp: 'jmv 10/Dec/2025 11:22:57'!
experimentsTowardsANewColorPaletteHSV: selectedColor
"
For UIs. HSV model (with Saturation and not Chroma):
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Saturation and Value (brightness).
	TopRight. For selected Value, change Hue and Saturation
	BottomRight. For selected Saturation, change Hue and Value.
"
| selectedHue selectedSaturation selectedValue hue saturation value color width height origin |
origin := 840@0.
width := 600.
height := 240.
selectedHue := selectedColor hue.
selectedSaturation := selectedColor saturation.
selectedValue := selectedColor brightness.
"TopLeft. Show 'selected' color."
DisplayScreen deferredUpdatingDisplay getCanvas fillRectangle: (origin extent: height@height) color: selectedColor.

0 to: height do: [ :y |
	value := 1.0 - (y / height).
	0 to: height do: [ :x |
		saturation := x / height.
		color := Color hue: selectedHue saturation: saturation brightness: value.
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x@(y+height)+origin put: color
	].
].
0 to: height do: [ :y |
	value:= 1.0 - (y / height).
	saturation := 1.0 - (y / height).
	0 to: width do: [ :x |
		hue := x / width * 360.
		
		color := Color hue: hue saturation: saturation brightness: selectedValue.
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@y+origin put: color.
		
		color := Color hue: hue saturation: selectedSaturation brightness: value.
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@(y+height)+origin put: color.
	].
].
DisplayScreen forceDeferredToScreen.! !

!Color class methodsFor: 'examples' stamp: 'jmv 10/Dec/2025 11:22:53'!
experimentsTowardsANewColorPaletteOKLCH: selectedColor
"
For UIs. OKLCH model.
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Chroma and Lightness).
	TopRight. For selected Lightness, change Hue and Chroma
	BottomRight. For selected Chroma, change Hue and Lightness.
"
| selectedHue selectedChroma selectedLightness hue chroma lightness color width height origin |
origin := 840@480.
width := 600.
height := 240.
selectedHue := selectedColor oklchHue.
selectedChroma := selectedColor oklchChroma.
selectedLightness := selectedColor oklchLightness.
"TopLeft. Show 'selected' color."
DisplayScreen deferredUpdatingDisplay getCanvas fillRectangle: (origin extent: height@height) color: selectedColor.

0 to: height do: [ :y |
	lightness := 1.0 - (y / height).
	0 to: height do: [ :x |
		chroma := x / height * 0.3.
		color := Color okl: lightness c: chroma h: selectedHue.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x@(y+height)+origin put: color
	].
].
0 to: height do: [ :y |
	lightness := 1.0 - (y / height).
	chroma := 1.0 - (y / height) * 0.3.
	0 to: width do: [ :x |
		hue := x / width * 360.
		
		color := Color okl: selectedLightness c: chroma h: hue.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@y+origin put: color.
		
		color := Color okl: lightness c: selectedChroma h: hue.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@(y+height)+origin put: color.
		
	].
].
DisplayScreen forceDeferredToScreen.! !

!Color class methodsFor: 'examples' stamp: 'jmv 10/Dec/2025 11:22:49'!
experimentsTowardsANewColorPaletteOKLCH2: selectedColor
"
For UIs. OKLCH model.
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Chroma and Lightness).
	TopRight. For selected Lightness, change Hue and Chroma
	BottomRight. For selected Chroma, change Hue and Lightness.
"
| selectedHue selectedChroma selectedLightness hue chroma lightness color width height origin |
origin := 0@480.
width := 600.
height := 240.
selectedHue := selectedColor oklchHue.
selectedChroma := selectedColor oklchChroma.
selectedLightness := selectedColor oklchLightness.
"TopLeft. Show 'selected' color."
DisplayScreen deferredUpdatingDisplay getCanvas fillRectangle: (origin extent: height@height) color: selectedColor.

0 to: height do: [ :y |
	lightness := 1.0 - (y / height).
	0 to: height do: [ :x |
		chroma := x / height * 0.3.
		color := Color okOrNilL: lightness c: chroma h: selectedHue.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x@(y+height)+origin put: color
	].
].
0 to: height do: [ :y |
	lightness := 1.0 - (y / height).
	chroma := 1.0 - (y / height) * 0.3.
	0 to: width do: [ :x |
		hue := x / width * 360.
		
		color := Color okOrNilL: selectedLightness c: chroma h: hue.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@y+origin put: color.
		
		color := Color okOrNilL: lightness c: selectedChroma h: hue.
		color ifNil: [ color := Color black ].
		((color diff: selectedColor) between: 0.02 and: 0.03) ifTrue: [ color := Color black ].
		DisplayScreen deferredUpdatingDisplay colorAt: x+height@(y+height)+origin put: color.
		
	].
].
DisplayScreen forceDeferredToScreen.! !


!Color class methodsFor: 'examples' stamp: 'jmv 10/Dec/2025 11:23:32'!
experimentsTowardsANewColorPalette
"
For UIs.
	TopLeft. Show 'selected' color.
	BottomLeft. For selected Hue, change Saturation and brightness. (HSV)
	TopRight. For selected Luminance, change Hue and Chroma (HSL)
	BottomRight. For selected Chroma, change Hue and Luminance. (HSL)

Evaluate multiple times and see results:
	Color experimentsTowardsANewColorPalette.
"
| selectedColor |
selectedColor := (Color r: 0.712 g: 0.747 b: 0.216).
selectedColor := Color random.
selectedColor print.
	Color experimentsTowardsANewColorPalette: selectedColor.
	Color experimentsTowardsANewColorPaletteOKLCH2: selectedColor.
	Color experimentsTowardsANewColorPaletteHSL: selectedColor.
	Color experimentsTowardsANewColorPaletteHSV: selectedColor.
	Color experimentsTowardsANewColorPaletteOKLCH: selectedColor.! !

