'From Cuis 5.0 of 7 November 2016 [latest update: #3526] on 18 December 2018 at 12:49:31 pm'!

!Color class methodsFor: 'examples' stamp: 'jmv 12/17/2018 16:29:53'!
experimentsTowardsANewColorPalette
"
self experimentsTowardsANewColorPalette
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
		color _ Color new setHue: selectedHue saturation: s brightness: v.
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
		
		color _ Color new setHue: h chroma: c luminance: selectedLuminance.
"		color _ Color new setHue: h chroma: c brightness: selectedV."
		color ifNil: [ color _ Color black ].
"		color _ Color new setHue: h saturation: s brightness: selectedV."
		Display colorAt: x+height@y put: color.
		
		color _ Color new setHue: h chroma: selectedChroma luminance: v.
"		color _ Color new setHue: h chroma: selectedChroma brightness: v."
		color ifNil: [ color _ Color black ].
"		color _ Color new setHue: h saturation: selectedSaturation brightness: v."
		Display colorAt: x+height@(y+height) put: color.
		
	].
	DisplayScreen screenUpdateRequired: nil
].! !

!methodRemoval: Color class #experimentsTowarsANewColorPalette!
Color class removeSelector: #experimentsTowarsANewColorPalette!
