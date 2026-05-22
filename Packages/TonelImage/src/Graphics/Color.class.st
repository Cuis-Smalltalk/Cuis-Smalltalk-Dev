"
This class represents abstract color, regardless of the depth of bitmap it will be shown in.  At the very last moment a Color is converted to a pixelValue that depends on the depth of the actual Bitmap inside the Form it will be used with.  The supported depths (in bits) are 1, 2, 4, 8, 16, and 32.  The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million.  (See comment in BitBlt.)  To change the depth of the Display and set how many colors you can see, execute: (Display newDepth: 8).  (See comment in DisplayMedium)
	Color is represented as the amount of light in red, green, and blue.  White is (1.0, 1.0, 1.0) and black is (0, 0, 0).  Pure red is (1.0, 0, 0).  These colors are "additive".  Think of Color's instance variables as:
	r	amount of red, a Float between 0.0 and 1.0.
	g	amount of green, a Float between 0.0 and 1.0.
	b	amount of blue, a Float between 0.0 and 1.0.
	Many colors are named.  You find a color by name by sending a message to class Color, for example (Color lightBlue).  Also, (Color red: 0.2 green: 0.6 blue: 1.0) or (Color r: 0.2 g: 0.6 b: 1.0) creates a color. (see below)
	A color is essentially immutable.  Once you set red, green, and blue, you cannot change them.  Instead, create a new Color and use it.
	Applications such as contour maps and bar graphs will want to display one of a set of shades based on a number.  Convert the range of this number to an integer from 1 to N.  Then call (Color green lightShades: N) to get an Array of colors from white to green.  Use the Array messages at:, atPin:, or atWrap: to pull out the correct color from the array.  atPin: gives the first (or last) color if the index is out of range.  atWrap: wraps around to the other end if the index is out of range.
	Here are some fun things to run in when your screen has color:
		Pen new mandala: 30 diameter: Display height-100.
		Pen new web  "Draw with the mouse, opt-click to end"
		Display fillWhite.  Pen new hilberts: 5.
		Form toothpaste: 30  "Draw with mouse, opt-click to end"

Messages:
	mixed: proportion with: aColor	Answer this color mixed with the given color additively. The proportion, a number between 0.0 and 1.0, determines what what fraction of the receiver to use in the mix.

	+ 	add two colors
	- 	subtract two colors
	*	multiply the values of r, g, b by a number or an Array of factors.  ((Color named: #white) * 0.3) gives a darkish gray.  (aColor * #(0 0 0.9)) gives a color with slightly less blue.
	/	divide a color by a factor or an array of three factors.

	hue			Returns the hue of the color. On a wheel from 0 to 360 with pure red at 0 and again at 360.
	saturation	Returns the saturation of the color.  0.0 to 1.0
	brightness	Returns the brightness of the color.  0.0 to 1.0

	name    Look to see if this Color has a name.
	display	Show a swatch of this color tracking the cursor.

	lightShades: thisMany		An array of thisMany colors from white to the receiver. 
	darkShades: thisMany		An array of thisMany colors from black to the receiver.  Array is of length num.
	mix: color2 shades: thisMany		An array of thisMany colors from the receiver to color2.
	wheel: thisMany			An array of thisMany colors around the color wheel starting and ending at the receiver.

	pixelValueForDepth: d    Returns the bits that appear be in a Bitmap of this depth for this color.  Represents the nearest available color at this depth.  Normal users do not need to know which pixelValue is used for which color. 

Messages to Class Color.
	red: r green: g blue: b		Return a color with the given r, g, and b components.
	r: g: b:		Same as above, for fast typing.

 	hue: h saturation: s brightness: b		Create a color with the given hue, saturation, and brightness.

	pink
 	blue
	red ...	Many colors have messages that return an instance of Color.
	canUnderstand: #brown	  Returns true if #brown is a defined color.
	names		An OrderedCollection of the names of the colors.
	named: #notAllThatGray put: aColor    Add a new color to the list and create an access message and a class variable for it.
	fromUser	Shows the palette of colors available at this display depth.  Click anywhere to return the color you clicked on.

	hotColdShades: thisMany	An array of thisMany colors showing temperature from blue to red to white hot.

    stdColorsForDepth: d        An Array of colors available at this depth.  For 16 bit and 32 bits, returns a ColorGenerator.  It responds to at: with a Color for that index, simulating a very big Array. 

   colorFromPixelValue: value depth: d    Returns a Color whose bit pattern (inside a Bitmap) at this depth is the number specified.  Normal users do not need to use this.

(See also comments in these classes: Form, Bitmap, BitBlt)

Default colorSpace is #sRGB. Subclasses might use other color spaces
"
Class {
	#name : 'Color',
	#superclass : 'ThirtyTwoBitSlotsObject',
	#classVars : [
		'CachedColormaps',
		'FromGrayColormaps',
		'GrayToIndexMap',
		'IndexedColors',
		'MaskingMap',
		'ToGrayColormaps'
	],
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'color space conversions' }
Color class >> applySimpleGamma: gamma to: value [
	"Encode / decode Gamma. Typical gamma are 0.4545 ( = 1.0 / 2.2, for encoding) and 2.2 (for decoding)
	In a non gamma encoded image, the pixel values are proportional to luminance, i.e. the actual light intensity, the photon count.
	In an gamma encoded image, the pixel value is close to the average perceived brightness, or 'lightness'. This optimizes the use of available bits in digital images, and dynamic range in analog images and film. This is like the 'luma' signal in video.

	Usually:
		- Images stored in file formats such as jpg, png, tiff and bmp are gamma encoded
				(unless they are actually raw images).
		- Images sent to a framebuffer (video memory) should be gamma encoded.
		- Raw images from sensors are not gamma encoded.

		- Image processing algorithms that simulate the real world, or are applied to real world data should work on unencoded (linear) images.
				This includes filtering, resizing, blending, and most operations done on images.
				This means that if the images come from jpg photos from a camera, they should be gamma decoded.
		- Image processing algorithms that works on human perception should work on gamma encoded images.
				This includes histograms, histogram equalization, bit allocation (i.e. quantization), compression, etc.
				This also includes detecting objects like a human would do
				

	See
		http://en.wikipedia.org/wiki/Gamma_correction
		http://www.poynton.com/notes/colour_and_gamma/GammaFAQ.html

	Mostly for documentation.
	value must be in [0.0 .. 1.0].
	Note that the sRGB standard specifies a function that is close to this, but slightly different.
	See #linearTosRGBGamma: and #sRGBGammaToLinear:"

	^ value raisedTo: gamma
]

{ #category : 'black-white-grays' }
Color class >> black [
	"Override traditional names existing in XKCD naming"
	^`Color fromHexString: '#000000'`
]

{ #category : 'named colors' }
Color class >> blue [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#0343df'`
]

{ #category : 'named colors' }
Color class >> brown [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#653700'`
]

{ #category : 'colormaps' }
Color class >> cachedColormapForGrayFrom: sourceDepth [
	"Return a cached colormap for mapping from the given depth to the 8bpp grays. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| srcIndex map |
	ToGrayColormaps ifNil: [
		ToGrayColormaps := Array new: 5].
	
	srcIndex := sourceDepth highBit min: 5.

	(ToGrayColormaps at: srcIndex) ifNotNil: [ :m | ^ m ].

	map := self computeColormapForGray8bppFrom: sourceDepth.
	ToGrayColormaps at: srcIndex put: map.
	^ map
]

{ #category : 'colormaps' }
Color class >> cachedColormapFrom: sourceDepth to: destDepth [
	"Return a cached colormap for mapping between the given depths. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| srcIndex dstIndex map |
	CachedColormaps
		ifNil: [CachedColormaps := (1 to: 5) collect: [:i | Array new: 6]].

	srcIndex := sourceDepth highBit min: 5.
	dstIndex := destDepth highBit.

	((CachedColormaps at: srcIndex) at: dstIndex) ifNotNil: [ :m | ^ m ].

	map := self computeColormapFrom: sourceDepth to: destDepth.
	(CachedColormaps at: srcIndex) at: dstIndex put: map.
	^ map
]

{ #category : 'colormaps' }
Color class >> cachedColormapFromGrayTo: destDepth [
	"Return a cached colormap for mapping from 8bpp grays to the given depth. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| dstIndex map |
	FromGrayColormaps
		ifNil: [FromGrayColormaps := Array new: 6 ].

	dstIndex := destDepth highBit.

	(FromGrayColormaps at: dstIndex) ifNotNil: [ :m | ^ m ].

	map := self computeColormapFromGray8bppForDepth: destDepth.
	FromGrayColormaps at: dstIndex put: map.
	^ map
]

{ #category : 'instance creation' }
Color class >> clipR: r g: g b: b [
	"Return a color with the given r, g, and b components in the range [0.0..1.0].
	Clip if necessary"

	^ self 
		r: (r min: 1.0 max: 0.0)
		g: (g min: 1.0 max: 0.0)
		b: (b min: 1.0 max: 0.0)
]

{ #category : 'instance creation' }
Color class >> colorFrom: parm [
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"

	| aColor firstParm |
	(parm is: #Color) ifTrue: [ ^ parm ].
	parm isSymbol ifTrue: [ ^ self perform: parm ].
	parm isString ifTrue: [ ^ self fromHexString: parm ].
	(parm isCollection and: [ parm isSequenceable and: [ parm size > 0 ]])
		ifTrue: [
			firstParm := parm first.
			firstParm isNumber ifTrue: [
				^ self fromRgbTriplet: parm].
			aColor := self colorFrom: firstParm.
			parm withIndexDo: [ :sym :ind |
				ind > 1 ifTrue: [
					aColor := aColor perform: sym ]].
			^ aColor].
	^ parm

"
Color colorFrom: #(blue darker)
Color colorFrom: Color blue darker
Color colorFrom: #blue
Color colorFrom: #(0.0 0.0 1.0)
Color colorFrom: '#D7B360'
"
]

{ #category : 'instance creation' }
Color class >> colorFromPixelValue: p depth: d [
	"Convert a pixel value for the given display depth into a color."
	"Details: For depths of 8 or less, the pixel value is simply looked up in a table. For greater depths, the color components are extracted and converted into a color."
	"Warning: In BitBlt, a pixel with pixelValue = 0 is transparent.
	Squeak usually assumes that r=g=b=0 => transparent. But this is false if we have alpha (opacity).
	A color with r=g=b=0 and opacity = 255 is BLACK, not TRANSPARENT.
	Squeak also answers darkest possible blue when asked for black. Again, this is not needed in 32 bits (with alpha). It is also not really needed for 16 bits as we have an extra, unused bit: 16r8000 is not zero, but rgb=0, and hence, black.
	The real rule is that pixelValue=0 means transparent.
	And that darkest blue must be used instead of black, but only for depths >8 and < 16 (no indexed colors, no alpha)
	This method is updated to reflect that."

	| r g b alpha |

	d = 1 ifTrue: [^ self oneBitColors at: (p bitAnd: 16r01) + 1].

	d = 8 ifTrue: [^ self indexedColors at: (p bitAnd: 16rFF) + 1].
	d = 4 ifTrue: [^ self indexedColors at: (p bitAnd: 16r0F) + 1].
	d = 2 ifTrue: [^ self indexedColors at: (p bitAnd: 16r03) + 1].

	d = 32 ifTrue: [
		"eight bits per component; 8 bits of alpha"
		alpha := p bitShift: -24.
		alpha = 0 ifTrue: [ ^`Color transparent` ].
		r := (p bitShift: -16) bitAnd: 16rFF.
		g := (p bitShift: -8) bitAnd: 16rFF.
		b := p bitAnd: 16rFF.
		^alpha < 255
			ifTrue: [ (Color r: r g: g b: b range: 255) alpha: alpha asFloat / 255.0 ]
			ifFalse: [ Color r: r g: g b: b range: 255 ]].

	d = 16 ifTrue: [
		"five bits per component. The most significant bit, unused, allows having real black, without p being zero"
		r := (p bitShift: -10) bitAnd: 16r1F.
		g := (p bitShift: -5) bitAnd: 16r1F.
		b := p bitAnd: 16r1F.
		^ Color r: r g: g b: b range: 31].

	d = 15 ifTrue: [
		"five bits per component"
		r := (p bitShift: -10) bitAnd: 16r1F.
		g := (p bitShift: -5) bitAnd: 16r1F.
		b := p bitAnd: 16r1F.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^`Color black` ].
		^ Color r: r g: g b: b range: 31].

	d = 12 ifTrue: [
		"four bits per component"
		r := (p bitShift: -8) bitAnd: 16rF.
		g := (p bitShift: -4) bitAnd: 16rF.
		b := p bitAnd: 16rF.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^`Color black` ].
		^ Color r: r g: g b: b range: 15].

	d = 9 ifTrue: [
		"three bits per component"
		r := (p bitShift: -6) bitAnd: 16r7.
		g := (p bitShift: -3) bitAnd: 16r7.
		b := p bitAnd: 16r7.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^`Color black` ].
		^ Color r: r g: g b: b range: 7].

	self error: 'unknown pixel depth: ', d printString
]

{ #category : 'colormaps' }
Color class >> colorMapIfNeededFrom: sourceDepth to: destDepth [
	"Return a colormap for mapping between the given depths, or nil if no colormap is needed."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!"

	sourceDepth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"

	(sourceDepth >= 16) & (destDepth >= 16) ifTrue: [
		"mapping is done in BitBlt by zero-filling or truncating each color component"
		^ nil].

	^ Color cachedColormapFrom: sourceDepth to: destDepth

]

{ #category : 'color from user' }
Color class >> colorPaletteForDepth: depth extent: chartExtent [
	"Display a palette of colors sorted horizontally by hue and vertically by lightness. Useful for eyeballing the color gamut of the display, or for choosing a color interactively."
	"Note: It is slow to build this palette, so it should be cached for quick access."
	"(Color colorPaletteForDepth: 16 extent: 190@60) display"

	| basicHue x y startHue palette transHt vSteps grayWidth hSteps |
	palette := Form extent: chartExtent depth: depth.
	transHt :=  FontFamily defaultPointSize * 3//2.
	palette fillWhite: (`0@0` extent: palette width@transHt).
	palette fillBlack: (0@transHt extent: palette width@1).
	'no color' displayOn: palette at: palette boundingBox topCenter - ((transHt * 2)@0).
	grayWidth := 10.
	startHue := 338.0.
	vSteps := palette height - transHt // 2.
	hSteps := palette width - grayWidth.
	x := 0.
	startHue to: startHue + 360.0 by: 360.0/hSteps do: [ :h |
		basicHue := Color h: h asFloat s: 1.0 v: 1.0.
		y := transHt+1.
		0 to: vSteps do: [ :n | | c |
 			c := basicHue mixed: (n asFloat / vSteps asFloat) with: Color white.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y := y + 1].
		1 to: vSteps do: [ :n | | c |
 			c := Color black mixed: (n asFloat / vSteps asFloat) with: basicHue.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y := y + 1].
		x := x + 1].
	y := transHt + 1.
	1 to: vSteps * 2 do: [ :n | | c |
 		c := Color black mixed: (n asFloat / (vSteps*2) asFloat) with: Color white.
		palette fill: (x@y extent: 10@1) fillColor: c.
		y := y + 1].
	^ palette

]

{ #category : 'examples' }
Color class >> colorRampForDepth: depth extent: aPoint [
	"Returns a form of the given size showing R, G, B, and gray ramps for the given depth. Useful for testing color conversions between different depths."
	"(Color colorRampForDepth: Display depth extent: 256@80) display"
	"(Color colorRampForDepth: 32 extent: 256@80) displayOn: Display at: 0@0 rule: Form paint"

	| f dx dy r |
	f := Form extent: aPoint depth: depth.
	dx := aPoint x // 256.
	dy := aPoint y // 4.
	0 to: 255 do: [:i |
		r := (dx * i)@0 extent: dx@dy.
		f fill: r fillColor: (Color r: i g: 0 b: 0 range: 255).
		r := r translatedBy: 0@dy.
		f fill: r fillColor: (Color r: 0 g: i b: 0 range: 255).
		r := r translatedBy: 0@dy.
		f fill: r fillColor: (Color r: 0 g: 0 b: i range: 255).
		r := r translatedBy: 0@dy.
		f fill: r fillColor: (Color r: i g: i b: i range: 255)].
	^ f

]

{ #category : 'color from user' }
Color class >> colorTest: depth extent: chartExtent colorMapper: colorMapper [
	"Create a palette of colors sorted horizontally by hue and vertically by lightness. Useful for eyeballing the color gamut of the display, or for choosing a color interactively."
	"Note: It is slow to build this palette, so it should be cached for quick access."
	"(Color colorTest: 32 extent: 570@180 colorMapper: [:c | c]) display"
	"(Color colorTest: 32 extent: 570@180 colorMapper:
		[:c | Color
			r: (c red * 7) asInteger / 7
			g: (c green * 7) asInteger / 7
			b: (c blue * 3) asInteger / 3]) display"
	"(Color colorTest: 32 extent: 570@180 colorMapper:
		[:c | Color
			r: (c red * 5) asInteger / 5
			g: (c green * 5) asInteger / 5
			b: (c blue * 5) asInteger / 5]) display"
	"(Color colorTest: 32 extent: 570@180 colorMapper:
		[:c | Color
			r: (c red * 15) asInteger / 15
			g: (c green * 15) asInteger / 15
			b: (c blue * 15) asInteger / 15]) display"
	"(Color colorTest: 32 extent: 570@180 colorMapper:
		[:c | Color
			r: (c red * 31) asInteger / 31
			g: (c green * 31) asInteger / 31
			b: (c blue * 31) asInteger / 31]) display"

	| basicHue x y c startHue palette transHt vSteps grayWidth hSteps |
	palette := Form extent: chartExtent depth: depth.
	transHt :=  FontFamily defaultPointSize * 3//2.
	palette fillWhite: (0@0 extent: palette width@transHt).
	palette fillBlack: (0@transHt extent: palette width@1).
	'no color' displayOn: palette at: palette boundingBox topCenter - ((transHt * 2)@0).
	grayWidth := 10.
	startHue := 338.0.
	vSteps := palette height - transHt // 2.
	hSteps := palette width - grayWidth.
	x := 0.
	startHue to: startHue + 360.0 by: 360.0/hSteps do: [:h |
		basicHue := Color h: h asFloat s: 1.0 v: 1.0.
		y := transHt+1.
		0 to: vSteps do: [:n |
 			c := basicHue mixed: (n asFloat / vSteps asFloat) with: Color white.
			c := colorMapper value: c.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y := y + 1].
		1 to: vSteps do: [:n |
 			c := Color black mixed: (n asFloat / vSteps asFloat) with: basicHue.
			c := colorMapper value: c.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y := y + 1].
		x := x + 1].
	y := transHt + 1.
	1 to: vSteps * 2 do: [:n |
 		c := Color black mixed: (n asFloat / (vSteps*2) asFloat) with: Color white.
		c := colorMapper value: c.
		palette fill: (x@y extent: 10@1) fillColor: c.
		y := y + 1].
	^ palette

]

{ #category : 'colormaps' }
Color class >> computeColorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix [
	"Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same"
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color.
		Assumed not to include subpixelAA"
		^ self computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth
	] ifFalse: [
		"source is 16-bit or 32-bit RGB.
		Might include subpixelAA"
		^ self computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix
	]
]

{ #category : 'colormaps' }
Color class >> computeColormapForGray8bppFrom: sourceDepth [
	"Compute a colorMap for translatingfrom the given depth to the 8bpp grays"
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map |
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color"
		map := sourceDepth = 1 ifTrue: [ self oneBitColors ] ifFalse: [ self indexedColors ].
		map := (map copyFrom: 1 to: (1 bitShift: sourceDepth))
					collect: [ :c | (c luminance * 255) rounded].
		map := map as: Bitmap.
		^ map ].
	^ self computeRGBColormapForGray8
]

{ #category : 'colormaps' }
Color class >> computeColormapFrom: sourceDepth to: destDepth [
	"Compute a colorMap for translating between the given depths. A colormap is a Bitmap whose entries contain the pixel values for the destination depth. Typical clients use cachedColormapFrom:to: instead."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map bitsPerColor |
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color"
		map := sourceDepth = 1 ifTrue: [ self oneBitColors ] ifFalse: [ self indexedColors ].
		map := (map copyFrom: 1 to: (1 bitShift: sourceDepth))
					collect: [:c | c pixelValueForDepth: destDepth].
		^map as: Bitmap ].
	"source is 16-bit or 32-bit RGB"
	destDepth > 8
		ifTrue: [bitsPerColor := 5]  "retain maximum color resolution"
		ifFalse: [bitsPerColor := 4].
	^self computeRGBColormapFor: destDepth bitsPerColor: bitsPerColor
]

{ #category : 'colormaps' }
Color class >> computeColormapFromGray8bppForDepth: destDepth [
	"Return a colormap for displaying a GrayForm at the given depth"

	| newMap |
	newMap := Bitmap new: 256.
	1 to: 256 do: [ :i |
		newMap
			at: i
			put: ((Color gray: (i-1) asFloat / 255.0) pixelValueForDepth: destDepth)].
	^ newMap
]

{ #category : 'colormaps' }
Color class >> computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth [
	| map |

	map := sourceDepth = 1 ifTrue: [ self oneBitColors ] ifFalse: [ self indexedColors ].
	map := (map copyFrom: 1 to: (1 bitShift: sourceDepth)) collect: [ :cc | | f c |
		f := 1.0 - (cc red + cc green + cc blue / 3.0 ).
		c := targetColor
			ifNotNil: [
				destDepth = 32
					ifTrue: [ targetColor * f alpha: f]
					ifFalse: [ targetColor alphaMixed: f*1.5 with: `Color white` ]]
			ifNil: [ cc ].
		destDepth = 32
			ifTrue: [ c pixelValueForDepth: destDepth]
			ifFalse: [
				f = 0.0
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ]]].
	^map as: Bitmap
]

{ #category : 'colormaps' }
Color class >> computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix [
	"Builds a colormap intended to convert from subpixelAA black values to targetColor values.
	keepSubPix
		ifTrue: [ Answer colors that also include subpixelAA ]
		ifFalse: [ 
			Take fullpixel luminance level. Apply it to targetColor.
			I.e. answer colors with NO subpixelAA ]"

	| mask map c bitsPerColor r g b f v |

	destDepth > 8
		ifTrue: [bitsPerColor := 5]  "retain maximum color resolution"
		ifFalse: [bitsPerColor := 4].
	"Usually a bit less is enough, but make it configurable"
	bitsPerColor := bitsPerColor min: (Preferences at: #aaFontsColormapDepth).
	mask := (1 bitShift: bitsPerColor) - 1.
	map := Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		r := (i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask.
		g := (i bitShift: 0 - bitsPerColor) bitAnd: mask.
		b := (i bitShift: 0) bitAnd: mask.
		f := 1.0 - (r + g + b / 3.0 / mask).
		c := targetColor
			ifNotNil: [
				(keepSubPix and: [destDepth > 8]) ifTrue: [
						Color
							r: 1.0 - (r asFloat/mask) * targetColor red
							g: 1.0 - (g asFloat/mask) * targetColor green
							b: 1.0 - (b asFloat/mask) * targetColor blue
							alpha: f	* targetColor alpha "alpha will be ignored below, in #pixelValueForDepth: if destDepth ~= 32" ]
				ifFalse: [
					destDepth = 32
						ifTrue: [ targetColor * f alpha: f * targetColor alpha ]
						ifFalse: [ targetColor alphaMixed: f*1.5 with: `Color white` ]]]
			ifNil: [ Color r: r g: g b: b range: mask].	"This is currently used only to keep some SubPixelAA on destDepth = 8, using a single pass of rule 25"
		v := destDepth = 32
			ifTrue: [ c pixelValueForDepth: destDepth]
			ifFalse: [
				f < 0.1
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ]].
		map at: i + 1 put: v ].
	^ map
]

{ #category : 'colormaps' }
Color class >> computeRGBColormapFor: destDepth bitsPerColor: bitsPerColor [
	"Compute a colorMap for translating from 16-bit or 32-bit RGB color to the given depth, using the given number of of bits per color component."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| mask map c |
	(#(3 4 5) includes: bitsPerColor)
		ifFalse: [self error: 'BitBlt only supports 3, 4, or 5 bits per color component'].
	mask := (1 bitShift: bitsPerColor) - 1.
	map := Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		c := Color
			r: ((i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask)
			g: ((i bitShift: 0 - bitsPerColor) bitAnd: mask)
			b: ((i bitShift: 0) bitAnd: mask)
			range: mask.
		map at: i + 1 put: (c pixelValueForDepth: destDepth)].

	map at: 1 put: (`Color transparent` pixelWordForDepth: destDepth).  "zero always transparent"
	^ map

]

{ #category : 'colormaps' }
Color class >> computeRGBColormapForGray8 [
	"Compute a colorMap for translating from 16-bit or 32-bit RGB color to 8bpp grays, using the default number of of bits per color component."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| bitsPerColor mask map c |
	bitsPerColor := 5.
	mask := (1 bitShift: bitsPerColor) - 1.
	map := Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		c := Color
			r: ((i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask)
			g: ((i bitShift: 0 - bitsPerColor) bitAnd: mask)
			b: ((i bitShift: 0) bitAnd: mask)
			range: mask.
		map at: i + 1 put: (c luminance * 255) rounded].

	^ map
]

{ #category : 'named colors' }
Color class >> cyan [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#00ffff'`
]

{ #category : 'black-white-grays' }
Color class >> darkGray [
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.44)`
]

{ #category : 'examples' }
Color class >> experimentsTowardsANewColorPalette [
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
	Color experimentsTowardsANewColorPaletteOKLCH: selectedColor.
]

{ #category : 'examples' }
Color class >> experimentsTowardsANewColorPalette: selectedColor [
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
DisplayScreen forceDeferredToScreen.
]

{ #category : 'examples' }
Color class >> experimentsTowardsANewColorPaletteHSL: selectedColor [
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
DisplayScreen forceDeferredToScreen.
]

{ #category : 'examples' }
Color class >> experimentsTowardsANewColorPaletteHSV: selectedColor [
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
DisplayScreen forceDeferredToScreen.
]

{ #category : 'examples' }
Color class >> experimentsTowardsANewColorPaletteOKLCH: selectedColor [
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
DisplayScreen forceDeferredToScreen.
]

{ #category : 'examples' }
Color class >> experimentsTowardsANewColorPaletteOKLCH2: selectedColor [
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
DisplayScreen forceDeferredToScreen.
]

{ #category : 'instance creation' }
Color class >> fromArray: colorDef [

	| answer |
	colorDef size = 0 ifTrue: [^ `Color transparent` ].
	(colorDef size between: 3 and: 4) ifFalse: [self error: 'Undefined color definition'].
	answer := self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3).
	^colorDef size = 3
		ifTrue: [answer]
		ifFalse: [answer alpha: (colorDef at: 4)]
]

{ #category : 'instance creation' }
Color class >> fromHexString: aString [
	"For HTML color spec: #FFCCAA.
	See http://www.w3schools.com/cssref/css_colors_legal.asp
	Also handles 3 digit shorthand."
	"
	Color fromHexString: '#FFCCAA'.
	"
	(aString size = 7 and: [ aString first == $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU := aString asUppercase.
		red := ('16r', (aColorHexU copyFrom: 2 to: 3)) asNumber/255.
		green := ('16r', (aColorHexU copyFrom: 4 to: 5)) asNumber/255.
		blue := ('16r', (aColorHexU copyFrom: 6 to: 7)) asNumber/255.
		^ self r: red g: green b: blue].
	(aString size = 4 and: [ aString first == $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU := aString asUppercase.
		red := aColorHexU at: 2.
		green := aColorHexU at: 3.
		blue := aColorHexU at: 4.
		red := ('16r', (String with: red with: red)) asNumber/255.
		green := ('16r', (String with: green with: green)) asNumber/255.
		blue := ('16r', (String with: blue with: blue)) asNumber/255.
		^ self r: red g: green b: blue].
	^ nil
]

{ #category : 'instance creation' }
Color class >> fromRgbOrRgba: list [
	^ list size caseOf: {
		[0] -> [ `Color transparent` ].
		[3] -> [ Color r: list first g: list second b: list third ].
		[4] -> [ TranslucentColor r: list first g: list second b: list third alpha: list fourth ]
	}
]

{ #category : 'instance creation' }
Color class >> fromRgbTriplet: list [
	^ self r: list first g: list second b: list last
]

{ #category : 'black-white-grays' }
Color class >> gray [
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.56)`
]

{ #category : 'instance creation' }
Color class >> gray: brightness [
	"Return a gray shade with the given brightness in the range [0.0..1.0]."

	^ self new
		setRed: brightness
		green: brightness
		blue: brightness
]

{ #category : 'cached state access' }
Color class >> grayToIndexMap [
	"
	Color grayToIndexMap explore.
	"
	GrayToIndexMap ifNil: [
		self initializeGrayToIndexMap ].
	^GrayToIndexMap
]

{ #category : 'named colors' }
Color class >> green [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#15b01a'`
]

{ #category : 'instance creation' }
Color class >> h: hue s: saturation v: brightness [
	"Create a color with the given hue, saturation, and brightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1) is pure red."
	"Note: By convention, brightness is abbreviated 'v' to to avoid confusion with blue."

	^ self new setHue: hue saturation: saturation brightness: brightness
]

{ #category : 'instance creation' }
Color class >> h: h s: s v: v alpha: alpha [

	^ (self h: h s: s v: v) alpha: alpha
]

{ #category : 'examples' }
Color class >> hotColdShades: thisMany [
	"An array of thisMany colors showing temperature from blue to red to white hot.  (Later improve this by swinging in hue.)  "
	"Color showColors: (Color hotColdShades: 25)"

	| n s1 s2 s3 s4 s5 |
	thisMany < 5 ifTrue: [^ self error: 'must be at least 5 shades'].
	n := thisMany // 5.
	s1 := self white mix: self yellow shades: (thisMany - (n*4)).
	s2 := self yellow mix: self red shades: n+1.
	s2 := s2 copyFrom: 2 to: n+1.
	s3 := self red mix: self green darker shades: n+1.
	s3 := s3 copyFrom: 2 to: n+1.
	s4 := self green darker mix: self blue shades: n+1.
	s4 := s4 copyFrom: 2 to: n+1.
	s5 := self blue mix: self black shades: n+1.
	s5 := s5 copyFrom: 2 to: n+1.
	^ s1, s2, s3, s4, s5

]

{ #category : 'instance creation' }
Color class >> hue: hue chroma: chroma brightness: brightness [
	^self new setHue: hue chroma: chroma brightness: brightness
]

{ #category : 'instance creation' }
Color class >> hue: hue chroma: chroma luminance: luma [
	^self new setHue: hue chroma: chroma luminance: luma
]

{ #category : 'instance creation' }
Color class >> hue: hue saturation: saturation brightness: brightness [
	"Create a color with the given hue, saturation, and brightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1) is pure red."
	"Note: By convention, brightness is abbreviated 'v' to to avoid confusion with blue."

	^ self h: hue s: saturation v: brightness
]

{ #category : 'cached state access' }
Color class >> indexedColors [
	"
	Color indexedColors explore.
	"
	IndexedColors ifNil: [
		self initializeIndexedColors ].
	^IndexedColors
]

{ #category : 'class initialization' }
Color class >> initializeGrayToIndexMap [
	"Build an array of gray values available in the 8-bit colormap. This array is indexed by a gray level between black (1) and white (256) and returns the pixel value for the corresponding gray level."
	"Note: This method must be called after initializeIndexedColors, since it uses IndexedColors."
	"
	Color initializeGrayToIndexMap
	"

	| grayLevels grayIndices c distToClosest dist indexOfClosest indexedColors |
	"record the level and index of each gray in the 8-bit color table"
	grayLevels := OrderedCollection new.
	grayIndices := OrderedCollection new.
	indexedColors := self indexedColors.
	"Note: skip the first entry, which is reserved for transparent"
	2 to: indexedColors size do: [:i |
		c := indexedColors at: i.
		c saturation = 0.0 ifTrue: [  "c is a gray"
			grayLevels add: (c green * 255) rounded.  "0 to 255; R, G, and B are the same"
			grayIndices add: i - 1]].  "pixel values are zero-based"
	grayLevels := grayLevels asArray.
	grayIndices := grayIndices asArray.

	"for each gray level in [0..255], select the closest match"
	GrayToIndexMap := ByteArray new: 256.
	0 to: 255 do: [ :level |
		distToClosest := 10000.  "greater than distance to any real gray"
		1 to: grayLevels size do: [:i |
			dist := (level - (grayLevels at: i)) abs.
			dist < distToClosest ifTrue: [
				distToClosest := dist.
				indexOfClosest := grayIndices at: i]].
		GrayToIndexMap at: (level + 1) put: indexOfClosest]
]

{ #category : 'class initialization' }
Color class >> initializeIndexedColors [
	"Build an array of colors corresponding to the fixed colormap used
	 for depths of 2, 4, or 8 bits."
	"
	Color initializeIndexedColors
	"
	"
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 1 g: 0 b: 0); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0 g: 1 b: 0); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0 g: 0 b: 1); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0.5 g: 0.5 b: 0.5); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color blue; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color green; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color red; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color cyan; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color yellow; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color magenta; display.
	"

	| a index grayVal |
	a := Array new: 256.

	"1-bit colors (monochrome)"
	a at: 1 put: `Color transparent`.								"Note: For 1bpp forms, it is white"
	a at: 2 put: `Color r: 0.0 g: 0.0 b: 0.0`.								"black"

	"additional colors for 2-bit color"
	a at: 3 put: `Color r: 1.0 g: 1.0 b: 1.0`.	"opaque white"
	a at: 4 put: `Color r: 0.5 g: 0.5 b: 0.5`.	"1/2 gray"

	"additional colors for 4-bit color"
	a at:  5 put: Color red.
	a at:  6 put: Color green.
	a at:  7 put: Color blue.
	a at:  8 put: Color cyan.
	a at:  9 put: Color yellow.
	a at: 10 put: Color magenta.

	a at: 11 put: `Color r: 0.125 g: 0.125 b: 0.125`.		"1/8 gray"
	a at: 12 put: `Color r: 0.25 g: 0.25 b: 0.25`.			"2/8 gray"
	a at: 13 put: `Color r: 0.375 g: 0.375 b: 0.375`.		"3/8 gray"
	a at: 14 put: `Color r: 0.625 g: 0.625 b: 0.625`.		"5/8 gray"
	a at: 15 put: `Color r: 0.75 g: 0.75 b: 0.75`.			"6/8 gray"
	a at: 16 put: `Color r: 0.875 g: 0.875 b: 0.875`.		"7/8 gray"

	"additional colors for 8-bit color"
	"24 more shades of gray (1/32 increments but not repeating 1/8 increments)"
	index := 17.
	1 to: 31 do: [:v |
		(v \\ 4) = 0 ifFalse: [
			grayVal := v / 32.0.
			a at: index put: (Color r: grayVal g: grayVal b: grayVal).
			index := index + 1]].

	"The remainder of color table defines a color cube with six steps
	 for each primary color. Note that the corners of this cube may repeat
	 previous colors, but this simplifies the mapping between RGB colors
	 and color map indices. This color cube spans indices 40 through 255
	 (indices 41-256 in this 1-based array)."
	0 to: 5 do: [:r |
		0 to: 5 do: [:g |
			0 to: 5 do: [:b |
				index := 41 + ((36 * r) + (6 * b) + g).
				index > 256 ifTrue: [
					self error: 'index out of range in color table compuation'].
				a at: index put: (Color r: r g: g b: b range: 5)]]].

	IndexedColors := a.

]

{ #category : 'instance creation' }
Color class >> jet: fraction [
	"Answer a suitable color for a HeatMap using the 'jet' color scheme.
	See https://en.wikipedia.org/wiki/Heat_map
	
	Color showColors: ((0.0 to: 1.0 count: 100) collect: [ :f | Color jet: f ])
	"
	
	| hue |
	hue := Color blue hue interpolateTo: Color red hue at: fraction.
	^ Color h: hue s: 1 v: 1
]

{ #category : 'named colors' }
Color class >> lightBlue [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#95d0fc'`
]

{ #category : 'named colors' }
Color class >> lightBrown [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#ad8150'`
]

{ #category : 'named colors' }
Color class >> lightCyan [
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 0.674 g: 1.0 b: 0.988`
]

{ #category : 'black-white-grays' }
Color class >> lightGray [
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.67)`
]

{ #category : 'named colors' }
Color class >> lightGreen [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#96f97b'`
]

{ #category : 'named colors' }
Color class >> lightMagenta [
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 0.98 g: 0.372 b: 0.969`
]

{ #category : 'named colors' }
Color class >> lightOrange [
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 0.992 g: 0.667 b: 0.283`
]

{ #category : 'named colors' }
Color class >> lightRed [
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 1.0 g: 0.279 b: 0.298`
]

{ #category : 'named colors' }
Color class >> lightYellow [
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 1.0 g: 0.996 b: 0.478`
]

{ #category : 'color space conversions' }
Color class >> linearTosRGBGamma: value [
	"The sRGB standard specifies a gamma curve that is close to gamma = 2.2, but is actually
	built from one stright line segment, and a curve with exponent = 2.4.
	This method converts value from linearSpace to gammaSpace, i.e. it 'encodes' with gamma.
	This is the operation done when producing an 8bit Form for displaying, or for saving on a JPG, PNG, etc; if source data is in linear space (for example, from raw data a sensor image, or data that was converted previously to linear space for proper image processing).
	value must be in [0.0 .. 1.0].

	See
		http://en.wikipedia.org/wiki/Gamma_correction
		http://www.poynton.com/notes/colour_and_gamma/GammaFAQ.html
		http://entropymine.com/imageworsener/srgbformula
	
	| r v |
	r := Random new.
	1000 timesRepeat: [
		v := r next.
		self assert: ((Color sRGBGammaToLinear: (Color linearTosRGBGamma: v)) closeTo: v) ]

	| r |
	r := 10@10 extent: 600@400.
	Display fill: r fillColor: Color white.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color linearTosRGBGamma: v ]) in: r color: Color black min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color applySimpleGamma: 1/2.2 to: v ]) in: r color: Color blue min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color sRGBGammaToLinear: v ]) in: r color: Color black min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color applySimpleGamma: 2.2 to: v ]) in: r color: Color blue min: 0.0 max: 1.0.
	"

	^ value <= 0.00313066844250063
		ifTrue: [ value * 12.92 ]
		ifFalse: [ (value raisedTo: 1.0/2.4) * 1.055 - 0.055 ]
]

{ #category : 'named colors' }
Color class >> magenta [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#c20078'`
]

{ #category : 'other' }
Color class >> maskingMap: depth [
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero. Cache the most recently used map."
	"Warning: The behavior is incorrect if depth = 32.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| sizeNeeded |
	depth <= 8
		ifTrue: [sizeNeeded := 1 bitShift: depth]
		ifFalse: [sizeNeeded := 4096].

	(MaskingMap == nil or: [MaskingMap size ~= sizeNeeded]) ifTrue:
		[MaskingMap := Bitmap new: sizeNeeded withAll: 16rFFFFFFFF.
		MaskingMap at: 1 put: 0.  "transparent"].

	^ MaskingMap

]

{ #category : 'instance creation' }
Color class >> numSlots [
	^3
]

{ #category : 'instance creation' }
Color class >> okOrNilL: lightness c: chroma h: hue [
	"Create a color from OKLCH: lightness (0.0 - 1.0), chroma (0.0 - ~0.4), hue (0.0 - 360.0).
	Return nil if the resulting color is out of the sRGB gamut (and range of Color)."
	| h A B L M S r g b |
	"OKLCH to OKLAB (Cylindrical to Cartesian)"
	h := hue * Float pi / 180.0.
	A := chroma * h cos.
	B := chroma * h sin.
    	"OKLAB to Linear LMS
	Use the standard OKLAB matrices to move to LMS cone response space.
	Note: The original non-linearity in OKLAB is a cube root, so we cube here to undo it."
	L := (lightness + (0.3963377774*A) + (0.2158037573*B)) cubed.
	M := (lightness - (0.1055613458*A) - (0.0638541728*B)) cubed.
	S := (lightness - (0.0894841775*A) - (1.2914855480*B)) cubed.
	"Linear LMS to Linear sRGB (matrix multiplication to shift from Cone response to RGB channels),
	and then Linear sRGB to sRGB (Gamma Correction)"
	r := self linearTosRGBGamma: 4.0767416621*L - (3.3077115913*M) + (0.2309699292*S).
	g := self linearTosRGBGamma: -1.2684380046*L + (2.6097574011*M) - (0.3413193965*S).
	b := self linearTosRGBGamma: -0.0041960863*L - (0.7034186147*M) + (1.7076147010*S).
	^ ((r between: 0.0 and: 1.0) and: [(g between: 0.0 and: 1.0) and: [b between: 0.0 and: 1.0]])
		ifTrue: [self r: r g: g b: b]
]

{ #category : 'instance creation' }
Color class >> okl: lightness c: chroma h: hue [
	"Create a color from OKLCH: lightness (0.0 - 1.0), chroma (0.0 - ~0.4), hue (0.0 - 360.0).
	If the color is out of sRGB gamut, reduce chroma until it fits. Use binary search for efficiency.
	
	See https://en.wikipedia.org/wiki/Oklab_color_space
	See https://evilmartians.com/chronicles/oklch-in-css-why-quit-rgb-hsl

		| step |
		step := 360 / 10.0.
		ColoredBoxMorph allInstancesDo: [ :a | a delete ].
		0 to: 360 - step by: step :: collect: [:h|
			(ColoredBoxMorph new color: (Color okl: 0.75 c: 0.4 h: h)) ]
	"
	| answer minChroma maxChroma midChroma |
	(answer := self okOrNilL: lightness c: chroma h: hue) ifNotNil: [^ answer].
	"The color is impossible to represent as an instance of Color, so find the maximum chroma that fits:"
	minChroma := 0.0.
	maxChroma := chroma.
	"Binary search. 20 iterations is enough precision for any screen."
	20 timesRepeat:
		[midChroma := minChroma + maxChroma / 2.0.
		(self okOrNilL: lightness c: midChroma h: hue)
			ifNil: [maxChroma := midChroma]
			ifNotNil: [:aColor| answer := aColor. minChroma := midChroma]].
	^ answer
]

{ #category : 'class initialization' }
Color class >> oneBitColors [
	"Answer the colors available for 1 bit Forms.
	Note: For depths 2 to 8, first entry is Color transparent, not white. See #initializeIndexedColors"

	^ `{ Color white. Color black }`
]

{ #category : 'named colors' }
Color class >> orange [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#f97306'`
]

{ #category : 'named colors' }
Color class >> pink [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#ff81c0'`
]

{ #category : 'named colors' }
Color class >> purple [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#7e1e9c'`
]

{ #category : 'instance creation' }
Color class >> r: r g: g b: b [
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."

	^ self new
		setRed: r
		green: g
		blue: b
]

{ #category : 'instance creation' }
Color class >> r: r g: g b: b alpha: alpha [
	^ (self r: r g: g b: b) alpha: alpha
]

{ #category : 'instance creation' }
Color class >> r: r g: g b: b range: range [
	"Return a color with the given r, g, and b components specified as integers in the range [0..r]. This avoids the floating point arithmetic in the red:green:blue: message and is thus a bit faster for certain applications (such as computing a sequence of colors for a palette)."

	^ self new setRed: r green: g blue: b range: range
]

{ #category : 'instance creation' }
Color class >> random [
	"Return a random color that isn't too dark or under-saturated.
		Display fill: (10@10 extent: 200@200) fillColor: Color random
	"

	^ Random withDefaultDo: [ :random |
		self random: random ]
]

{ #category : 'instance creation' }
Color class >> random: aRandom [
	"Return a random color that isn't too dark or under-saturated."

	^ self new
		setHue: (360.0 * aRandom next)
		saturation: (0.3 + (aRandom next * 0.7))
		brightness: (0.4 + (aRandom next * 0.6))
]

{ #category : 'instance creation' }
Color class >> random2 [
	"Return a random color with a distribution that spans over all possible colors.
		Display fill: (10@10 extent: 200@200) fillColor: Color random2
	"

	^ Random withDefaultDo: [ :random |
		self random2: random ]
]

{ #category : 'instance creation' }
Color class >> random2: aRandom [
	"Return a random color with a distribution that spans over all possible colors."

	^ self new
		setRed: aRandom next
		green: aRandom next
		blue: aRandom next
]

{ #category : 'named colors' }
Color class >> red [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#e50000'`
]

{ #category : 'cached state access' }
Color class >> releaseClassState [
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."
	"
	Color releaseClassState
	"
	CachedColormaps := nil. 			"Maps to translate between color depths"
	ToGrayColormaps := nil.			"Maps colors to 8bpp grays for various color depths"
	FromGrayColormaps := nil.		"Maps from 8bpp grays to colors of various depths"
	MaskingMap := nil. 				"Maps all colors except transparent to black for creating a mask"
	IndexedColors := nil.
	GrayToIndexMap := nil
]

{ #category : 'color space conversions' }
Color class >> sRGBGammaToLinear: value [
	"The sRGB standard specifies a gamma curve that is close to gamma = 2.2, but is actually
	built from one stright line segment, and a curve with exponent = 2.4.
	This method converts value from gammaSpace to linearSpace, i.e. it 'decodes'.
	This is the operation done by display screens. It is also needed to convert 8bit Forms (from a frameBuffer, or from PNG, JPG, etc.) to linear space for proper image processing.
	value must be in [0.0 .. 1.0].

	See
		http://en.wikipedia.org/wiki/Gamma_correction
		http://www.poynton.com/notes/colour_and_gamma/GammaFAQ.html
		http://entropymine.com/imageworsener/srgbformula
	"

	^ value <= 0.0404482362771082
		ifTrue: [ value / 12.92 ]
		ifFalse: [ value + 0.055 / 1.055 raisedTo: 2.4 ]
]

{ #category : 'examples' }
Color class >> showColorCube [
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

]

{ #category : 'examples' }
Color class >> showColors: colorList [
	"Display the given collection of colors across the top of the Display."

	| w r |
	w := DisplayScreen deferredUpdatingDisplay width // colorList size.
	r := 0@0 extent: w@((w min: 30) max: 10).
	colorList do: [:c |
		DisplayScreen deferredUpdatingDisplay fill: r fillColor: c.
		r := r translatedBy: w@0].
	DisplayScreen forceDeferredToScreen.
]

{ #category : 'examples' }
Color class >> showHSVPalettes [
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
	DisplayScreen forceDeferredToScreen.
]

{ #category : 'examples' }
Color class >> showHuesInteractively [
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
	^ (s min: 1.0) @ (v min: 1.0)
]

{ #category : 'named colors' }
Color class >> tan [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#d1b26f'`
]

{ #category : 'named colors' }
Color class >> transparent [
	^ `TranslucentColor r: 0.0 g: 0.0 b: 0.0 alpha: 0.0`
]

{ #category : 'black-white-grays' }
Color class >> veryDarkGray [
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.33)`
]

{ #category : 'black-white-grays' }
Color class >> veryLightGray [
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.78)`
]

{ #category : 'black-white-grays' }
Color class >> veryVeryDarkGray [
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.22)`
]

{ #category : 'black-white-grays' }
Color class >> veryVeryLightGray [
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.89)`
]

{ #category : 'examples' }
Color class >> wheel: thisMany [
	"Return a collection of thisMany colors evenly spaced around the color wheel."
	"Color showColors: (Color wheel: 12)"

	^ Color wheel: thisMany saturation: 0.9 brightness: 0.7

]

{ #category : 'examples' }
Color class >> wheel: thisMany saturation: s brightness: v [
	"Return a collection of thisMany colors evenly spaced around the color wheel, all of the given saturation and brightness."
	"Color showColors: (Color wheel: 12 saturation: 0.4 brightness: 1.0)"
	"Color showColors: (Color wheel: 12 saturation: 0.8 brightness: 0.5)"

	^ (Color h: 0.0 s: s v: v) wheel: thisMany

]

{ #category : 'black-white-grays' }
Color class >> white [
	^ `Color r: 1.0 g: 1.0 b: 1.0`
]

{ #category : 'named colors' }
Color class >> yellow [
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#ffff14'`
]

{ #category : 'transformations' }
Color >> * aNumberOrColor [
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Color brown *2) display
	"
	(aNumberOrColor is: #Color) ifTrue: [
		^ (Color new
			setRed: (self red * aNumberOrColor red min: 1.0 max: 0.0)
			green: (self green * aNumberOrColor green min: 1.0 max: 0.0)
			blue: (self blue * aNumberOrColor blue min: 1.0 max: 0.0))
				alpha: self alpha * aNumberOrColor alpha
		].
	^ (Color new
		setRed: (self red * aNumberOrColor min: 1.0 max: 0.0)
		green: (self green * aNumberOrColor min: 1.0 max: 0.0)
		blue: (self blue * aNumberOrColor min: 1.0 max: 0.0))
			alpha: self alpha
]

{ #category : 'transformations' }
Color >> + aColor [
	"Answer this color mixed with the given color in an additive color space.  "
	"
	(Color blue + Color green) display
	"
	^ Color new
		setRed: (self red + aColor red min: 1.0 max: 0.0)
		green: (self green + aColor green min: 1.0 max: 0.0)
		blue: (self blue + aColor blue min: 1.0 max: 0.0)
]

{ #category : 'transformations' }
Color >> - aColor [
	"Answer aColor is subtracted from the given color in an additive color space.  "
	"
	(Color white - Color red) display
	"
	^ Color new
		setRed: (self red - aColor red min: 1.0 max: 0.0)
		green: (self green - aColor green min: 1.0 max: 0.0)
		blue: (self blue - aColor blue min: 1.0 max: 0.0)
]

{ #category : 'transformations' }
Color >> / aNumberOrColor [
	"Answer this color with its RGB divided by the given number. "
	"
	(Color red / 2) display
	"
	(aNumberOrColor is: #Color) ifTrue: [
		^ Color new
			setRed: (self red / aNumberOrColor red min: 1.0 max: 0.0)
			green: (self green / aNumberOrColor green min: 1.0 max: 0.0)
			blue: (self blue / aNumberOrColor blue min: 1.0 max: 0.0)
		].
	^ Color new
		setRed: (self red / aNumberOrColor min: 1.0 max: 0.0)
		green: (self green / aNumberOrColor min: 1.0 max: 0.0)
		blue: (self blue / aNumberOrColor min: 1.0 max: 0.0)
]

{ #category : 'comparing' }
Color >> = another  [
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	^self primitiveEqual: another
]

{ #category : 'transformations' }
Color >> adjustBrightness: brightness [
	"Adjust the relative brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ Color
		h: self hue
		s: self saturation
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha
]

{ #category : 'transformations' }
Color >> adjustSaturation: saturation brightness: brightness [
	"Adjust the relative saturation and brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	| v |
	v := self brightness + brightness min: 1.0 max: 0.005.
	self saturation < 0.005 ifTrue: [
		"Don't adjust saturation if gray. Hue is meaningless."
		^ (Color gray: v) alpha: self alpha ].
	^ Color
		h: self hue
		s: (self saturation + saturation min: 1.0 max: 0.005)
		v: v
		alpha: self alpha
]

{ #category : 'accessing' }
Color >> alpha [
	"Return the opacity ('alpha') value of opaque so that normal colors can be compared to TranslucentColors."

	^ 1.0

]

{ #category : 'transformations' }
Color >> alpha: alphaValue [
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifFalse: [
		^ TranslucentColor new
			setRed: self red
			green: self green
			blue: self blue
			alpha: alphaValue ]
]

{ #category : 'transformations' }
Color >> alphaMixed: proportion with: aColor  [
	"Answer this color mixed with the given color. The proportion, a number 
	between 0.0 and 1.0, determines what what fraction of the receiver to  
	use in the mix. For example, 0.9 would yield a color close to the  
	receiver. This method uses RGB interpolation; HSV interpolation can lead 
	to surprises.  Mixes the alphas (for transparency) also."

	| frac1 frac2 |
	frac1 := proportion asFloat min: 1.0 max: 0.0.
	frac2 := 1.0 - frac1.
	^ Color
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)
		alpha: self alpha * frac1 + (aColor alpha * frac2)
]

{ #category : 'conversions' }
Color >> asNontranslucentColor [
	^ self
]

{ #category : 'transformations' }
Color >> atLeastAsLuminentAs: aFloat [

	| revisedColor |
	revisedColor := self.
	[revisedColor luminance < aFloat] whileTrue: [revisedColor := revisedColor slightlyLighter].
	^revisedColor

]

{ #category : 'transformations' }
Color >> atMostAsLuminentAs: aFloat [

	| revisedColor |
	revisedColor := self.
	[revisedColor luminance > aFloat] whileTrue: [revisedColor := revisedColor slightlyDarker].
	^revisedColor

]

{ #category : 'private' }
Color >> attemptToMutateError [
	"A color is immutable. Once a color's red, green, and blue have been initialized, you cannot change them. Instead, create a new Color and use it."

	self error: 'Color objects are immutable once created'

]

{ #category : 'private' }
Color >> basicSetRed: r green: g blue: b [
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	self
		float32SlotAt: 1 put: r;
		float32SlotAt: 2 put: g;
		float32SlotAt: 3 put: b
]

{ #category : 'conversions' }
Color >> bitPatternForDepth: depth [
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"

	^ Bitmap with: (self pixelWordForDepth: depth)
]

{ #category : 'conversions' }
Color >> bitPatternForGrayForm [
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"
	"Details: The pattern for the most recently requested depth is cached."

	^Bitmap with: (self pixelWordFor: 8 filledWith: (self luminance * 255) rounded)
]

{ #category : 'transformations' }
Color >> blacker [

	^ self alphaMixed: 0.8333 with: `Color black`

]

{ #category : 'accessing' }
Color >> blue [
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ self float32SlotAt: 3
]

{ #category : 'accessing' }
Color >> brightness [
	"Return the brightness of this color, a float in the range [0.0..1.0].
	Color red brightness
	"

	"Do not include alpha if TranslucentColor"
	^ (self red max: self green) max: self blue
]

{ #category : 'accessing' }
Color >> chroma [
	"Return the chroma of this color, a value between 0.0 and 1.0, somewhat related to saturation.
	See http://en.wikipedia.org/wiki/HSL_and_HSV
	Color red chroma
	Color gray chroma
	"

	| r g b max min |
	r := self red.
	g := self green.
	b := self blue. 
	max := (r max: g) max: b.
	min := (r min: g) min: b.
	^max - min
]

{ #category : 'private' }
Color >> clipToValidValues [
	| v |
	1 to: self size do: [ :i |
		v := self float32SlotAt: i.
		v > 1 ifTrue: [self float32SlotAt: i put: 1.0].
		v < 0 ifTrue: [self float32SlotAt: i put: 0.0]]
]

{ #category : 'conversions' }
Color >> closestPixelValue1 [
	"Return the nearest approximation to this color for a monochrome Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 0].  "white"

	self luminance > 0.5
		ifTrue: [^ 0]  "white"
		ifFalse: [^ 1].  "black"
]

{ #category : 'conversions' }
Color >> closestPixelValue2 [
	"Return the nearest approximation to this color for a 2-bit deep Form."

	| lum |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 2].  "opaque white"

	lum := self luminance.
	lum < 0.2 ifTrue: [^ 1].  "black"
	lum > 0.6 ifTrue: [^ 2].  "opaque white"
	^ 3  "50% gray"

]

{ #category : 'conversions' }
Color >> closestPixelValue4 [
	"Return the nearest approximation to this color for a 4-bit deep Form."

	| bIndex |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 2].  "opaque white"

	self isRed ifTrue: [^ 4].
	self isGreen ifTrue: [^ 5].
	self isBlue ifTrue: [^ 6].
	self isCyan ifTrue: [^ 7].
	self isYellow ifTrue: [^ 8].
	self isMagenta ifTrue: [^ 9].

	bIndex := (self luminance * 8.0) rounded.  "bIndex in [0..8]"
	^ #(
		1	"black"
		10	"1/8 gray"
		11	"2/8 gray"
		12	"3/8 gray"
		3	"4/8 gray"
		13	"5/8 gray"
		14	"6/8 gray"
		15	"7/8 gray"
		2	"opaque white"
	) at: bIndex + 1.

]

{ #category : 'conversions' }
Color >> closestPixelValue8 [
	"Return the nearest approximation to this color for an 8-bit deep Form.
	Note: Answer values in the standard Color indexedColors.
	Not useful for ColorForms with custom palettes!"

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 255].  "white"

	^self saturation < 0.2
		ifTrue: [
			^ self class grayToIndexMap at:(self green * 255) rounded + 1.  "nearest gray"
			]
		ifFalse: [
			"compute nearest entry in the color cube"
			40 + 
				((self red * 5) rounded * 36) +
				((self blue * 5) rounded * 6) +
				(self green * 5) rounded ]
]

{ #category : 'accessing' }
Color >> color [
	^ self
]

{ #category : 'other' }
Color >> colorName [
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color.
	Return nil if named color support is not present"

	Color classPool 
		at: #ColorNamesDict
		ifPresent: [ :dict | ^dict keyAtValue: self ifAbsent: nil].
	^nil
]

{ #category : 'object serialization' }
Color >> convertToCurrentVersion: varDict refStream: smartRefStrm [

	"subclasses should implement if they wish to convert old instances to modern ones"
	self size = 0 ifTrue: [
		^ Color new copyFrom: (varDict at: 'floatRGB') ].
	^ self
]

{ #category : 'transformations' }
Color >> dansDarker [
	"Return a darker shade of the same color.
	An attempt to do better than the current darker method.
	(now obsolete, since darker has been changed to do this. -dew)"
	^ Color h: self hue s: self saturation
		v: (self brightness - 0.16 max: 0.0)
]

{ #category : 'groups of shades' }
Color >> darkShades: thisMany [
	"An array of thisMany colors from black to the receiver.  Array is of length num. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red darkShades: 12)"

	^ self class black mix: self shades: thisMany

]

{ #category : 'transformations' }
Color >> darker [
	"Answer a darker shade of this color."

	^ self adjustBrightness: -0.08
]

{ #category : 'equality' }
Color >> diff: theOther [
	"Returns a number between 0.0 and 1.0.
	Color gray diff: Color red
	"
	^(self - theOther ) brightness max: (theOther -self ) brightness
]

{ #category : 'conversions' }
Color >> dominantColor [
	^ self
]

{ #category : 'transformations' }
Color >> duller [

	^ self adjustSaturation: -0.03 brightness: -0.2
]

{ #category : 'accessing' }
Color >> green [
	"Return the green component of this color, a float in the range [0.0..1.0]."

	^ self float32SlotAt: 2
]

{ #category : 'comparing' }
Color >> hash [
	| hash |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	hash := (self species hash + self size hash) hashMultiply.
	1 to: self size do: [ :i | hash := (hash + (self basicAt: i)) hashMultiply].
	^hash
]

{ #category : 'printing' }
Color >> hexStringRGB [
	"
	Color fromUser hexStringRGB
	"
	^String streamContents: [ :strm |
		(self red * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self green * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self blue * 255) rounded printOn: strm base: 16 length: 2 padded: true ]
]

{ #category : 'accessing' }
Color >> hue [
	"Return the hue of this color, an angle in the range [0.0..360.0].
	Color orange hue
	"

	| r g b max min span h |
	r := self red.
	g := self green.
	b := self blue. 
	max := (r max: g) max: b.
	min := (r min: g) min: b.
	span := (max - min) asFloat.
	span = 0.0 ifTrue: [ ^ 0.0 ].

	r = max ifTrue: [
		h := ((g - b) asFloat / span) * 60.0.
	] ifFalse: [
		g = max
			ifTrue: [ h := 120.0 + (((b - r) asFloat / span) * 60.0). ]
			ifFalse: [ h := 240.0 + (((r - g) asFloat / span) * 60.0). ].
	].

	^h mod: 360.0
]

{ #category : 'accessing' }
Color >> icon [
	"Answer a swatch to display in a menu or browser"
	^self swatch
]

{ #category : 'accessing' }
Color >> iconOrThumbnailOfSize: aNumberOrPoint  [
	"Answer an appropiate form to represent the receiver"
	| form |
	form := Form extent: aNumberOrPoint asPoint asPoint depth: 32.
	form fillColor: self.
	^ form
]

{ #category : 'conversions' }
Color >> indexInMap: aColorMap [
	"Return the index corresponding to this color in the given color map. RGB colors are truncated to 3-, 4-, or 5-bits per color component when indexing into such a colorMap.  "

	aColorMap size = 2 ifTrue: [^ (self pixelValueForDepth: 1) + 1].
	aColorMap size = 4 ifTrue: [^ (self pixelValueForDepth: 2) + 1].
	aColorMap size = 16 ifTrue: [^ (self pixelValueForDepth: 4) + 1].
	aColorMap size = 256 ifTrue: [^ (self pixelValueForDepth: 8) + 1].
	aColorMap size = 512 ifTrue: [^ (self pixelValueForDepth: 9) + 1].
	aColorMap size = 4096 ifTrue: [^ (self pixelValueForDepth: 12) + 1].
	aColorMap size = 32768 ifTrue: [^ (self pixelValueForDepth: 15) + 1].
	self error: 'unknown pixel depth'.

]

{ #category : 'testing' }
Color >> is: aSymbol [
	^ aSymbol == #Color or: [ super is: aSymbol ]
]

{ #category : 'queries' }
Color >> isBlack [
	"Return true if the receiver represents black"
	(self float32SlotAt: 1) = 0.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 2) = 0.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 3) = 0.0 ifFalse: [ ^ false ].
	^ true
]

{ #category : 'selection' }
Color >> isBlue [
	"Am I considered Blue?"

	^self blue > (self green + 0.3)
		and: [self blue > (self red + 0.3)]
			and: [(self green - self red) abs < 0.4]
]

{ #category : 'testing' }
Color >> isCollection [
	^ false
]

{ #category : 'selection' }
Color >> isCyan [
	"Am I considered Cyan ?"

	^self red < 0.05 
		and: [(self green min: self blue) > 0.5]					
			and: [(self green - self blue) abs < 0.2]
]

{ #category : 'selection' }
Color >> isGreen [
	"Am I considered Green ?"

	^self green > (self blue + 0.3)
		and: [self green > (self red + 0.3)]
]

{ #category : 'selection' }
Color >> isMagenta [
	"Am I considered Magenta ?"

	^self green < 0.05 
		and: [(self red min: self blue) > 0.4]					
			and: [(self red - self blue) abs < 0.3]
]

{ #category : 'queries' }
Color >> isOpaque [
	^true
]

{ #category : 'selection' }
Color >> isRed [
	"Am I considered Red ?"

	^self red > (self green + 0.4)
		and: [self red > (self blue + 0.6)]
			and: [(self green - self blue) abs < 0.4]
]

{ #category : 'queries' }
Color >> isTransparent [

	^ false

]

{ #category : 'queries' }
Color >> isWhite [
	"Return true if the receiver represents white"
	(self float32SlotAt: 1) = 1.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 2) = 1.0 ifFalse: [ ^ false ].
	(self float32SlotAt: 3) = 1.0 ifFalse: [ ^ false ].
	^ true
]

{ #category : 'selection' }
Color >> isYellow [
	"Am I considered Yellow ?"

	^self blue < 0.1
		and: [(self red min: self green) > 0.5]					
			and: [(self red - self green) abs < 0.2]
]

{ #category : 'groups of shades' }
Color >> lightShades: thisMany [
	"An array of thisMany colors from white to self. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red lightShades: 12)"

	^ self class white mix: self shades: thisMany

]

{ #category : 'transformations' }
Color >> lighter [
	"Answer a lighter shade of this color."

	^ self adjustSaturation: -0.03 brightness: 0.08
]

{ #category : 'accessing' }
Color >> luminance [
	"Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity.
	See https://en.wikipedia.org/wiki/HSL_and_HSV"

	^ (0.2627 * self red) +
	   (0.6780 * self green) +
	   (0.0593 * self blue)
]

{ #category : 'conversions' }
Color >> makeForegroundColor [
        "Make a foreground color contrasting with me"
        ^self luminance >= 0.5
                ifTrue: [`Color black`]
                ifFalse: [`Color white`]
]

{ #category : 'testing' }
Color >> mightBeTranslucent [
	"For Colors, answer if actually translucent or not."
	^self isOpaque not
]

{ #category : 'groups of shades' }
Color >> mix: color2 shades: thisMany [
	"Return an array of thisMany colors from self to color2. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red mix: Color green shades: 12)"

	| redInc greenInc blueInc rr gg bb c out |
	thisMany = 1 ifTrue: [^ Array with: color2].
	redInc := color2 red - self red / (thisMany-1).
	greenInc := color2 green - self green / (thisMany-1).
	blueInc := color2 blue - self blue / (thisMany-1).
	rr := self red.  gg := self green.  bb := self blue.
	out := (1 to: thisMany) collect: [:num |
		c := Color r: rr g: gg b: bb.
		rr := rr + redInc.
		gg := gg + greenInc.
		bb := bb + blueInc.
		c].
	out at: out size put: color2.	"hide roundoff errors"
	^ out

]

{ #category : 'transformations' }
Color >> mixed: proportion with: aColor  [
	"Mix with another color and do not preserve transpareny.  Only use this for extracting the RGB value and mixing it.  All other callers should use instead: 
	aColor alphaMixed: proportion with: anotherColor
	"

	| frac1 frac2 |
	frac1 := proportion asFloat min: 1.0 max: 0.0.
	frac2 := 1.0 - frac1.
	^ Color
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)
]

{ #category : 'transformations' }
Color >> muchDarker [

	^ self alphaMixed: 0.5 with: `Color black`

]

{ #category : 'transformations' }
Color >> muchLighter [

	^ self alphaMixed: 0.233 with: `Color white`
]

{ #category : 'other' }
Color >> name [
	"Return this color's name, or description if unnamed."

	^ self printString

]

{ #category : 'transformations' }
Color >> negated [
	"Return an RGB inverted color"
	^Color
		r: 1.0 - self red
		g: 1.0 - self green
		b: 1.0 - self blue
]

{ #category : 'accessing' }
Color >> oklchChroma [
	^ self oklchDo: [:L :C :h| C]
]

{ #category : 'accessing' }
Color >> oklchDo: aBlock [
	"Evaluate aBlock with the OKLCH coordinates for this color (lightness, chroma, hue)."
	| r g b oneThird L M S lightness A B |
	r := self class sRGBGammaToLinear: self red.
	g := self class sRGBGammaToLinear: self green.
	b := self class sRGBGammaToLinear: self blue.
	"Linear RGB to LMS:"
	oneThird := 1.0 / 3.0.
	L := 0.4122214708 * r + (0.5363325363 * g) + (0.0514459929 * b) raisedTo: oneThird.
	M := 0.2119034982 * r + (0.6806995451 * g) + (0.1073969566 * b) raisedTo: oneThird.
	S := 0.0883024619 * r + (0.2817188376 * g) + (0.6299787005 * b) raisedTo: oneThird.
	"LMS to OKLAB:"
	lightness := 0.2104542553 * L + (0.7936177850 * M) - (0.0040720468 * S).
	A := 1.9779984951 * L - (2.4285922050 * M) + (0.4505937099 * S).
	B := 0.0259040371 * L + (0.7827717662 * M) - (0.8086757660 * S).
	"OKLAB to OKLCH:"
	^ aBlock
		value: lightness "OKLCH lightness is the same as OKLAB"
		value: (A squared + B squared) sqrt "chroma"
		value: (B arcTan: A) radiansToDegrees \\ 360.0 "hue in degrees"
]

{ #category : 'accessing' }
Color >> oklchHue [
	^ self oklchDo: [:L :C :h| h]
]

{ #category : 'accessing' }
Color >> oklchLightness [
	^ self oklchDo: [:L :C :h| L]
]

{ #category : 'transformations' }
Color >> orColorUnlike: theOther [
	"If this color is a lot like theOther, then return its complement, otherwide, return self"

	^ (self diff: theOther) < 0.3
		ifTrue: [theOther negated]
		ifFalse: [self]
]

{ #category : 'transformations' }
Color >> paler [
	"Answer a paler shade of this color."

	^ self adjustSaturation: -0.09 brightness: 0.09

]

{ #category : 'conversions' }
Color >> pixelValueForDepth: d [
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8 and less than 32 (no Indexed colors, no real alpha), black maps to the darkest possible blue.
	Note that 
		Color transparent class = TranslucentColor
	this special case is handled in TranslucentColor >> #pixelValueForDepth:
	"

	| bitBltFakeBlack val |
	"Most common case"
	"eight bits per component; top 8 bits set to all ones (opaque alpha)"
	d = 32 ifTrue: [
		^ 16rFF000000 bitOr:
			((((self float32SlotAt: 1) * 255.999) truncated bitShift: 16) bitOr:
			((((self float32SlotAt: 2) * 255.999) truncated bitShift: 8) bitOr: 
			(((self float32SlotAt: 3) * 255.999) truncated))) ].

"Faster in 32 bit systems, but slower in 64 bits"
"	d = 32 ifTrue: [
		val := LargePositiveInteger new: 4.
		val at: 3 put: ((self at: 1) * 255.999) truncated.
		val at: 2 put: ((self at: 2)  * 255.999) truncated.
		val at: 1 put: ((self at: 3)  * 255.999) truncated.
		val at: 4 put: 16rFF.
		^ val normalize]."

	d = 8 ifTrue: [^ self closestPixelValue8].  "common case"
	d < 8 ifTrue: [
		d = 4 ifTrue: [^ self closestPixelValue4].
		d = 2 ifTrue: [^ self closestPixelValue2].
		d = 1 ifTrue: [^ self closestPixelValue1]].

	"For the depth 16, pixelValue = 0 means transparent, black is represented as 16r8000 (rgb=0, pixelvalue != 0)."
	(d = 16) | (d = 15) ifTrue: [
		"five bits per component; top bits ignored"
		val := ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0
			ifTrue: [d = 16 ifTrue: [16r8000] ifFalse: [1]]
			ifFalse: [val]].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 12 ifTrue: [  "for indexing a color map with 4 bits per color component"
		val := ((self red * 15) rounded bitShift: 8) bitOr:
			(((self green * 15) rounded bitShift: 4) bitOr: 
			((self blue * 15) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	d = 9 ifTrue: [  "for indexing a color map with 3 bits per color component"
		val := ((self red * 7) rounded bitShift: 6) bitOr:
			(((self green * 7) rounded bitShift: 3) bitOr: 
			((self blue * 7) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	self error: 'unknown pixel depth: ', d printString
]

{ #category : 'conversions' }
Color >> pixelWordFor: depth filledWith: pixelValue [
	"Return to a 32-bit word that concatenates enough copies of the given pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."
	| halfword |
	depth = 32 ifTrue: [^ pixelValue].
	depth = 16
		ifTrue: [halfword := pixelValue]
		ifFalse: [halfword := pixelValue * 
					(#(16rFFFF				"replicates at every bit"
						16r5555 -			"replicates every 2 bits"
						16r1111 - - -			"replicates every 4 bits"
						16r0101) at: depth)	"replicates every 8 bits"].
	^ halfword bitOr: (halfword bitShift: 16)
]

{ #category : 'conversions' }
Color >> pixelWordForDepth: depth [
	"Return to a 32-bit word that concatenates enough copies of the receiver's pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."

	| pixelValue |
	pixelValue := self pixelValueForDepth: depth.
	^ self pixelWordFor: depth filledWith: pixelValue

]

{ #category : 'comparing' }
Color >> primitiveEqual: aColor  [
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aColor class == self class ifFalse: [^ false].
	length := self size.
	length = aColor size ifFalse: [^ false].
	1 to: self size do: [ :i |
		(self basicAt: i) = (aColor basicAt: i) ifFalse: [^ false]].
	^ true
]

{ #category : 'printing' }
Color >> printOn: aStream [
	 self colorName ifNotNil: [ :name |
		^ aStream
			nextPutAll: 'Color ';
			nextPutAll: name].
	self storeOn: aStream.

]

{ #category : 'printing' }
Color >> printString [
	"Answer a String whose characters are a description of the receiver."
	
	^ String streamContents: [ :stream | self printOn: stream ]
]

{ #category : 'transformations' }
Color >> quiteBlacker [

	^ self alphaMixed: 0.8 with: `Color black`

]

{ #category : 'transformations' }
Color >> quiteWhiter [

	^ self alphaMixed: 0.6 with: `Color white`
]

{ #category : 'accessing' }
Color >> red [
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ self float32SlotAt: 1
]

{ #category : 'equality' }
Color >> rgbDistance: otherColor [
	"Compare two colors in distance"
"
	^ (self hue - otherColor hue) abs +
	  (self saturation - otherColor saturation) abs *10 +
	  (self brightness - otherColor brightness) abs
"
	"See http://www.compuphase.com/cmetric.htm"
	| meanRed deltaRed deltaGreen deltaBlue |
	meanRed := (self red + otherColor red) abs / 2.
	deltaRed := (self red - otherColor red) abs.
	deltaGreen := (self green - otherColor green) abs.
	deltaBlue := (self blue - otherColor blue) abs.
	
	^ ( ((2 + (meanRed / 256)) * (deltaRed * deltaRed)) +
		(4 * deltaGreen) +
		((2 + ((255 - meanRed) / 256)) * deltaBlue)
	  ) sqrt
]

{ #category : 'accessing' }
Color >> saturation [
	"Return the saturation of this color, a value between 0.0 and 1.0.
	Color red saturation
	Color gray saturation
	"

	| r g b max min |
	r := self red.
	g := self green.
	b := self blue. 
	max := (r max: g) max: b.
	min := (r min: g) min: b.
	max = 0.0 ifTrue: [ ^0.0 ].
	^max - min / max
]

{ #category : 'private' }
Color >> setHue: hue chroma: chroma brightness: brightness [
	"Initialize this color to the given hue, chroma, and luma.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 
	hf := hue \\ 360.
	i := hf // 60.  				"integer part of hue"
	x := (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x := x \\  2 .
	i \\ 2 = 1 ifTrue: [ x := 1.0 - x ].
	x := chroma  * x.

	0 = i ifTrue: [ r1 := chroma. g1 := x. b1 := 0.0 ].
	1 = i ifTrue: [ r1 := x. g1 := chroma. b1 := 0.0 ].
	2 = i ifTrue: [ r1 := 0.0. g1 := chroma. b1 := x ].
	3 = i ifTrue: [ r1 := 0.0. g1 := x. b1 := chroma ].
	4 = i ifTrue: [ r1 := x. g1 := 0.0. b1 := chroma ].
	5 = i ifTrue: [ r1 := chroma. g1 := 0.0. b1 := x ].

	m := brightness - ((r1 max: g1) max: b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 := r1 + m.
	r1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 := g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 := b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1
]

{ #category : 'private' }
Color >> setHue: hue chroma: chroma luminance: luma [
	"Initialize this color to the given hue, chroma, and luma.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 

	hf := hue \\ 360.
	i := hf // 60.  				"integer part of hue"
	x := (hf \\ 60) / 60.0 .         	"fractional part of hue"
	x := x \\  2 .
	i \\ 2 = 1 ifTrue: [ x := 1.0 - x ].
	x := chroma  * x.

	0 = i ifTrue: [ r1 := chroma. g1 := x. b1 := 0.0 ].
	1 = i ifTrue: [ r1 := x. g1 := chroma. b1 := 0.0 ].
	2 = i ifTrue: [ r1 := 0.0. g1 := chroma. b1 := x ].
	3 = i ifTrue: [ r1 := 0.0. g1 := x. b1 := chroma ].
	4 = i ifTrue: [ r1 := x. g1 := 0.0. b1 := chroma ].
	5 = i ifTrue: [ r1 := chroma. g1 := 0.0. b1 := x ].

	m := luma - (0.2627*r1) - (0.6780*g1) - (0.0593*b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 := r1 + m.
	r1 > 1.0 
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 := g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 := b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1
]

{ #category : 'private' }
Color >> setHue: hue saturation: saturation brightness: brightness [
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."

	| s v hf i f p q t | 
	s := saturation asFloat min: 1.0 max: 0.0.
	v := brightness asFloat min: 1.0 max: 0.0.
	hf := hue \\ 360.
	i := hf // 60.  			"integer part of hue"
	f := (hf \\ 60) / 60.0.         	"fractional part of hue"

	p := (1.0 - s) * v.
	q := (1.0 - (s * f)) * v.
	t := (1.0 - (s * (1.0 - f))) * v.

	0 = i ifTrue: [ ^ self setRed: v green: t blue: p ].
	1 = i ifTrue: [ ^ self setRed: q green: v blue: p ].
	2 = i ifTrue: [ ^ self setRed: p green: v blue: t ].
	3 = i ifTrue: [ ^ self setRed: p green: q blue: v ].
	4 = i ifTrue: [ ^ self setRed: t green: p blue: v ].
	5 = i ifTrue: [ ^ self setRed: v green: p blue: q ].

	self error: 'implementation error'
]

{ #category : 'private' }
Color >> setRed: r green: g blue: b [

	self basicSetRed: r green: g blue: b
]

{ #category : 'private' }
Color >> setRed: r green: g blue: b range: range [
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	self basicSetRed: r/range green: g/range blue: b/range.
	self clipToValidValues
]

{ #category : 'transformations' }
Color >> slightlyDarker [

	^ self adjustBrightness: -0.03

]

{ #category : 'transformations' }
Color >> slightlyLighter [

	^ self adjustSaturation: -0.01 brightness: 0.03
]

{ #category : 'transformations' }
Color >> slightlyWhiter [

	^ self alphaMixed: 0.85 with: `Color white`

]

{ #category : 'printing' }
Color >> storeArrayOn: aStream [

	aStream nextPutAll: '#('.
	self storeArrayValuesOn: aStream.
	aStream nextPutAll: ') '

]

{ #category : 'printing' }
Color >> storeArrayValuesOn: aStream [


	self red printOn: aStream fractionDigits: 3.
	aStream space.
	self green printOn: aStream fractionDigits: 3.
	aStream space.
	self blue printOn: aStream fractionDigits: 3


]

{ #category : 'printing' }
Color >> storeOn: aStream [

	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' r: '.
	self red printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' g: '.
	self green printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' b: '.
	self blue printOn: aStream fractionDigits: 3.
	aStream nextPut: $)
]

{ #category : 'accessing' }
Color >> swatch [
	"Answer a swatch to display in a menu or browser"
	^self iconOrThumbnailOfSize: 16
]

{ #category : 'transformations' }
Color >> twiceDarker [
	"Answer a significantly darker shade of this color."

	^ self adjustSaturation: 0.076 brightness: -0.15
]

{ #category : 'transformations' }
Color >> twiceLighter [
	"Answer a significantly lighter shade of this color."

	^ self adjustSaturation: -0.06 brightness: 0.15
]

{ #category : 'transformations' }
Color >> veryMuchDarker [

	^ self alphaMixed: 0.25 with: `Color black`
]

{ #category : 'transformations' }
Color >> veryMuchLighter [

	^ self alphaMixed: 0.07 with: `Color white`
]

{ #category : 'groups of shades' }
Color >> wheel: thisMany [
	"An array of thisMany colors around the color wheel starting at self and ending all the way around the hue space just before self.  Array is of length thisMany.  Very useful for displaying color based on a variable in your program.  "

	| sat bri hue step c |
	sat := self saturation.
	bri := self brightness.
	hue := self hue.
	step := 360.0 / (thisMany max: 1).
	^ (1 to: thisMany) collect: [:num |
		c := Color h: hue s: sat v: bri.  "hue is taken mod 360"
		hue := hue + step.
		c].
"
(Color wheel: 8) withIndexDo: [:c :i | Display fill: (i*10@20 extent: 10@20) fillColor: c]
"
]

{ #category : 'transformations' }
Color >> whiter [

	^ self alphaMixed: 0.8333 with: `Color white`

]
