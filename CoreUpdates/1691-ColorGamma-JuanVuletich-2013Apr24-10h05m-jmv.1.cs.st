'From Cuis 4.1 of 12 December 2012 [latest update: #1690] on 24 April 2013 at 12:04:47.348058 pm'!

!Color commentStamp: '<historical>' prior: 0!
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

(See also comments in these classes: Form, Bitmap, BitBlt)!


!Color class methodsFor: 'color space conversions' stamp: 'jmv 4/24/2013 10:55'!
applySimpleGamma: gamma to: value
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

	^ value raisedTo: gamma! !

!Color class methodsFor: 'color space conversions' stamp: 'jmv 4/24/2013 10:49'!
linearTosRGBGamma: value
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
	r _ Random new.
	1000 timesRepeat: [
		v _ r next.
		self assert: ((Color sRGBGammaToLinear: (Color linearTosRGBGamma: v)) closeTo: v) ]

	| r |
	r _ 10@10 extent: 600@400.
	Display fill: r fillColor: Color white.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color linearTosRGBGamma: v ]) in: r color: Color black min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color applySimpleGamma: 1/2.2 to: v ]) in: r color: Color blue min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color sRGBGammaToLinear: v ]) in: r color: Color black min: 0.0 max: 1.0.
	FFT new plot: ((0.0 to: 1.0 count: 100) collect: [ :v | Color applySimpleGamma: 2.2 to: v ]) in: r color: Color blue min: 0.0 max: 1.0.
	"

	^ value <= 0.00313066844250063
		ifTrue: [ value * 12.92 ]
		ifFalse: [ (value raisedTo: 1.0/2.4) * 1.055 - 0.055 ]! !

!Color class methodsFor: 'color space conversions' stamp: 'jmv 4/24/2013 10:50'!
sRGBGammaToLinear: value
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
		ifFalse: [ value + 0.055 / 1.055 raisedTo: 2.4 ]! !


!Color class reorganize!
('instance creation' colorFrom: colorFromPixelValue:depth: fromArray: fromRgbTriplet: fromString: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: new r:g:b: r:g:b:alpha: r:g:b:range: random random2)
('class initialization' initialize initializeGrayToIndexMap initializeIndexedColors initializeNames named:put:)
('examples' colorRampForDepth:extent: experimentsTowarsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('named colors' aqua beige black blue brightGreen brown cyan darkBlue darkGray darkGreen darkPink darkPurple darktan forestGreen gray green grey hotPink indigo lavender lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightPink lightPurple lightRed lightYellow lilac lime limeGreen magenta maroon mauve mustard navyBlue olive oliveGreen orange paleBlue paleBuff paleGreen paleMagenta paleOrange palePeach paleRed paleTan paleYellow peach periwinkle pink purple red royalBlue salmon seaGreen skyBlue tan teal transparent turquoise veryDarkGray veryLightGray veryPaleRed veryVeryDarkGray veryVeryLightGray violet white yellow)
('colormaps' cachedColormapFrom:to: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapFrom:to: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor:)
('other' indexedColors maskingMap: shutDown)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper: fromUser oldColorPaletteForDepth:extent:)
('instance protocol testing')
('color name lookup' colorNames colorNamesDict defaultColorNamesDictionary doesNotUnderstand: exactColorNamed: setColorNamesDict: traditionalColorNamesDictionary xkcdFirst48ColorNamesDictionary)
('selection' blueColorDict blueColorDict: blueGreenColorDict blueGreenColorDict: brightColorDict brightColorDict: brownColorDict brownColorDict: darkColorDict darkColorDict: grayColorDict: greenColorDict greenColorDict: greyColorDict greyColorDict: lightColorDict lightColorDict: orangeColorDict orangeColorDict: pastelColorDict pastelColorDict: pinkColorDict pinkColorDict: purpleColorDict purpleColorDict: redColorDict redColorDict: saturatedColorDict saturatedColorDict: yellowColorDict yellowColorDict:)
('color space conversions' applySimpleGamma:to: linearTosRGBGamma: sRGBGammaToLinear:)
!

