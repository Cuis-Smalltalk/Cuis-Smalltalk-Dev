'From Cuis 5.0 of 7 November 2016 [latest update: #3108] on 19 June 2017 at 11:58:54 am'!
!classDefinition: #Color category: #'Graphics-Primitives'!
FloatArray variableWordSubclass: #Color
	instanceVariableNames: ''
	classVariableNames: 'CachedColormaps ColorNamesDict FromGrayColormaps GrayToIndexMap IndexedColors MaskingMap ToGrayColormaps'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color commentStamp: 'jmv 6/18/2017 20:14:31' prior: 0!
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

Default colorSpace is #sRGB. Subclasses might use other color spaces!

!classDefinition: #TranslucentColor category: #'Graphics-Primitives'!
Color variableWordSubclass: #TranslucentColor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!TranslucentColor commentStamp: '<historical>' prior: 0!
A TranslucentColor behaves just like a normal color, except that it will pack its alpha value into the fourth position.  This allows creating forms with translucency for use with the alpha blend function of BitBlt.  An alpha of zero is transparent, and 1.0 is opaque.!


!Color methodsFor: 'access' stamp: 'jmv 1/31/2011 09:25'!
alpha
	"Return the opacity ('alpha') value of opaque so that normal colors can be compared to TranslucentColors."

	^ 1.0
! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:35:34'!
blue
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ self at: 3! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:33:10'!
brightness
	"Return the brightness of this color, a float in the range [0.0..1.0].
	Color red brightness
	"

	"Do not include alpha if TranslucentColor"
	^ (self red max: self green) max: self blue! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:34:30'!
chroma
	"Return the chroma of this color, a value between 0.0 and 1.0, somewhat related to saturation.
	See http://en.wikipedia.org/wiki/HSL_and_HSV
	Color red chroma
	Color gray chroma
	"

	| r g b max min |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	^max - min! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:35:28'!
green
	"Return the green component of this color, a float in the range [0.0..1.0]."

	^ self at: 2! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:31:54'!
hue
	"Return the hue of this color, an angle in the range [0.0..360.0].
	Color orange hue
	"

	| r g b max min span h |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	span _ (max - min) asFloat.
	span = 0.0 ifTrue: [ ^ 0.0 ].

	r = max ifTrue: [
		h _ ((g - b) asFloat / span) * 60.0.
	] ifFalse: [
		g = max
			ifTrue: [ h _ 120.0 + (((b - r) asFloat / span) * 60.0). ]
			ifFalse: [ h _ 240.0 + (((r - g) asFloat / span) * 60.0). ].
	].

	^h mod: 360.0! !

!Color methodsFor: 'access' stamp: 'KenD 1/23/2013 18:19'!
icon
	"Answer a swatch to display in a menu or browser"
	^self swatch! !

!Color methodsFor: 'access' stamp: 'KenD 1/23/2013 18:17'!
iconOrThumbnailOfSize: aNumberOrPoint 
	"Answer an appropiate form to represent the receiver"
	| form |
	form := Form extent: aNumberOrPoint asPoint asPoint depth: 32.
	form fillColor: self.
	^ form! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:46'!
luminance
	"Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity."

	^ ((299 * self red) +
	   (587 * self green) +
	   (114 * self blue)) / 1000! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:35:25'!
red
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ self at: 1! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:34:08'!
saturation
	"Return the saturation of this color, a value between 0.0 and 1.0.
	Color red saturation
	Color gray saturation
	"

	| r g b max min |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	max = 0.0 ifTrue: [ ^0.0 ].
	^max - min / max! !

!Color methodsFor: 'access' stamp: 'KenD 1/23/2013 18:18'!
swatch
	"Answer a swatch to display in a menu or browser"
	^self iconOrThumbnailOfSize: 16! !

!Color methodsFor: 'conversions' stamp: 'sw 10/27/1999 10:51'!
asNontranslucentColor
	^ self! !

!Color methodsFor: 'conversions' stamp: 'jmv 6/18/2017 20:20:07'!
bitPatternForDepth: depth
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"

	^ Bitmap with: (self pixelWordForDepth: depth)! !

!Color methodsFor: 'conversions' stamp: 'jmv 6/3/2016 17:28'!
bitPatternForGrayForm
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"
	"Details: The pattern for the most recently requested depth is cached."

	^Bitmap with: (self pixelWordFor: 8 filledWith: (self luminance * 255) rounded)! !

!Color methodsFor: 'conversions' stamp: 'jmv 4/19/2013 16:17'!
closestPixelValue1
	"Return the nearest approximation to this color for a monochrome Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 0].  "white"

	self luminance > 0.5
		ifTrue: [^ 0]  "white"
		ifFalse: [^ 1].  "black"! !

!Color methodsFor: 'conversions' stamp: 'jmv 12/13/2014 16:18'!
closestPixelValue2
	"Return the nearest approximation to this color for a 2-bit deep Form."

	| lum |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 2].  "opaque white"

	lum _ self luminance.
	lum < 0.2 ifTrue: [^ 1].  "black"
	lum > 0.6 ifTrue: [^ 2].  "opaque white"
	^ 3  "50% gray"
! !

!Color methodsFor: 'conversions' stamp: 'jmv 12/13/2014 16:17'!
closestPixelValue4
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

	bIndex _ (self luminance * 8.0) rounded.  "bIndex in [0..8]"
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
! !

!Color methodsFor: 'conversions' stamp: 'jmv 4/21/2015 09:57'!
closestPixelValue8
	"Return the nearest approximation to this color for an 8-bit deep Form."

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
				(self green * 5) rounded ]! !

!Color methodsFor: 'conversions' stamp: 'di 9/2/97 20:21'!
dominantColor
	^ self! !

!Color methodsFor: 'conversions' stamp: 'jmv 6/2/2016 14:48'!
indexInMap: aColorMap
	"Return the index corresponding to this color in the given color map. RGB colors are truncated to 3-, 4-, or 5-bits per color component when indexing into such a colorMap.  "

	aColorMap size = 2 ifTrue: [^ (self pixelValueForDepth: 1) + 1].
	aColorMap size = 4 ifTrue: [^ (self pixelValueForDepth: 2) + 1].
	aColorMap size = 16 ifTrue: [^ (self pixelValueForDepth: 4) + 1].
	aColorMap size = 256 ifTrue: [^ (self pixelValueForDepth: 8) + 1].
	aColorMap size = 512 ifTrue: [^ (self pixelValueForDepth: 9) + 1].
	aColorMap size = 4096 ifTrue: [^ (self pixelValueForDepth: 12) + 1].
	aColorMap size = 32768 ifTrue: [^ (self pixelValueForDepth: 15) + 1].
	self error: 'unknown pixel depth'.
! !

!Color methodsFor: 'conversions' stamp: 'bf 4/18/2001 16:25'!
makeForegroundColor
        "Make a foreground color contrasting with me"
        ^self luminance >= 0.5
                ifTrue: [Color black]
                ifFalse: [Color white]! !

!Color methodsFor: 'conversions' stamp: 'jmv 6/2/2016 14:50'!
pixelValueForDepth: d
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8 and less than 32 (no Indexed colors, no real alpha), black maps to the darkest possible blue.
	Note that 
		Color transparent class = TranslucentColor
	this special case is handled in TranslucentColor >> #pixelValueForDepth:
	"

	| bitBltFakeBlack val |
	d = 8 ifTrue: [^ self closestPixelValue8].  "common case"
	d < 8 ifTrue: [
		d = 4 ifTrue: [^ self closestPixelValue4].
		d = 2 ifTrue: [^ self closestPixelValue2].
		d = 1 ifTrue: [^ self closestPixelValue1]].

	d = 32 ifTrue: [
		"eight bits per component; top 8 bits set to all ones (opaque alpha)"
		val _ LargePositiveInteger new: 4.
		val at: 3 put: (self red * 255) rounded.
		val at: 2 put: (self green * 255) rounded.
		val at: 1 put: (self blue * 255) rounded.
		val at: 4 put: 16rFF.  "opaque alpha"
		^ val normalize].

	"For the depth 16, pixelValue = 0 means transparent, black is represented as 16r8000 (rgb=0, pixelvalue !!= 0)."
	(d = 16) | (d = 15) ifTrue: [
		"five bits per component; top bits ignored"
		val _ ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0
			ifTrue: [d = 16 ifTrue: [16r8000] ifFalse: [1]]
			ifFalse: [val]].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 12 ifTrue: [  "for indexing a color map with 4 bits per color component"
		val _ ((self red * 15) rounded bitShift: 8) bitOr:
			(((self green * 15) rounded bitShift: 4) bitOr: 
			((self blue * 15) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	d = 9 ifTrue: [  "for indexing a color map with 3 bits per color component"
		val _ ((self red * 7) rounded bitShift: 6) bitOr:
			(((self green * 7) rounded bitShift: 3) bitOr: 
			((self blue * 7) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	self error: 'unknown pixel depth: ', d printString! !

!Color methodsFor: 'conversions' stamp: 'di 11/30/1998 09:03'!
pixelWordFor: depth filledWith: pixelValue
	"Return to a 32-bit word that concatenates enough copies of the given pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."
	| halfword |
	depth = 32 ifTrue: [^ pixelValue].
	depth = 16
		ifTrue: [halfword _ pixelValue]
		ifFalse: [halfword _ pixelValue * 
					(#(16rFFFF				"replicates at every bit"
						16r5555 -			"replicates every 2 bits"
						16r1111 - - -			"replicates every 4 bits"
						16r0101) at: depth)	"replicates every 8 bits"].
	^ halfword bitOr: (halfword bitShift: 16)! !

!Color methodsFor: 'conversions'!
pixelWordForDepth: depth
	"Return to a 32-bit word that concatenates enough copies of the receiver's pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."

	| pixelValue |
	pixelValue _ self pixelValueForDepth: depth.
	^ self pixelWordFor: depth filledWith: pixelValue
! !

!Color methodsFor: 'equality' stamp: 'jmv 6/18/2017 20:38:11'!
diff: theOther
	"Returns a number between 0.0 and 1.0.
	Color gray diff: Color red
	"
	^(self - theOther ) abs sum / self size! !

!Color methodsFor: 'equality' stamp: 'KenD 12/8/2013 08:35'!
rgbDistance: otherColor
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
	  ) sqrt! !

!Color methodsFor: 'groups of shades' stamp: 'tk 6/18/96'!
darkShades: thisMany
	"An array of thisMany colors from black to the receiver.  Array is of length num. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red darkShades: 12)"

	^ self class black mix: self shades: thisMany
! !

!Color methodsFor: 'groups of shades' stamp: 'tk 6/18/96'!
lightShades: thisMany
	"An array of thisMany colors from white to self. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red lightShades: 12)"

	^ self class white mix: self shades: thisMany
! !

!Color methodsFor: 'groups of shades' stamp: 'tk 6/18/96'!
mix: color2 shades: thisMany
	"Return an array of thisMany colors from self to color2. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red mix: Color green shades: 12)"

	| redInc greenInc blueInc rr gg bb c out |
	thisMany = 1 ifTrue: [^ Array with: color2].
	redInc _ color2 red - self red / (thisMany-1).
	greenInc _ color2 green - self green / (thisMany-1).
	blueInc _ color2 blue - self blue / (thisMany-1).
	rr _ self red.  gg _ self green.  bb _ self blue.
	out _ (1 to: thisMany) collect: [:num |
		c _ Color r: rr g: gg b: bb.
		rr _ rr + redInc.
		gg _ gg + greenInc.
		bb _ bb + blueInc.
		c].
	out at: out size put: color2.	"hide roundoff errors"
	^ out
! !

!Color methodsFor: 'groups of shades' stamp: 'di 10/23/2000 09:45'!
wheel: thisMany
	"An array of thisMany colors around the color wheel starting at self and ending all the way around the hue space just before self.  Array is of length thisMany.  Very useful for displaying color based on a variable in your program.  "

	| sat bri hue step c |
	sat _ self saturation.
	bri _ self brightness.
	hue _ self hue.
	step _ 360.0 / (thisMany max: 1).
	^ (1 to: thisMany) collect: [:num |
		c _ Color h: hue s: sat v: bri.  "hue is taken mod 360"
		hue _ hue + step.
		c].
"
(Color wheel: 8) withIndexDo: [:c :i | Display fill: (i*10@20 extent: 10@20) fillColor: c]
"! !

!Color methodsFor: 'named colors' stamp: 'KenD 12/8/2013 14:59'!
closestAssocFrom: aColorDict
	"Answer closest associated color in aColorDict"
"
	((Color r: 0.3 g: 0.2 b: 0.8) closestAssocFrom: (Color colorNamesDict)) explore.
"
	"Filter values for those close in hue (cheap test) then use rgbDistance"
	| closeInHue isClose close distance myHue |
	closeInHue := OrderedCollection new.
	myHue := self hue.
	isClose := [ :assoc | | delta |
		delta := ((assoc value hue) - myHue) abs.
		(delta < 20) or: [ delta > 340 ] "hues within 20 degrees"
	].
	aColorDict  associationsDo:  [ :assoc |
		(isClose value: assoc) ifTrue: [ closeInHue add: assoc ]
	].

	close := nil.
	distance := 1000. "big"
	(closeInHue size > 0)
		ifFalse: [ "fallback -- no color really close"
			aColorDict  associationsDo:  [ :assoc | | dist |
				dist := self rgbDistance: (assoc value). 
				(dist < distance)  ifTrue: [distance := dist. close := assoc]
				]
		]
		ifTrue: [ closeInHue do: [ :assoc | | dist |
				dist := self rgbDistance: (assoc value). 
				(dist < distance)  ifTrue: [distance := dist. close := assoc]
				]
		].
	
	^ close! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:33'!
closestColor
	"Answer the closest matching color in the colorNames dictionary"

"
	(Color red lighter lighter) closestColor explore.
"
	^ self closestColorFrom: (Color colorNamesDict)! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:36'!
closestColorAssociation
	"Answer this color's closest name->color match."

"
	(Color blue lighter lighter) closestColorAssociation explore.
"	
	^ self closestAssocFrom: (Color colorNamesDict)! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/25/2013 14:31'!
closestColorFrom: aColorDict
	"Answer closest associated color"
"
	((Color lightBlue) closestColorFrom: (Color css2NamedColors)) explore..
"
	^(self closestAssocFrom: aColorDict) value! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:36'!
closestColorName
	"Answer the name symbol of the closest matching color in the colorNames dictionary"
"
	(Color red lighter lighter) closestColor name.
"
	
	^ self closestNameFrom: (Color colorNamesDict)! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:36'!
closestNameFrom: aColorDict
	"Answer closest associated color name"
"
	((Color lightBlue) closestNameFrom: (Color namedColors)) print.
"
	^(self closestAssocFrom: aColorDict) key! !

!Color methodsFor: 'other' stamp: 'KenD 7/18/2015 20:41'!
colorName
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color."

	^(Color colorNamesDict keyAtValue: self  ifAbsent: [nil])
! !

!Color methodsFor: 'other' stamp: 'KenD 7/18/2015 20:46'!
name
	"Return this color's name, or description if unnamed."

	^ self printString
! !

!Color methodsFor: 'printing' stamp: 'jmv 2/13/2014 13:41'!
hexStringRGB
	"
	Color fromUser hexStringRGB
	"
	^String streamContents: [ :strm |
		(self red * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self green * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self blue * 255) rounded printOn: strm base: 16 length: 2 padded: true ]! !

!Color methodsFor: 'printing' stamp: 'KenD 7/18/2015 20:42'!
printOn: aStream
	 self colorName ifNotNil: [ :name |
		^ aStream
			nextPutAll: 'Color ';
			nextPutAll: name].
	self storeOn: aStream.
! !

!Color methodsFor: 'printing' stamp: 'KenD 7/18/2015 20:44'!
printString
	"Answer a String whose characters are a description of the receiver."
	
	^ String streamContents: [ :stream | self printOn: stream ]! !

!Color methodsFor: 'printing' stamp: 'mir 7/21/1999 11:41'!
storeArrayOn: aStream

	aStream nextPutAll: '#('.
	self storeArrayValuesOn: aStream.
	aStream nextPutAll: ') '
! !

!Color methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:04'!
storeArrayValuesOn: aStream


	self red printOn: aStream fractionDigits: 3.
	aStream space.
	self green printOn: aStream fractionDigits: 3.
	aStream space.
	self blue printOn: aStream fractionDigits: 3

! !

!Color methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:01'!
storeOn: aStream

	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' r: '.
	self red printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' g: '.
	self green printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' b: '.
	self blue printOn: aStream fractionDigits: 3.
	aStream nextPut: $)! !

!Color methodsFor: 'queries' stamp: 'jmv 6/18/2017 20:40:55'!
isBlack
	"Return true if the receiver represents black"
	(self at: 1) = 0.0 ifFalse: [ ^ false ].
	(self at: 2) = 0.0 ifFalse: [ ^ false ].
	(self at: 3) = 0.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'queries' stamp: 'ar 4/20/2001 04:33'!
isOpaque
	^true! !

!Color methodsFor: 'queries'!
isTransparent

	^ false
! !

!Color methodsFor: 'queries' stamp: 'jmv 6/18/2017 20:40:46'!
isWhite
	"Return true if the receiver represents white"
	(self at: 1) = 1.0 ifFalse: [ ^ false ].
	(self at: 2) = 1.0 ifFalse: [ ^ false ].
	(self at: 3) = 1.0 ifFalse: [ ^ false ].
	^ true! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:35'!
isBlue
	"Am I considered Blue?"

	^self blue > (self green + 0.3)
		and: [self blue > (self red + 0.3)]
			and: [(self green - self red) abs < 0.4]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:37'!
isBlueGreen
	"Am I considered BlueGreen ?"

	^self green > self red 
		and: [self red < 0.3]
			and: [(self green - self blue) abs < 0.1]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:50'!
isBright
	"Am I considered a Bright color ?"

	^self brightness > 0.6! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:40'!
isBrown
	"Am I considered Brown ?"

	^self red >= self green  
		and: [self green > self blue]					
			and: [(self red - self green) < 0.5]	
				and: [(self green - self blue) < 0.3]! !

!Color methodsFor: 'selection' stamp: 'jmv 4/19/2013 17:14'!
isCyan
	"Am I considered Cyan ?"

	^self red < 0.05 
		and: [(self green min: self blue) > 0.5]					
			and: [(self green - self blue) abs < 0.2]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:50'!
isDark
	"Am I considered a Dark color ?"

	^self brightness < 0.5! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:42'!
isGray
	"Am I considered Gray  ?"

	^(self red closeTo: self green)
		and: [self blue closeTo: self green ]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:38'!
isGreen
	"Am I considered Green ?"

	^self green > (self blue + 0.3)
		and: [self blue > (self red + 0.3)]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:40'!
isGrey
	"Am I considered Grey  ?"

	^self isGray! !

!Color methodsFor: 'selection' stamp: 'jmv 4/19/2013 17:06'!
isLight
	"Am I considered a Light color ?"

	^self brightness > 0.6! !

!Color methodsFor: 'selection' stamp: 'jmv 4/19/2013 17:13'!
isMagenta
	"Am I considered Magenta ?"

	^self green < 0.05 
		and: [(self red min: self blue) > 0.5]					
			and: [(self red - self blue) abs < 0.2]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:44'!
isOrange
	"Am I considered Orange ?"

	^self red > ((self green max: self blue) + 0.2)  
		and: [self green > (self blue + 0.2)]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:45'!
isPastel
	"Am I considered Pastel ?"

	^self saturation < 0.4! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:46'!
isPink
	"Am I considered Pink ?"

	^self red > ((self green max: self blue) + 0.3)  
		and: [self blue > (self green + 0.2)]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:47'!
isRed
	"Am I considered Red ?"

	^self red > (self green + 0.4)
		and: [self red > (self blue + 0.6)]
			and: [(self green - self blue) abs < 0.4]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:55'!
isSaturated
	"Am I considered to be a Saturated color ?"

	^self saturation > 0.6! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:49'!
isYellow
	"Am I considered Yellow ?"

	^self blue < 0.05 
		and: [(self red min: self green) > 0.5]					
			and: [(self red - self green) abs < 0.2]! !

!Color methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #Color or: [ super is: aSymbol ]! !

!Color methodsFor: 'testing' stamp: 'jmv 2/10/2011 21:46'!
mightBeTranslucent
	"For Colors, answer if actually translucent or not."
	^self isOpaque not! !

!Color methodsFor: 'transformations' stamp: 'jmv 6/18/2017 20:56:14'!
* aNumber
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Color brown *2) display
	"
	^ (Color new
		setRed: (self red * aNumber min: 1.0 max: 0.0)
		green: (self green * aNumber min: 1.0 max: 0.0)
		blue: (self blue * aNumber min: 1.0 max: 0.0))
			alpha: self alpha! !

!Color methodsFor: 'transformations' stamp: 'jmv 6/18/2017 20:56:18'!
+ aColor
	"Answer this color mixed with the given color in an additive color space.  "
	"
	(Color blue + Color green) display
	"
	^ Color new
		setRed: (self red + aColor red min: 1.0 max: 0.0)
		green: (self green + aColor green min: 1.0 max: 0.0)
		blue: (self blue + aColor blue min: 1.0 max: 0.0)! !

!Color methodsFor: 'transformations' stamp: 'jmv 6/18/2017 20:56:22'!
- aColor
	"Answer aColor is subtracted from the given color in an additive color space.  "
	"
	(Color white - Color red) display
	"
	^ Color new
		setRed: (self red - aColor red min: 1.0 max: 0.0)
		green: (self green - aColor green min: 1.0 max: 0.0)
		blue: (self blue - aColor blue min: 1.0 max: 0.0)! !

!Color methodsFor: 'transformations' stamp: 'jmv 6/18/2017 20:56:26'!
/ aNumber
	"Answer this color with its RGB divided by the given number. "
	"
	(Color red / 2) display
	"
	^ Color new
		setRed: (self red / aNumber min: 1.0 max: 0.0)
		green: (self green / aNumber min: 1.0 max: 0.0)
		blue: (self blue / aNumber min: 1.0 max: 0.0)! !

!Color methodsFor: 'transformations' stamp: 'dew 3/19/2002 23:50'!
adjustBrightness: brightness
	"Adjust the relative brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ Color
		h: self hue
		s: self saturation
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha! !

!Color methodsFor: 'transformations' stamp: 'dew 3/19/2002 23:51'!
adjustSaturation: saturation brightness: brightness
	"Adjust the relative saturation and brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ Color
		h: self hue
		s: (self saturation + saturation min: 1.0 max: 0.005)
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha! !

!Color methodsFor: 'transformations' stamp: 'jmv 6/18/2017 21:10:41'!
alpha: alphaValue
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifFalse: [
		^ TranslucentColor new
			setRed: self red
			green: self green
			blue: self blue
			alpha: alphaValue ]! !

!Color methodsFor: 'transformations' stamp: 'tk 7/4/2000 11:55'!
alphaMixed: proportion with: aColor 
	"Answer this color mixed with the given color. The proportion, a number 
	between 0.0 and 1.0, determines what what fraction of the receiver to  
	use in the mix. For example, 0.9 would yield a color close to the  
	receiver. This method uses RGB interpolation; HSV interpolation can lead 
	to surprises.  Mixes the alphas (for transparency) also."

	| frac1 frac2 |
	frac1 _ proportion asFloat min: 1.0 max: 0.0.
	frac2 _ 1.0 - frac1.
	^ Color
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)
		alpha: self alpha * frac1 + (aColor alpha * frac2)! !

!Color methodsFor: 'transformations' stamp: 'RAA 6/2/2000 08:47'!
atLeastAsLuminentAs: aFloat

	| revisedColor |
	revisedColor _ self.
	[revisedColor luminance < aFloat] whileTrue: [revisedColor _ revisedColor slightlyLighter].
	^revisedColor
! !

!Color methodsFor: 'transformations' stamp: 'nk 3/8/2004 09:43'!
atMostAsLuminentAs: aFloat

	| revisedColor |
	revisedColor _ self.
	[revisedColor luminance > aFloat] whileTrue: [revisedColor _ revisedColor slightlyDarker].
	^revisedColor
! !

!Color methodsFor: 'transformations' stamp: 'dew 3/23/2002 01:38'!
blacker

	^ self alphaMixed: 0.8333 with: Color black
! !

!Color methodsFor: 'transformations' stamp: 'dew 3/19/2002 23:54'!
dansDarker
	"Return a darker shade of the same color.
	An attempt to do better than the current darker method.
	(now obsolete, since darker has been changed to do this. -dew)"
	^ Color h: self hue s: self saturation
		v: (self brightness - 0.16 max: 0.0)! !

!Color methodsFor: 'transformations' stamp: 'dew 3/4/2002 01:40'!
darker
	"Answer a darker shade of this color."

	^ self adjustBrightness: -0.08! !

!Color methodsFor: 'transformations' stamp: 'dew 3/8/2002 00:13'!
duller

	^ self adjustSaturation: -0.03 brightness: -0.2! !

!Color methodsFor: 'transformations' stamp: 'dew 1/23/2002 20:19'!
lighter
	"Answer a lighter shade of this color."

	^ self adjustSaturation: -0.03 brightness: 0.08! !

!Color methodsFor: 'transformations' stamp: 'tk 7/4/2000 12:00'!
mixed: proportion with: aColor 
	"Mix with another color and do not preserve transpareny.  Only use this for extracting the RGB value and mixing it.  All other callers should use instead: 
	aColor alphaMixed: proportion with: anotherColor
	"

	| frac1 frac2 |
	frac1 _ proportion asFloat min: 1.0 max: 0.0.
	frac2 _ 1.0 - frac1.
	^ Color
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)! !

!Color methodsFor: 'transformations' stamp: 'dew 1/19/2002 01:29'!
muchDarker

	^ self alphaMixed: 0.5 with: Color black
! !

!Color methodsFor: 'transformations' stamp: 'tk 7/4/2000 12:07'!
muchLighter

	^ self alphaMixed: 0.233 with: Color white
! !

!Color methodsFor: 'transformations' stamp: 'ar 6/19/1999 00:36'!
negated
	"Return an RGB inverted color"
	^Color
		r: 1.0 - self red
		g: 1.0 - self green
		b: 1.0 - self blue! !

!Color methodsFor: 'transformations' stamp: 'jmv 6/18/2017 20:37:53'!
orColorUnlike: theOther
	"If this color is a lot like theOther, then return its complement, otherwide, return self"

	^ (self diff: theOther) < 0.3
		ifTrue: [theOther negated]
		ifFalse: [self]! !

!Color methodsFor: 'transformations' stamp: 'dew 3/4/2002 01:42'!
paler
	"Answer a paler shade of this color."

	^ self adjustSaturation: -0.09 brightness: 0.09
! !

!Color methodsFor: 'transformations' stamp: 'jmv 9/10/2009 18:41'!
quiteBlacker

	^ self alphaMixed: 0.8 with: Color black
! !

!Color methodsFor: 'transformations' stamp: 'jmv 9/10/2009 18:44'!
quiteWhiter

	^ self alphaMixed: 0.6 with: Color white! !

!Color methodsFor: 'transformations' stamp: 'dew 3/4/2002 01:43'!
slightlyDarker

	^ self adjustBrightness: -0.03
! !

!Color methodsFor: 'transformations' stamp: 'dew 3/4/2002 01:43'!
slightlyLighter

	^ self adjustSaturation: -0.01 brightness: 0.03! !

!Color methodsFor: 'transformations' stamp: 'dew 1/19/2002 01:25'!
slightlyWhiter

	^ self alphaMixed: 0.85 with: Color white
! !

!Color methodsFor: 'transformations' stamp: 'jmv 5/7/2012 15:05'!
twiceDarker
	"Answer a significantly darker shade of this color."

	^ self adjustSaturation: 0.076 brightness: -0.15! !

!Color methodsFor: 'transformations' stamp: 'dew 3/4/2002 01:45'!
twiceLighter
	"Answer a significantly lighter shade of this color."

	^ self adjustSaturation: -0.06 brightness: 0.15! !

!Color methodsFor: 'transformations' stamp: 'jmv 8/23/2009 23:24'!
veryMuchDarker

	^ self alphaMixed: 0.25 with: Color black
! !

!Color methodsFor: 'transformations' stamp: 'jmv 7/28/2016 15:43:04'!
veryMuchLighter

	^ self alphaMixed: 0.07 with: Color white! !

!Color methodsFor: 'transformations' stamp: 'dew 3/23/2002 01:38'!
whiter

	^ self alphaMixed: 0.8333 with: Color white
! !

!Color methodsFor: 'private'!
attemptToMutateError
	"A color is immutable. Once a color's red, green, and blue have been initialized, you cannot change them. Instead, create a new Color and use it."

	self error: 'Color objects are immutable once created'
! !

!Color methodsFor: 'private' stamp: 'jmv 6/18/2017 20:29:32'!
basicSetRed: r green: g blue: b
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	self
		at: 1 put: r;
		at: 2 put: g;
		at: 3 put: b! !

!Color methodsFor: 'private' stamp: 'jmv 6/18/2017 20:15:44'!
setHue: hue chroma: chroma brightness: brightness
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 
	hf _ hue \\ 360.
	i _ hf // 60.  				"integer part of hue"
	x _ (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x _ x \\  2 .
	i \\ 2 = 1 ifTrue: [ x _ 1.0 - x ].
	x _ chroma  * x.

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].

	m _ brightness - ((r1 max: g1) max: b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 _ r1 + m.
	r1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 _ g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 _ b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1! !

!Color methodsFor: 'private' stamp: 'jmv 6/18/2017 20:15:49'!
setHue: hue chroma: chroma luminance: luma
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 

	hf _ hue \\ 360.
	i _ hf // 60.  				"integer part of hue"
	x _ (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x _ x \\  2 .
	i \\ 2 = 1 ifTrue: [ x _ 1.0 - x ].
	x _ chroma  * x.

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].

	m _ luma - (0.299*r1) - (0.587*g1) - (0.114*b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 _ r1 + m.
	r1 > 1.0 
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 _ g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 _ b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1! !

!Color methodsFor: 'private' stamp: 'jmv 6/18/2017 20:16:07'!
setHue: hue saturation: saturation brightness: brightness
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."

	| s v hf i f p q t | 
	s _ saturation asFloat min: 1.0 max: 0.0.
	v _ brightness asFloat min: 1.0 max: 0.0.
	hf _ hue \\ 360.
	i _ hf // 60.  			"integer part of hue"
	f _ (hf \\ 60) / 60.0.         	"fractional part of hue"

	p _ (1.0 - s) * v.
	q _ (1.0 - (s * f)) * v.
	t _ (1.0 - (s * (1.0 - f))) * v.

	0 = i ifTrue: [ ^ self setRed: v green: t blue: p ].
	1 = i ifTrue: [ ^ self setRed: q green: v blue: p ].
	2 = i ifTrue: [ ^ self setRed: p green: v blue: t ].
	3 = i ifTrue: [ ^ self setRed: p green: q blue: v ].
	4 = i ifTrue: [ ^ self setRed: t green: p blue: v ].
	5 = i ifTrue: [ ^ self setRed: v green: p blue: q ].

	self error: 'implementation error'! !

!Color methodsFor: 'private' stamp: 'jmv 6/18/2017 20:13:04'!
setRed: r green: g blue: b

	self basicSetRed: r green: g blue: b! !

!Color methodsFor: 'private' stamp: 'jmv 6/18/2017 20:29:48'!
setRed: r green: g blue: b range: range
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	self basicSetRed: r green: g blue: b.
	self /= range! !

!Color methodsFor: 'as yet unclassified' stamp: 'jmv 8/15/2015 18:23'!
color
	^ self! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 9/17/2015 15:22'!
clipR: r g: g b: b
	"Return a color with the given r, g, and b components in the range [0.0..1.0].
	Clip if necessary"

	^ self 
		r: (r min: 1.0 max: 0.0)
		g: (g min: 1.0 max: 0.0)
		b: (b min: 1.0 max: 0.0)! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/2/2016 23:05'!
colorFrom: parm
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"

	| aColor firstParm |
	(parm is: #Color) ifTrue: [ ^ parm ].
	parm isSymbol ifTrue: [ ^ self perform: parm ].
	(parm isSequenceableCollection and: [ parm size > 0 ])
		ifTrue: [
			firstParm _ parm first.
			firstParm isNumber ifTrue: [
				^ self fromRgbTriplet: parm].
			aColor _ self colorFrom: firstParm.
			parm withIndexDo: [ :sym :ind |
				ind > 1 ifTrue: [
					aColor _ aColor perform: sym ]].
			^ aColor].
	^ parm

"
Color colorFrom: #(blue darker)
Color colorFrom: Color blue darker
Color colorFrom: #blue
Color colorFrom: #(0.0 0.0 1.0)
"! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/2/2016 15:04'!
colorFromPixelValue: p depth: d
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

	d = 1 ifTrue: [^ self indexedColors at: (p bitAnd: 16r01) + 1].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	p = 0 ifTrue: [ ^Color transparent ].

	d = 8 ifTrue: [^ self indexedColors at: (p bitAnd: 16rFF) + 1].
	d = 4 ifTrue: [^ self indexedColors at: (p bitAnd: 16r0F) + 1].
	d = 2 ifTrue: [^ self indexedColors at: (p bitAnd: 16r03) + 1].

	d = 32 ifTrue: [
		"eight bits per component; 8 bits of alpha"
		alpha _ p bitShift: -24.
		alpha = 0 ifTrue: [ ^Color transparent ].
		r _ (p bitShift: -16) bitAnd: 16rFF.
		g _ (p bitShift: -8) bitAnd: 16rFF.
		b _ p bitAnd: 16rFF.
		^alpha < 255
			ifTrue: [ (Color r: r g: g b: b range: 255) alpha: alpha asFloat / 255.0 ]
			ifFalse: [ Color r: r g: g b: b range: 255 ]].

	d = 16 ifTrue: [
		"five bits per component. The most significant bit, unused, allows having real black, without p being zero"
		r _ (p bitShift: -10) bitAnd: 16r1F.
		g _ (p bitShift: -5) bitAnd: 16r1F.
		b _ p bitAnd: 16r1F.
		^ Color r: r g: g b: b range: 31].

	d = 15 ifTrue: [
		"five bits per component"
		r _ (p bitShift: -10) bitAnd: 16r1F.
		g _ (p bitShift: -5) bitAnd: 16r1F.
		b _ p bitAnd: 16r1F.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^Color black ].
		^ Color r: r g: g b: b range: 31].

	d = 12 ifTrue: [
		"four bits per component"
		r _ (p bitShift: -8) bitAnd: 16rF.
		g _ (p bitShift: -4) bitAnd: 16rF.
		b _ p bitAnd: 16rF.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^Color black ].
		^ Color r: r g: g b: b range: 15].

	d = 9 ifTrue: [
		"three bits per component"
		r _ (p bitShift: -6) bitAnd: 16r7.
		g _ (p bitShift: -3) bitAnd: 16r7.
		b _ p bitAnd: 16r7.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^Color black ].
		^ Color r: r g: g b: b range: 7].

	self error: 'unknown pixel depth: ', d printString! !

!Color class methodsFor: 'instance creation' stamp: 'sqr 10/15/2016 20:41:04'!
fromArray: colorDef

	| answer |
	colorDef size = 0 ifTrue: [^self transparent].
	colorDef size between: 3 and: 4 :: ifFalse: [self error: 'Undefined color definition'].
	answer _ self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3).
	^colorDef size = 3
		ifTrue: [answer]
		ifFalse: [answer alpha: (colorDef at: 4)]! !

!Color class methodsFor: 'instance creation' stamp: 'sw 8/8/97 22:03'!
fromRgbTriplet: list
	^ self r: list first g: list second b: list last! !

!Color class methodsFor: 'instance creation' stamp: 'pb 10/16/2016 18:42:44'!
fromString: aString
	"For HTML color spec: #FFCCAA. Also support named colors.
	See http://www.w3schools.com/cssref/css_colors_legal.asp"
	"
	Color fromString: '#FFCCAA'.
	Color fromString: 'white'.
	Color fromString: 'orange'
	Color fromString: 'rgb(255,0,98)'
	"
	(aString size = 7 and: [ aString first = $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU _ aString asUppercase.
		red _ ('16r', (aColorHexU copyFrom: 2 to: 3)) asNumber/255.
		green _ ('16r', (aColorHexU copyFrom: 4 to: 5)) asNumber/255.
		blue _ ('16r', (aColorHexU copyFrom: 6 to: 7)) asNumber/255.
		^ self r: red g: green b: blue].

	(aString beginsWith: 'rgb')
		ifTrue: [|values r g b|
				values := (aString allButFirst: 4) allButLast findTokens: ','.
				r  := (values first includes: $%)
						ifTrue: [(values first asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values first asInteger min: 255 max: 0) / 255.0].
				g := (values second includes: $%)
						ifTrue: [(values second asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values second asInteger min: 255 max: 0) / 255.0].
				b := (values third includes: $%)
						ifTrue: [(values third asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values third asInteger min: 255 max: 0) / 255.0].
				^self r: r g: g b: b].

	^self exactColorNamed: aString! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:29'!
gray: brightness
	"Return a gray shade with the given brightness in the range [0.0..1.0]."

	^ self new
		setRed: brightness
		green: brightness
		blue: brightness! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:31'!
h: hue s: saturation v: brightness
	"Create a color with the given hue, saturation, and brightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1) is pure red."
	"Note: By convention, brightness is abbreviated 'v' to to avoid confusion with blue."

	^ self new setHue: hue saturation: saturation brightness: brightness! !

!Color class methodsFor: 'instance creation' stamp: 'dew 3/19/2002 23:49'!
h: h s: s v: v alpha: alpha

	^ (self h: h s: s v: v) alpha: alpha! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:34'!
hue: hue chroma: chroma brightness: brightness
	^self new setHue: hue chroma: chroma brightness: brightness! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:37'!
hue: hue chroma: chroma luminance: luma
	^self new setHue: hue chroma: chroma luminance: luma! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:39'!
hue: hue saturation: saturation brightness: brightness
	"Create a color with the given hue, saturation, and brightness. Hue is given as the angle in degrees of the color on the color circle where red is zero degrees. Saturation and brightness are numbers in [0.0..1.0] where larger values are more saturated or brighter colors. For example, (Color h: 0 s: 1 v: 1) is pure red."
	"Note: By convention, brightness is abbreviated 'v' to to avoid confusion with blue."

	^ self new setHue: hue saturation: saturation brightness: brightness! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:54:20'!
new
	^ self new: 3! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:41'!
r: r g: g b: b
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."

	^ self new
		setRed: r
		green: g
		blue: b! !

!Color class methodsFor: 'instance creation'!
r: r g: g b: b alpha: alpha

	^ (self r: r g: g b: b) alpha: alpha! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:43'!
r: r g: g b: b range: range
	"Return a color with the given r, g, and b components specified as integers in the range [0..r]. This avoids the floating point arithmetic in the red:green:blue: message and is thus a bit faster for certain applications (such as computing a sequence of colors for a palette)."

	^ self new setRed: r green: g blue: b range: range! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:30'!
random
	"Return a random color that isn't too dark or under-saturated.
		Display fill: (10@10 extent: 200@200) fillColor: Color random
	"

	^ Random withDefaultDo: [ :random |
		self random: random ]! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/4/2015 20:30'!
random2
	"Return a random color with a distribution that spans over all possible colors.
		Display fill: (10@10 extent: 200@200) fillColor: Color random2
	"

	^ Random withDefaultDo: [ :random |
		self random2: random ]! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:46'!
random2: aRandom
	"Return a random color with a distribution that spans over all possible colors."

	^ self new
		setRed: aRandom next
		green: aRandom next
		blue: aRandom next! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:56:48'!
random: aRandom
	"Return a random color that isn't too dark or under-saturated."

	^ self new
		setHue: (360.0 * aRandom next)
		saturation: (0.3 + (aRandom next * 0.7))
		brightness: (0.4 + (aRandom next * 0.6))! !

!Color class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 15:06'!
initializeGrayToIndexMap
	"Build an array of gray values available in the 8-bit colormap. This array is indexed by a gray level between black (1) and white (256) and returns the pixel value for the corresponding gray level."
	"Note: This method must be called after initializeIndexedColors, since it uses IndexedColors."
	"
	Color initializeGrayToIndexMap
	"

	| grayLevels grayIndices c distToClosest dist indexOfClosest indexedColors |
	"record the level and index of each gray in the 8-bit color table"
	grayLevels _ OrderedCollection new.
	grayIndices _ OrderedCollection new.
	indexedColors _ self indexedColors.
	"Note: skip the first entry, which is reserved for transparent"
	2 to: indexedColors size do: [:i |
		c _ indexedColors at: i.
		c saturation = 0.0 ifTrue: [  "c is a gray"
			grayLevels add: (c green * 255) rounded.  "0 to 255; R, G, and B are the same"
			grayIndices add: i - 1]].  "pixel values are zero-based"
	grayLevels _ grayLevels asArray.
	grayIndices _ grayIndices asArray.

	"for each gray level in [0..255], select the closest match"
	GrayToIndexMap _ ByteArray new: 256.
	0 to: 255 do: [ :level |
		distToClosest _ 10000.  "greater than distance to any real gray"
		1 to: grayLevels size do: [:i |
			dist _ (level - (grayLevels at: i)) abs.
			dist < distToClosest ifTrue: [
				distToClosest _ dist.
				indexOfClosest _ grayIndices at: i]].
		GrayToIndexMap at: (level + 1) put: indexOfClosest]! !

!Color class methodsFor: 'class initialization'!
initializeIndexedColors
	"Build an array of colors corresponding to the fixed colormap used
	 for display depths of 1, 2, 4, or 8 bits."
	"Color initializeIndexedColors"

	| a index grayVal |
	a _ Array new: 256.

	"1-bit colors (monochrome)"
	a at: 1 put: (Color r: 1.0 g: 1.0 b: 1.0).		"white or transparent"
	a at: 2 put: (Color r: 0.0 g: 0.0 b: 0.0).	"black"

	"additional colors for 2-bit color"
	a at: 3 put: (Color r: 1.0 g: 1.0 b: 1.0).	"opaque white"
	a at: 4 put: (Color r: 0.5 g: 0.5 b: 0.5).	"1/2 gray"

	"additional colors for 4-bit color"
	a at:  5 put: (Color r: 1.0 g: 0.0 b: 0.0).	"red"
	a at:  6 put: (Color r: 0.0 g: 1.0 b: 0.0).	"green"
	a at:  7 put: (Color r: 0.0 g: 0.0 b: 1.0).	"blue"
	a at:  8 put: (Color r: 0.0 g: 1.0 b: 1.0).	"cyan"
	a at:  9 put: (Color r: 1.0 g: 1.0 b: 0.0).	"yellow"
	a at: 10 put: (Color r: 1.0 g: 0.0 b: 1.0).	"magenta"

	a at: 11 put: (Color r: 0.125 g: 0.125 b: 0.125).		"1/8 gray"
	a at: 12 put: (Color r: 0.25 g: 0.25 b: 0.25).		"2/8 gray"
	a at: 13 put: (Color r: 0.375 g: 0.375 b: 0.375).		"3/8 gray"
	a at: 14 put: (Color r: 0.625 g: 0.625 b: 0.625).		"5/8 gray"
	a at: 15 put: (Color r: 0.75 g: 0.75 b: 0.75).		"6/8 gray"
	a at: 16 put: (Color r: 0.875 g: 0.875 b: 0.875).		"7/8 gray"

	"additional colors for 8-bit color"
	"24 more shades of gray (1/32 increments but not repeating 1/8 increments)"
	index _ 17.
	1 to: 31 do: [:v |
		(v \\ 4) = 0 ifFalse: [
			grayVal _ v / 32.0.
			a at: index put: (Color r: grayVal g: grayVal b: grayVal).
			index _ index + 1]].

	"The remainder of color table defines a color cube with six steps
	 for each primary color. Note that the corners of this cube repeat
	 previous colors, but this simplifies the mapping between RGB colors
	 and color map indices. This color cube spans indices 40 through 255
	 (indices 41-256 in this 1-based array)."
	0 to: 5 do: [:r |
		0 to: 5 do: [:g |
			0 to: 5 do: [:b |
				index _ 41 + ((36 * r) + (6 * b) + g).
				index > 256 ifTrue: [
					self error: 'index out of range in color table compuation'].
				a at: index put: (Color r: r g: g b: b range: 5)]]].

	IndexedColors _ a.
! !

!Color class methodsFor: 'examples' stamp: 'jmv 8/17/2012 18:48'!
colorRampForDepth: depth extent: aPoint
	"Returns a form of the given size showing R, G, B, and gray ramps for the given depth. Useful for testing color conversions between different depths."
	"(Color colorRampForDepth: Display depth extent: 256@80) display"
	"(Color colorRampForDepth: 32 extent: 256@80) displayOn: Display at: 0@0 rule: Form paint"

	| f dx dy r |
	f _ Form extent: aPoint depth: depth.
	dx _ aPoint x // 256.
	dy _ aPoint y // 4.
	0 to: 255 do: [:i |
		r _ (dx * i)@0 extent: dx@dy.
		f fill: r fillColor: (Color r: i g: 0 b: 0 range: 255).
		r _ r translatedBy: 0@dy.
		f fill: r fillColor: (Color r: 0 g: i b: 0 range: 255).
		r _ r translatedBy: 0@dy.
		f fill: r fillColor: (Color r: 0 g: 0 b: i range: 255).
		r _ r translatedBy: 0@dy.
		f fill: r fillColor: (Color r: i g: i b: i range: 255)].
	^ f
! !

!Color class methodsFor: 'examples' stamp: 'jmv 6/18/2017 21:29:30'!
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

!Color class methodsFor: 'examples' stamp: 'tk 6/19/96'!
hotColdShades: thisMany
	"An array of thisMany colors showing temperature from blue to red to white hot.  (Later improve this by swinging in hue.)  "
	"Color showColors: (Color hotColdShades: 25)"

	| n s1 s2 s3 s4 s5 |
	thisMany < 5 ifTrue: [^ self error: 'must be at least 5 shades'].
	n _ thisMany // 5.
	s1 _ self white mix: self yellow shades: (thisMany - (n*4)).
	s2 _ self yellow mix: self red shades: n+1.
	s2 _ s2 copyFrom: 2 to: n+1.
	s3 _ self red mix: self green darker shades: n+1.
	s3 _ s3 copyFrom: 2 to: n+1.
	s4 _ self green darker mix: self blue shades: n+1.
	s4 _ s4 copyFrom: 2 to: n+1.
	s5 _ self blue mix: self black shades: n+1.
	s5 _ s5 copyFrom: 2 to: n+1.
	^ s1, s2, s3, s4, s5
! !

!Color class methodsFor: 'examples'!
showColorCube
	"Show a 12x12x12 color cube."
	"Color showColorCube"

	0 to: 11 do: [:r |
		0 to: 11 do: [:g |
			0 to: 11 do: [:b |	
				Display fill: (((r*60) + (b*5)) @ (g*5) extent: 5@5)
					fillColor: (Color r: r g: g b: b range: 11)]]].
! !

!Color class methodsFor: 'examples' stamp: 'jmv 8/17/2012 18:48'!
showColors: colorList
	"Display the given collection of colors across the top of the Display."

	| w r |
	w _ Display width // colorList size.
	r _ 0@0 extent: w@((w min: 30) max: 10).
	colorList do: [:c |
		Display fill: r fillColor: c.
		r _ r translatedBy: w@0].
! !

!Color class methodsFor: 'examples'!
showHSVPalettes
	"Shows a palette of hues, varying the saturation and brightness for each one. Best results are with depths 16 and 32."
	"Color showHSVPalettes"

	| left top c |
	left _ top _ 0.
	0 to: 179 by: 15 do: [:h |
		0 to: 10 do: [:s |
			left _ (h * 4) + (s * 4).
			0 to: 10 do: [:v |
				c _ Color h: h s: s asFloat / 10.0 v: v asFloat / 10.0.
				top _ (v * 4).
				Display fill: (left@top extent: 4@4) fillColor: c.

				c _ Color h: h + 180 s: s asFloat / 10.0 v: v asFloat / 10.0.
				top _ (v * 4) + 50.
				Display fill: (left@top extent: 4@4) fillColor: c]]].
! !

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

!Color class methodsFor: 'examples'!
wheel: thisMany
	"Return a collection of thisMany colors evenly spaced around the color wheel."
	"Color showColors: (Color wheel: 12)"

	^ Color wheel: thisMany saturation: 0.9 brightness: 0.7
! !

!Color class methodsFor: 'examples'!
wheel: thisMany saturation: s brightness: v
	"Return a collection of thisMany colors evenly spaced around the color wheel, all of the given saturation and brightness."
	"Color showColors: (Color wheel: 12 saturation: 0.4 brightness: 1.0)"
	"Color showColors: (Color wheel: 12 saturation: 0.8 brightness: 0.5)"

	^ (Color h: 0.0 s: s v: v) wheel: thisMany
! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:58'!
aqua

	^ self colorNamesDict at: #aqua! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:58'!
beige

	^ self colorNamesDict at: #beige! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:58'!
black

	^ self colorNamesDict at: #black! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:58'!
blue

	^ self colorNamesDict at: #blue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:58'!
brightGreen

	^ self colorNamesDict at: #brightGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:58'!
brown

	^ self colorNamesDict at: #brown! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
cyan

	^ self colorNamesDict at: #cyan! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
darkBlue

	^ self colorNamesDict at: #darkBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
darkGray

	^ self colorNamesDict at: #darkGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
darkGreen

	^ self colorNamesDict at: #darkGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
darkPink

	^ self colorNamesDict at: #darkPink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
darkPurple

	^ self colorNamesDict at: #darkPurple! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
forestGreen

	^ self colorNamesDict at: #forestGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
gray

	^ self colorNamesDict at: #gray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
green

	^ self colorNamesDict at: #green! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
grey

	^ self colorNamesDict at: #grey! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
hotPink

	^ self colorNamesDict at: #hotPink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
indigo

	^ self colorNamesDict at: #indigo! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lavender

	^ self colorNamesDict at: #lavender! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lightBlue

	^ self colorNamesDict at: #lightBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lightBrown

	^ self colorNamesDict at: #lightBrown! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lightCyan

	^ self colorNamesDict at: #lightCyan! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lightGray

	^ self colorNamesDict at: #lightGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lightGreen

	^ self colorNamesDict at: #lightGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lightMagenta

	^ self colorNamesDict at: #lightMagenta! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 14:59'!
lightOrange

	^ self colorNamesDict at: #lightOrange! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
lightPink

	^ self colorNamesDict at: #lightPink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
lightPurple

	^ self colorNamesDict at: #lightPurple! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
lightRed

	^ self colorNamesDict at: #lightRed! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
lightYellow

	^ self colorNamesDict at: #lightYellow! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
lilac

	^ self colorNamesDict at: #lilac! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
lime

	^ self colorNamesDict at: #lime! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
limeGreen

	^ self colorNamesDict at: #limeGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
magenta

	^ self colorNamesDict at: #magenta! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
maroon

	^ self colorNamesDict at: #maroon! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
mauve

	^ self colorNamesDict at: #mauve! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
mustard

	^ self colorNamesDict at: #mustard! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
navyBlue

	^ self colorNamesDict at: #navyBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
olive

	^ self colorNamesDict at: #olive! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
oliveGreen

	^ self colorNamesDict at: #oliveGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
orange

	^ self colorNamesDict at: #orange! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
paleGreen

	^ self colorNamesDict at: #paleGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
peach

	^ self colorNamesDict at: #peach! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
periwinkle

	^ self colorNamesDict at: #periwinkle! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
pink

	^ self colorNamesDict at: #pink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
purple

	^ self colorNamesDict at: #purple! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:00'!
red

	^ self colorNamesDict at: #red! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
royalBlue

	^ self colorNamesDict at: #royalBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
salmon

	^ self colorNamesDict at: #salmon! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
seaGreen

	^ self colorNamesDict at: #seaGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
skyBlue

	^ self colorNamesDict at: #skyBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
tan

	^ self colorNamesDict at: #tan! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
teal

	^ self colorNamesDict at: #teal! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
transparent

	^ self colorNamesDict at: #transparent! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
turquoise

	^ self colorNamesDict at: #turquoise! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
veryDarkGray

	^ self colorNamesDict at: #veryDarkGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
veryLightGray

	^ self colorNamesDict at: #veryLightGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
veryVeryDarkGray

	^ self colorNamesDict at: #veryVeryDarkGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
veryVeryLightGray

	^ self colorNamesDict at: #veryVeryLightGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
violet

	^ self colorNamesDict at: #violet! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
white

	^ self colorNamesDict at: #white! !

!Color class methodsFor: 'named colors' stamp: 'jmv 4/17/2015 15:01'!
yellow

	^ self colorNamesDict at: #yellow! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:49'!
cachedColormapForGrayFrom: sourceDepth
	"Return a cached colormap for mapping from the given depth to the 8bpp grays. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| srcIndex map |
	ToGrayColormaps ifNil: [
		ToGrayColormaps _ Array new: 5].
	
	srcIndex _ sourceDepth highBit min: 5.

	(ToGrayColormaps at: srcIndex) ifNotNil: [ :m | ^ m ].

	map _ self computeColormapForGray8bppFrom: sourceDepth.
	ToGrayColormaps at: srcIndex put: map.
	^ map! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 14:50'!
cachedColormapFrom: sourceDepth to: destDepth
	"Return a cached colormap for mapping between the given depths. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| srcIndex dstIndex map |
	CachedColormaps
		ifNil: [CachedColormaps _ (1 to: 5) collect: [:i | Array new: 6]].

	srcIndex _ sourceDepth highBit min: 5.
	dstIndex _ destDepth highBit.

	((CachedColormaps at: srcIndex) at: dstIndex) ifNotNil: [ :m | ^ m ].

	map _ self computeColormapFrom: sourceDepth to: destDepth.
	(CachedColormaps at: srcIndex) at: dstIndex put: map.
	^ map! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:49'!
cachedColormapFromGrayTo: destDepth
	"Return a cached colormap for mapping from 8bpp grays to the given depth. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| dstIndex map |
	FromGrayColormaps
		ifNil: [FromGrayColormaps _ Array new: 6 ].

	dstIndex _ destDepth highBit.

	(FromGrayColormaps at: dstIndex) ifNotNil: [ :m | ^ m ].

	map _ self computeColormapFromGray8bppForDepth: destDepth.
	FromGrayColormaps at: dstIndex put: map.
	^ map! !

!Color class methodsFor: 'colormaps'!
colorMapIfNeededFrom: sourceDepth to: destDepth
	"Return a colormap for mapping between the given depths, or nil if no colormap is needed."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"

	sourceDepth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"

	(sourceDepth >= 16) & (destDepth >= 16) ifTrue: [
		"mapping is done in BitBlt by zero-filling or truncating each color component"
		^ nil].

	^ Color cachedColormapFrom: sourceDepth to: destDepth
! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 8/27/2009 08:47'!
computeColorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix
	"Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same"
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color.
		Assumed not to include subpixelAA"
		^ self computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth
	] ifFalse: [
		"source is 16-bit or 32-bit RGB.
		Might include subpixelAA"
		^ self computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix
	]! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:55'!
computeColormapForGray8bppFrom: sourceDepth
	"Compute a colorMap for translatingfrom the given depth to the 8bpp grays"
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map |
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color"
		map _ (self indexedColors copyFrom: 1 to: (1 bitShift: sourceDepth))
					collect: [ :c | (c luminance * 255) rounded].
		map _ map as: Bitmap.
		^ map
		]
	ifFalse: [
		^ self computeRGBColormapForGray8 ]! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 4/17/2015 15:05'!
computeColormapFrom: sourceDepth to: destDepth
	"Compute a colorMap for translating between the given depths. A colormap is a Bitmap whose entries contain the pixel values for the destination depth. Typical clients use cachedColormapFrom:to: instead."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map bitsPerColor |
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color"
		map _ (self indexedColors copyFrom: 1 to: (1 bitShift: sourceDepth))
					collect: [:c | c pixelValueForDepth: destDepth].
		map _ map as: Bitmap.
	] ifFalse: [
		"source is 16-bit or 32-bit RGB"
		destDepth > 8
			ifTrue: [bitsPerColor _ 5]  "retain maximum color resolution"
			ifFalse: [bitsPerColor _ 4].
		map _ self computeRGBColormapFor: destDepth bitsPerColor: bitsPerColor].

	"Note: zero is transparent except when source depth is one-bit deep"
	sourceDepth > 1 ifTrue: [map at: 1 put: 0].
	^ map
! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:56'!
computeColormapFromGray8bppForDepth: destDepth
	"Return a colormap for displaying a GrayForm at the given depth"

	| newMap |
	newMap _ Bitmap new: 256.
	1 to: 256 do: [ :i |
		newMap
			at: i
			put: ((Color gray: (i-1) asFloat / 255.0) pixelValueForDepth: destDepth)].
	^ newMap! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 4/17/2015 15:05'!
computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth
	| map |
	
	map _ (self indexedColors copyFrom: 1 to: (1 bitShift: sourceDepth)) collect: [ :cc | | f c |
		f _ 1.0 - (cc red + cc green + cc blue / 3.0 ).
		c _ targetColor
			ifNotNil: [
				destDepth = 32
					ifTrue: [ targetColor * f alpha: f]
					ifFalse: [ targetColor alphaMixed: f*1.5 with: Color white ]]
			ifNil: [ cc ].
		destDepth = 32
			ifTrue: [ c pixelValueForDepth: destDepth]
			ifFalse: [
				f = 0.0
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ]]].
	map _ map as: Bitmap.
	^map! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 12/26/2011 13:49'!
computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix
	"Builds a colormap intended to convert from subpixelAA black values to targetColor values.
	keepSubPix
		ifTrue: [ Answer colors that also include subpixelAA ]
		ifFalse: [ 
			Take fullpixel luminance level. Apply it to targetColor.
			I.e. answer colors with NO subpixelAA ]"

	| mask map c bitsPerColor r g b f v |

	destDepth > 8
		ifTrue: [bitsPerColor _ 5]  "retain maximum color resolution"
		ifFalse: [bitsPerColor _ 4].
	"Usually a bit less is enough, but make it configurable"
	bitsPerColor _ bitsPerColor min: Preferences aaFontsColormapDepth.
	mask _ (1 bitShift: bitsPerColor) - 1.
	map _ Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		r _ (i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask.
		g _ (i bitShift: 0 - bitsPerColor) bitAnd: mask.
		b _ (i bitShift: 0) bitAnd: mask.
		f _ 1.0 - (r + g + b / 3.0 / mask).
		c _ targetColor
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
						ifFalse: [ targetColor alphaMixed: f*1.5 with: Color white ]]]
			ifNil: [ Color r: r g: g b: b range: mask].	"This is currently used only to keep some SubPixelAA on destDepth = 8, using a single pass of rule 25"
		v _ destDepth = 32
			ifTrue: [ c pixelValueForDepth: destDepth]
			ifFalse: [
				f < 0.1
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ]].
		map at: i + 1 put: v ].
	^ map! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 1/30/2011 23:10'!
computeRGBColormapFor: destDepth bitsPerColor: bitsPerColor
	"Compute a colorMap for translating from 16-bit or 32-bit RGB color to the given depth, using the given number of of bits per color component."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| mask map c |
	(#(3 4 5) includes: bitsPerColor)
		ifFalse: [self error: 'BitBlt only supports 3, 4, or 5 bits per color component'].
	mask _ (1 bitShift: bitsPerColor) - 1.
	map _ Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		c _ Color
			r: ((i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask)
			g: ((i bitShift: 0 - bitsPerColor) bitAnd: mask)
			b: ((i bitShift: 0) bitAnd: mask)
			range: mask.
		map at: i + 1 put: (c pixelValueForDepth: destDepth)].

	map at: 1 put: (Color transparent pixelWordForDepth: destDepth).  "zero always transparent"
	^ map
! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:57'!
computeRGBColormapForGray8
	"Compute a colorMap for translating from 16-bit or 32-bit RGB color to 8bpp grays, using the default number of of bits per color component."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| bitsPerColor mask map c |
	bitsPerColor _ 5.
	mask _ (1 bitShift: bitsPerColor) - 1.
	map _ Bitmap new: (1 bitShift: (3 * bitsPerColor)).
	0 to: map size - 1 do: [:i |
		c _ Color
			r: ((i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask)
			g: ((i bitShift: 0 - bitsPerColor) bitAnd: mask)
			b: ((i bitShift: 0) bitAnd: mask)
			range: mask.
		map at: i + 1 put: (c luminance * 255) rounded].

	^ map! !

!Color class methodsFor: 'other' stamp: 'jmv 1/31/2011 09:30'!
maskingMap: depth
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero. Cache the most recently used map."
	"Warning: The behavior is incorrect if depth = 32.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| sizeNeeded |
	depth <= 8
		ifTrue: [sizeNeeded _ 1 bitShift: depth]
		ifFalse: [sizeNeeded _ 4096].

	(MaskingMap == nil or: [MaskingMap size ~= sizeNeeded]) ifTrue:
		[MaskingMap _ Bitmap new: sizeNeeded withAll: 16rFFFFFFFF.
		MaskingMap at: 1 put: 0.  "transparent"].

	^ MaskingMap
! !

!Color class methodsFor: 'color from user' stamp: 'jmv 3/1/2010 15:13'!
colorPaletteForDepth: depth extent: chartExtent
	"Display a palette of colors sorted horizontally by hue and vertically by lightness. Useful for eyeballing the color gamut of the display, or for choosing a color interactively."
	"Note: It is slow to build this palette, so it should be cached for quick access."
	"(Color colorPaletteForDepth: 16 extent: 190@60) display"

	| basicHue x y startHue palette transHt vSteps transCaption grayWidth hSteps |
	palette _ Form extent: chartExtent depth: depth.
	transCaption _ 
		(Form extent: 34@9 depth: 1
			fromArray: #(0 0 256 0 256 0 3808663859 2147483648 2491688266 2147483648 2491688266 0 2491688266 0 2466486578 0 0 0)
			offset: 0@0).
	transHt _ transCaption height.
	palette fillWhite: (0@0 extent: palette width@transHt).
	palette fillBlack: (0@transHt extent: palette width@1).
	transCaption displayOn: palette at: palette boundingBox topCenter - ((transCaption width // 2)@0).
	grayWidth _ 10.
	startHue _ 338.0.
	vSteps _ palette height - transHt // 2.
	hSteps _ palette width - grayWidth.
	x _ 0.
	startHue to: startHue + 360.0 by: 360.0/hSteps do: [ :h |
		basicHue _ Color h: h asFloat s: 1.0 v: 1.0.
		y _ transHt+1.
		0 to: vSteps do: [ :n | | c |
 			c _ basicHue mixed: (n asFloat / vSteps asFloat) with: Color white.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y _ y + 1].
		1 to: vSteps do: [ :n | | c |
 			c _ Color black mixed: (n asFloat / vSteps asFloat) with: basicHue.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y _ y + 1].
		x _ x + 1].
	y _ transHt + 1.
	1 to: vSteps * 2 do: [ :n | | c |
 		c _ Color black mixed: (n asFloat / (vSteps*2) asFloat) with: Color white.
		palette fill: (x@y extent: 10@1) fillColor: c.
		y _ y + 1].
	^ palette
! !

!Color class methodsFor: 'color from user' stamp: 'jmv 7/27/2009 11:32'!
colorTest: depth extent: chartExtent colorMapper: colorMapper
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

	| basicHue x y c startHue palette transHt vSteps transCaption grayWidth hSteps |
	palette _ Form extent: chartExtent depth: depth.
	transCaption _ 
		(Form extent: 34@9 depth: 1
			fromArray: #(0 0 256 0 256 0 3808663859 2147483648 2491688266 2147483648 2491688266 0 2491688266 0 2466486578 0 0 0)
			offset: 0@0).
	transHt _ transCaption height.
	palette fillWhite: (0@0 extent: palette width@transHt).
	palette fillBlack: (0@transHt extent: palette width@1).
	transCaption displayOn: palette at: palette boundingBox topCenter - ((transCaption width // 2)@0).
	grayWidth _ 10.
	startHue _ 338.0.
	vSteps _ palette height - transHt // 2.
	hSteps _ palette width - grayWidth.
	x _ 0.
	startHue to: startHue + 360.0 by: 360.0/hSteps do: [:h |
		basicHue _ Color h: h asFloat s: 1.0 v: 1.0.
		y _ transHt+1.
		0 to: vSteps do: [:n |
 			c _ basicHue mixed: (n asFloat / vSteps asFloat) with: Color white.
			c _ colorMapper value: c.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y _ y + 1].
		1 to: vSteps do: [:n |
 			c _ Color black mixed: (n asFloat / vSteps asFloat) with: basicHue.
			c _ colorMapper value: c.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y _ y + 1].
		x _ x + 1].
	y _ transHt + 1.
	1 to: vSteps * 2 do: [:n |
 		c _ Color black mixed: (n asFloat / (vSteps*2) asFloat) with: Color white.
		c _ colorMapper value: c.
		palette fill: (x@y extent: 10@1) fillColor: c.
		y _ y + 1].
	^ palette
! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/26/2013 20:13'!
colorNames
	"Answer the current dictionary of  name->color associations."

	^self colorNamesDict keys! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/28/2013 20:34'!
defaultColorNamesDictionary
	"Answer a amall but  useful name->color dictionary"
	
	| defaultDict |
	"Meld most used xkcd colors into traditional colors."
	defaultDict := (self traditionalColorNamesDictionary addAll: 
					self xkcdFirst48ColorNamesDictionary; 
					yourself).
		
	"Override traditional names existing in extended XKCD naming"
	defaultDict at: #lightYellow    put: (Color r: 1.0 g: 0.996 b: 0.478).
	defaultDict at: #lightOrange   put: (Color r: 0.992 g: 0.667 b: 0.283).
	defaultDict at: #lightCyan        put: (Color r: 0.674 g: 1.0 b: 0.988).
	defaultDict at: #lightRed          put: (Color r: 1.0 g: 0.279 b: 0.298).
	defaultDict at: #lightMagenta put: (Color r: 0.98 g: 0.372 b: 0.969).
	
	^defaultDict! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 2/1/2013 14:39'!
doesNotUnderstand: aMessage
	"Some code takes
		 Color colorNames 
	and does
		Color perform: aColorname.
		
	Make this work."

	^(Color colorNamesDict) 
		at: (aMessage selector)
		ifAbsent: [super doesNotUnderstand: aMessage]! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/26/2013 20:51'!
exactColorNamed: aColorName

	"Answer color matching name or nil 
	from my ColorName->Color Dictionary"
	
	^ self colorNamesDict at: (aColorName asSymbol) ifAbsent: [nil]! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 7/25/2014 21:22'!
setColorNamesDict: aNameColorDictionary
	"Answer the current dictionary of  name->color associations."
	
	"Note: This is a non-standard name because this is a non-standard action!!
	Do this if you really, really know what you are doing."
	
	"Some Colors are required for proper system operation"
	Color defaultColorNamesDictionary keysAndValuesDo: [ :colorName :colorValue |
		aNameColorDictionary at: colorName ifAbsentPut: colorValue
	].

	ColorNamesDict := aNameColorDictionary! !

!Color class methodsFor: 'color name lookup' stamp: 'sqr 10/21/2016 12:44:48'!
traditionalColorNamesDictionary
	"Answer a dictionary of Squeak traditional name->color associations.."

	| nameDict |
	nameDict _ Dictionary new.
	nameDict at: #black put: (Color r: 0 g: 0 b: 0).
	nameDict at: #veryVeryDarkGray put: (Color r: 0.125 g: 0.125 b: 0.125).
	nameDict at: #veryDarkGray put: (Color r: 0.25 g: 0.25 b: 0.25).
	nameDict at: #darkGray put: (Color r: 0.375 g: 0.375 b: 0.375).
	nameDict at: #gray put: (Color r: 0.5 g: 0.5 b: 0.5).
	nameDict at: #lightGray put: (Color r: 0.625 g: 0.625 b: 0.625).
	nameDict at: #veryLightGray put: (Color r: 0.75 g: 0.75 b: 0.75).
	nameDict at: #veryVeryLightGray put: (Color r: 0.875 g: 0.875 b: 0.875).
	nameDict at: #white put: (Color r: 1.0 g: 1.0 b: 1.0).
	nameDict at: #red put: (Color r: 1.0 g: 0 b: 0).
	nameDict at: #yellow put: (Color r: 1.0 g: 1.0 b: 0).
	nameDict at: #green put: (Color r: 0 g: 1.0 b: 0).
	nameDict at: #cyan put: (Color r: 0 g: 1.0 b: 1.0).
	nameDict at: #blue put: (Color r: 0 g: 0 b: 1.0).
	nameDict at: #magenta put: (Color r: 1.0 g: 0 b: 1.0).
	nameDict at: #brown put: (Color r: 0.6 g: 0.2 b: 0).
	nameDict at: #orange put: (Color r: 1.0 g: 0.6 b: 0).
	nameDict at: #lightRed put: (Color r: 1.0 g: 0.8 b: 0.8).
	nameDict at: #lightYellow put: (Color r: 1.0 g: 1.0 b: 0.8).
	nameDict at: #lightGreen put: (Color r: 0.8 g: 1.0 b: 0.6).
	nameDict at: #lightCyan put: (Color r: 0.4 g: 1.0 b: 1.0).
	nameDict at: #lightBlue put: (Color r: 0.8 g: 1.0 b: 1.0).
	nameDict at: #lightMagenta put: (Color r: 1.0 g: 0.8 b: 1.0).
	nameDict at: #lightBrown put: (Color r: 1.0 g: 0.6 b: 0.2).
	nameDict at: #lightOrange put: (Color r: 1.0 g: 0.8 b: 0.4).
	nameDict at: #transparent put: (TranslucentColor r: 0.0 g: 0.0 b: 0.0 alpha: 0.0).
	
	^nameDict
! !

!Color class methodsFor: 'color name lookup' stamp: 'jmv 6/30/2014 15:22'!
xkcdFirst48ColorNamesDictionary
	"XKCD color names are based on a survey oc colors people call by the same name.
	 http://blog.xkcd.com/2010/05/03/color-survey-results/"

	"Answer a dictionary of the most agreed upon first 48 xkcd colors"
"
	Color xkcdFirst48ColorNamesDictionary explore.
"	
	| xkcdDict |
	xkcdDict := Dictionary new
	at: #lightPink put: (Color fromString: '#ffd1df') ;
	at: #mustard put: (Color fromString: '#ceb301') ;
	at: #indigo put: (Color fromString: '#380282') ;
	at: #lime put: (Color fromString: '#aaff32') ;
	at: #seaGreen put: (Color fromString: '#53fca1') ;
	at: #periwinkle put: (Color fromString: '#8e82fe') ;
	at: #darkPink put: (Color fromString: '#cb416b') ;
	at: #oliveGreen put: (Color fromString: '#677a04') ;
	at: #peach put: (Color fromString: '#ffb07c') ;
	at: #paleGreen put: (Color fromString: '#c7fdb5') ;
	at: #lightBrown put: (Color fromString: '#ad8150') ;
	at: #hotPink put: (Color fromString: '#ff028d') ;
	at: #black put: (Color fromString: '#000000') ;
	at: #lilac put: (Color fromString: '#cea2fd') ;
	at: #navyBlue put: (Color fromString: '#001146') ;
	at: #royalBlue put: (Color fromString: '#0504aa') ;
	at: #beige put: (Color fromString: '#e6daa6') ;
	at: #salmon put: (Color fromString: '#ff796c') ;
	at: #olive put: (Color fromString: '#6e750e') ;
	at: #maroon put: (Color fromString: '#650021') ;
	at: #brightGreen put: (Color fromString: '#01ff07') ;
	at: #darkPurple put: (Color fromString: '#35063e') ;
	at: #mauve put: (Color fromString: '#ae7181') ;
	at: #forestGreen put: (Color fromString: '#06470c') ;
	at: #aqua put: (Color fromString: '#13eac9') ;
	at: #cyan put: (Color fromString: '#00ffff') ;
	at: #tan put: (Color fromString: '#d1b26f') ;
	at: #darkBlue put: (Color fromString: '#00035b') ;
	at: #lavender put: (Color fromString: '#c79fef') ;
	at: #turquoise put: (Color fromString: '#06c2ac') ;
	at: #darkGreen put: (Color fromString: '#033500') ;
	at: #violet put: (Color fromString: '#9a0eea') ;
	at: #lightPurple put: (Color fromString: '#bf77f6') ;
	at: #limeGreen put: (Color fromString: '#89fe05') ;
	at: #grey put: (Color fromString: '#929591') ;
	at: #skyBlue put: (Color fromString: '#75bbfd') ;
	at: #yellow put: (Color fromString: '#ffff14') ;
	at: #magenta put: (Color fromString: '#c20078') ;
	at: #lightGreen put: (Color fromString: '#96f97b') ;
	at: #orange put: (Color fromString: '#f97306') ;
	at: #teal put: (Color fromString: '#029386') ;
	at: #lightBlue put: (Color fromString: '#95d0fc') ;
	at: #red put: (Color fromString: '#e50000') ;
	at: #brown put: (Color fromString: '#653700') ;
	at: #pink put: (Color fromString: '#ff81c0') ;
	at: #blue put: (Color fromString: '#0343df') ;
	at: #green put: (Color fromString: '#15b01a') ;
	at: #purple put: (Color fromString: '#7e1e9c') ;
  	yourself.

	^xkcdDict

! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:26'!
blueColorDict
	"Answer a dictionary of named colors considered Blue"
"
	Color blueColorDict explore.
"
	^self blueColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:24'!
blueColorDict: aColorDict
	"Answer a dictionary of named colors considered Blue"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isBlue)
				or: [assoc key asString asLowercase includesSubString: 'blue'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:28'!
blueGreenColorDict
	"Answer a dictionary of named colors considered Blue"
"
	Color blueGreenColorDict explore.
"
	^self blueGreenColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:29'!
blueGreenColorDict: aColorDict
	"Answer a dictionary of named colors considered Blue"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isBlueGreen)
				or: [assoc key asString asLowercase includesSubString: 'bluegreen'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:31'!
brightColorDict
	"Answer a dictionary of named colors considered Bright"
"
	Color brightColorDict explore.
"
	^self brightColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:31'!
brightColorDict: aColorDict
	"Answer a dictionary of named colors considered Bright"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isBright) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:34'!
brownColorDict
	"Answer a dictionary of named colors considered Brown"
"
	Color brownColorDict explore.
"
	^self brownColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:33'!
brownColorDict: aColorDict
	"Answer a dictionary of named colors considered Brown"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isBrown)
				or: [assoc key asString asLowercase includesSubString: 'brown'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:36'!
darkColorDict
	"Answer a dictionary of named colors considered Dark"
"
	Color darkColorDict explore.
"
	^self darkColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:37'!
darkColorDict: aColorDict
	"Answer a dictionary of named colors considered Dark"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isDark) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:41'!
grayColorDict: aColorDict
	"Answer a dictionary of named colors considered Grey"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isGrey)
				or: [assoc key asString asLowercase includesSubString: 'grey'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:38'!
greenColorDict
	"Answer a dictionary of named colors considered Green"
"
	Color greenColorDict explore.
"
	^self greenColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:38'!
greenColorDict: aColorDict
	"Answer a dictionary of named colors considered Green"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isGreen)
				or: [assoc key asString asLowercase includesSubString: 'green'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:41'!
greyColorDict
	"Answer a dictionary of named colors considered Grey"
"
	Color greyColorDict explore.
"
	^self greyColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:40'!
greyColorDict: aColorDict
	"Answer a dictionary of named colors considered Grey"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isGrey)
				or: [assoc key asString asLowercase includesSubString: 'grey'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:44'!
lightColorDict
	"Answer a dictionary of named colors considered light"
"
	Color lightColorDict explore.
"
	^self lightColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:43'!
lightColorDict: aColorDict
	"Answer a dictionary of named colors considered Liight"

	^self brightColorDict: aColorDict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:46'!
orangeColorDict
	"Answer a dictionary of named colors considered Orange"
"
	Color orangeColorDict explore.
"
	^self orangeColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:46'!
orangeColorDict: aColorDict
	"Answer a dictionary of named colors considered Orange"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isOrange)
				or: [assoc key asString asLowercase includesSubString: 'orange'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:48'!
pastelColorDict
	"Answer a dictionary of named colors considered Pastel"
"
	Color pastelColorDict explore.
"
	^self pastelColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:48'!
pastelColorDict: aColorDict
	"Answer a dictionary of named colors considered Bright"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isPastel) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:50'!
pinkColorDict
	"Answer a dictionary of named colors considered Pink"
"
	Color pinkColorDict explore.
"
	^self pinkColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:50'!
pinkColorDict: aColorDict
	"Answer a dictionary of named colors considered Pink"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isPink)
				or: [assoc key asString asLowercase includesSubString: 'pink'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:52'!
purpleColorDict
	"Answer a dictionary of named colors considered Purple"
"
	Color purpleColorDict explore.
"
	^self purpleColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:52'!
purpleColorDict: aColorDict
	"Answer a dictionary of named colors considered Pink"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc key asString asLowercase includesSubString: 'purple')
				or: [assoc key asString asLowercase includesSubString: 'violet'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:54'!
redColorDict
	"Answer a dictionary of named colors considered Red"
"
	Color redColorDict explore.
"
	^self redColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:53'!
redColorDict: aColorDict
	"Answer a dictionary of named colors considered Red"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isRed)
				or: [assoc key asString asLowercase includesSubString: 'red'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:56'!
saturatedColorDict
	"Answer a dictionary of named colors considered Saturated"
"
	Color saturatedColorDict explore.
"
	^self saturatedColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:55'!
saturatedColorDict: aColorDict
	"Answer a dictionary of named colors considered Saturated"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isSaturated) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:26'!
yellowColorDict
	"Answer a dictionary of named colors considered Yellow"
"
	Color yellowColorDict explore.
"
	^self yellowColorDict: (self colorNamesDict)! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:19'!
yellowColorDict: aColorDict
	"Answer a dictionary of named colors considered Yellow"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isYellow)
				or: [assoc key asString asLowercase includesSubString: 'yellow'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

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

!Color class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 14:58'!
colorNamesDict
	"Answer the current dictionary of  name->color associations."
"
	Color colorNamesDict explore.
"
	ColorNamesDict ifNil: [
		self setColorNamesDict: self defaultColorNamesDictionary ].
	^ColorNamesDict! !

!Color class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:08'!
grayToIndexMap
	"
	Color grayToIndexMap explore.
	"
	GrayToIndexMap ifNil: [
		self initializeGrayToIndexMap ].
	^GrayToIndexMap! !

!Color class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:04'!
indexedColors
	"
	Color indexedColors explore.
	"
	IndexedColors ifNil: [
		self initializeIndexedColors ].
	^IndexedColors! !

!Color class methodsFor: 'cached state access' stamp: 'jmv 5/12/2016 14:58'!
releaseClassCachedState
	"
	Color releaseClassCachedState
	"
	CachedColormaps _ nil. 		"Maps to translate between color depths"
	ToGrayColormaps _ nil.			"Maps colors to 8bpp grays for various color depths"
	FromGrayColormaps _ nil.		"Maps from 8bpp grays to colors of various depths"
	MaskingMap _ nil. 				"Maps all colors except transparent to black for creating a mask"
	ColorNamesDict _ nil.
	IndexedColors _ nil.
	GrayToIndexMap _ nil! !


!TranslucentColor methodsFor: 'accessing' stamp: 'jmv 6/18/2017 20:41:17'!
alpha
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ self at: 4! !

!TranslucentColor methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:06'!
storeArrayValuesOn: aStream

	self isTransparent ifTrue: [
		^ aStream space].
	super storeArrayValuesOn: aStream.
	aStream space.
	self alpha printOn: aStream fractionDigits: 3! !

!TranslucentColor methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:20'!
storeOn: aStream

	self isTransparent ifTrue: [^ aStream nextPutAll: '(Color transparent)'].
	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' r: '.
	self red printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' g: '.
	self green printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' b: '.
	self blue printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' alpha: '.
	self alpha printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ')'! !

!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 6/18/2017 20:55:03'!
alpha: alphaValue
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifTrue: [
		^ Color new
			setRed: self red
			green: self green
			blue: self blue].
	^ super alpha: alphaValue! !

!TranslucentColor methodsFor: 'conversions' stamp: 'sw 10/27/1999 10:51'!
asNontranslucentColor
	^ self alpha: 1.0! !

!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 4/19/2013 16:10'!
bitPatternForDepth: depth
	"Return an appropriate bit pattern. This will almost never be meaningful for tranlucentColors, except for the degenerate case of tranparency."

	self isTransparent ifTrue: [
		^ Bitmap with: 0].
	^ super bitPatternForDepth: depth! !

!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 6/18/2017 20:41:47'!
pixelValueForDepth: d
	"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."
	| basicPixelWord |
	"In 32bpp, if alpha = 0, ignore any color components, and answer R=0, G=0, B=0, Alpha=0.
	In depths > 8 and < 32, pixelValue zero is the special value used by BitBlt to denote transparent."
	self isTransparent ifTrue: [
		^ 0].
	basicPixelWord _ super pixelValueForDepth: d.
	^d < 32
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16rFFFFFF) bitOr:
			((self alpha*255.0) rounded
				bitShift: 24) ]! !

!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 6/18/2017 20:41:50'!
pixelWordForDepth: depth
	"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."

	| basicPixelWord |
	self isTransparent ifTrue: [^ 0].
	basicPixelWord _ super pixelWordForDepth: depth.
	^depth < 32
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16rFFFFFF) bitOr:
			((self alpha*255.0) rounded
				bitShift: 24) ]! !

!TranslucentColor methodsFor: 'private' stamp: 'jmv 6/18/2017 20:42:05'!
setRed: r green: g blue: b alpha: alphaValue

	self basicSetRed: r green: g blue: b.
	self at: 4 put: alphaValue! !

!TranslucentColor methodsFor: 'queries' stamp: 'jmv 6/18/2017 20:41:26'!
isOpaque

	^self alpha = 1.0! !

!TranslucentColor methodsFor: 'queries' stamp: 'jmv 6/18/2017 20:41:54'!
isTransparent
	^ self alpha = 0.0! !


!TranslucentColor class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:54:35'!
new
	^ self new: 4! !

!TranslucentColor class methodsFor: 'instance creation' stamp: 'jmv 6/18/2017 20:57:08'!
r: r g: g b: b alpha: alphaValue
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."

	^ self new
		setRed: r
		green: g
		blue: b
		alpha: alphaValue! !

