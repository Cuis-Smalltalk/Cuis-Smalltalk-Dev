'From Cuis 4.1 of 12 December 2012 [latest update: #1689] on 19 April 2013 at 6:11:30.726367 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color methodsFor: 'equality' stamp: 'jmv 4/19/2013 18:03'!
hash

	^ floatRGB hash! !

!Color methodsFor: 'conversions' stamp: 'jmv 4/19/2013 18:01'!
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

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 16 ifTrue: [
		"five bits per component; top bits ignored"
		val _ ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

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

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 18:09'!
setHue: hue chroma: chroma brightness: brightness
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m componentMax | 

	hf _ hue asFloat.
	(hf < 0.0 or: [ hf >= 360.0 ])
		ifTrue: [ hf _ hf rem: 360.0 ].
	hf _ hf / 60.0.
	x _ chroma * (1.0 - ((hf rem: 2.0) -1.0) abs).
	i _ hf asInteger.  		"integer part of hue"

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].
componentMax _ 1023.0.
	m _ brightness - ((r1 max: g1) max: b1).
	m < 0.0 ifTrue: [
		m * componentMax < -0.01
			ifTrue: [ ^nil ]. "No color exists with required parameters"
		m _ 0.0 ].
	r1 _ r1 + m.
	r1 > 1.0 ifTrue: [
		(r1 * componentMax) rounded > componentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		r1 _ 1.0 ].
	g1 _ g1 + m.
	g1 > 1.0 ifTrue: [
		(g1 * componentMax) rounded > componentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		g1 _ 1.0 ].
	b1 _ b1 + m.
	b1 > 1.0 ifTrue: [
		(b1 * componentMax) rounded > componentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		b1 _ 1.0 ].
	self setRed: r1 green: g1 blue: b1 colorSpace: #sRGB! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 18:09'!
setHue: hue chroma: chroma luminance: luma
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m componentMax | 

	hf _ hue asFloat.
	(hf < 0.0 or: [ hf >= 360.0 ])
		ifTrue: [ hf _ hf rem: 360.0 ].
	hf _ hf / 60.0.
	x _ chroma * (1.0 - ((hf rem: 2.0) -1.0) abs).
	i _ hf asInteger.  		"integer part of hue"

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].
componentMax _ 1023.0.
	m _ luma - (0.299*r1) - (0.587*g1) - (0.114*b1).
	m < 0.0 ifTrue: [
		m * componentMax < -0.01
			ifTrue: [ ^nil ]. "No color exists with required parameters"
		m _ 0.0 ].
	r1 _ r1 + m.
	r1 > 1.0 ifTrue: [
		(r1 * componentMax) rounded > componentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		r1 _ 1.0 ].
	g1 _ g1 + m.
	g1 > 1.0 ifTrue: [
		(g1 * componentMax) rounded > componentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		g1 _ 1.0 ].
	b1 _ b1 + m.
	b1 > 1.0 ifTrue: [
		(b1 * componentMax) rounded > componentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		b1 _ 1.0 ].
	self setRed: r1 green: g1 blue: b1 colorSpace: #sRGB! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 18:07'!
setRed: r green: g blue: b colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	floatRGB ifNotNil: [self attemptToMutateError].
	floatRGB _ FloatArray with: r with: g with: b.
	colorSpace _ aSymbol.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 18:07'!
setRed: r green: g blue: b range: range colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	floatRGB ifNotNil: [self attemptToMutateError].
	floatRGB _ FloatArray with: r with: g with: b.
	floatRGB /= range.
	colorSpace _ aSymbol.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !


!Color class methodsFor: 'class initialization' stamp: 'jmv 4/19/2013 18:10'!
initialize
	"
	Color initialize
	"

	"Details: Externally, the red, green, and blue components of color
	are floats in the range [0.0..1.0]. Internally, they are represented
	as integers in the range [0..ComponentMask] packing into a
	small integer to save space and to allow fast hashing and
	equality testing.

	For a general description of color representations for computer
	graphics, including the relationship between the RGB and HSV
	color models used here, see Chapter 17 of Foley and van Dam,
	Fundamentals of Interactive Computer Graphics, Addison-Wesley,
	1982."

	PureRed		_ self r: 1 g: 0 b: 0.
	PureGreen	_ self r: 0 g: 1 b: 0.
	PureBlue	 	_ self r: 0 g: 0 b: 1.
	PureYellow	_ self r: 1 g: 1 b: 0.
	PureCyan 	_ self r: 0 g: 1 b: 1.
	PureMagenta _ self r: 1 g: 0 b: 1.

	RandomStream _ Random new.

	self initializeIndexedColors.
	self initializeGrayToIndexMap.
	self initializeNames! !


!TranslucentColor methodsFor: 'equality' stamp: 'jmv 4/19/2013 18:03'!
hash

	^ floatRGB hash bitXor: floatAlpha hash
! !

!TranslucentColor methodsFor: 'private' stamp: 'jmv 4/19/2013 18:07'!
setRed: r green: g blue: b alpha: alphaValue colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	floatRGB ifNotNil: [self attemptToMutateError].
	floatRGB _ FloatArray with: r with: g with: b.
	colorSpace _ aSymbol.
	floatAlpha _ alphaValue.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !

Color initialize!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'Aqua Beige Black Blue BrightGreen Brown CachedColormaps ColorChart ColorNamesDict Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green Grey HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
