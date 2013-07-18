'From Cuis 4.1 of 12 December 2012 [latest update: #1687] on 19 April 2013 at 5:16:46.377621 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HalfComponentMask HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color methodsFor: 'queries' stamp: 'jmv 4/19/2013 16:16'!
isWhite
	"Return true if the receiver represents white"
	^floatRGB sum = 3.0! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 16:42'!
floatRGB
	^floatRGB! !

!Color methodsFor: 'selection' stamp: 'jmv 4/19/2013 17:14'!
isCyan
	"Am I considered Cyan ?"

	^self red < 0.05 
		and: [(self green min: self blue) > 0.5]					
			and: [(self green - self blue) abs < 0.2]! !

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


!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:37'!
blue
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ floatRGB at: 3! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:39'!
brightness
	"Return the brightness of this color, a float in the range [0.0..1.0].
	Color red brightness
	"

	^ floatRGB max! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:41'!
chroma
	"Return the chroma of this color, a value between 0.0 and 1.0, somewhat related to saturation.
	See http://en.wikipedia.org/wiki/HSL_and_HSV
	Color red chroma
	Color gray chroma
	"

	^floatRGB max - floatRGB min! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:38'!
green
	"Return the green component of this color, a float in the range [0.0..1.0]."

	^ floatRGB at: 2! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:46'!
hue
	"Return the hue of this color, an angle in the range [0.0..360.0].
	Color orange hue
	"

	| r g b max min span h |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ floatRGB max.
	min _ floatRGB min.
	span _ (max - min) asFloat.
	span = 0.0 ifTrue: [ ^ 0.0 ].

	r = max ifTrue: [
		h _ ((g - b) asFloat / span) * 60.0.
	] ifFalse: [
		g = max
			ifTrue: [ h _ 120.0 + (((b - r) asFloat / span) * 60.0). ]
			ifFalse: [ h _ 240.0 + (((r - g) asFloat / span) * 60.0). ].
	].

	h < 0.0 ifTrue: [ h _ 360.0 + h ].
	^ h! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:46'!
luminance
	"Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity."

	^ ((299 * self red) +
	   (587 * self green) +
	   (114 * self blue)) / 1000! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:38'!
red
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ floatRGB at: 1! !

!Color methodsFor: 'access' stamp: 'jmv 4/19/2013 16:54'!
saturation
	"Return the saturation of this color, a value between 0.0 and 1.0.
	Color red saturation
	Color gray saturation
	"

	| max |
	max _ floatRGB max.
	max = 0.0 ifTrue: [ ^0.0 ].
	^max - floatRGB min / max! !

!Color methodsFor: 'equality' stamp: 'jmv 4/19/2013 17:05'!
= aColor
	"Return true if the receiver equals the given color. This method handles TranslucentColors, too."

	self == aColor ifTrue: [^true].
	(aColor is: #Color) ifFalse: [^ false].
	^ aColor floatRGB = floatRGB and: [
		aColor alpha = self alpha ]! !

!Color methodsFor: 'equality' stamp: 'jmv 4/19/2013 16:43'!
diff: theOther
	"Returns a number between 0.0 and 1.0.
	Color gray diff: Color red
	"
	^(floatRGB - theOther floatRGB) abs sum / 3.0! !

!Color methodsFor: 'equality' stamp: 'jmv 4/19/2013 16:49'!
rgbDistance: otherColor
	"Compare two colors in distance"

	^(self hue - otherColor hue) abs *10 +
		(self saturation - otherColor saturation) abs +
		(self brightness - otherColor brightness) abs! !

!Color methodsFor: 'queries' stamp: 'jmv 4/19/2013 16:16'!
isBlack
	"Return true if the receiver represents black"
	^floatRGB sum = 0.0! !

!Color methodsFor: 'conversions' stamp: 'jmv 4/19/2013 16:17'!
closestPixelValue1
	"Return the nearest approximation to this color for a monochrome Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 0].  "white"

	self luminance > 0.5
		ifTrue: [^ 0]  "white"
		ifFalse: [^ 1].  "black"! !

!Color methodsFor: 'conversions' stamp: 'jmv 4/19/2013 16:18'!
closestPixelValue2
	"Return the nearest approximation to this color for a 2-bit deep Form."

	| lum |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 0].  "opaque white"

	lum _ self luminance.
	lum < 0.2 ifTrue: [^ 1].  "black"
	lum > 0.6 ifTrue: [^ 2].  "opaque white"
	^ 3  "50% gray"
! !

!Color methodsFor: 'conversions' stamp: 'jmv 4/19/2013 17:15'!
closestPixelValue4
	"Return the nearest approximation to this color for a 4-bit deep Form."

	| bIndex |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 0].  "opaque white"

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

!Color methodsFor: 'conversions' stamp: 'jmv 4/19/2013 17:05'!
closestPixelValue8
	"Return the nearest approximation to this color for an 8-bit deep Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 0].  "white"

	^self saturation < 0.2
		ifTrue: [
			^ GrayToIndexMap at:(self green * 255) rounded + 1.  "nearest gray"
			]
		ifFalse: [
			"compute nearest entry in the color cube"
			40 + 
				((self red * 5) rounded * 36) +
				((self blue * 5) rounded * 6) +
				(self green * 5) rounded ]! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 17:16'!
setRed: r green: g blue: b colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	rgb ifNotNil: [self attemptToMutateError].
	rgb _
		(((r * ComponentMax) rounded bitAnd: ComponentMask) bitShift: RedShift) +
		(((g * ComponentMax) rounded bitAnd: ComponentMask) bitShift: GreenShift) +
		 ((b * ComponentMax) rounded bitAnd: ComponentMask).
	floatRGB ifNotNil: [self attemptToMutateError].
	floatRGB _ FloatArray with: r with: g with: b.
	colorSpace _ aSymbol.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 17:16'!
setRed: r green: g blue: b range: range colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	rgb ifNotNil: [self attemptToMutateError].
	rgb _
		((((r * ComponentMask) // range) bitAnd: ComponentMask) bitShift: RedShift) +
		((((g * ComponentMask) // range) bitAnd: ComponentMask) bitShift: GreenShift) +
		 (((b * ComponentMask) // range) bitAnd: ComponentMask).
	floatRGB ifNotNil: [self attemptToMutateError].
	floatRGB _ FloatArray with: r with: g with: b.
	floatRGB /= range.
	colorSpace _ aSymbol.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !

!Color methodsFor: 'named colors' stamp: 'jmv 4/19/2013 16:49'!
closestAssocFrom: aColorDict
	"Answer closest associated color in aColorDict"

"
	((Color lightBlue) closestAssocFrom: (Color namedColors)) explore.
"
	| close distance |
	close := nil.

	distance := 100. "big"
	aColorDict  associationsDo:  [ :assoc || dist |  
		dist := self rgbDistance: (assoc value). 
		(dist < distance) 
		ifTrue: [distance := dist. close := assoc]
	].
	^close! !


!Color class methodsFor: 'class initialization' stamp: 'jmv 4/19/2013 16:54'!
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

	ComponentMask _ 1023.
	ComponentMax _ 1023.0.  "a Float used to normalize components"
	RedShift _ 20.
	GreenShift _ 10.
	BlueShift _ 0.

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

!Color class methodsFor: 'class initialization' stamp: 'jmv 4/19/2013 17:02'!
initializeGrayToIndexMap
	"Build an array of gray values available in the 8-bit colormap. This array is indexed by a gray level between black (1) and white (256) and returns the pixel value for the corresponding gray level."
	"Note: This method must be called after initializeIndexedColors, since it uses IndexedColors."
	"
	Color initializeGrayToIndexMap
	"

	| grayLevels grayIndices c distToClosest dist indexOfClosest |
	"record the level and index of each gray in the 8-bit color table"
	grayLevels _ OrderedCollection new.
	grayIndices _ OrderedCollection new.
	"Note: skip the first entry, which is reserved for transparent"
	2 to: IndexedColors size do: [:i |
		c _ IndexedColors at: i.
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


!TranslucentColor methodsFor: 'accessing' stamp: 'jmv 4/19/2013 16:09'!
alpha
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ floatAlpha! !

!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 4/19/2013 16:10'!
bitPatternForDepth: depth
	"Return an appropriate bit pattern. This will almost never be meaningful for tranlucentColors, except for the degenerate case of tranparency."

	self isTransparent ifTrue: [
		^ Bitmap with: 0].
	^ super bitPatternForDepth: depth! !

!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 4/19/2013 16:14'!
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
			((floatAlpha*255.0) rounded
				bitShift: 24) ]! !

!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 4/19/2013 16:13'!
pixelWordForDepth: depth
	"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."

	| basicPixelWord |
	self isTransparent ifTrue: [^ 0].
	basicPixelWord _ super pixelWordForDepth: depth.
	^depth < 32
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16rFFFFFF) bitOr:
			((floatAlpha*255.0) rounded
				bitShift: 24) ]! !

!TranslucentColor methodsFor: 'private' stamp: 'jmv 4/19/2013 17:16'!
setRed: r green: g blue: b alpha: alphaValue colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	rgb ifNotNil: [self attemptToMutateError].
	rgb _
		(((r * ComponentMax) rounded bitAnd: ComponentMask) bitShift: RedShift) +
		(((g * ComponentMax) rounded bitAnd: ComponentMask) bitShift: GreenShift) +
		 ((b * ComponentMax) rounded bitAnd: ComponentMask).
	floatRGB ifNotNil: [self attemptToMutateError].
	floatRGB _ FloatArray with: r with: g with: b.
	colorSpace _ aSymbol.
	alpha _ (255.0 * alphaValue) asInteger min: 255 max: 0.
	floatAlpha _ alphaValue.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !

!TranslucentColor methodsFor: 'queries' stamp: 'jmv 4/19/2013 16:12'!
isOpaque

	^floatAlpha = 1.0! !

!TranslucentColor methodsFor: 'queries' stamp: 'jmv 4/19/2013 16:08'!
isTransparent
	^ floatAlpha = 0.0! !

!methodRemoval: TranslucentColor #privateAlpha!
TranslucentColor removeSelector: #privateAlpha!
Color initialize!
!methodRemoval: Color #isLght!
Color removeSelector: #isLght!
!methodRemoval: Color #privateAlpha!
Color removeSelector: #privateAlpha!
!methodRemoval: Color #privateBlue!
Color removeSelector: #privateBlue!
!methodRemoval: Color #privateGreen!
Color removeSelector: #privateGreen!
!methodRemoval: Color #privateRGB!
Color removeSelector: #privateRGB!
!methodRemoval: Color #privateRed!
Color removeSelector: #privateRed!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
