'From Cuis 4.2 of 25 July 2013 [latest update: #2250] on 17 April 2015 at 3:10:32.929607 pm'!

!Color class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:08'!
grayToIndexMap
	"
	Color grayToIndexMap explore.
	"
	GrayToIndexMap ifNil: [
		self initializeGrayToIndexMap ].
	^GrayToIndexMap! !

!Color class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:08'!
releaseClassCachedState
	"
	Color releaseClassCachedState
	"
	CachedColormaps _ nil. 		"Maps to translate between color depths"
	MaskingMap _ nil. 			"Maps all colors except transparent to black for creating a mask"
	ColorNamesDict _ nil.
	IndexedColors _ nil.
	GrayToIndexMap _ nil! !


!Color methodsFor: 'conversions' stamp: 'jmv 4/17/2015 15:08'!
closestPixelValue8
	"Return the nearest approximation to this color for an 8-bit deep Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 255].  "white"

	^self saturation < 0.2
		ifTrue: [
			^ self grayToIndexMap at:(self green * 255) rounded + 1.  "nearest gray"
			]
		ifFalse: [
			"compute nearest entry in the color cube"
			40 + 
				((self red * 5) rounded * 36) +
				((self blue * 5) rounded * 6) +
				(self green * 5) rounded ]! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 4/17/2015 15:05'!
colorFromPixelValue: p depth: d
	"Convert a pixel value for the given display depth into a color."
	"Details: For depths of 8 or less, the pixel value is simply looked up in a table. For greater depths, the color components are extracted and converted into a color."
	"Warning: In BitBlt, a pixel with pixelValue = 0 is transparent.
	Squeak usually assumes that r=g=b=0 => transparent. But this is false if we have alpha (opacity).
	A color with r=g=b=0 and opacity = 255 is BLACK, not TRANSPARENT.
	Squeak also answers darkest possible blue when asked for black. Again, this is not needed in 32 bits (with alpha).
	The real rule is that pixelValue=0 means transparent.
	And that darkest blue must be used instead of black, but only for depths >8 and < 32 (no indexed colors, no alpha)
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

	(d = 16) | (d = 15) ifTrue: [
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

!Color class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 14:58'!
colorNamesDict
	"Answer the current dictionary of  name->color associations."
"
	Color colorNamesDict explore.
"
	ColorNamesDict ifNil: [
		self setColorNamesDict: self defaultColorNamesDictionary ].
	^ColorNamesDict! !

!Color class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:04'!
indexedColors
	"
	Color indexedColors explore.
	"
	IndexedColors ifNil: [
		self initializeIndexedColors ].
	^IndexedColors! !

!methodRemoval: Color class #initialize!
Color class removeSelector: #initialize!
!methodRemoval: Color class #initializeNames!
Color class removeSelector: #initializeNames!
!methodRemoval: Color class #shutDown!
Color class removeSelector: #shutDown!
Color initialize!

!Color class reorganize!
('instance creation' colorFrom: colorFromPixelValue:depth: fromArray: fromRgbTriplet: fromString: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: new r:g:b: r:g:b:alpha: r:g:b:range: random random2 random2: random:)
('class initialization' initializeGrayToIndexMap initializeIndexedColors)
('examples' colorRampForDepth:extent: experimentsTowarsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('named colors' aqua beige black blue brightGreen brown cyan darkBlue darkGray darkGreen darkPink darkPurple forestGreen gray green grey hotPink indigo lavender lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightPink lightPurple lightRed lightYellow lilac lime limeGreen magenta maroon mauve mustard navyBlue olive oliveGreen orange paleGreen peach periwinkle pink purple red royalBlue salmon seaGreen skyBlue tan teal transparent turquoise veryDarkGray veryLightGray veryVeryDarkGray veryVeryLightGray violet white yellow)
('colormaps' cachedColormapFrom:to: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapFrom:to: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor:)
('other' maskingMap:)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper:)
('color name lookup' colorNames defaultColorNamesDictionary doesNotUnderstand: exactColorNamed: setColorNamesDict: traditionalColorNamesDictionary xkcdFirst48ColorNamesDictionary)
('selection' blueColorDict blueColorDict: blueGreenColorDict blueGreenColorDict: brightColorDict brightColorDict: brownColorDict brownColorDict: darkColorDict darkColorDict: grayColorDict: greenColorDict greenColorDict: greyColorDict greyColorDict: lightColorDict lightColorDict: orangeColorDict orangeColorDict: pastelColorDict pastelColorDict: pinkColorDict pinkColorDict: purpleColorDict purpleColorDict: redColorDict redColorDict: saturatedColorDict saturatedColorDict: yellowColorDict yellowColorDict:)
('color space conversions' applySimpleGamma:to: linearTosRGBGamma: sRGBGammaToLinear:)
('cached state access' colorNamesDict grayToIndexMap indexedColors releaseClassCachedState)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk removeFromShutDownList: Color!

