'From Cuis 4.1 of 12 December 2012 [latest update: #1594] on 14 February 2013 at 10:54:42 am'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern '
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HalfComponentMask HighLightBitmaps HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal TranslucentPatterns Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!FormCanvas commentStamp: '<historical>' prior: 0!
origin is aPoint, and it is expressed relative to the form. Usually, the form doesn't cover the whole World, and origin is negative. For instance, if we just want to draw the part of the World in (100@100 extent: 50@50) to another form, then fom extent = 50@50, and origin = -100@-100.

clipRect is relative to the form. For instance, if we only need to draw the part in (110@110 extent: 20@20) to the form above, then clipRect is (10@10 extent: 20@20)

All public protocol and drawing services is relative to World. The idea is that we only care about origin/form when we set them. Afterwards, we don't need to care about them. All the operations are done as if the whole World was drawn on Display.!


!Color methodsFor: 'conversions' stamp: 'jmv 2/2/2013 00:45'!
bitPatternForDepth: depth
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps. The resulting Bitmap may be multiple words to represent a stipple pattern of several lines.  "
	"See also:	pixelValueAtDepth:	-- value for single pixel
				pixelWordAtDepth:	-- a 32-bit word filled with the pixel value"
	"Details: The pattern for the most recently requested depth is cached."

	(depth = cachedDepth and: [depth <= 2 or: [cachedBitPattern size = 1]]) ifTrue: [^ cachedBitPattern].
	cachedDepth _ depth.

	^ cachedBitPattern _ Bitmap with: (self pixelWordForDepth: depth)! !


!Color class methodsFor: 'class initialization' stamp: 'jmv 2/14/2013 10:44'!
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
	HalfComponentMask _ 512.  "used to round up in integer calculations"
	ComponentMax _ 1023.0.  "a Float used to normalize components"
	RedShift _ 20.
	GreenShift _ 10.
	BlueShift _ 0.

	PureRed		 _ self r: 1 g: 0 b: 0.
	PureGreen	 _ self r: 0 g: 1 b: 0.
	PureBlue	 _ self r: 0 g: 0 b: 1.
	PureYellow	 _ self r: 1 g: 1 b: 0.
	PureCyan	 _ self r: 0 g: 1 b: 1.
	PureMagenta _ self r: 1 g: 0 b: 1.

	RandomStream _ Random new.

	self initializeIndexedColors.
	self initializeGrayToIndexMap.
	self initializeNames! !


!DisplayMedium methodsFor: 'coloring' stamp: 'jmv 2/2/2013 00:53'!
reverse
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white.
	Display reverse
	"

	self fill: self boundingBox rule: Form reverse fillColor: Color white! !

!DisplayMedium methodsFor: 'coloring' stamp: 'jmv 2/2/2013 00:53'!
reverse: aRectangle
	"Change all the bits in the receiver's area that intersects with aRectangle 
	that are white to black, and the ones that are black to white."

	self fill: aRectangle rule: Form reverse fillColor: Color white! !


!Form methodsFor: 'color mapping' stamp: 'jmv 2/2/2013 00:32'!
bitPatternFor: aColorOrPattern
	"Return the pixel word for representing the given color on the receiver"
	^aColorOrPattern bitPatternForDepth: self depth! !

!Form methodsFor: 'color mapping' stamp: 'jmv 2/2/2013 00:34'!
colormapIfNeededFor: destForm
	"Return a ColorMap mapping from the receiver to destForm."
	^self colormapIfNeededForDepth: destForm depth! !

!Form methodsFor: 'color mapping' stamp: 'jmv 2/14/2013 10:37'!
pixelValueFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	^aColor pixelValueForDepth: self depth! !


!FormCanvas methodsFor: 'private' stamp: 'jmv 2/2/2013 00:46'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		(paintColor isKindOf: InfiniteForm) ifFalse: [ ^self error: 'Cannot install color' ].
		^port fillPattern: paintColor; combinationRule: Form paint ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 16]) ifTrue: [
		port fillPattern: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillPattern: paintColor.
	self depth = 16
		ifTrue: [ port alphaBits: paintColor privateAlpha; combinationRule: 31 ]
		ifFalse: [ port combinationRule: Form blend ]! !


!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 2/2/2013 00:47'!
bitPatternForDepth: depth
	"Return an appropriate bit pattern. This will almost never be meaningful for tranlucentColors, except for the degenerate case of tranparency."

	alpha = 0 ifTrue: [^ Bitmap with: 0].
	^ super bitPatternForDepth: depth! !

!methodRemoval: ColorForm #colormapIfNeededFor:!
ColorForm removeSelector: #colormapIfNeededFor:!

!ColorForm reorganize!
('accessing' colors colors:)
('displaying' displayOn:at:clippingBox:rule:fillColor: displayOnPort:at: maskingMap)
('pixel accessing' colorAt: colorAt:put: isTransparentAt: pixelValueAt:)
('color manipulation' asGrayScale colormapIfNeededForDepth: colorsUsed ensureTransparentColor indexOfColor: mapColor:to: replaceColor:with: replaceColorAt:with: replaceColorAtIndex:with: transparentAllPixelsLike: transparentColor: twoToneFromDisplay:backgroundColor:)
('copying' asCursorForm blankCopyOf:scaledBy: copy: postCopy)
('private' clearColormapCache depth: ensureColorArrayExists setColors:cachedColormap:depth: setExtent:depth: unusedColormapEntry)
('scaling, rotation' flippedBy:centerAt:)
('fileIn/Out' hibernate readAttributesFrom: storeOn: unhibernate writeAttributesOn:)
('testing' mightBeTranslucent)
!

!methodRemoval: Form #colormapFromARGB!
Form removeSelector: #colormapFromARGB!
!methodRemoval: Form #colormapToARGB!
Form removeSelector: #colormapToARGB!
!methodRemoval: Form #hasNonStandardPalette!
Form removeSelector: #hasNonStandardPalette!
!methodRemoval: Form #pixelWordFor:!
Form removeSelector: #pixelWordFor:!
!methodRemoval: Form #rgbaBitMasks!
Form removeSelector: #rgbaBitMasks!
!methodRemoval: Form #storeBits:to:on:!
Form removeSelector: #storeBits:to:on:!
!methodRemoval: ColorMap class #mapBitsFrom:to:!
ColorMap class removeSelector: #mapBitsFrom:to:!
!methodRemoval: ColorMap class #mappingFrom:to:!
ColorMap class removeSelector: #mappingFrom:to:!
!methodRemoval: ColorMap class #mappingFromARGB:!
ColorMap class removeSelector: #mappingFromARGB:!
!methodRemoval: ColorMap class #mappingToARGB:!
ColorMap class removeSelector: #mappingToARGB:!
!methodRemoval: ColorMap #mappingTo:!
ColorMap removeSelector: #mappingTo:!
!methodRemoval: ColorMap #rgbaBitMasks!
ColorMap removeSelector: #rgbaBitMasks!
!methodRemoval: Color class #css2NamedColors1!
Color class removeSelector: #css2NamedColors1!
!methodRemoval: Color class #css2NamedColors2!
Color class removeSelector: #css2NamedColors2!
!methodRemoval: Color class #initializeHighLights!
Color class removeSelector: #initializeHighLights!
!methodRemoval: Color class #initializeTranslucentPatterns!
Color class removeSelector: #initializeTranslucentPatterns!
!methodRemoval: Color class #pixelScreenForDepth:!
Color class removeSelector: #pixelScreenForDepth:!
!methodRemoval: Color class #quickHighLight:!
Color class removeSelector: #quickHighLight:!
!methodRemoval: Color class #translucentMaskFor:depth:!
Color class removeSelector: #translucentMaskFor:depth:!
Color initialize!
!methodRemoval: Color #halfTonePattern1!
Color removeSelector: #halfTonePattern1!
!methodRemoval: Color #halfTonePattern2!
Color removeSelector: #halfTonePattern2!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern'
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HalfComponentMask HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!methodRemoval: Bitmap #storeBits:to:on:!
Bitmap removeSelector: #storeBits:to:on:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Color initialize!

