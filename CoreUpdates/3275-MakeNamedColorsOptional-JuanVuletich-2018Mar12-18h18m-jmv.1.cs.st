'From Cuis 5.0 of 7 November 2016 [latest update: #3274] on 12 March 2018 at 7:21:38 pm'!

!Color methodsFor: 'other' stamp: 'jmv 3/12/2018 19:20:35'!
colorName
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color.
	Return nil if named color support is not present"

	^ ColorNamesDict ifNotNil: [ :dict|
		dict keyAtValue: self ifAbsent: [nil]]! !

!Color methodsFor: 'selection' stamp: 'jmv 3/12/2018 19:15:42'!
isGreen
	"Am I considered Green ?"

	^self green > (self blue + 0.3)
		and: [self green > (self red + 0.3)]! !

!Color methodsFor: 'selection' stamp: 'jmv 3/12/2018 19:16:29'!
isMagenta
	"Am I considered Magenta ?"

	^self green < 0.05 
		and: [(self red min: self blue) > 0.4]					
			and: [(self red - self blue) abs < 0.3]! !

!Color methodsFor: 'selection' stamp: 'jmv 3/12/2018 19:17:05'!
isYellow
	"Am I considered Yellow ?"

	^self blue < 0.1
		and: [(self red min: self green) > 0.5]					
			and: [(self red - self green) abs < 0.2]! !


!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:48:04'!
black
	^`Color r: 0 g: 0 b: 0`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:01:23'!
blue
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.012 g: 0.263 b: 0.875`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:59:10'!
brown
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.396 g: 0.216 b: 0.0`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:49:47'!
cyan
	^ `Color r: 0 g: 1.0 b: 1.0`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:03:42'!
darkGray
	^ `Color r: 0.375 g: 0.375 b: 0.375`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:03:57'!
gray
	^ `Color r: 0.5 g: 0.5 b: 0.5`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:02:25'!
green
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.082 g: 0.690 b: 0.102`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:57:12'!
lightBlue
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.584 g: 0.816 b: 0.988`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:47:23'!
lightBrown
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.678 g: 0.506 b: 0.314`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:40:26'!
lightCyan
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 0.674 g: 1.0 b: 0.988`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:04:01'!
lightGray
	^ `Color r: 0.625 g: 0.625 b: 0.625`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:54:54'!
lightGreen
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.588 g: 0.976 b: 0.482`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:40:33'!
lightMagenta
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 0.98 g: 0.372 b: 0.969`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:40:22'!
lightOrange
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 0.992 g: 0.667 b: 0.283`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:40:30'!
lightRed
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 1.0 g: 0.279 b: 0.298`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:40:18'!
lightYellow
	"Override traditional names existing in extended XKCD naming"
	^ `Color r: 1.0 g: 0.996 b: 0.478`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:53:42'!
magenta
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.760 g: 0.0 b: 0.471`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:55:57'!
orange
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.976 g: 0.451 b: 0.024`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:00:11'!
pink
	"Override traditional names existing in XKCD naming"
	^ `Color r: 1.0 g: 0.506 b: 0.753`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:03:18'!
purple
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.494 g: 0.118 b: 0.612`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:58:04'!
red
	"Override traditional names existing in XKCD naming"
	^ `Color r: 0.898 g: 0 b: 0`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:05:22'!
tan
	^ `Color r: 0.820 g: 0.698 b: 0.435`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:04:45'!
transparent
	^ `TranslucentColor r: 0.0 g: 0.0 b: 0.0 alpha: 0.0`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:04:05'!
veryDarkGray
	^ `Color r: 0.25 g: 0.25 b: 0.25`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:04:10'!
veryLightGray
	^ `Color r: 0.75 g: 0.75 b: 0.75`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:05:08'!
veryVeryDarkGray
	^ `Color r: 0.125 g: 0.125 b: 0.125`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:04:18'!
veryVeryLightGray
	^ `Color r: 0.875 g: 0.875 b: 0.875`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:04:24'!
white
	^ `Color r: 1.0 g: 1.0 b: 1.0`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 18:52:36'!
yellow
	"Override traditional names existing in XKCD naming"
	^ `Color r: 1.0 g: 1.0 b: 0.078`! !

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

!methodRemoval: Color class #aqua!
Color class removeSelector: #aqua!
!methodRemoval: Color class #beige!
Color class removeSelector: #beige!
!methodRemoval: Color class #blueColorDict!
Color class removeSelector: #blueColorDict!
!methodRemoval: Color class #blueColorDict:!
Color class removeSelector: #blueColorDict:!
!methodRemoval: Color class #blueGreenColorDict!
Color class removeSelector: #blueGreenColorDict!
!methodRemoval: Color class #blueGreenColorDict:!
Color class removeSelector: #blueGreenColorDict:!
!methodRemoval: Color class #brightColorDict!
Color class removeSelector: #brightColorDict!
!methodRemoval: Color class #brightColorDict:!
Color class removeSelector: #brightColorDict:!
!methodRemoval: Color class #brightGreen!
Color class removeSelector: #brightGreen!
!methodRemoval: Color class #brownColorDict!
Color class removeSelector: #brownColorDict!
!methodRemoval: Color class #brownColorDict:!
Color class removeSelector: #brownColorDict:!
!methodRemoval: Color class #colorNames!
Color class removeSelector: #colorNames!
!methodRemoval: Color class #colorNamesDict!
Color class removeSelector: #colorNamesDict!
!methodRemoval: Color class #darkBlue!
Color class removeSelector: #darkBlue!
!methodRemoval: Color class #darkColorDict!
Color class removeSelector: #darkColorDict!
!methodRemoval: Color class #darkColorDict:!
Color class removeSelector: #darkColorDict:!
!methodRemoval: Color class #darkGreen!
Color class removeSelector: #darkGreen!
!methodRemoval: Color class #darkPink!
Color class removeSelector: #darkPink!
!methodRemoval: Color class #darkPurple!
Color class removeSelector: #darkPurple!
!methodRemoval: Color class #defaultColorNamesDictionary!
Color class removeSelector: #defaultColorNamesDictionary!
!methodRemoval: Color class #doesNotUnderstand:!
Color class removeSelector: #doesNotUnderstand:!
!methodRemoval: Color class #exactColorNamed:!
Color class removeSelector: #exactColorNamed:!
!methodRemoval: Color class #forestGreen!
Color class removeSelector: #forestGreen!
!methodRemoval: Color class #fromString:!
Color class removeSelector: #fromString:!
!methodRemoval: Color class #grayColorDict:!
Color class removeSelector: #grayColorDict:!
!methodRemoval: Color class #greenColorDict!
Color class removeSelector: #greenColorDict!
!methodRemoval: Color class #greenColorDict:!
Color class removeSelector: #greenColorDict:!
!methodRemoval: Color class #grey!
Color class removeSelector: #grey!
!methodRemoval: Color class #greyColorDict!
Color class removeSelector: #greyColorDict!
!methodRemoval: Color class #greyColorDict:!
Color class removeSelector: #greyColorDict:!
!methodRemoval: Color class #hotPink!
Color class removeSelector: #hotPink!
!methodRemoval: Color class #indigo!
Color class removeSelector: #indigo!
!methodRemoval: Color class #lavender!
Color class removeSelector: #lavender!
!methodRemoval: Color class #lightColorDict!
Color class removeSelector: #lightColorDict!
!methodRemoval: Color class #lightColorDict:!
Color class removeSelector: #lightColorDict:!
!methodRemoval: Color class #lightPink!
Color class removeSelector: #lightPink!
!methodRemoval: Color class #lightPurple!
Color class removeSelector: #lightPurple!
!methodRemoval: Color class #lilac!
Color class removeSelector: #lilac!
!methodRemoval: Color class #lime!
Color class removeSelector: #lime!
!methodRemoval: Color class #limeGreen!
Color class removeSelector: #limeGreen!
!methodRemoval: Color class #maroon!
Color class removeSelector: #maroon!
!methodRemoval: Color class #mauve!
Color class removeSelector: #mauve!
!methodRemoval: Color class #mustard!
Color class removeSelector: #mustard!
!methodRemoval: Color class #navyBlue!
Color class removeSelector: #navyBlue!
!methodRemoval: Color class #olive!
Color class removeSelector: #olive!
!methodRemoval: Color class #oliveGreen!
Color class removeSelector: #oliveGreen!
!methodRemoval: Color class #orangeColorDict!
Color class removeSelector: #orangeColorDict!
!methodRemoval: Color class #orangeColorDict:!
Color class removeSelector: #orangeColorDict:!
!methodRemoval: Color class #paleGreen!
Color class removeSelector: #paleGreen!
!methodRemoval: Color class #pastelColorDict!
Color class removeSelector: #pastelColorDict!
!methodRemoval: Color class #pastelColorDict:!
Color class removeSelector: #pastelColorDict:!
!methodRemoval: Color class #peach!
Color class removeSelector: #peach!
!methodRemoval: Color class #periwinkle!
Color class removeSelector: #periwinkle!
!methodRemoval: Color class #pinkColorDict!
Color class removeSelector: #pinkColorDict!
!methodRemoval: Color class #pinkColorDict:!
Color class removeSelector: #pinkColorDict:!
!methodRemoval: Color class #purpleColorDict!
Color class removeSelector: #purpleColorDict!
!methodRemoval: Color class #purpleColorDict:!
Color class removeSelector: #purpleColorDict:!
!methodRemoval: Color class #redColorDict!
Color class removeSelector: #redColorDict!
!methodRemoval: Color class #redColorDict:!
Color class removeSelector: #redColorDict:!
!methodRemoval: Color class #royalBlue!
Color class removeSelector: #royalBlue!
!methodRemoval: Color class #salmon!
Color class removeSelector: #salmon!
!methodRemoval: Color class #saturatedColorDict!
Color class removeSelector: #saturatedColorDict!
!methodRemoval: Color class #saturatedColorDict:!
Color class removeSelector: #saturatedColorDict:!
!methodRemoval: Color class #seaGreen!
Color class removeSelector: #seaGreen!
!methodRemoval: Color class #setColorNamesDict:!
Color class removeSelector: #setColorNamesDict:!
!methodRemoval: Color class #skyBlue!
Color class removeSelector: #skyBlue!
!methodRemoval: Color class #teal!
Color class removeSelector: #teal!
!methodRemoval: Color class #traditionalColorNamesDictionary!
Color class removeSelector: #traditionalColorNamesDictionary!
!methodRemoval: Color class #turquoise!
Color class removeSelector: #turquoise!
!methodRemoval: Color class #violet!
Color class removeSelector: #violet!
!methodRemoval: Color class #xkcdFirst48ColorNamesDictionary!
Color class removeSelector: #xkcdFirst48ColorNamesDictionary!
!methodRemoval: Color class #yellowColorDict!
Color class removeSelector: #yellowColorDict!
!methodRemoval: Color class #yellowColorDict:!
Color class removeSelector: #yellowColorDict:!
!methodRemoval: Color #closestAssocFrom:!
Color removeSelector: #closestAssocFrom:!
!methodRemoval: Color #closestColor!
Color removeSelector: #closestColor!
!methodRemoval: Color #closestColorAssociation!
Color removeSelector: #closestColorAssociation!
!methodRemoval: Color #closestColorFrom:!
Color removeSelector: #closestColorFrom:!
!methodRemoval: Color #closestColorName!
Color removeSelector: #closestColorName!
!methodRemoval: Color #closestNameFrom:!
Color removeSelector: #closestNameFrom:!
!methodRemoval: Color #isBlueGreen!
Color removeSelector: #isBlueGreen!
!methodRemoval: Color #isBright!
Color removeSelector: #isBright!
!methodRemoval: Color #isBrown!
Color removeSelector: #isBrown!
!methodRemoval: Color #isDark!
Color removeSelector: #isDark!
!methodRemoval: Color #isGray!
Color removeSelector: #isGray!
!methodRemoval: Color #isGrey!
Color removeSelector: #isGrey!
!methodRemoval: Color #isLight!
Color removeSelector: #isLight!
!methodRemoval: Color #isOrange!
Color removeSelector: #isOrange!
!methodRemoval: Color #isPastel!
Color removeSelector: #isPastel!
!methodRemoval: Color #isPink!
Color removeSelector: #isPink!
!methodRemoval: Color #isSaturated!
Color removeSelector: #isSaturated!

!Color reorganize!
('access' alpha blue brightness chroma green hue icon iconOrThumbnailOfSize: luminance red saturation swatch)
('conversions' asNontranslucentColor bitPatternForDepth: bitPatternForGrayForm closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor indexInMap: makeForegroundColor pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth:)
('equality' diff: rgbDistance:)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('other' colorName name)
('printing' hexStringRGB printOn: printString storeArrayOn: storeArrayValuesOn: storeOn:)
('queries' isBlack isOpaque isTransparent isWhite)
('selection' isBlue isCyan isGreen isMagenta isRed isYellow)
('testing' is: isCollection mightBeTranslucent)
('transformations' * + - / adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter slightlyDarker slightlyLighter slightlyWhiter twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('private' attemptToMutateError basicSetRed:green:blue: setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setRed:green:blue: setRed:green:blue:range:)
('as yet unclassified' color convertToCurrentVersion:refStream:)
!


!Color class reorganize!
('instance creation' clipR:g:b: colorFrom: colorFromPixelValue:depth: fromArray: fromRgbTriplet: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: jet: new r:g:b: r:g:b:alpha: r:g:b:range: random random2 random2: random:)
('class initialization' initializeGrayToIndexMap initializeIndexedColors)
('examples' colorRampForDepth:extent: experimentsTowarsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('named colors' black blue brown cyan darkGray gray green lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightRed lightYellow magenta orange pink purple red tan transparent veryDarkGray veryLightGray veryVeryDarkGray veryVeryLightGray white yellow)
('colormaps' cachedColormapForGrayFrom: cachedColormapFrom:to: cachedColormapFromGrayTo: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapForGray8bppFrom: computeColormapFrom:to: computeColormapFromGray8bppForDepth: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor: computeRGBColormapForGray8)
('other' maskingMap:)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper:)
('color space conversions' applySimpleGamma:to: linearTosRGBGamma: sRGBGammaToLinear:)
('cached state access' grayToIndexMap indexedColors releaseClassCachedState)
!

