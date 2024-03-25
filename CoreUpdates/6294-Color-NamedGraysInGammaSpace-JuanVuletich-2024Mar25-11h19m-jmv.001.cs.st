'From Cuis6.3 [latest update: #6293] on 25 March 2024 at 11:21:51 am'!

!Color class methodsFor: 'black-white-grays' stamp: 'jmv 3/25/2024 11:16:43'!
darkGray
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.44)`! !

!Color class methodsFor: 'black-white-grays' stamp: 'jmv 3/25/2024 11:16:57'!
gray
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.56)`! !

!Color class methodsFor: 'black-white-grays' stamp: 'jmv 3/25/2024 11:17:09'!
lightGray
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.67)`! !

!Color class methodsFor: 'black-white-grays' stamp: 'jmv 3/25/2024 11:16:34'!
veryDarkGray
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.33)`! !

!Color class methodsFor: 'black-white-grays' stamp: 'jmv 3/25/2024 11:17:19'!
veryLightGray
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.78)`! !

!Color class methodsFor: 'black-white-grays' stamp: 'jmv 3/25/2024 11:16:08'!
veryVeryDarkGray
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.22)`! !

!Color class methodsFor: 'black-white-grays' stamp: 'jmv 3/25/2024 11:17:31'!
veryVeryLightGray
	"Perceptual scale is logarithmic (we see in Gamma space). Color is in Linear Space.
	See other methods in this category.
	Divide the black - white range in 9 intervals of equal distance in gamma space, disregard darkest gray."
	^ `Color white * (Color sRGBGammaToLinear: 0.89)`! !


!Color class reorganize!
('instance creation' clipR:g:b: colorFrom: colorFromPixelValue:depth: fromArray: fromHexString: fromRgbOrRgba: fromRgbTriplet: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: jet: numSlots r:g:b: r:g:b:alpha: r:g:b:range: random random2 random2: random:)
('class initialization' initializeGrayToIndexMap initializeIndexedColors newFromStream: oneBitColors)
('examples' colorRampForDepth:extent: experimentsTowardsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('black-white-grays' black darkGray gray lightGray veryDarkGray veryLightGray veryVeryDarkGray veryVeryLightGray white)
('named colors' blue brown cyan green lightBlue lightBrown lightCyan lightGreen lightMagenta lightOrange lightRed lightYellow magenta orange pink purple red tan transparent yellow)
('colormaps' cachedColormapForGrayFrom: cachedColormapFrom:to: cachedColormapFromGrayTo: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapForGray8bppFrom: computeColormapFrom:to: computeColormapFromGray8bppForDepth: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor: computeRGBColormapForGray8)
('other' maskingMap:)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper:)
('color space conversions' applySimpleGamma:to: linearTosRGBGamma: sRGBGammaToLinear:)
('cached state access' grayToIndexMap indexedColors releaseClassState)
!

