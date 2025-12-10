'From Cuis7.5 [latest update: #7752] on 10 December 2025 at 4:42:26 pm'!

!Color methodsFor: 'accessing' stamp: 'len 10/Dec/2025 15:34:47'!
oklchChroma
	^ self oklchDo: [:L :C :h| C]! !

!Color methodsFor: 'accessing' stamp: 'len 10/Dec/2025 16:13:36'!
oklchDo: aBlock
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
		value: (B arcTan: A) radiansToDegrees \\ 360.0 "hue in degrees"! !

!Color methodsFor: 'accessing' stamp: 'len 10/Dec/2025 15:34:57'!
oklchHue
	^ self oklchDo: [:L :C :h| h]! !

!Color methodsFor: 'accessing' stamp: 'len 10/Dec/2025 15:34:39'!
oklchLightness
	^ self oklchDo: [:L :C :h| L]! !


!Color class methodsFor: 'instance creation' stamp: 'len 10/Dec/2025 15:33:52'!
okOrNilL: lightness c: chroma h: hue
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
		ifTrue: [self r: r g: g b: b]! !


!Color class methodsFor: 'instance creation' stamp: 'len 10/Dec/2025 15:10:24'!
okl: lightness c: chroma h: hue
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
	^ answer! !

!methodRemoval: Color class #okl:c:h:sRGBDo: stamp: 'len 10/Dec/2025 15:08:22'!
Color class removeSelector: #okl:c:h:sRGBDo:!

!Color reorganize!
('accessing' alpha blue brightness chroma color green hue icon iconOrThumbnailOfSize: luminance oklchChroma oklchDo: oklchHue oklchLightness red saturation swatch)
('comparing' = hash primitiveEqual:)
('conversions' asNontranslucentColor bitPatternForDepth: bitPatternForGrayForm closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor indexInMap: makeForegroundColor pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth:)
('equality' diff: rgbDistance:)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('other' colorName name)
('printing' hexStringRGB printOn: printString storeArrayOn: storeArrayValuesOn: storeOn:)
('queries' isBlack isOpaque isTransparent isWhite)
('selection' isBlue isCyan isGreen isMagenta isRed isYellow)
('testing' is: isCollection mightBeTranslucent)
('transformations' * + - / adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter shade: slightlyDarker slightlyLighter slightlyWhiter tone: twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('private' attemptToMutateError basicSetRed:green:blue: clipToValidValues setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setRed:green:blue: setRed:green:blue:range:)
('object serialization' convertToCurrentVersion:refStream:)
!

