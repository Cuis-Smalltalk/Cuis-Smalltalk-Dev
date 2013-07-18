'From Cuis 4.1 of 12 December 2012 [latest update: #1687] on 19 April 2013 at 4:04:56.17463 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HalfComponentMask HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!classDefinition: #TranslucentColor category: #'Graphics-Primitives'!
Color subclass: #TranslucentColor
	instanceVariableNames: 'alpha floatAlpha '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 16:00'!
fix
	"
	Color allInstancesDo: [ :c | c fix ].
	TranslucentColor allInstancesDo: [ :c | c fix ].
	"

	floatRGB _ FloatArray with: self red with: self green with: self blue.
	colorSpace _ colorSpace ifNil: [ #sRGB ]! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 16:01'!
setRed: r green: g blue: b colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	rgb ifNotNil: [self attemptToMutateError].
	rgb _
		(((r * ComponentMax) rounded bitAnd: ComponentMask) bitShift: RedShift) +
		(((g * ComponentMax) rounded bitAnd: ComponentMask) bitShift: GreenShift) +
		 ((b * ComponentMax) rounded bitAnd: ComponentMask).
	floatRGB _ FloatArray with: r with: g with: b.
	colorSpace _ aSymbol.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 15:14'!
setRed: r green: g blue: b range: range colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	rgb ifNotNil: [self attemptToMutateError].
	rgb _
		((((r * ComponentMask) // range) bitAnd: ComponentMask) bitShift: RedShift) +
		((((g * ComponentMask) // range) bitAnd: ComponentMask) bitShift: GreenShift) +
		 (((b * ComponentMask) // range) bitAnd: ComponentMask).
	floatRGB _ FloatArray with: r with: g with: b.
	floatRGB /= range.
	colorSpace _ aSymbol.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !


!TranslucentColor methodsFor: 'private' stamp: 'jmv 4/19/2013 16:01'!
fix

	super fix.
	floatAlpha _ self alpha! !

!TranslucentColor methodsFor: 'private' stamp: 'jmv 4/19/2013 16:01'!
setRed: r green: g blue: b alpha: alphaValue colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	rgb ifNotNil: [self attemptToMutateError].
	rgb _
		(((r * ComponentMax) rounded bitAnd: ComponentMask) bitShift: RedShift) +
		(((g * ComponentMax) rounded bitAnd: ComponentMask) bitShift: GreenShift) +
		 ((b * ComponentMax) rounded bitAnd: ComponentMask).
	floatRGB _ FloatArray with: r with: g with: b.
	colorSpace _ aSymbol.
	alpha _ (255.0 * alphaValue) asInteger min: 255 max: 0.
	floatAlpha _ alphaValue.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !


!Color methodsFor: 'transformations' stamp: 'jmv 4/19/2013 15:57'!
* aNumber
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Color brown *2) display
	"
	^ Color basicNew
		setRed: (self red * aNumber min: 1.0 max: 0.0)
		green: (self green * aNumber min: 1.0 max: 0.0)
		blue: (self blue * aNumber min: 1.0 max: 0.0)
		colorSpace: colorSpace! !

!Color methodsFor: 'transformations' stamp: 'jmv 4/19/2013 15:58'!
+ aColor
	"Answer this color mixed with the given color in an additive color space.  "
	"
	(Color blue + Color green) display
	"
	^ Color basicNew
		setRed: (self red * aColor red min: 1.0 max: 0.0)
		green: (self green * aColor green min: 1.0 max: 0.0)
		blue: (self blue * aColor blue min: 1.0 max: 0.0)
		colorSpace: colorSpace! !

!Color methodsFor: 'transformations' stamp: 'jmv 4/19/2013 15:58'!
- aColor
	"Answer aColor is subtracted from the given color in an additive color space.  "
	"
	(Color white - Color red) display
	"
	^ Color basicNew
		setRed: (self red - aColor red min: 1.0 max: 0.0)
		green: (self green - aColor green min: 1.0 max: 0.0)
		blue: (self blue - aColor blue min: 1.0 max: 0.0)
		colorSpace: colorSpace! !

!Color methodsFor: 'transformations' stamp: 'jmv 4/19/2013 15:58'!
/ aNumber
	"Answer this color with its RGB divided by the given number. "
	"
	(Color red / 2) display
	"
	^ Color basicNew
		setRed: (self red / aNumber min: 1.0 max: 0.0)
		green: (self green / aNumber min: 1.0 max: 0.0)
		blue: (self blue / aNumber min: 1.0 max: 0.0)
		colorSpace: colorSpace! !

!Color methodsFor: 'transformations' stamp: 'jmv 4/19/2013 15:33'!
alpha: alphaValue
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifFalse: [
		^ TranslucentColor basicNew
			setRed: self red
			green: self green
			blue: self blue
			alpha: alphaValue
			colorSpace: colorSpace ]! !

!Color methodsFor: 'conversions' stamp: 'jmv 4/19/2013 14:59'!
bitPatternForDepth: depth
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"
	"Details: The pattern for the most recently requested depth is cached."

	depth = cachedDepth ifTrue: [
		^ cachedBitPattern].

	cachedDepth _ depth.
	^ cachedBitPattern _ Bitmap with: (self pixelWordForDepth: depth)! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 15:16'!
setHue: hue chroma: chroma brightness: brightness
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 

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
	m _ brightness - ((r1 max: g1) max: b1).
	m < 0.0 ifTrue: [
		m * ComponentMax < -0.01
			ifTrue: [ ^nil ]. "No color exists with required parameters"
		m _ 0.0 ].
	r1 _ r1 + m.
	r1 > 1.0 ifTrue: [
		(r1 * ComponentMax) rounded > ComponentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		r1 _ 1.0 ].
	g1 _ g1 + m.
	g1 > 1.0 ifTrue: [
		(g1 * ComponentMax) rounded > ComponentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		g1 _ 1.0 ].
	b1 _ b1 + m.
	b1 > 1.0 ifTrue: [
		(b1 * ComponentMax) rounded > ComponentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		b1 _ 1.0 ].
	self setRed: r1 green: g1 blue: b1 colorSpace: #sRGB! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 15:16'!
setHue: hue chroma: chroma luminance: luma
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 

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
	m _ luma - (0.299*r1) - (0.587*g1) - (0.114*b1).
	m < 0.0 ifTrue: [
		m * ComponentMax < -0.01
			ifTrue: [ ^nil ]. "No color exists with required parameters"
		m _ 0.0 ].
	r1 _ r1 + m.
	r1 > 1.0 ifTrue: [
		(r1 * ComponentMax) rounded > ComponentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		r1 _ 1.0 ].
	g1 _ g1 + m.
	g1 > 1.0 ifTrue: [
		(g1 * ComponentMax) rounded > ComponentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		g1 _ 1.0 ].
	b1 _ b1 + m.
	b1 > 1.0 ifTrue: [
		(b1 * ComponentMax) rounded > ComponentMax
			ifTrue: [ ^nil ]. "No color exists with required parameters".
		b1 _ 1.0 ].
	self setRed: r1 green: g1 blue: b1 colorSpace: #sRGB! !

!Color methodsFor: 'private' stamp: 'jmv 4/19/2013 16:03'!
setHue: hue saturation: saturation brightness: brightness
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."

	| s v hf i f p q t | 
	s _ saturation asFloat min: 1.0 max: 0.0.
	v _ brightness asFloat min: 1.0 max: 0.0.

	hf _ hue asFloat.
	(hf < 0.0 or: [hf >= 360.0])
		ifTrue: [hf _ hf - ((hf quo: 360.0) asFloat * 360.0)].
	hf _ hf / 60.0.
	i _ hf asInteger.  "integer part of hue"
	f _ hf fractionPart.         "fractional part of hue"
	p _ (1.0 - s) * v.
	q _ (1.0 - (s * f)) * v.
	t _ (1.0 - (s * (1.0 - f))) * v.

	0 = i ifTrue: [ ^ self setRed: v green: t blue: p colorSpace: #sRGB ].
	1 = i ifTrue: [ ^ self setRed: q green: v blue: p colorSpace: #sRGB ].
	2 = i ifTrue: [ ^ self setRed: p green: v blue: t colorSpace: #sRGB ].
	3 = i ifTrue: [ ^ self setRed: p green: q blue: v colorSpace: #sRGB ].
	4 = i ifTrue: [ ^ self setRed: t green: p blue: v colorSpace: #sRGB ].
	5 = i ifTrue: [ ^ self setRed: v green: p blue: q colorSpace: #sRGB ].

	self error: 'implementation error'! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 4/19/2013 15:17'!
gray: brightness
	"Return a gray shade with the given brightness in the range [0.0..1.0]."

	^ self basicNew
		setRed: brightness
		green: brightness
		blue: brightness
		colorSpace: #sRGB! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/19/2013 15:17'!
r: r g: g b: b
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."

	^ self basicNew
		setRed: r
		green: g
		blue: b
		colorSpace: #sRGB! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/19/2013 15:15'!
r: r g: g b: b range: range
	"Return a color with the given r, g, and b components specified as integers in the range [0..r]. This avoids the floating point arithmetic in the red:green:blue: message and is thus a bit faster for certain applications (such as computing a sequence of colors for a palette).
	Default colorSpace is #sRGB"

	^ self basicNew setRed: r green: g blue: b range: range colorSpace: #sRGB! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 4/19/2013 15:17'!
random2
	"Return a random color with a distribution that spans over all possible colors."

	^ self basicNew
		setRed: RandomStream next
		green: RandomStream next
		blue: RandomStream next
		colorSpace: #sRGB! !


!TranslucentColor methodsFor: 'conversions' stamp: 'jmv 4/19/2013 15:59'!
alpha: alphaValue
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifTrue: [
		^ Color basicNew
		setRed: self red
		green: self green
		blue: self blue
		colorSpace: colorSpace].
	^ super alpha: alphaValue! !

!methodRemoval: TranslucentColor #setRgb:alpha:!
TranslucentColor removeSelector: #setRgb:alpha:!
!classDefinition: #TranslucentColor category: #'Graphics-Primitives'!
Color subclass: #TranslucentColor
	instanceVariableNames: 'alpha floatAlpha'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!methodRemoval: Color #setPrivateRed:green:blue:!
Color removeSelector: #setPrivateRed:green:blue:!
!methodRemoval: Color #setRed:green:blue:!
Color removeSelector: #setRed:green:blue:!
!methodRemoval: Color #setRed:green:blue:range:!
Color removeSelector: #setRed:green:blue:range:!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HalfComponentMask HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	Color allInstancesDo: [ :c | c fix ].
	TranslucentColor allInstancesDo: [ :c | c fix ].!

