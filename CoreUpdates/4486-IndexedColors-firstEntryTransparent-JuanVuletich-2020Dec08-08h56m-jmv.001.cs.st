'From Cuis 5.0 [latest update: #4484] on 8 December 2020 at 9:11:10 am'!

!Color class methodsFor: 'class initialization' stamp: 'jmv 12/8/2020 09:02:12'!
oneBitColors
	"Answer the colors available for 1 bit Forms.
	Note: For depths 2 to 8, first entry is Color transparent, not white. See #initializeIndexedColors"

	^ `{ Color white. Color black }`! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 12/8/2020 09:04:11'!
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

	d = 1 ifTrue: [^ self oneBitColors at: (p bitAnd: 16r01) + 1].

	d = 8 ifTrue: [^ self indexedColors at: (p bitAnd: 16rFF) + 1].
	d = 4 ifTrue: [^ self indexedColors at: (p bitAnd: 16r0F) + 1].
	d = 2 ifTrue: [^ self indexedColors at: (p bitAnd: 16r03) + 1].

	d = 32 ifTrue: [
		"eight bits per component; 8 bits of alpha"
		alpha _ p bitShift: -24.
		alpha = 0 ifTrue: [ ^`Color transparent` ].
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
			^`Color black` ].
		^ Color r: r g: g b: b range: 31].

	d = 12 ifTrue: [
		"four bits per component"
		r _ (p bitShift: -8) bitAnd: 16rF.
		g _ (p bitShift: -4) bitAnd: 16rF.
		b _ p bitAnd: 16rF.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^`Color black` ].
		^ Color r: r g: g b: b range: 15].

	d = 9 ifTrue: [
		"three bits per component"
		r _ (p bitShift: -6) bitAnd: 16r7.
		g _ (p bitShift: -3) bitAnd: 16r7.
		b _ p bitAnd: 16r7.
		(r = 0 and: [ g = 0 and: [ b = 1]]) ifTrue: [
			^`Color black` ].
		^ Color r: r g: g b: b range: 7].

	self error: 'unknown pixel depth: ', d printString! !

!Color class methodsFor: 'class initialization' stamp: 'jmv 12/8/2020 09:03:11'!
initializeIndexedColors
	"Build an array of colors corresponding to the fixed colormap used
	 for depths of 2, 4, or 8 bits."
	"
	Color initializeIndexedColors
	"
	"
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 1 g: 0 b: 0); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0 g: 1 b: 0); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0 g: 0 b: 1); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: (Color r: 0.5 g: 0.5 b: 0.5); display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color blue; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color green; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color red; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color cyan; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color yellow; display.
ColorForm extent: 128@128 :: fillColor: Color black :: colorAt: 10@10 put: Color magenta; display.
	"

	| a index grayVal |
	a _ Array new: 256.

	"1-bit colors (monochrome)"
	a at: 1 put: `Color transparent`.								"Note: For 1bpp forms, it is white"
	a at: 2 put: `Color r: 0.0 g: 0.0 b: 0.0`.								"black"

	"additional colors for 2-bit color"
	a at: 3 put: `Color r: 1.0 g: 1.0 b: 1.0`.	"opaque white"
	a at: 4 put: `Color r: 0.5 g: 0.5 b: 0.5`.	"1/2 gray"

	"additional colors for 4-bit color"
	a at:  5 put: Color red.
	a at:  6 put: Color green.
	a at:  7 put: Color blue.
	a at:  8 put: Color cyan.
	a at:  9 put: Color yellow.
	a at: 10 put: Color magenta.

	a at: 11 put: `Color r: 0.125 g: 0.125 b: 0.125`.		"1/8 gray"
	a at: 12 put: `Color r: 0.25 g: 0.25 b: 0.25`.			"2/8 gray"
	a at: 13 put: `Color r: 0.375 g: 0.375 b: 0.375`.		"3/8 gray"
	a at: 14 put: `Color r: 0.625 g: 0.625 b: 0.625`.		"5/8 gray"
	a at: 15 put: `Color r: 0.75 g: 0.75 b: 0.75`.			"6/8 gray"
	a at: 16 put: `Color r: 0.875 g: 0.875 b: 0.875`.		"7/8 gray"

	"additional colors for 8-bit color"
	"24 more shades of gray (1/32 increments but not repeating 1/8 increments)"
	index _ 17.
	1 to: 31 do: [:v |
		(v \\ 4) = 0 ifFalse: [
			grayVal _ v / 32.0.
			a at: index put: (Color r: grayVal g: grayVal b: grayVal).
			index _ index + 1]].

	"The remainder of color table defines a color cube with six steps
	 for each primary color. Note that the corners of this cube may repeat
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

!Color class methodsFor: 'colormaps' stamp: 'jmv 12/8/2020 09:07:06'!
computeColormapForGray8bppFrom: sourceDepth
	"Compute a colorMap for translatingfrom the given depth to the 8bpp grays"
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map |
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color"
		map _ sourceDepth = 1 ifTrue: [ self oneBitColors ] ifFalse: [ self indexedColors ].
		map _ (map copyFrom: 1 to: (1 bitShift: sourceDepth))
					collect: [ :c | (c luminance * 255) rounded].
		map _ map as: Bitmap.
		^ map ].
	^ self computeRGBColormapForGray8! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 12/8/2020 09:08:06'!
computeColormapFrom: sourceDepth to: destDepth
	"Compute a colorMap for translating between the given depths. A colormap is a Bitmap whose entries contain the pixel values for the destination depth. Typical clients use cachedColormapFrom:to: instead."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map bitsPerColor |
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color"
		map _ sourceDepth = 1 ifTrue: [ self oneBitColors ] ifFalse: [ self indexedColors ].
		map _ (map copyFrom: 1 to: (1 bitShift: sourceDepth))
					collect: [:c | c pixelValueForDepth: destDepth].
		^map as: Bitmap ].
	"source is 16-bit or 32-bit RGB"
	destDepth > 8
		ifTrue: [bitsPerColor _ 5]  "retain maximum color resolution"
		ifFalse: [bitsPerColor _ 4].
	^self computeRGBColormapFor: destDepth bitsPerColor: bitsPerColor! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 12/8/2020 09:08:57'!
computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth
	| map |

	map _ sourceDepth = 1 ifTrue: [ self oneBitColors ] ifFalse: [ self indexedColors ].
	map _ (map copyFrom: 1 to: (1 bitShift: sourceDepth)) collect: [ :cc | | f c |
		f _ 1.0 - (cc red + cc green + cc blue / 3.0 ).
		c _ targetColor
			ifNotNil: [
				destDepth = 32
					ifTrue: [ targetColor * f alpha: f]
					ifFalse: [ targetColor alphaMixed: f*1.5 with: `Color white` ]]
			ifNil: [ cc ].
		destDepth = 32
			ifTrue: [ c pixelValueForDepth: destDepth]
			ifFalse: [
				f = 0.0
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ]]].
	^map as: Bitmap! !


!Form methodsFor: 'converting' stamp: 'jmv 12/8/2020 09:10:47'!
as8BitColorForm
	"Simple conversion of zero pixels to transparent. Force it to 8 bits."

	| f |
	f _ ColorForm extent: self extent depth: 8.
	self displayOn: f at: self offset negated.
	f colors: Color indexedColors copy.
	f offset: self offset.
	^ f! !


!ColorForm methodsFor: 'private' stamp: 'jmv 12/8/2020 09:09:50'!
ensureColorArrayExists
	"Return my color palette."

	colors ifNil: [
		self depth > 8 ifTrue: [^ self error: 'ColorForms only support depths up to 8 bits'].
		self colors: (Color indexedColors copyFrom: 1 to: (1 bitShift: self depth)) ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Color initializeIndexedColors!

