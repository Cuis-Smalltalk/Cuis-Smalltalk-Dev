'From Cuis 5.0 of 7 November 2016 [latest update: #3109] on 19 June 2017 at 12:07:06 pm'!

!Integer methodsFor: 'converting' stamp: 'ar 10/31/1998 23:04'!
asColorOfDepth: d
	"Return a color value representing the receiver as color of the given depth"
	^Color colorFromPixelValue: self depth: d! !


!InputSensor methodsFor: 'keyboard' stamp: 'jmv 9/2/2016 11:08:09'!
kbdTest
	"
	Sensor kbdTest
	"
	"This test routine will print the unmodified character, its keycode,
	and the OR of all its modifier bits, until the character x is typed

Also useful:
	Sensor test
	"
	| char |
	char _ nil.
	[ char = $x ] whileFalse: [
		[ self keyboardPressed ] whileFalse.
		char _ self keyboard.
		Display fill: (5@5 extent: 400@20) fillColor: Color white.
		(String streamContents: [ :s |
			s
				nextPut: $-;
				nextPut: char;
				nextPut: $-;
				space;
				print: char numericValue;
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ]])
		displayAt: 10 @ 10 ].! !


!String methodsFor: 'displaying' stamp: 'jmv 5/12/2015 15:52'!
displayOn: aDisplayMedium at: aPoint 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	^self displayOn: aDisplayMedium at: aPoint textColor: Color black
	"
	'Display' displayOn: Display at: 10@10
	"! !


!Color methodsFor: 'conversions' stamp: 'bf 4/18/2001 16:25'!
makeForegroundColor
        "Make a foreground color contrasting with me"
        ^self luminance >= 0.5
                ifTrue: [Color black]
                ifFalse: [Color white]! !

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

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:36'!
closestColorName
	"Answer the name symbol of the closest matching color in the colorNames dictionary"
"
	(Color red lighter lighter) closestColor name.
"
	
	^ self closestNameFrom: (Color colorNamesDict)! !

!Color methodsFor: 'other' stamp: 'KenD 7/18/2015 20:41'!
colorName
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color."

	^(Color colorNamesDict keyAtValue: self  ifAbsent: [nil])
! !

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

!Color methodsFor: 'transformations' stamp: 'jmv 9/10/2009 18:41'!
quiteBlacker

	^ self alphaMixed: 0.8 with: Color black
! !

!Color methodsFor: 'transformations' stamp: 'jmv 9/10/2009 18:44'!
quiteWhiter

	^ self alphaMixed: 0.6 with: Color white! !

!Color methodsFor: 'transformations' stamp: 'dew 1/19/2002 01:25'!
slightlyWhiter

	^ self alphaMixed: 0.85 with: Color white
! !

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


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/25/2016 15:31'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 7/28/2015 08:26'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			0@0 + (0@(d+AbstractFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue.
	DisplayScreen screenUpdateRequired: nil! !

!Transcripter methodsFor: 'private' stamp: 'di 8/14/97 12:12'!
black
	Display depth = 1 ifTrue: [^ Bitmap with: 16rFFFFFFFF "Works without color support"].
	^ Color black! !

!Transcripter methodsFor: 'private' stamp: 'di 8/14/97 12:12'!
white
	Display depth = 1 ifTrue: [^ Bitmap with: 0 "Works without color support"].
	^ Color white! !


!DataStream class methodsFor: 'as yet unclassified'!
example
    "An example and test of DataStream/ReferenceStream.
     11/19/92 jhm: Use self testWith:."
    "DataStream example"
    "ReferenceStream example"
    | input sharedPoint |

    "Construct the test data."
    input _ Array new: 9.
    input at: 1 put: nil.
    input at: 2 put: true.
    input at: 3 put: (Form extent: 63 @ 50 depth: 8).
		(input at: 3) fillWithColor: Color lightBlue.
    input at: 4 put: #(3 3.0 'three').
    input at: 5 put: false.
    input at: 6 put: 1024 @ -2048.
    input at: 7 put: #x.
    input at: 8 put: (Array with: (sharedPoint _ 0 @ -30000)).
    input at: 9 put: sharedPoint.

    "Write it out, read it back, and return it for inspection."
    ^ self testWith: input! !


!ReferenceStream class methodsFor: 'as yet unclassified'!
example2
"Here is the way to use DataStream and ReferenceStream:
	rr _ ReferenceStream fileNamed: ''test.obj''.
	rr nextPut: <your object>.
	rr close.

To get it back:
	rr _ ReferenceStream fileNamed: ''test.obj''.
	<your object> _ rr next.
	rr close.
"
"An example and test of DataStream/ReferenceStream.
	 11/19/92 jhm: Use self testWith:."
	"ReferenceStream example2"
	| input sharedPoint |

	"Construct the test data."
	input _ Array new: 9.
	input at: 1 put: nil.
	input at: 2 put: true.
	input at: 3 put: false.
	input at: 4 put: #(-4 -4.0 'four' four).
	input at: 5 put: (Form extent: 63 @ 50 depth: 8).
		(input at: 5) fillWithColor: Color lightOrange.
	input at: 6 put: 1024 @ -2048.
	input at: 7 put: input. "a cycle"
	input at: 8 put: (Array with: (sharedPoint _ 0 @ -30000)).
	input at: 9 put: sharedPoint.

	"Write it out, read it back, and return it for inspection."
	^ self testWith: input
! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 12/12/2014 15:53'!
offerColorMenu
	"Present a menu of available colors, and if one is chosen, apply it to the current selection."
	"This is a user command, and generates undo"

	| attribute colors index thisSel |
	thisSel _ self selection.
	colors _ #(#black #magenta #red #yellow #green #blue #cyan #white ).
	index _ (PopUpMenu
		labelArray: colors , #('choose color...' )
		lines: (Array with: colors size + 1)) startUpMenu.
	index = 0 ifTrue: [ ^ true ].
	index <= colors size
		ifTrue: [ attribute _ TextColor color: (Color perform: (colors at: index)) ]
		ifFalse: [
			index _ index - colors size - 1.
			"Re-number!!!!!!"
			index = 0 ifTrue: [ attribute _ self chooseColor ].
			thisSel ifNil: [ ^ true ]].
	attribute ifNotNil: [ self applyAttribute: attribute ].
	^ true.! !


!TextAction class methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 09:38'!
textActionColor
	^Color r: 0.4 g: 0 b: 1.0! !


!TextColor methodsFor: 'testing' stamp: 'jmv 1/21/2011 11:33'!
isSet
	"Do not include Color black, as it is the default color."
	^color ~= Color black! !


!TextColor class methodsFor: 'constants'!
black
	^ self new color: Color black! !

!TextColor class methodsFor: 'constants'!
blue
	^ self new color: Color blue! !

!TextColor class methodsFor: 'constants'!
cyan
	^ self new color: Color cyan! !

!TextColor class methodsFor: 'constants' stamp: 'ajh 9/10/2002 02:26'!
gray
	^ self new color: Color gray! !

!TextColor class methodsFor: 'constants'!
green
	^ self new color: Color green! !

!TextColor class methodsFor: 'constants'!
magenta
	^ self new color: Color magenta! !

!TextColor class methodsFor: 'constants'!
red
	^ self new color: Color red! !

!TextColor class methodsFor: 'constants' stamp: 'sma 3/24/2000 10:50'!
white 
	^ self new color: Color white! !

!TextColor class methodsFor: 'constants'!
yellow
	^ self new color: Color yellow! !


!Preferences class methodsFor: 'halos' stamp: 'jmv 4/20/2015 16:17'!
installHaloSpecsFromArray: anArray

	| aColor |
	^ self parameters at: #HaloSpecs put: (anArray collect: [ :each |
				aColor _ Color.
				each fourth do: [ :sel | aColor _ aColor perform: sel].
				HaloSpec new 
					horizontalPlacement: each second
					verticalPlacement: each third 
					color: aColor
					iconSymbol: each fifth
					addHandleSelector: each first
					hoverHelp: each sixth])! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/19/2016 20:45:55'!
displayOn: aForm in: aRectangle
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f canvas innerR |
	aForm fill: aRectangle fillColor: Color white.
	font _ AbstractFont default.

	innerR _ aRectangle insetBy: self padding.
	fh _ font height.
	count _ innerR height // fh-1.
	x _ innerR left.
	y _ innerR top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerR.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/19/2016 20:53:35'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas r innerR |
	innerR _ bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font _ AbstractFont default.
		fh _ font height.
		count _ innerR height // fh-1.
		x _ innerR left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerR top.
		canvas _ aForm getCanvas.
		r _ innerR left: lastDisplayPosition.
		canvas newClipRect: r.
		(canvas drawString: string at: x@y font: font color: Color veryDarkGray) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x.
			^r ]].
	^nil! !


!Form methodsFor: 'analyzing' stamp: 'ar 5/17/2001 15:40'!
dominantColor
	| tally max maxi |
	self depth > 16 ifTrue:
		[^(self asFormOfDepth: 16) dominantColor].
	tally _ self tallyPixelValues.
	max _ maxi _ 0.
	tally withIndexDo: [:n :i | n > max ifTrue: [max _ n. maxi _ i]].
	^ Color colorFromPixelValue: maxi - 1 depth: self depth! !

!Form methodsFor: 'bordering' stamp: 'jmv 1/22/2015 10:22'!
border: aRectangle width: borderWidth 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses black for 
	drawing the border."

	self border: aRectangle width: borderWidth fillColor: Color black! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillBlack
	"Set all bits in the receiver to black (ones)."

	self fill: self boundingBox fillColor: Color black! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillBlack: aRectangle 
	"Set all bits in the receiver's area defined by aRectangle to black (ones)."

	self fill: aRectangle rule: Form over fillColor: Color black! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillGray
	"Set all bits in the receiver to gray."

	self fill: self boundingBox fillColor: Color gray! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:20'!
fillGray: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to the gray mask."

	self fill: aRectangle rule: Form over fillColor: Color gray! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
fillWhite
	"Set all bits in the form to white."

	self fill: self boundingBox fillColor: Color white! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
fillWhite: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to white."

	self fill: aRectangle rule: Form over fillColor: Color white! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
reverse
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white.
	Display reverse
	"

	self fill: self boundingBox rule: Form reverse fillColor: Color white! !

!Form methodsFor: 'coloring' stamp: 'jmv 1/22/2015 10:21'!
reverse: aRectangle
	"Change all the bits in the receiver's area that intersects with aRectangle 
	that are white to black, and the ones that are black to white."

	self fill: aRectangle rule: Form reverse fillColor: Color white! !

!Form methodsFor: 'color mapping' stamp: 'ar 5/17/2001 15:42'!
colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	self depth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"
	^ Color colorMapIfNeededFrom: self depth to: destDepth
! !

!Form methodsFor: 'color mapping' stamp: 'jmv 5/12/2016 13:53'!
colormapIfNeededForGray8bpp
	"Return a colormap for displaying the receiver at the given depth.
	Note: Uses 5 bits per color component. 32bit Forms will lose information!!"

	^ Color cachedColormapForGrayFrom: self depth! !

!Form methodsFor: 'color mapping' stamp: 'jmv 9/23/2012 21:42'!
mapColor: oldColor to: newColor
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."

	| map |
	map _ (Color cachedColormapFrom: self depth to: self depth) copy.
	map at: (oldColor indexInMap: map) put: (newColor pixelWordForDepth: self depth).
	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits.
! !

!Form methodsFor: 'color mapping' stamp: 'jmv 1/31/2011 09:21'!
maskingMap
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."
	^Color maskingMap: self depth! !

!Form methodsFor: 'converting' stamp: 'jm 11/12/97 19:28'!
as8BitColorForm
	"Simple conversion of zero pixels to transparent.  Force it to 8 bits."

	| f map |
	f _ ColorForm extent: self extent depth: 8.
	self displayOn: f at: self offset negated.
	map _ Color indexedColors copy.
	map at: 1 put: Color transparent.
	f colors: map.
	f offset: self offset.
	^ f
! !

!Form methodsFor: 'pixel access' stamp: 'ar 5/17/2001 15:42'!
colorAt: aPoint
	"Return the color in the pixel at the given point.  "

	^ Color 
		colorFromPixelValue: (self pixelValueAt: aPoint)
		depth: self depth
! !

!Form methodsFor: 'pixel access' stamp: 'jmv 11/4/2015 12:40'!
colorInterpolatedAt: aPoint
	"Evaluate a bilinear (i.e. cheap) interpolation
	Like OpenCV's CV_INTER_LINEAR
	Answer Color transparent if outside image bounds.
	Copied almost verbatim from FloatImage.
	Answer Colors with float components. Will be rounded to be stored in, for example, a 32-bit Form."

	| x y w interpolated xWeight1 xWeight0 yWeight1 yWeight0 xIndex0 xIndex1 yIndex0 yIndex1 |
	x _ aPoint x.
	y _ aPoint y.
	x < 0.0 ifTrue: [ ^Color transparent ].
	xIndex0 _ x truncated. 		"Could be #floor. But as we only care for values >=0, it is the same. But faster."
	xIndex0 > (width-1) ifTrue: [ ^Color transparent ].
	(xIndex0 = (width-1) and: [ x > (width-1) ]) ifTrue: [ ^Color transparent ].
	xIndex1 _ xIndex0 = (width-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ xIndex0 + 1 ]
		ifTrue: [ xIndex0 ].

	y < 0.0 ifTrue: [ ^Color transparent ].
	yIndex0 _ y truncated.			"Could be #floor. But as we only care for values >=0, it is the same. But faster."
	yIndex0 > (height-1) ifTrue: [ ^Color transparent ].
	(yIndex0 = (height-1) and: [ y > (height-1) ]) ifTrue: [ ^Color transparent ].
	yIndex1 _ yIndex0 = (height-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ yIndex0 + 1 ]
		ifTrue: [ yIndex0 ].

	xWeight1 _ x - xIndex0.
	xWeight0 _ 1.0 - xWeight1.

	yWeight1 _ y - yIndex0.
	yWeight0 _ 1.0 - yWeight1.

	"/* perform interpolation */"
	w _ ((self colorAt: xIndex0 @ yIndex0) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex0) * xWeight1).
	interpolated _ w * yWeight0.

	w _ ((self colorAt: xIndex0 @ yIndex1) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex1) * xWeight1).
	interpolated _ w * yWeight1 + interpolated.

	^interpolated! !

!Form methodsFor: 'transitions' stamp: 'jmv 7/28/2015 08:32'!
pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d |
	pageRect _ otherImage boundingBox.
	oldPage _ self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: Color black.
	oldBottom _ self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps _ 8.
	buffer _ Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d _ pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: 0@0 rule: Form over.
					p _ pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect _ pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad _ Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp _ (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceToScreen.
		].

	buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceToScreen.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"
! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 9/23/2012 21:44'!
dotOfSize: diameter
	"Create a form which contains a round black dot."
	| radius form bb rect centerX centerY centerYBias centerXBias radiusSquared xOverY maxy dx |
	radius _ diameter//2.
	form _ self extent: diameter@diameter offset: (0@0) - (radius@radius).	
	bb _ (BitBlt toForm: form)
		sourceX: 0; sourceY: 0;
		combinationRule: Form over;
		fillColor: Color black.
	rect _ form boundingBox.
	centerX _ rect center x.
	centerY _ rect center y.
	centerYBias _ rect height odd ifTrue: [0] ifFalse: [1].
	centerXBias _ rect width odd ifTrue: [0] ifFalse: [1].
	radiusSquared _ (rect height asFloat / 2.0) squared - 0.01.
	xOverY _ rect width asFloat / rect height asFloat.
	maxy _ rect height - 1 // 2.

	"First do the inner fill, and collect x values"
	0 to: maxy do:
		[:dy |
		dx _ ((radiusSquared - (dy * dy) asFloat) sqrt * xOverY) truncated.
		bb	destX: centerX - centerXBias - dx
			destY: centerY - centerYBias - dy
			width: dx + dx + centerXBias + 1
			height: 1;
			copyBits.
		bb	destY: centerY + dy;
			copyBits].
	^ form
"
Time millisecondsToRun:
	[1 to: 20 do: [:i | (Form dotOfSize: i) displayAt: (i*20)@(i*20)]]
"! !

!Form class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 17:43'!
fakeSphereAt: aPoint diameter: diameter color: aColor
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb _ GrafPort toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect _ aPoint extent: diameter.
	bb fillOval: rect.
	p _ (diameter * 2 // 5) asPoint + aPoint.
	#(2 3 4 5 6 ) do: [ :i |
		"simulate facade by circles of gray"
		bb fillColor: (Color white alpha: 0.3).
		d _ i * diameter // 10.
		rect _ Rectangle
			center: p
			extent: d.
		bb fillOval: rect ].
	DisplayScreen screenUpdateRequired: nil.
"
	Form fakeSphereAt: 50@50 diameter: 30 color:  (Color red alpha: 0.5)
"! !

!Form class methodsFor: 'examples' stamp: 'jmv 3/20/2013 22:36'!
toothpaste: diam	
	"
	Display restoreAfter: [Form toothpaste: 30]
	"
	"Not completely unlike the ST-80 implementation :)
	Original comment:
	Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| point queue color q colors colr colr2 |
	colors _ Color red wheel: 12.
	color _ 8.
	[ true ] whileTrue: [
		queue _ OrderedCollection new: 32.
		16 timesRepeat: [queue addLast: -40@-40].
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		point _ Sensor mousePoint.
		colr _ colors atWrap: (color _ color + 5).  "choose increment relatively prime to colors size"
		colr2 _ colr alpha: 0.3.
		[Sensor isMouseButton1Pressed or: [queue size > 0]] whileTrue: [
			point _ point * 4 + Sensor mousePoint // 5.
			point _ point.
			Form fakeSphereAt: point diameter: diam color: colr.
			(q _ queue removeFirst) ifNil: [^ self].	"exit"
			Form fakeSphereAt: q diameter: diam color: colr2.
			Sensor isMouseButton1Pressed ifTrue: [queue addLast: point]]]! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:37'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (width - 1 - x @ dy) r.
							l _ ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:47'!
bottomLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy l1 l2 |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (width - 1 - x @ dy) r.
							l1 _ ((r - d-1+aaw max: 0.0) min: aaw) / aaw.
							l2 _ ((d - r+bw+aaw max: 0.0) min: aaw) / aaw.
							l _ l1 min: l2.
							].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:38'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	"Create and answer a Form for the bottomRight corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (x @ dy) r.
							l _ ((r - d max: 0.0) min: aaw) / aaw ].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 5/15/2015 09:41'!
bottomRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor borderWidth: bw
	"Create and answer a Form for the bottomLeft corner of a rounded rectangle"
	| f aaw topColor bottomColor l d c width dy l1 l2 |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	width _ r.
	aaw _ 1.3.
	f _ Form
		extent: width @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: width - 1
				do: [ :x |
					dy _ height - r - y.
					dy > 0
						ifTrue: [ l _ 1.0 ]
						ifFalse: [
							d _ (x @ dy) r.
							l1 _ ((r - d-1+aaw max: 0.0) min: aaw) / aaw.
							l2 _ ((d - r+bw+aaw max: 0.0) min: aaw) / aaw.
							l _ l1 min: l2.
							].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 11/30/2010 10:34'!
topLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	"Create and answer a Form with a vertical gray gradient as specified for the topLeft corner of a rounded rectangle"
	| f bw topColor bottomColor l d c w |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	w _ r.
	bw _ 1.3.
	f _ Form
		extent: w @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: w - 1
				do: [ :x |
					l _ 1.0.
					y < r ifTrue: [
						d _ (w - 1 - x @ (w - 1 - y)) r.
						l _ ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !

!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 11/30/2010 10:36'!
topRightCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	"Create and answer a Form with a vertical gray gradient as specified for the topRight corner of a rounded rectangle"
	| f bw topColor bottomColor l d c w |
	topColor _ Color white * gradientTopFactor.
	bottomColor _ Color white * gradientBottomFactor.
	w _ r.
	bw _ 1.3.
	f _ Form
		extent: w @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1)
				with: topColor.
			0
				to: w - 1
				do: [ :x |
					l _ 1.0.
					y < r ifTrue: [
						d _ (x @ (w - y - 1)) r.
						l _ ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !


!ColorForm methodsFor: 'accessing' stamp: 'ar 5/17/2001 15:45'!
colors: colorList
	"Set my color palette to the given collection."

	| colorArray colorCount newColors |
	colorList ifNil: [
		colors _ cachedDepth _ cachedColormap _ nil.
		^ self].

	colorArray _ colorList asArray.
	colorCount _ colorArray size.
	newColors _ Array new: (1 bitShift: self depth).
	1 to: newColors size do: [:i |
		i <= colorCount
			ifTrue: [newColors at: i put: (colorArray at: i)]
			ifFalse: [newColors at: i put: Color transparent]].

	colors _ newColors.
	cachedDepth _ nil.
	cachedColormap _ nil.
! !

!ColorForm methodsFor: 'color manipulation' stamp: 'di 11/11/1998 13:20'!
asGrayScale
	"Return a grayscale ColorForm computed by mapping each color into its grayscale equivalent"
	^ self copy colors:
		(colors collect:
			[:c | c isTransparent ifTrue: [c]
						ifFalse: [Color gray: c luminance]])! !

!ColorForm methodsFor: 'color manipulation' stamp: 'jmv 3/1/2010 09:41'!
colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	| newMap |
	colors ifNil: [
		"use the standard colormap"
		^ Color colorMapIfNeededFrom: self depth to: destDepth].

	(destDepth = cachedDepth and:[cachedColormap isColormap not]) 
		ifTrue: [^ cachedColormap].
	newMap _ Bitmap new: colors size.
	1 to: colors size do: [:i |
		newMap
			at: i
			put: ((colors at: i) pixelValueForDepth: destDepth)].

	cachedDepth _ destDepth.
	^ cachedColormap _ newMap.
! !

!ColorForm methodsFor: 'private' stamp: 'ar 5/17/2001 15:44'!
ensureColorArrayExists
	"Return my color palette."

	colors ifNil: [
		self depth > 8 ifTrue: [^ self error: 'ColorForms only support depths up to 8 bits'].
		self colors: (Color indexedColors copyFrom: 1 to: (1 bitShift: self depth))].
! !


!ColorForm class methodsFor: 'as yet unclassified' stamp: 'jmv 4/10/2015 23:20'!
mappingWhiteToTransparentFrom: aFormOrCursor
	"Return a ColorForm copied from the given Form or Cursor with white mapped to transparent."

	| f map |
	aFormOrCursor depth <= 8 ifFalse: [
		^ self error: 'argument depth must be 8-bits per pixel or less'].
	(aFormOrCursor is: #ColorForm) ifTrue: [
		f _ aFormOrCursor copy.
		map _ aFormOrCursor colors.
	] ifFalse: [
		f _ ColorForm extent: aFormOrCursor extent depth: aFormOrCursor depth.
		f copyBits: aFormOrCursor boundingBox
			from: aFormOrCursor
			at: 0@0
			clippingBox: aFormOrCursor boundingBox
			rule: Form over.
		map _ Color indexedColors copyFrom: 1 to: (1 bitShift: aFormOrCursor depth)].
	map _ map collect: [:c |
		c = Color white ifTrue: [Color transparent] ifFalse: [c]].
	f colors: map.
	^ f
! !

!ColorForm class methodsFor: 'instance creation' stamp: 'jmv 5/4/2016 16:31'!
grayScaleAndTransparentExtent: aPoint
"Native depth"
	"Answer a ColorForm for storing 8bpp gray scale. (do not include any slot for transparent)"

	| grays result |
	result _ self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ]).
	grays _ (0 to: 255) collect: [ :brightness | Color gray: brightness asFloat / 255.0].
	grays at: 1 put: Color transparent.
	result colors: grays.
	^result! !


!Cursor methodsFor: 'converting' stamp: 'jmv 2/25/2011 19:47'!
asCursorForm
	| form |
	form _ Form extent: self extent depth: 8.
	form fillShape: self fillColor: Color black at: offset negated.
	^ form offset: offset! !


!CursorWithMask methodsFor: 'converting' stamp: 'jmv 2/25/2011 19:47'!
asCursorForm
	| form |
	form _ Form extent: self extent depth: 8.
	form fillShape: maskForm fillColor: Color white.
	form fillShape: self fillColor: Color black at: offset negated.
	^ form offset: offset! !


!GrayForm methodsFor: 'pixel accessing' stamp: 'jmv 5/11/2016 19:57'!
grayAt: aPoint
	"Return the color of the pixel at aPoint."

	^Color gray: (self pixelValueAt: aPoint) asFloat / 255.0! !

!GrayForm methodsFor: 'color manipulation' stamp: 'jmv 5/12/2016 15:04'!
colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	^ Color cachedColormapFromGrayTo: destDepth! !


!BMPReadWriter methodsFor: 'reading' stamp: 'ar 10/24/2005 20:57'!
readColorMap
	"Read colorCount BMP color map entries from the given binary stream. Answer an array of Colors."
	| colorCount colors maxLevel b g r ccStream |
	colorCount := (bfOffBits - 54) // 4.
	"Note: some programs (e.g. Photoshop 4.0) apparently do not set colorCount; assume that any data between the end of the header and the start of the pixel data is the color map"
	biBitCount >= 16 ifTrue:[^nil].
	colorCount = 0 ifTrue: [ "this BMP file does not have a color map"
		"default monochrome color map"
		biBitCount = 1 ifTrue: [^ Array with: Color white with: Color black].
		"default gray-scale color map"
		maxLevel := (2 raisedTo: biBitCount) - 1.
		^ (0 to: maxLevel) collect: [:level | Color gray: (level asFloat / maxLevel)]].
	ccStream := ReadStream on: (stream next: colorCount*4).
	colors := Array new: colorCount.
	1 to: colorCount do: [:i |
		b := ccStream next.
		g := ccStream next.
		r := ccStream next.
		ccStream next. "skip reserved"
		colors at: i put: (Color r: r g: g b: b range: 255)].
	^ colors
! !


!BitBlt methodsFor: 'accessing' stamp: 'jmv 3/20/2013 00:50'!
fillColor
	"Return the current fill color as a Color.  
	 Gives the wrong answer if the halftoneForm is a complex pattern of more than one word."

	halftoneForm ifNil: [^ Color black].
	^ Color colorFromPixelValue: halftoneForm first depth: destForm depth! !


!BitBlt class methodsFor: 'examples' stamp: 'jmv 7/27/2015 17:07'!
alphaBlendDemo
	"To run this demo, use...
		Display restoreAfter: [BitBlt alphaBlendDemo]	
	Displays 10 alphas, then lets you paint.  Option-Click to stop painting."

	"This code exhibits alpha blending in any display depth by performing
	the blend in an off-screen buffer with 32-bit pixels, and then copying
	the result back onto the screen with an appropriate color map. - tk 3/10/97"
	
	"This version uses a sliding buffer for painting that keeps pixels in 32 bits
	as long as they are in the buffer, so as not to lose info by converting down
	to display resolution and back up to 32 bits at each operation. - di 3/15/97"

	| brush buff dispToBuff buffToDisplay mapDto32 map32toD prevP p brushToBuff theta buffRect buffSize buffToBuff brushRect delta newBuffRect updateRect |  

	"compute color maps if needed"
	Display depth <= 8 ifTrue: [
		mapDto32 _ Color cachedColormapFrom: Display depth to: 32.
		map32toD _ Color cachedColormapFrom: 32 to: Display depth].

	"display 10 different alphas, across top of screen"
	buff _ Form extent: 500@50 depth: 32.
	dispToBuff _ BitBlt toForm: buff.
	dispToBuff colorMap: mapDto32.
	dispToBuff copyFrom: (50@10 extent: 500@50) in: Display to: 0@0.
	1 to: 10 do: [:i | dispToBuff fill: (50*(i-1)@0 extent: 50@50)
						fillColor: (Color red alpha: i/10)
						rule: Form blend].
	buffToDisplay _ BitBlt toForm: Display.
	buffToDisplay colorMap: map32toD.
	buffToDisplay copyFrom: buff boundingBox in: buff to: 50@10.
	DisplayScreen screenUpdateRequired: nil.

	"Create a brush with radially varying alpha"
	brush _ Form extent: 30@30 depth: 32.
	1 to: 5 do: 
		[:i | brush fillShape: (Form dotOfSize: brush width*(6-i)//5)
				fillColor: (Color red alpha: 0.02 * i - 0.01)
				at: brush extent // 2].

	"Now paint with the brush using alpha blending."
	buffSize _ 100.
	buff _ Form extent: brush extent + buffSize depth: 32.  "Travelling 32-bit buffer"
	dispToBuff _ BitBlt toForm: buff.  "This is from Display to buff"
	dispToBuff colorMap: mapDto32.
	brushToBuff _ BitBlt toForm: buff.  "This is from brush to buff"
	brushToBuff sourceForm: brush; sourceOrigin: 0@0.
	brushToBuff combinationRule: Form blend.
	buffToBuff _ BitBlt toForm: buff.  "This is for slewing the buffer"

	[Sensor isMouseButton2Pressed] whileFalse:
		[prevP _ nil.
		buffRect _ Sensor mousePoint - (buffSize // 2) extent: buff extent.
		dispToBuff copyFrom: buffRect in: Display to: 0@0.
		[Sensor isMouseButton1Pressed] whileTrue:
			["Here is the painting loop"
			p _ Sensor mousePoint - (brush extent // 2).
			(prevP == nil or: [prevP ~= p]) ifTrue:
				[prevP == nil ifTrue: [prevP _ p].
				(p dist: prevP) > buffSize ifTrue:
					["Stroke too long to fit in buffer -- clip to buffer,
						and next time through will do more of it"
					theta _ (p-prevP) theta.
					p _ ((theta cos@theta sin) * buffSize asFloat + prevP) truncated].
				brushRect _ p extent: brush extent.
				(buffRect containsRect: brushRect) ifFalse:
					["Brush is out of buffer region.  Scroll the buffer,
						and fill vacated regions from the display"
					delta _ brushRect amountToTranslateWithin: buffRect.
					buffToBuff copyFrom: buff boundingBox in: buff to: delta.
					newBuffRect _ buffRect translatedBy: delta negated.
					newBuffRect
						areasOutside: buffRect
						do: [ :r | dispToBuff copyFrom: r in: Display to: r origin - newBuffRect origin ].
					buffRect _ newBuffRect].

				"Interpolate from prevP to p..."
				brushToBuff drawFrom: prevP - buffRect origin
									to: p - buffRect origin
									withFirstPoint: false.

				"Update (only) the altered pixels of the destination"
				updateRect _ (p min: prevP) corner: (p max: prevP) + brush extent.
				buffToDisplay copy: updateRect from: updateRect origin - buffRect origin in: buff.
				DisplayScreen screenUpdateRequired: updateRect.
				prevP _ p]]]! !


!WarpBlt methodsFor: 'setup' stamp: 'mtf 8/14/2009 18:21'!
cellSize: s
	"Set the number of samples used for averaging"
	cellSize := s.
	cellSize = 1 ifTrue: [^ self].
	"Install the colorMap to used for mapping the averaged RGBA 32bit pixels to the
	destination depth. Note that we need to install the 32->32 color map explicitly because
	the VM will substitute a colorMap derived from sourceForm->destForm mapping which
	is just plain wrong for <32 source and 32bit dest depth"
	(destForm depth = 32 and: [sourceForm notNil] and: [sourceForm depth < 32])
		ifTrue:[colorMap := ColorMap shifts: #(0 0 0 0) masks:#(16rFF0000 16rFF00 16rFF 16rFF000000) colors: nil]
		ifFalse:[colorMap := Color colorMapIfNeededFrom: 32 to: destForm depth].
! !


!GrafPort methodsFor: 'text' stamp: 'jmv 6/1/2015 13:28'!
displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer last affected pixel position
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	self installStrikeFont: font foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font.
		combinationRule := prevRule ].
	^answer! !

!GrafPort methodsFor: 'private' stamp: 'jmv 5/12/2016 14:52'!
cachedFontColormapFrom1BitTo: destDepth

	| map dstIndex |
	CachedFontColorMaps 
		ifNil: [CachedFontColorMaps _ Array new: 6].

	dstIndex _ destDepth highBit.
	(CachedFontColorMaps at: dstIndex) ifNotNil: [ :m | ^ m ].

	map _ (Color cachedColormapFrom: 1 to: destDepth) copy.
	CachedFontColorMaps at: dstIndex put: map.
	^ map! !

!GrafPort methodsFor: 'private' stamp: 'jmv 5/12/2016 14:55'!
colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix
	"Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same"
	
	| srcIndex dstIndex map mapsForSource mapsForSourceAndDest |
	ColorConvertingMaps 
		ifNil: [ColorConvertingMaps _ (1 to: 6) collect: [:i | Array new: 6]].

	srcIndex _ sourceDepth highBit.
	sourceDepth > 8 ifTrue: [ srcIndex _ keepSubPix ifTrue: [5] ifFalse: [6] ].
	dstIndex _ destDepth highBit.
	
	mapsForSource _ ColorConvertingMaps at: srcIndex.
	(mapsForSourceAndDest _ mapsForSource at: dstIndex) ifNil: [
		mapsForSourceAndDest _ mapsForSource at: dstIndex put: Dictionary new ].
	
	map _ mapsForSourceAndDest at: targetColor ifAbsentPut: [
		Color 
			computeColorConvertingMap: targetColor 
			from: sourceDepth 
			to: destDepth 
			keepSubPixelAA: keepSubPix ].

	^ map! !

!GrafPort methodsFor: 'private' stamp: 'jmv 4/17/2014 16:57'!
setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor

	| targetColor destDepth |
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: Color transparent).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			Preferences subPixelRenderFonts and: [ foregroundColor = Color black or: [ 
				Preferences subPixelRenderColorFonts and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= Color black or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:color:"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor _ foregroundColor = Color black ifFalse: [ foregroundColor ].
				colorMap _ self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display. TTCFont uses it, but it builds a glyphs cache for each color used!!"
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap _ self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]! !


!Rectangle methodsFor: 'transforming' stamp: 'jmv 9/24/2013 22:36'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay _ Delay forMilliseconds: 10.
	buttonStart _ buttonNow _ Sensor isAnyButtonPressed.
	rect _ self.
	Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow _ Sensor isAnyButtonPressed.
		newRect _ newRectBlock value: rect.
		newRect = rect ifFalse:
			[Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
			Display border: newRect width: 2 rule: Form reverse fillColor: Color gray.
			rect _ newRect]].
	Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
	" pay the price for reading the sensor directly ; get this party started "
	aHand _ self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 4/10/2015 23:21'!
makeBoldGlyphs
	"Make a bold set of glyphs with same widths by ORing 1 bit to the right
		(requires at least 1 pixel of intercharacter space)"
	| g bonkForm |
	g _ glyphs copy.
	bonkForm _ (Form extent: 1@16) fillBlack offset: -1@0.
	self bonk: g with: bonkForm.
	glyphs depth = 1 ifTrue: [
		g copyBits: g boundingBox from: g at: (1@0)
			clippingBox: g boundingBox rule: Form under ]
		ifFalse: [
			0 to: g width - 2 do: [ :x | 0 to: g height-1 do: [ :y |
				(glyphs colorAt:  x@y) = Color white ifFalse: [
					g colorAt: x+1@y put: 
						((glyphs colorAt: x+1@y) = Color white 
							ifTrue: [glyphs colorAt:  x@y]
							ifFalse: [Color black])]]]].
	glyphs _ g.
	self isSynthetic: true! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:22'!
makeControlCharsVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character space).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
	self glyphAt: (Character numericValue: 134) put: glyph.
	
	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(0 1 2 3 4 5 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27)
		do: [ :ascii |
			characterToGlyphMap at: ascii + 1 put: 134 ]! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:27'!
makeCrVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character numericValue: 182).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: (Character numericValue: 133) put: glyph.
	characterToGlyphMap at: 14 put: 133! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:31'!
makeLfVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character numericValue: 163).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: (Character numericValue: 132) put: glyph.
	characterToGlyphMap at: 11 put: 132! !


!Morph methodsFor: 'accessing' stamp: 'jmv 8/21/2012 20:40'!
color

	^ Color blue! !

!Morph methodsFor: 'drawing' stamp: 'jmv 4/14/2015 08:53'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self morphLocalBounds
		color: Color blue! !


!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 18:46'!
defaultColor
	^ Color orange! !


!BorderedRectMorph methodsFor: 'initialization' stamp: 'jmv 8/17/2014 21:28'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ Color gray! !


!PasteUpMorph methodsFor: 'initialization' stamp: 'dgd 2/14/2003 22:35'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ Color
		r: 0.861
		g: 1.0
		b: 0.722! !

!PasteUpMorph methodsFor: 'initialization' stamp: 'dgd 2/14/2003 22:29'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Color
		r: 0.8
		g: 1.0
		b: 0.6! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 11/10/2013 19:32'!
findWindow: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu _ MenuMorph new.
	expanded _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ].
	collapsed _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ].
	nakedMorphs _ self submorphsSatisfying: [ :m |
		(m is: #SystemWindow) not ].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) ifTrue: [ ^ Smalltalk beep ].
	(expanded asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activateAndForceLabelToShow.
		w canDiscardEdits ifFalse: [ menu lastItem color: Color red ]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) ifFalse: [ menu addLine ].
	(collapsed asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #expand.
		w canDiscardEdits ifFalse: [ menu lastItem color: Color red ]].
	nakedMorphs isEmpty ifFalse: [ menu addLine ].
	(nakedMorphs asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #comeToFrontAndAddHalo ].
	menu addTitle: 'find window'.
	menu popUpInWorld: self! !


!EllipseMorph methodsFor: 'visual properties' stamp: 'ar 6/25/1999 11:14'!
defaultColor
	"Return the default fill style for the receiver"
	^Color yellow! !


!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 8/6/2014 09:15'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Color lightGray! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:50'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ (0@0 extent: extent) insetBy: 1@3.
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2012 22:37'!
iconColor

	^ self isPressed
		ifTrue: [ Color gray: 0.75 ]
		ifFalse: [
			self mouseIsOver
				ifTrue: [ Color gray: 0.75 ]
				ifFalse: [ Color white ]].! !


!PluggableButtonMorph class methodsFor: 'example' stamp: 'jmv 1/4/2013 13:31'!
example
	"
	PluggableButtonMorph example openInWorld
	"

	| s1 s2 s3 b1 b2 b3 row |
	s1 _ Switch new.
	s2 _ Switch new turnOn.
	s3 _ Switch new.
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 _ (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 _ (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 _ (PluggableButtonMorph model: s3 stateGetter: #isOn action: #turnOn) label: 'S3'.
	b1 color: Color lightRed.
	b2 color: Color lightRed.
	b3 color: Color lightRed.
	row _ LayoutMorph newRow
		addMorphs: (Array with: b1 with: b2 with: b3);
		morphExtent: 120@35.
	^ row
! !


!TextModelMorph methodsFor: 'drawing' stamp: 'cbr 10/10/2012 23:04'!
drawOn: aCanvas 
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"


	| bw bc |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"


	super drawOn: aCanvas.
	bw _ Preferences focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [  "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan]
			ifFalse: [
				self textMorph hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ] 
					ifFalse: [
						self textMorph hasUnacceptedEdits
							ifTrue: [
								bc _ Color red]]]].

	(drawKeyboardFocusIndicator and: [ self textMorph hasKeyboardFocus ]) ifTrue: [
		bc ifNil: [
			bc _ Theme current focusIndicator ]]
	ifFalse: [
		bc ifNotNil: [
			bc _ bc alphaMixed: 0.4 with: Color white ]].
	bc ifNotNil: [
		aCanvas frameRectangle: self focusIndicatorRectangle borderWidth: bw color: bc ]! !


!SystemWindow methodsFor: 'initialization' stamp: 'cbr 11/7/2010 18:58'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Color white! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 6/7/2013 23:23'!
setWindowColor: incomingColor
	| existingColor aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor _ incomingColor asNontranslucentColor.
	aColor = Color black ifTrue: [^ self].
	existingColor _ self widgetsColor.
	existingColor ifNil: [^ Smalltalk beep].
	self widgetsColor: aColor.
	self redrawNeeded! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 8/27/2015 14:39'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow  description summary backColor labelBackground textHeigth |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	textHeigth := AbstractFont default height.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Unsaved?') fixedHeight: textHeigth;
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' Package Name') fixedHeight: textHeigth;
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorph: (StringMorph new contents: ' File Name') fixedHeight: textHeigth;
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: names proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.6.
		
	description := TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.

	summary := TextModelMorph
		textProvider: model
		textGetter: #summary.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: self buildButtonPane proportionalHeight: 0.1;
		addAdjusterAndMorph: summary proportionalHeight: 0.18;
		addAdjusterAndMorph: description proportionalHeight: 0.22;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.2.
	self setLabel: 'Installed Packages'! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'len 5/27/2016 21:51'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete'.
	deleteReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newColumn.
	buttonLayout addMorph: deleteReqButton 
					layoutSpec: (LayoutSpec 
										proportionalWidth: 1.0 
										proportionalHeight: 1.0
										minorDirectionPadding: #top);
					color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.9 
							proportionalHeight: 1.0 
							minorDirectionPadding: #left);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.1 
							proportionalHeight: 1.0 
							minorDirectionPadding: #right);
		color: Color transparent;
		yourself
		! !


!CodeWindow methodsFor: 'updating' stamp: 'jmv 9/2/2013 10:25'!
decorateForInheritance
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton _ self inheritanceButton) ifNil: [^ self].
	buttonColor _ self buttonColor.

	Preferences decorateBrowserButtons
		ifFalse: [ ^aButton color: buttonColor ].
	cm _ model currentCompiledMethod.
	(cm is: #CompiledMethod)
		ifFalse: [ ^aButton color: buttonColor ].

	flags _ 0.
	model isThisAnOverride ifTrue: [ flags _ flags bitOr: 4 ].
	cm sendsToSuper ifTrue: [ flags _ flags bitOr: 2 ].
	model isThereAnOverride ifTrue: [ flags _ flags bitOr: 1 ].
	aColor _ {

		"This is NOTan override. There is no super implementation."
		buttonColor.							"no sends to super. there is not override in any subclass"
		Color tan.							"no sends to super. there is an override in some subclass"
		Color red.							"sends to super. there is no override in any subclass. Error: no super to call (or calls super with a different message)"
		Color red.							"sends to super. there is  an override in some subclass. Error: no super to call (or calls super with a different message)"

		"This is an override. There is some super implementation"
		Color red muchLighter.			"doesn't have sub; has super but doesn't call it"
		Color r: 0.94 g: 0.823 b: 0.673.		"has sub; has super but doesn't call it"
		Color green muchLighter.			"doesn't have sub; has super and callsl it"
		Color blue muchLighter.			"has sub; has super and callsl it"

	} at: flags + 1.
	aButton color: aColor! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'DM 8/22/2015 12:34'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: backColor.
	dirtyFlags _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: changeSetList proportionalWidth: 0.47;
		addAdjusterAndMorph: classList proportionalWidth: 0.4.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: Color transparent) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'HAW 1/12/2017 18:47:35'!
buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons _ OrderedCollection new.
	triads _ OrderedCollection withAll: self preDebugButtonSpec.
	aDebugger shouldBeAbleToCreateMethod ifTrue: [ triads add: { 'Create'. #createMethod. 'create the missing method' }].
	
	triads do: [ :triad |
		aButton _ PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: Color transparent.
	row separation: 1.
	row addMorphs: buttons.
	^row! !


!TestRunnerWindow methodsFor: 'constants' stamp: 'jmv 4/16/2011 13:57'!
runButtonColor
	^ Color green lighter duller! !


!ProgressBarMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:52'!
defaultColor
	^Color white! !

!ProgressBarMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:53'!
initialize
	super initialize.
	progressColor _ Color gray.
	value _ 0.0! !


!MenuMorph methodsFor: 'construction' stamp: 'len 11/16/2015 02:52'!
addStayUpIcons
	| closeBox pinBox w |
	Preferences optionalButtons ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	closeBox _ PluggableButtonMorph model: self action: #delete.
	closeBox icon: Theme current closeIcon; color: Color transparent.
	pinBox _ PluggableButtonMorph model: self action: #stayUp.
	pinBox icon: Theme current pushPinIcon; color: Color transparent.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: 19);
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 6/8/2014 20:32'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		self addMorphBack: feedbackMorph lock position: (0@ -20).
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 4/14/2016 15:10'!
drawOn: aCanvas
	| rectangle w y0 h y1 y2 scrollbarThickness |
	aCanvas frameAndFillRectangle: (0@0 extent: extent) fillColor: self color borderWidth: borderWidth borderColor: borderColor.
	y0 _ 1.
	w _ extent x-2.
	scrollbarThickness _ ScrollBar scrollbarThickness.
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w - scrollbarThickness.
		aCanvas
			frameRectangle: (extent x - scrollbarThickness@0
				extent: scrollbarThickness @ extent y)
			borderWidth: 1
			color: borderColor.
		aCanvas
			image: (BitBltCanvas arrowOfDirection: #up size: scrollbarThickness)
			at: self upButtonPosition.
		aCanvas
			image: (BitBltCanvas arrowOfDirection: #down size: scrollbarThickness)
			at: self downButtonPosition.
		h _ extent y - (2 * scrollbarThickness).
		y1 _ (1.0 * self firstVisible-1 / completer entryCount * h) ceiling + y0 + scrollbarThickness-1.
		y2 _ (1.0 * self lastVisible / completer entryCount * h) floor + y0 + scrollbarThickness -1.
		aCanvas
			fillRectangle: (extent x - scrollbarThickness+2@y1 corner:  extent x-2 @ y2)
			color: Color veryLightGray ].
	self firstVisible
		to: self lastVisible
		do: [ :index |
			rectangle _ 1@y0 extent: w@self class itemHeight.
			index = self selected
				ifTrue: [
					aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].
			aCanvas
				drawString: (completer entries at: index) asString
				at: rectangle topLeft
				font: self class listFont
				color: Theme current text.
			y0 _ y0 + self itemHeight ]! !

!AutoCompleterMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:09'!
defaultBorderColor
	^ Color gray! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:59'!
drawOn: aCanvas 
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	 aCanvas
		stencil: Cursor move
		at: 0@0
		color: Color black! !


!ImageMorph methodsFor: 'accessing' stamp: 'jmv 3/14/2011 09:15'!
color: aColor
        super color: aColor.
        (image depth = 1 and: [aColor is: #Color]) ifTrue: [
                image colors: {Color transparent. aColor}.
                self redrawNeeded]! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 3/13/2009 10:04'!
isEnabled: aBoolean

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled _ aBoolean.
	self color: (aBoolean ifTrue: [Color black] ifFalse: [Color gray]).
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 23:55'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.9) 
			borderWidth: 1 borderColor: Color black.
	^form! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 23:55'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		fillRectangle: (form boundingBox insetBy: 2) color: Color black.
	^form! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 12/1/2015 09:53'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: paneColor) ]
		ifFalse: [ self color: Color transparent ]! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 8/17/2014 21:38'!
defaultColor
	^Color gray! !


!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 4/3/2011 22:59'!
initializedInstance
	"Answer a row by default so the 'new morph' menu doesn't fail..."
	^self newRow color: (Color red alpha: 0.2)! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 10:57'!
example1
"
	self example1
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (BorderedRectMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/12/2015 16:42'!
example10
"
	self example10 openInWorld
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example10.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
^ pane! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 10:55'!
example11
"
	self example11
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example11.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addAdjusterMorph; 
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 11:08'!
example13
	"
	self example13
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example13.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BorderedRectMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 				minorDirectionPadding: #center);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 				minorDirectionPadding: #top);
		addMorph: LayoutAdjustingMorph new layoutSpec: (LayoutSpec fixedWidth: 5); 
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 2/16/2016 13:27'!
example1b
"
Based on #example1, but using some Morph instead of RectangleLikeMorph -> extent is not adjustable
	self example1b
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (Morph new name: #B);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (Morph new name: #G);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (Morph new name: #J);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 11:25'!
example2
	"
	self example2
	"
	| pane row |
	pane _ (LayoutMorph newColumn separation: 5)	name: #example2.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #A)
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #B)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #C)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 200@180 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 11/18/2015 09:45'!
example20
"
	self example20
"
| column |

column _ (LayoutMorph newColumn separation: 5) name: #Column.
column
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 20);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec fixedWidth: 40 proportionalHeight: 0.5);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 30).
column morphPosition: 150@130 extent: 400@300.
column openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'dhn 4/5/2015 11:13'!
example3
	"
	self example3
	"
	| pane row innerRow |
pane _ (LayoutMorph newColumn separation: 5)	name: #example3.
	pane color: Color red.
	row _ (LayoutMorph newRow separation: 5) name: #Row.
	innerRow _ (LayoutMorph newRow separation: 5) name: #InnerRow;
		color: Color red.
	innerRow
		addMorph: (BorderedRectMorph new name: #Box1)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Box2)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (BorderedRectMorph new name: #Bar)
		layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 				minorDirectionPadding: #center);
		addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #Rect1)
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 				minorDirectionPadding: #top);
		addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #Rect2)
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane morphPosition: 250@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 6/8/2014 20:03'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane addMorph: (StringMorph contents: '1').

rect1 := BorderedRectMorph new color: (Color lightOrange); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect1.
rect2 := BorderedRectMorph new color: (Color cyan); 
	morphExtent: 20 @ 30.
pane addMorphFront: rect2.
pane
	color: Color lightGreen;
	morphPosition: 120 @ 50 extent: 180 @ 100;
	openInWorld.

pane _ LayoutMorph newRow separation: 5. "2"
pane addMorph: (StringMorph contents: '2').

rect1 := BorderedRectMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect1.
rect2 := BorderedRectMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane
	color: Color lightGreen;
	morphPosition: 320 @ 50 extent: 180 @ 100;
	openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane addMorph: (StringMorph contents: '3').

rect1 := BorderedRectMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
rect2 := BorderedRectMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect2.
pane
	color: Color lightGreen;
	morphPosition: 520 @ 50 extent: 180 @ 100;
	openInWorld! !

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 1/4/2013 13:42'!
launcherExample
	"
	self launcherExample
	"
	| b1 b2 b3 row b4 random buttons |
	random _ Random new.
	b1 _ PluggableButtonMorph model: [ Date today print ] action: #value label: 'Date'.
	b2 _ PluggableButtonMorph model: [ Time now print ] action: #value label: 'Time'.
	b3 _ PluggableButtonMorph model: [ SystemVersion current print ] action: #value label: 'Version'.
	b4 _ PluggableButtonMorph model: [ random next print ] action: #value label: 'Random'.
	buttons _ {b1. b2. b3. b4}.
	buttons do: [ :button |
		button color: Color lightRed ].
	row _ LayoutMorph newRow
		 color: Color red;
		 addMorphs: buttons;
		 morphExtent: 300 @ 40.
	^ row openInWorld! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:55'!
defaultColor
	^Color veryLightGray! !


!HaloHandleMorph class methodsFor: 'accessing' stamp: 'jmv 11/18/2010 09:44'!
circleForm: extent
	"
	CircleForm _ nil
	"
	| r d l bw center |
	(CircleForm isNil or: [ CircleForm extent ~= extent ]) ifTrue: [
		center _ extent -1 * 0.5.
		r _ center r * 0.75.
		bw _ 1.3.
		CircleForm _ Form extent: extent depth: 32.
		0 to: extent y - 1 do: [ :y |
			0 to: extent x - 1 do: [ :x |
				d _ (x@y - center) r.
				l _ (r - d max: 0.0) min: bw.
				CircleForm
					colorAt: x @ y
					put: (Color white alpha: (l / bw))
					]].
		].
	^CircleForm! !


!HaloMorph methodsFor: 'initialization' stamp: 'dgd 3/7/2003 14:28'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Color
		r: 0.6
		g: 0.8
		b: 1.0! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 5/27/2015 13:42'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint colorToUse form icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: self class handleSize asPoint).
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: 0@0 ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 7/4/2016 22:14'!
addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: ((target is: #SystemWindow) ifTrue: [target windowColor] ifFalse: [Color lightBlue alpha: 0.9]).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color black.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	namePosition _ haloBox width - nameMorph morphWidth // 2 @ (haloBox height).
	self addMorph: nameBackground position: namePosition - 2.
	self addMorph: nameMorph position: namePosition.
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/22/2012 15:18'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| degrees |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
	degrees _ (evt eventPosition - target referencePosition) degrees.
	degrees _ degrees - angleOffset degrees.
	degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false.
	degrees = 0.0
		ifTrue: [rotHandle color: Color lightBlue]
		ifFalse: [rotHandle color: Color blue].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	self removeAllHandlesBut: rotHandle.

	target rotationDegrees: degrees.

	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2012 00:01'!
setDismissColor: evt with: dismissHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	evt hand obtainHalo: self.
	colorToUse _  (dismissHandle morphContainsPoint:  (dismissHandle internalizeFromWorld: evt eventPosition))
		ifFalse: [ Color red muchLighter ]
		ifTrue: [ Color lightGray ].
	dismissHandle color: colorToUse! !


!InnerListMorph methodsFor: 'initialization' stamp: 'jmv 5/27/2013 09:39'!
initialize
	super initialize.
	self color: Color black.
	font _ Preferences standardListFont.
	listItems _ #().
	selectedRow _ nil.
	selectedRows _ Set new.
	highlightedRow _ nil! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 19:55'!
debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"

	self textComposition lines do: [ :line |
		aCanvas
			frameRectangle: line rectangle
			borderWidth: 1
			color: Color brown ]
! !


!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/17/2015 15:51'!
initialize
	super initialize.
	extent _ 400@300.
	color _ Color white.
	grid _ 8@6.
	gridLineWidth _ 2.
	gridColor _ Color black.
	selectionColor _ Color red! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 23:18'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	from _ self toGridPoint: localEventPosition.
	outlineMorph _ BorderedRectMorph new
		borderColor: Color black;
		color: Color transparent;
		openInWorld;
		hide.
	self selectTo: localEventPosition! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 9/25/2011 23:12'!
defaultColor

	^Color r: 1.0 g: 1.0 b: 0.7! !

!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 12/20/2014 15:09'!
drawOn: aCanvas

	| r |
	r _ self morphLocalBounds.
	aCanvas roundRect: r color: self color radius: 4.
	aCanvas
		textComposition: textComposition
		bounds: (r insetBy: 4)
		color: Color black
		selectionColor: (Theme current textHighlightFocused: false)! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 3/2/2017 19:40:12'!
drawInvalidAreasSubmorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |

	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand.
	See en.wikipedia.org/wiki/Painter's_algorithm"
	true not ifTrue: [ ^self simpleDrawInvalidAreasSubmorphs: submorphs ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.
	damageRecorder reset.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.

	"The idea here is to iterate morphs front to back, i.e. first the ones on top.
	For each morph, record the rectangles it affects. And if a morph is opaque, remove the area behind it from the list of rectangles to be repaired.
	This means, for example, that morphs completely covered might even not be redrawn.
	this is a huge performance gain when there are many morphs on screen, especially if there are more than ten open windows, and Theme uses opaque colors.
	See 'A reverse painter's algorithm' at en.wikipedia.org/wiki/Painter's_algorithm"
	[ i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph _ submorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			reuse _ currentRectsToRepair.
			currentRectsToRepair _ newRectsToRepair.
			newRectsToRepair _ reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect _ morphBounds intersect: r.
						thisMorphRects add: morphClipRect. "We could perhaps try and join adjacent rectangles in this collection..."
						morph addPossiblyUncoveredAreasIn: r to: newRectsToRepair ]
					ifFalse: [
						newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects _ OrderedCollection new.
			]].
		i _ i + 1 ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ world fillRects: initialRectsToRepair color: Color gray ].

	"Draw world background on those areas that were not completely covered by opaque morphs"
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			world drawOn: (canvas newClipRect: r) ]].

	"Now, draw the recorded areas of selected morphs, back to front."
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could think about merging all xrects into just one call...
		This would mean drawing each morph just once.
		But that would mean drawing pixels we were not told to. There could be other morphs in that area that are not even being drawn!!
		See WorldState class >> #experiment1"
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas newClipRect: r) fullDraw: m.
		].
"		(canvas newClipRect: rr) fullDraw: m"
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage2 ifTrue: [
			world flashRects: xrects color: Color random ].
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !


!WorldState class methodsFor: 'sample code' stamp: 'jmv 3/2/2017 19:39:21'!
experiment1
	"To play with and learn about morphic Display update logic.
	Remove the morphs when done!!"
	"
	WorldState experiment1
	"
	| dr morph1 morph2 s w |
	morph1 _ RectangleLikeMorph new openInWorld.
	morph2 _ RectangleLikeMorph new openInWorld.
	morph2 color: Color green.
	morph2 morphPosition: 200@800 extent: 50@40.
	morph1 morphPosition: 120@720 extent: 220@100.

	w _ self runningWorld.
	s _ w instVarNamed: 'worldState'.
	dr _ s instVarNamed: 'damageRecorder'.

	dr doFullRepaint.
	dr reset;
	"	recordInvalidRect: (100@700 corner: 400@900);"
		recordInvalidRect: (100@700 corner: 150@900);
		recordInvalidRect: (300@700 corner: 400@900).
	Display fillColor: Color gray.
	"Do not draw the area used by the small rectangle, unless it is also drawn!!"
	s drawInvalidAreasSubmorphs: {morph1. morph2 }! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 12/20/2014 15:35'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBounds.
	w _ r extent > (50@50)
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: Color red
		borderWidth: w
		borderColor: Color yellow.
	self line: r topLeft to: r bottomRight-w width: w color: Color yellow.
	self line: r topRight -(w@0) to: r bottomLeft -(0@w)width: w color: Color yellow! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/1/2015 13:30'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	aColor = Color black ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: Color black.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: fontOrNil
			color: topColor ].
	aColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: Color white.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + (0@2)
			font: fontOrNil
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + (0@1)
		font: fontOrNil
		color: aColor! !


!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:01'!
fillRectangle: aRectangle color: aColor
	"Fill the given rectangle."

	aColor isTransparent ifFalse: [
		self 
			frameAndFillRectangle: aRectangle
			fillColor: aColor
			borderWidth: 0
			borderColor: Color transparent ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 11/29/2015 18:14'!
reverseRectangleBorder: r borderWidth: borderWidth
	"
	Display getCanvas
		reverseRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| rect |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	port
		sourceForm: nil;
		fillColor: Color gray;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 4/9/2015 09:46'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ aColor ifNil: [ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		^self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 32]) ifTrue: [
		port fillColor: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillColor: paintColor.
	port combinationRule: Form blend! !


!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
steButtonForm: extent
	^self cachedForms
		at: { #steButton . extent }
		ifAbsentPut: [
			| form canvas |
			form _ Form extent: extent depth: 32.
			canvas _ form getCanvas.
			canvas
				roundRect: (0@0 extent: extent)
				color: (Color gray: 0.4)
				radius: 4.
			canvas
				roundRect: (1@1 extent: extent-2)
				color: Color white
				radius: 4
				gradientTop: 1.0
				gradientCenter: 0.73
				gradientBottom: 0.94
				gradient1Height: (extent y-8+1 max: extent y//2).
			form]! !

!BitBltCanvas class methodsFor: 'cached forms' stamp: 'jmv 4/17/2015 12:16'!
verticalGrayGradient: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	^self cachedForms
		at: { #vertical. height. gradientTopFactor . gradientBottomFactor }
		ifAbsentPut: [
			Form
				verticalGradient: height
				topColor: (Color gray: gradientTopFactor)
				bottomColor: (Color gray: gradientBottomFactor) ]! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 1/7/2015 08:12'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	| resizeFactor resizedForm f e c bottomMargin topMargin |
	resizeFactor _ 4.
	e _ finalSizeInteger@finalSizeInteger*resizeFactor.
	f _ Form extent: e depth: 32.
	c _ Color gray: 0.5.
	topMargin _ finalSizeInteger * 3//4.
	bottomMargin _ finalSizeInteger * 4//4.
	0 to: e y-1-bottomMargin do: [ :y |
		0 to: e x -1 do: [ :x |
			(e x / 2 - 1 - x) abs * 2 + topMargin < y ifTrue: [
				f colorAt: x@y put: c
			]
		]
	].
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotatedByDegrees: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotatedByDegrees: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotatedByDegrees:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !

!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 10/9/2014 23:02'!
buildArrowWith: insideForm borderForm: borderForm
	| extent translucentForm color insideSpec borderSpec border background alpha|
	"
	Display getCanvas
		image: (BitBltCanvas buildArrowWith: BitBltCanvas downInsideForm borderForm: BitBltCanvas downBorderForm)
		at: 20@20
	Display getCanvas
		image: (BitBltCanvas buildArrowWith:BitBltCanvas upInsideForm borderForm: BitBltCanvas upBorderForm)
		at: 40@20
	"
	extent _ insideForm extent - (1@2).
	translucentForm _ Form extent: insideForm extent depth: 32.
	0 to: extent x-1 do: [ :x |
		0 to: extent y-1 do: [ :y |
			insideSpec _ insideForm colorAt: x@(y+1).
			borderSpec _ borderForm colorAt: x@(y+1).
			insideSpec = (Color r: 0.0 g: 0.0 b: 1.0)
				ifTrue: [ color _ Color transparent ]
				ifFalse: [
					borderSpec = (Color r: 1.0 g: 0.0 b: 0.0)
						ifTrue: [ color _ insideSpec ]
						ifFalse: [
							border _ x < (extent x//2)
								ifTrue: [Color white]
								ifFalse: [Color black].
							background _ borderForm colorAt: extent x@(y+1).
							alpha _ borderSpec red asFloat - background red / (border red - background red).
							color _ border alpha: (alpha min: 1 max: 0) ]].
			translucentForm colorAt: x@y put: color ]].
	^translucentForm! !


!Theme methodsFor: 'colors' stamp: 'jmv 11/19/2010 14:06'!
background
	^ Color r: 0.7 g: 0.72 b: 0.83! !

!Theme methodsFor: 'colors' stamp: 'jmv 11/23/2010 09:42'!
buttonColorFrom: aColor
	^ Display depth <= 8
			ifTrue: [ Color transparent ]
			ifFalse: [ aColor paler ]! !

!Theme methodsFor: 'colors' stamp: 'jmv 3/14/2011 08:50'!
buttonLabel
	^Color gray: 0.18! !

!Theme methodsFor: 'colors' stamp: 'len 6/9/2016 17:38'!
errorColor
	^ Color red lighter! !

!Theme methodsFor: 'colors' stamp: 'len 6/9/2016 17:40'!
failureColor
	^ Color yellow lighter! !

!Theme methodsFor: 'colors' stamp: 'len 11/14/2015 02:34'!
scrollbarButtonColor
	^Color gray: 0.95! !

!Theme methodsFor: 'colors' stamp: 'len 11/17/2015 23:55'!
scrollbarColor
	^Color white! !

!Theme methodsFor: 'colors' stamp: 'len 11/14/2015 02:34'!
scrollbarSliderShadowColor
	^Color white! !

!Theme methodsFor: 'colors' stamp: 'len 6/9/2016 17:38'!
successColor
	^ Color green lighter! !

!Theme methodsFor: 'colors' stamp: 'cbr 11/7/2010 18:00'!
text
	^ Color black! !

!Theme methodsFor: 'colors' stamp: 'jmv 10/16/2013 22:08'!
textCursor
	^ Display depth <= 2
		ifTrue: [ Color black ]
		ifFalse: [ self text ]! !

!Theme methodsFor: 'colors' stamp: 'jmv 5/30/2011 14:31'!
textHighlight
	"A nice light blue."
	"
	^ Color r: 0.71 g: 0.835 b: 1.0
	^ Color hue: 214 chroma: 0.29 luminance: 0.816
	"
	^Color hue: 204 chroma: 0.29 luminance: 0.77! !

!Theme methodsFor: 'colors' stamp: 'jmv 11/23/2010 09:58'!
textHighlightFocused: focused
	"A nice light blue."
	| textHighlight |
	Display depth = 1 ifTrue: [^ Color veryLightGray].
	Display depth = 2 ifTrue: [^ Color gray: 0.87].
	textHighlight _ self textHighlight.
	^focused
		ifTrue: [ textHighlight ]
		ifFalse: [ self unfocusedTextHighlightFrom: textHighlight ]! !

!Theme methodsFor: 'colors' stamp: 'cbr 12/6/2010 20:08'!
windowLabel
	^Color gray: 0.3! !

!Theme methodsFor: 'menu colors' stamp: 'jmv 5/26/2011 09:07'!
menu
	Display depth <= 2 ifTrue: [^ Color white].
	^Color r: 0.75 g: 0.75 b: 0.75 alpha: 0.93! !

!Theme methodsFor: 'menu colors' stamp: 'jmv 11/23/2010 09:45'!
menuHighlight
	^ Display depth < 8
		ifTrue: [ Color veryLightGray ]
		ifFalse: [ self textHighlight ]! !

!Theme methodsFor: 'menu colors' stamp: 'cbr 11/7/2010 18:28'!
menuText
	^ Color black! !

!Theme methodsFor: 'menu colors' stamp: 'jmv 11/28/2010 08:04'!
menuTitleBar
	Display depth = 1 ifTrue: [^ Color white].
	Display depth = 2 ifTrue: [^ Color gray].
	^ self menu darker! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 4/15/2011 14:59'!
browser
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.5 g: 0.7 b: 0.4]! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 2/14/2013 11:05'!
debugger
	^Color h: 0.0 s: 0.6 v: 0.7! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 11/23/2010 09:16'!
defaultWindowColor
	^ Color lightGray! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 12:44'!
fileContentsBrowser
	^Color tan duller! !

!Theme methodsFor: 'tool colors' stamp: 'cbr 12/4/2010 18:44'!
fileList
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.7 g: 0.55 b: 0.7 ]! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 5/23/2012 19:23'!
messageNames

	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.53 g: 0.77 b: 0.382 ]! !

!Theme methodsFor: 'tool colors' stamp: 'cbr 12/4/2010 18:45'!
messageSet
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.45 g: 0.6 b: 0.85 ]! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 11/18/2010 12:44'!
object
	^Color white duller! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 3/28/2012 09:41'!
packageList
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.63 g: 0.47 b: 0.08 ]! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 11/23/2010 09:17'!
testRunner
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ (Color r: 0.650 g: 0.753 b: 0.976) duller ]! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 3/16/2011 08:15'!
textEditor
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color gray: 0.6 ]! !

!Theme methodsFor: 'tool colors' stamp: 'cbr 12/4/2010 18:45'!
transcript
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color r: 0.8 g: 0.6 b: 0.3 ]! !

!Theme methodsFor: 'tool colors' stamp: 'cbr 12/18/2010 17:56'!
versionsBrowser
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ (Color r: 0.869 g: 0.753 b: 1.0) duller ]! !

!Theme methodsFor: 'tool colors' stamp: 'jmv 3/16/2011 08:13'!
workspace
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ Color h: 60.0 s: 0.73 v: 0.72 ]! !

!Theme methodsFor: 'widget colors' stamp: 'jmv 5/23/2012 18:49'!
acceptButton

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ Color r: 0.2 g: 0.6 b: 0.1 ])! !

!Theme methodsFor: 'widget colors' stamp: 'jmv 5/23/2012 18:51'!
cancelButton

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ Color r: 0.8 g: 0.2 b: 0.2 ])! !

!Theme methodsFor: 'widget colors' stamp: 'cbr 10/10/2012 23:36'!
textPane
	^Color white! !


!SHTextStylerST80 class methodsFor: 'style table' stamp: 'pb 5/4/2016 17:44'!
initialTextAttributes
	| d element color emphasis attrArray |
	d _ IdentityDictionary new.
	self styleTable do: [ :each |
		element _ each first.
		color _ each at: 2 ifAbsent: nil.
		color _ color ifNotNil: [ Color colorFrom: color ].
		emphasis _ each at: 3 ifAbsent: nil.
		attrArray _ self attributeArrayForColor: color emphasis: emphasis.
		attrArray notEmpty ifTrue: [
			d at: element put: attrArray ]].
	^ d! !

