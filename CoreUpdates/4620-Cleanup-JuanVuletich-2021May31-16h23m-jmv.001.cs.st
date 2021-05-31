'From Cuis 5.0 [latest update: #4619] on 31 May 2021 at 4:25:47 pm'!

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:31:54'!
hue
	"Return the hue of this color, an angle in the range [0.0..360.0].
	Color orange hue
	"

	| r g b max min span h |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	span _ (max - min) asFloat.
	span = 0.0 ifTrue: [ ^ 0.0 ].

	r = max ifTrue: [
		h _ ((g - b) asFloat / span) * 60.0.
	] ifFalse: [
		g = max
			ifTrue: [ h _ 120.0 + (((b - r) asFloat / span) * 60.0). ]
			ifFalse: [ h _ 240.0 + (((r - g) asFloat / span) * 60.0). ].
	].

	^h mod: 360.0! !

!Color methodsFor: 'access' stamp: 'jmv 6/18/2017 20:34:08'!
saturation
	"Return the saturation of this color, a value between 0.0 and 1.0.
	Color red saturation
	Color gray saturation
	"

	| r g b max min |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	max = 0.0 ifTrue: [ ^0.0 ].
	^max - min / max! !

!Color methodsFor: 'conversions' stamp: 'jmv 5/31/2021 10:13:33'!
closestPixelValue8
	"Return the nearest approximation to this color for an 8-bit deep Form.
	Note: Answer values in the standard Color indexedColors.
	Not useful for ColorForms with custom palettes!!"

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 255].  "white"

	^self saturation < 0.2
		ifTrue: [
			^ self class grayToIndexMap at:(self green * 255) rounded + 1.  "nearest gray"
			]
		ifFalse: [
			"compute nearest entry in the color cube"
			40 + 
				((self red * 5) rounded * 36) +
				((self blue * 5) rounded * 6) +
				(self green * 5) rounded ]! !


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


!TranslucentColor methodsFor: 'queries' stamp: 'jmv 6/18/2017 20:41:54'!
isTransparent
	^ self alpha = 0.0! !


!Float methodsFor: 'converting' stamp: 'nice 4/23/2011 02:24'!
withNegativeSign
	"Same as super, but handle the subtle case of Float negativeZero"
	
	self = 0.0 ifTrue: [^self class negativeZero].  
	^super withNegativeSign! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 5/31/2021 07:43:09'!
ulp
	"Answer the unit of least precision of the receiver.
	Follow John Harrison's definition as described at
	https://en.wikipedia.org/wiki/Unit_in_the_last_place"
	
	self isFinite ifFalse: [^self abs].
	self = 0.0 ifTrue: [^0.0 nextAwayFromZero].
	^ (self - self nextTowardsZero) abs! !

!Float methodsFor: 'printing' stamp: 'jmv 7/11/2018 17:45:59'!
printAsIEEE32BitPrecisionFloatOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:mantissaSignificantBits:
	Print as a 32 bit Float" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base mantissaSignificantBits: 24]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base mantissaSignificantBits: 24]]! !

!Float methodsFor: 'printing' stamp: 'jmv 5/31/2021 07:37:11'!
printOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self negated absPrintOn: aStream base: base]]! !

!Float methodsFor: 'printing' stamp: 'jmv 5/31/2021 07:41:56'!
storeOn: aStream base: base 
	
	"Print the Number exactly so it can be interpreted back unchanged"
	
	self sign = -1 ifTrue: [aStream nextPutAll: '-'].
	base = 10 ifFalse: [aStream print: base; nextPut: $r].
	self = 0.0
		ifTrue: [aStream nextPutAll: '0.0']
		ifFalse: [self abs absPrintExactlyOn: aStream base: base]! !


!Form methodsFor: 'converting' stamp: 'jmv 5/31/2021 07:19:06'!
asGrayForm
	| answer map |

	"For lower bit depths, the 15 bit ColorMap loses no information,
	and answers the real #luminance of each pixel."
	self depth < 32 ifTrue: [
		answer _ GrayForm extent: width@height.
		map _ self colormapIfNeededForGray8bpp.
		(BitBlt toForm: answer)
			colorMap: map;
			copy: self boundingBox
			from: `0@0` in: self
			fillColor: nil rule: Form over.
		answer offset: self offset.
		^ answer ].

	"For 32bpp, the approach above would use just 5bits per component.
	Generally it is best to extract a component in full 8bpp and keep full dynamic range.
	Green usually is a good choice."
	^ self asGrayForm: 3! !

!Form methodsFor: 'converting' stamp: 'jmv 5/31/2021 07:20:06'!
orderedDither32To16
	"Do an ordered dithering for converting from 32 to 16 bit depth."
	| ditherMatrix ii out inBits outBits index pv dmv r di dmi dmo g b pvOut outIndex |
	self depth = 32 ifFalse:[^self error:'Must be 32bit for this'].
	ditherMatrix _ #(	
						0	8	2	10
						12	4	14	6
						3	11	1	9
						15	7	13	5).
	ii _ (0 to: 31) collect:[:i| i].
	out _ Form extent: self extent depth: 16.
	inBits _ self bits.
	outBits _ out bits.
	index _ outIndex _ 0.
	pvOut _ 0.
	0 to: self height-1 do:[:y|
		0 to: self width-1 do:[:x|
			pv _ inBits at: (index _ index + 1).
			dmv _ ditherMatrix at: (y bitAnd: 3) * 4 + (x bitAnd: 3) + 1.
			r _ pv bitAnd: 255.	di _ r * 496 bitShift: -8.
			dmi _ di bitAnd: 15.	dmo _ di bitShift: -4.
			r _ dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			g _ (pv bitShift: -8) bitAnd: 255.	di _ g * 496 bitShift: -8.
			dmi _ di bitAnd: 15.	dmo _ di bitShift: -4.
			g _ dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			b _ (pv bitShift: -16) bitAnd: 255.	di _ b * 496 bitShift: -8.
			dmi _ di bitAnd: 15.	dmo _ di bitShift: -4.
			b _ dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			pvOut _ (pvOut bitShift: 16) + 
						(b bitShift: 10) + (g bitShift: 5) + r.
			(x bitAnd: 1) = 1 ifTrue:[
				outBits at: (outIndex _ outIndex+1) put: pvOut.
				pvOut _ 0].
		].
		(self width bitAnd: 1) = 1 ifTrue:[
			outBits at: (outIndex _ outIndex+1) put: (pvOut bitShift: -16).
			pvOut _ 0].
	].
	^out! !

!Form methodsFor: 'displaying-generic' stamp: 'jmv 5/31/2021 09:14:11'!
displayOn: aForm at: aDisplayPoint
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	| toBeDrawn rule |
	"Rule Form paint treats pixels with a value of zero as transparent"
	toBeDrawn _ self.
	(aForm depth = 32 and: [ self depth = 32 ]) 
		ifTrue: [ rule _ Form blend ] 	"Handle translucent pixels correctly. Requires both source and dest of 32bpp"
		ifFalse: [
			"Warning. Using 'Form paint' with a 32bpp source that includes
			traslucent or transparent alphas will give incorrect results (alpha values will be ignored).
			Doing what follows might be terribly slow. It is best to convert to lower depth on image load."
			"self depth = 32 ifTrue: [
				toBeDrawn _ self asFormOfDepth: aForm depth ]."
			rule _ Form paint ].
	toBeDrawn displayOn: aForm
		at: aDisplayPoint
		clippingBox: aForm boundingBox
		rule: rule! !

