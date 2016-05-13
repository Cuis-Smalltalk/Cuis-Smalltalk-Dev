'From Cuis 4.2 of 25 July 2013 [latest update: #2764] on 12 May 2016 at 4:07:04.916473 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'CachedColormaps ColorNamesDict GrayToIndexMap IndexedColors MaskingMap FromGrayColormaps ToGrayColormaps '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!classDefinition: #GrayForm category: #'Graphics-Display Objects'!
Form subclass: #GrayForm
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!GrayForm commentStamp: 'jmv 5/11/2016 19:40' prior: 0!
GrayForms can only have depth 8 or -8. Each pixel is a byte, and it specifies a gray level.!


!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:49'!
cachedColormapForGrayFrom: sourceDepth
	"Return a cached colormap for mapping from the given depth to the 8bpp grays. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| srcIndex map |
	ToGrayColormaps ifNil: [
		ToGrayColormaps _ Array new: 5].
	
	srcIndex _ sourceDepth highBit min: 5.

	(ToGrayColormaps at: srcIndex) ifNotNil: [ :m | ^ m ].

	map _ self computeColormapForGray8bppFrom: sourceDepth.
	ToGrayColormaps at: srcIndex put: map.
	^ map! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:49'!
cachedColormapFromGrayTo: destDepth
	"Return a cached colormap for mapping from 8bpp grays to the given depth. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	"Note: The color maps for sourceDepth=16 and for sourceDepth=32 are the same"

	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| dstIndex map |
	FromGrayColormaps
		ifNil: [FromGrayColormaps _ Array new: 6 ].

	dstIndex _ destDepth highBit.

	(FromGrayColormaps at: dstIndex) ifNotNil: [ :m | ^ m ].

	map _ self computeColormapFromGray8bppForDepth: destDepth.
	FromGrayColormaps at: dstIndex put: map.
	^ map! !

!Color class methodsFor: 'colormaps' stamp: 'jmv 5/12/2016 15:55'!
computeColormapForGray8bppFrom: sourceDepth
	"Compute a colorMap for translatingfrom the given depth to the 8bpp grays"
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."

	| map |
	sourceDepth < 16 ifTrue: [
		"source is 1-, 2-, 4-, or 8-bit indexed color"
		map _ (self indexedColors copyFrom: 1 to: (1 bitShift: sourceDepth))
					collect: [ :c | (c luminance * 255) rounded].
		map _ map as: Bitmap.
		^ map
		]
	ifFalse: [
		^ self computeRGBColormapForGray8 ]! !

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


!Form methodsFor: 'color mapping' stamp: 'jmv 5/12/2016 13:53'!
colormapIfNeededForGray8bpp
	"Return a colormap for displaying the receiver at the given depth.
	Note: Uses 5 bits per color component. 32bit Forms will lose information!!"

	^ Color cachedColormapForGrayFrom: self depth! !

!Form methodsFor: 'color mapping' stamp: 'jmv 5/12/2016 10:38'!
colormapIfNeededFrom: sourceForm
	"Return a ColorMap mapping from sourceForm to the receiver."
	^sourceForm colormapIfNeededForDepth: self depth! !

!Form methodsFor: 'converting' stamp: 'jmv 5/12/2016 14:21'!
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
			from: 0@0 in: self
			fillColor: nil rule: Form over.
		^ answer ].

	"For 32bpp, the approach below would use just 5bits per component.
	Generally it is best to extract a component in full 8bpp and keep full dynamic range.
	Green usually is a good choice."
	^ self asGrayForm: 3! !

!Form methodsFor: 'converting' stamp: 'jmv 5/12/2016 15:14'!
asGrayForm: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a GrayForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit GrayForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayForm: componentIndex ].
	
	result _ GrayForm extent: width@height.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: (0@0 extent: width@height);
		destOrigin: 0@0;
		copyBits.
	^ result! !


!ColorForm methodsFor: 'color manipulation' stamp: 'jmv 5/12/2016 14:06'!
asGrayForm
	"Build an optimal GrayForm,
	for any color palette in the receiver."
	| answer map |
	answer _ GrayForm extent: width@height.
	map _ self colormapIfNeededForGray8bpp.
	(BitBlt toForm: answer)
		colorMap: map;
		copy: self boundingBox
		from: 0@0 in: self
		fillColor: nil rule: Form over.
	^ answer! !

!ColorForm methodsFor: 'color manipulation' stamp: 'jmv 5/12/2016 13:51'!
colormapIfNeededForGray8bpp
	"Return a colormap for displaying the receiver at the given depth."

	| newMap |

	"cachedDepth used to be anumber but..."
	(#gray8 = cachedDepth and: [cachedColormap isColormap not]) 
		ifTrue: [^ cachedColormap].

	self ensureColorArrayExists.
	newMap _ Bitmap new: colors size.
	1 to: colors size do: [:i |
		newMap
			at: i
			put: ((colors at: i) luminance * 255) rounded].

	cachedDepth _ #gray8.
	^ cachedColormap _ newMap! !


!GrayForm methodsFor: 'displaying' stamp: 'jmv 5/11/2016 20:00'!
maskingMap
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."

	^Bitmap new: (1 bitShift: depth) withAll: 16rFFFFFFFF! !

!GrayForm methodsFor: 'pixel accessing' stamp: 'jmv 5/11/2016 19:57'!
colorAt: aPoint
	"Return the color of the pixel at aPoint."

	^ self grayAt: aPoint! !

!GrayForm methodsFor: 'pixel accessing' stamp: 'jmv 5/11/2016 20:02'!
colorAt: aPoint put: aColor
	"Store the given color into the pixel at aPoint. Convert to gray level."

	self grayAt: aPoint put: aColor! !

!GrayForm methodsFor: 'pixel accessing' stamp: 'jmv 5/11/2016 19:57'!
grayAt: aPoint
	"Return the color of the pixel at aPoint."

	^Color gray: (self pixelValueAt: aPoint) asFloat / 255.0! !

!GrayForm methodsFor: 'pixel accessing' stamp: 'jmv 5/11/2016 20:03'!
grayAt: aPoint put: aGrayColor
	"Store the given color into the pixel at aPoint.
	Convert to gray level."

	self pixelValueAt: aPoint put: (aGrayColor luminance * 255) rounded
! !

!GrayForm methodsFor: 'pixel accessing' stamp: 'jmv 5/11/2016 19:58'!
isTransparentAt: aPoint 
	"Return true if the receiver is transparent at the given point."

	^ false! !

!GrayForm methodsFor: 'color manipulation' stamp: 'jmv 5/12/2016 10:24'!
asGrayForm

	^ self! !

!GrayForm methodsFor: 'color manipulation' stamp: 'jmv 5/12/2016 15:04'!
colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	^ Color cachedColormapFromGrayTo: destDepth! !

!GrayForm methodsFor: 'color manipulation' stamp: 'jmv 5/12/2016 10:42'!
colormapIfNeededForGray8bpp
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed.
	We are already Gray 8 bpp. No colormap needed."

	^nil! !

!GrayForm methodsFor: 'color manipulation' stamp: 'jmv 5/11/2016 19:45'!
mapColor: oldColor to: newColor
	"Replace all occurances of the given color with the given new color in my color map."

	^self error: 'Unsupported for GrayForms'! !

!GrayForm methodsFor: 'copying' stamp: 'jmv 5/11/2016 19:55'!
asCursorForm

	^self error: 'Unsupported for GrayForms'! !

!GrayForm methodsFor: 'copying' stamp: 'jmv 5/12/2016 15:13'!
copy: aRect
 	"Return a new instance containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	^ newForm! !

!GrayForm methodsFor: 'private' stamp: 'jmv 5/12/2016 15:16'!
setExtent: extent depth: bitsPerPixel
	"Create a virtual bit map with the given extent and bitsPerPixel."

	bitsPerPixel abs = 8 ifFalse: [self error: 'GrayForms only support depths of +/-8 bits'].
	super setExtent: extent depth: bitsPerPixel! !

!GrayForm methodsFor: 'testing' stamp: 'jmv 5/11/2016 19:44'!
is: aSymbol
	^ aSymbol == #GrayForm or: [ super is: aSymbol ]! !

!GrayForm methodsFor: 'testing' stamp: 'jmv 5/11/2016 19:44'!
mightBeTranslucent
	"Answer whether this form may be translucent"
	^false! !

!GrayForm methodsFor: 'converting' stamp: 'jmv 5/11/2016 19:39'!
asFormOfDepth: d
	| answer |
	d = depth ifTrue: [ ^self ].
	"Same depth, but opposite endianness."
	d abs = depth abs ifTrue: [
		answer _ self copy.
		answer swapEndianness.
		BitBlt swapBytesIn32BitWords: answer bits.
		^ answer ].
	^ super asFormOfDepth: d! !

!GrayForm methodsFor: 'color mapping' stamp: 'jmv 5/12/2016 10:50'!
colormapIfNeededFrom: sourceForm
	"Return a ColorMap mapping from sourceForm to the receiver."
	^sourceForm colormapIfNeededForGray8bpp! !


!GrayForm class methodsFor: 'instance creation' stamp: 'jmv 5/12/2016 15:06'!
extent: aPoint
	"Native depth"

	^self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ])! !

!GrayForm class methodsFor: 'instance creation' stamp: 'jmv 5/12/2016 15:10'!
extent: aPoint bits: eightBitPixels
	"Native depth.
	eightBitPixels might be a ByteArray or WordArray.
	If a ByteArray, pixels must be ordered as you expect.
	If a WordArray, pixels in a word must match native endianness:
		- For Little Endian, first pixel goes in lower byte in word.
		- For Bit Endian, first pixel goes in higher byte in word."

	^self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ]) bits: eightBitPixels! !


!Bitmap methodsFor: 'accessing' stamp: 'jmv 5/11/2016 19:24'!
byteAt: byteAddress bigEndian: isBigEndian
	"Extract a byte from a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:.  See Form pixelAt:  7/1/96 tk"
	| lowBits shift |
	lowBits _ byteAddress - 1 bitAnd: 3.
	shift _ isBigEndian 
		ifTrue: [ (lowBits - 3) * 8 ]
		ifFalse: [ (0 - lowBits) * 8 ].
	^((self at: byteAddress - 1 - lowBits // 4 + 1)
		bitShift: shift * 8)
		bitAnd: 16rFF! !

!Bitmap methodsFor: 'accessing' stamp: 'jmv 5/11/2016 19:24'!
byteAt: byteAddress put: byte bigEndian: isBigEndian
	"Insert a byte into a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:put:.  See Form pixelAt:put:  7/1/96 tk"
	| longWord shift lowBits longAddr |
	(byte < 0 or:[byte > 255]) ifTrue:[^self errorImproperStore].
	lowBits _ byteAddress - 1 bitAnd: 3.
	longWord _ self at: (longAddr _ (byteAddress - 1 - lowBits) // 4 + 1).
	shift _ isBigEndian 
		ifTrue: [ (3 - lowBits) * 8 ]
		ifFalse: [ (lowBits) * 8 ].
	longWord _ longWord - (longWord bitAnd: (16rFF bitShift: shift)) 
		+ (byte bitShift: shift).
	self at: longAddr put: longWord.
	^ byte! !


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


!Form methodsFor: 'color mapping' stamp: 'jmv 5/12/2016 10:38'!
colormapIfNeededFor: destForm
	"Return a ColorMap mapping from the receiver to destForm.
	double dispatch"

	^destForm colormapIfNeededFrom: self! !

!Form methodsFor: 'converting' stamp: 'jmv 5/12/2016 14:14'!
asGrayScale
	"Redefined in ColorForm as an optimization."

	^ self asGrayForm! !

!Form methodsFor: 'converting' stamp: 'jmv 5/12/2016 13:49'!
asGrayScaleAndTransparent
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)"

	"By default, take the green component"
	^self asGrayScaleAndTransparent: 3! !

!Form methodsFor: 'converting' stamp: 'jmv 5/12/2016 13:49'!
asGrayScaleAndTransparent: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayScaleAndTransparent: componentIndex ].

	result _ ColorForm grayScaleAndTransparentExtent: width@height.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: (0@0 extent: width@height);
		destOrigin: 0@0;
		copyBits.

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map _ Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: 0@0;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	^ result! !


!ColorForm methodsFor: 'color manipulation' stamp: 'jmv 5/12/2016 12:23'!
replaceColorAtIndex: index with: newColor
	"Replace a color map entry with newColor."

	self ensureColorArrayExists.
	colors at: index put: newColor.
	cachedDepth _ nil.
	cachedColormap _ nil.! !

!methodRemoval: ColorForm class #grayScaleExtent:!
ColorForm class removeSelector: #grayScaleExtent:!
!methodRemoval: ColorForm class #grayScaleExtent:bits:!
ColorForm class removeSelector: #grayScaleExtent:bits:!
!methodRemoval: ColorForm class #grayScaleExtent:bitsLinear:!
ColorForm class removeSelector: #grayScaleExtent:bitsLinear:!
!methodRemoval: ColorForm #grayAt:put:!
ColorForm removeSelector: #grayAt:put:!
!methodRemoval: Form #asGrayScale:!
Form removeSelector: #asGrayScale:!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'CachedColormaps ColorNamesDict FromGrayColormaps GrayToIndexMap IndexedColors MaskingMap ToGrayColormaps'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
