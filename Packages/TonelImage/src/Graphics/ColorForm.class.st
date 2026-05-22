"
ColorForm is a normal Form plus a color map of up to 2^depth Colors. Typically, one reserves one entry in the color map for transparent. This allows 1, 3, 15, or 255 non-transparent colors in ColorForms of depths 1, 2, 4, and 8 bits per pixel. ColorForms don't support depths greater than 8 bits because that would require excessively large color maps with little real benefit, since 16-bit and 32-bit depths already support thousands and millions of colors.

ColorForms have several uses:
  1) Precise colors. You can have up to 256 true colors, instead being limited to the 8-bit color palette.
  2) Easy transparency. Just store (Color transparent) at the desired position in the color map.
  3) Cheap color remapping by changing the color map.

A color map is an Array of up to 2^depth Color objects. A Bitmap colorMap is automatically computed and cached for rapid display. Note that if you change the color map, you must resubmit it via the colors: method to flush this cache.

ColorForms can be a bit tricky. Note that:
  a) When you BitBlt from one ColorForm to another, you must remember to copy the color map of the source ColorForm to the destination ColorForm.
  b) A ColorForm's color map is an array of depth-independent Color objects. BitBlt requires a BitMap of actual pixel values, adjusted to the destination depth. These are different things! ColorForms automatically maintain a cache of the BitBlt-style color map corresponding to the colors array for the last depth on which the ColorForm was displayed, so there should be little need for clients to work with BitBlt-style color maps.
  c) The default map for 8 bit depth has black in the first entry, not transparent.  Say (cform colors at: 1 put: Color transparent).

"
Class {
	#name : 'ColorForm',
	#superclass : 'Form',
	#instVars : [
		'colors',
		'cachedDepth',
		'cachedColormap'
	],
	#category : 'Graphics-Display Objects',
	#package : 'Graphics',
	#tag : 'Display Objects'
}

{ #category : 'as yet unclassified' }
ColorForm class >> extent: aPoint [
	"Native depth"

	^self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ])
]

{ #category : 'instance creation' }
ColorForm class >> grayScaleAndTransparentExtent: aPoint [
"Native depth"
	"Answer a ColorForm for storing 8bpp gray scale. (do not include any slot for transparent)"

	| grays result |
	result := self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ]).
	grays := (0 to: 255) collect: [ :brightness | Color gray: brightness asFloat / 255.0].
	grays at: 1 put: `Color transparent`.
	result colors: grays.
	^result
]

{ #category : 'as yet unclassified' }
ColorForm class >> mappingWhiteToTransparentFrom: aFormOrCursor [
	"Return a ColorForm copied from the given Form or Cursor with white mapped to transparent."

	| f map |
	aFormOrCursor depth <= 8 ifFalse: [
		^ self error: 'argument depth must be 8-bits per pixel or less'].
	(aFormOrCursor is: #ColorForm) ifTrue: [
		f := aFormOrCursor copy.
		map := aFormOrCursor colors.
	] ifFalse: [
		f := ColorForm extent: aFormOrCursor extent depth: aFormOrCursor depth.
		f copyBits: aFormOrCursor boundingBox
			from: aFormOrCursor
			at: `0@0`
			clippingBox: aFormOrCursor boundingBox
			rule: Form over.
		map := Color indexedColors copyFrom: 1 to: (1 bitShift: aFormOrCursor depth)].
	map := map collect: [:c |
		c = `Color white` ifTrue: [`Color transparent`] ifFalse: [c]].
	f colors: map.
	^ f
]

{ #category : 'copying' }
ColorForm >> asCursorForm [

	^ (self asFormOfDepth: 32) offset: offset; as: Form
]

{ #category : 'converting' }
ColorForm >> asFormOfDepth: d [
	| answer |
	d = depth ifTrue: [ ^self ].
	"Same depth, but opposite endianness."
	d abs = depth abs ifTrue: [
		answer := self copy.
		answer swapEndianness.
		BitBlt swapBytesIn32BitWords: answer bits.
		^ answer ].
	^ super asFormOfDepth: d
]

{ #category : 'color manipulation' }
ColorForm >> asGrayForm [
	"Build an optimal GrayForm,
	for any color palette in the receiver."
	| answer map |
	answer := GrayForm extent: width@height.
	map := self colormapIfNeededForGray8bpp.
	(BitBlt toForm: answer)
		colorMap: map;
		copy: self boundingBox
		from: `0@0` in: self
		fillColor: nil rule: Form over.
	^ answer
]

{ #category : 'color manipulation' }
ColorForm >> asGrayScale [
	"Return a grayscale ColorForm computed by mapping each color into its grayscale equivalent"
	^ self copy colors:
		(colors collect:
			[:c | c isTransparent ifTrue: [c]
						ifFalse: [Color gray: c luminance]])
]

{ #category : 'copying' }
ColorForm >> blankCopyOf: aRectangle scaledBy: scale [

        | newForm |
        newForm := super blankCopyOf: aRectangle scaledBy: scale.
        colors ifNotNil: [newForm colors: colors copy].
        ^ newForm
]

{ #category : 'private' }
ColorForm >> clearColormapCache [

	cachedDepth := nil.
	cachedColormap := nil.

]

{ #category : 'pixel accessing' }
ColorForm >> colorAt: aPoint [
	"Return the color of the pixel at aPoint."

	^ self colors at: (self pixelValueAt: aPoint) + 1

]

{ #category : 'pixel accessing' }
ColorForm >> colorAt: aPoint put: aColor [
	"Store the given color into the pixel at aPoint. The given color must match one of the colors in the receiver's colormap."

	| i |
	i := self colors indexOf: aColor
		ifAbsent: [^ self error: 'trying to use a color that is not in my colormap'].
	self pixelValueAt: aPoint put: i - 1.

]

{ #category : 'color manipulation' }
ColorForm >> colormapIfNeededForDepth: destDepth [
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	| newMap |
	colors ifNil: [
		"use the standard colormap"
		^ Color colorMapIfNeededFrom: self depth to: destDepth].

	(destDepth = cachedDepth and:[cachedColormap isColormap not]) 
		ifTrue: [^ cachedColormap].
	newMap := Bitmap new: colors size.
	1 to: colors size do: [:i |
		newMap
			at: i
			put: ((colors at: i) pixelValueForDepth: destDepth)].

	cachedDepth := destDepth.
	^ cachedColormap := newMap.

]

{ #category : 'color manipulation' }
ColorForm >> colormapIfNeededForGray8bpp [
	"Return a colormap for displaying the receiver at the given depth."

	| newMap |

	"cachedDepth used to be anumber but..."
	(#gray8 = cachedDepth and: [cachedColormap isColormap not]) 
		ifTrue: [^ cachedColormap].

	self ensureColorArrayExists.
	newMap := Bitmap new: colors size.
	1 to: colors size do: [:i |
		newMap
			at: i
			put: ((colors at: i) luminance * 255) rounded].

	cachedDepth := #gray8.
	^ cachedColormap := newMap
]

{ #category : 'accessing' }
ColorForm >> colors [
	"Return my color palette."

	self ensureColorArrayExists.
	^ colors

]

{ #category : 'accessing' }
ColorForm >> colors: colorList [
	"Set my color palette to the given collection."

	| colorArray colorCount newColors |
	colorList ifNil: [
		colors := cachedDepth := cachedColormap := nil.
		^ self].

	colorArray := colorList asArray.
	colorCount := colorArray size.
	newColors := ColorArray new: (1 bitShift: self depth).
	1 to: newColors size do: [:i |
		i <= colorCount
			ifTrue: [newColors at: i put: (colorArray at: i)]
			ifFalse: [newColors at: i put: `Color transparent` ]].

	colors := newColors.
	cachedDepth := nil.
	cachedColormap := nil.

]

{ #category : 'fileIn/Out' }
ColorForm >> colorsFromArray: anArrayOfTriplesOrQuartets [
	"Set my color palette to the given collection."

	| colorCount newColors |
	anArrayOfTriplesOrQuartets ifNil: [
		colors := cachedDepth := cachedColormap := nil.
		^ self].

	colorCount := anArrayOfTriplesOrQuartets size.
	newColors := ColorArray new: (1 bitShift: self depth).
	1 to: newColors size do: [:i |
		i <= colorCount
			ifTrue: [newColors at: i put: (Color fromRgbOrRgba: (anArrayOfTriplesOrQuartets at: i))]
			ifFalse: [newColors at: i put: `Color transparent` ]].

	colors := newColors.
	cachedDepth := nil.
	cachedColormap := nil.

]

{ #category : 'copying' }
ColorForm >> copy: aRect [
 	"Return a new ColorForm containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm := self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		combinationRule: Form over
		destOrigin: `0@0`
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	colors ifNotNil: [newForm colors: colors copy].
	^ newForm
]

{ #category : 'private' }
ColorForm >> ensureColorArrayExists [
	"Return my color palette."

	colors ifNil: [
		self depth > 8 ifTrue: [^ self error: 'ColorForms only support depths up to 8 bits'].
		self colors: (Color indexedColors copyFrom: 1 to: (1 bitShift: self depth)) ].
]

{ #category : 'scaling, rotation' }
ColorForm >> flippedBy: direction [
	| oldColors newForm |
	oldColors := colors.
	self colors: nil.
	newForm := super flippedBy: direction.
	self colors: oldColors.
	newForm colors: oldColors.
	^newForm
]

{ #category : 'color manipulation' }
ColorForm >> indexOfColor: aColor [
	"Return the index of aColor in my color array"

	self ensureColorArrayExists.
	^ colors indexOf: aColor ifAbsent: [0]
]

{ #category : 'testing' }
ColorForm >> is: aSymbol [
	^ aSymbol == #ColorForm or: [ super is: aSymbol ]
]

{ #category : 'pixel accessing' }
ColorForm >> isTransparentAt: aPoint  [
	"Return true if the receiver is transparent at the given point."

	^ (self colorAt: aPoint) isTransparent

]

{ #category : 'color manipulation' }
ColorForm >> mapColor: oldColor to: newColor [
	"Replace all occurances of the given color with the given new color in my color map."

	self ensureColorArrayExists.
	1 to: colors size do: [:i | 
		(colors at: i) = oldColor ifTrue: [colors at: i put: newColor]].
	self clearColormapCache.

]

{ #category : 'displaying' }
ColorForm >> maskingMap [
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."
	| maskingMap |
	maskingMap := Bitmap new: (1 bitShift: depth) withAll: 16rFFFFFFFF.
	1 to: colors size do:[:i|
		(colors at: i) isTransparent ifTrue:[maskingMap at: i put: 0].
	].
	colors size+1 to: maskingMap size do:[:i| maskingMap at: i put: 0].
	^maskingMap
]

{ #category : 'testing' }
ColorForm >> mightBeTranslucent [
	"Answer whether this form may be translucent"
	^true
]

{ #category : 'copying' }
ColorForm >> postCopy [
	"Just to make visible the fact that we are sharing the colors... Is it ok to do this?"
	super postCopy.
	self colors: colors
]

{ #category : 'fileIn/Out' }
ColorForm >> readAttributesFrom: aBinaryStream [
	super readAttributesFrom: aBinaryStream.
	colors := ColorArray new: (2 raisedTo: depth).
	1 to: colors size do: [:idx | 
		colors basicAt: idx put: (aBinaryStream nextUint32BigEndian: false) ]
]

{ #category : 'color manipulation' }
ColorForm >> replaceColorAtIndex: index with: newColor [
	"Replace a color map entry with newColor."

	self ensureColorArrayExists.
	colors at: index put: newColor.
	cachedDepth := nil.
	cachedColormap := nil.
]

{ #category : 'private' }
ColorForm >> setColors: colorArray cachedColormap: aBitmap depth: anInteger [
	"Semi-private. Set the color array, cached colormap, and cached colormap depth to avoid having to recompute the colormap when switching color palettes in animations."

	colors := colorArray asColorArray.
	cachedDepth := anInteger.
	cachedColormap := aBitmap.

]

{ #category : 'private' }
ColorForm >> setExtent: extent depth: bitsPerPixel [
	"Create a virtual bit map with the given extent and bitsPerPixel."

	bitsPerPixel abs > 8 ifTrue: [self error: 'ColorForms only support depths up to 8 bits'].
	super setExtent: extent depth: bitsPerPixel.

]

{ #category : 'fileIn/Out' }
ColorForm >> storeOn: aStream [
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream
		newLine; tab;
		nextPutAll: 'colorsFromArray: #('.
	self colors do: [:color |
		color storeArrayOn: aStream].
	aStream nextPutAll: ' ))'.
]

{ #category : 'private' }
ColorForm >> unusedColormapEntry [
	"Return the index of an unused color map entry, or zero if there isn't one."

	| tallies |
	tallies := self tallyPixelValues.
	1 to: tallies size do: [:i |
		(tallies at: i) = 0 ifTrue: [^ i]].
	^ 0

]

{ #category : 'fileIn/Out' }
ColorForm >> writeAttributesOn: file [
	| colorArray |
	super writeAttributesOn: file.
	colorArray := self colors asColorArray.
	1 to: (2 raisedTo: depth) do: [:idx |
		file nextUint32Put: (colorArray basicAt: idx) bigEndian: false ]
]
