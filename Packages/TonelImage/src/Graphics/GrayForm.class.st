"
GrayForms can only have depth 8 or -8. Each pixel is a byte, and it specifies a gray level.
"
Class {
	#name : 'GrayForm',
	#superclass : 'Form',
	#category : 'Graphics-Display Objects',
	#package : 'Graphics',
	#tag : 'Display Objects'
}

{ #category : 'instance creation' }
GrayForm class >> extent: aPoint [
	"Native depth"

	^self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ])
]

{ #category : 'instance creation' }
GrayForm class >> extent: aPoint bits: eightBitPixels [
	"Native depth.
	eightBitPixels might be a ByteArray or WordArray.
	If a ByteArray, pixels must be ordered as you expect.
	If a WordArray, pixels in a word must match native endianness:
		- For Little Endian, first pixel goes in lower byte in word.
		- For Bit Endian, first pixel goes in higher byte in word."

	^self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ]) bits: eightBitPixels
]

{ #category : 'copying' }
GrayForm >> asCursorForm [

	^self error: 'Unsupported for GrayForms'
]

{ #category : 'converting' }
GrayForm >> asFormOfDepth: d [
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
GrayForm >> asGrayForm [

	^ self
]

{ #category : 'color mapping' }
GrayForm >> bitPatternFor: aColor [
	"Return the pixel word for representing the given color on the receiver"
	^aColor bitPatternForGrayForm
]

{ #category : 'pixel accessing' }
GrayForm >> colorAt: aPoint [
	"Return the color of the pixel at aPoint."

	^ self grayAt: aPoint
]

{ #category : 'pixel accessing' }
GrayForm >> colorAt: aPoint put: aColor [
	"Store the given color into the pixel at aPoint. Convert to gray level."

	self grayAt: aPoint put: aColor
]

{ #category : 'color manipulation' }
GrayForm >> colormapIfNeededForDepth: destDepth [
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	^ Color cachedColormapFromGrayTo: destDepth
]

{ #category : 'color manipulation' }
GrayForm >> colormapIfNeededForGray8bpp [
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed.
	We are already Gray 8 bpp. No colormap needed."

	^nil
]

{ #category : 'color mapping' }
GrayForm >> colormapIfNeededFrom: sourceForm [
	"Return a ColorMap mapping from sourceForm to the receiver."
	^sourceForm colormapIfNeededForGray8bpp
]

{ #category : 'copying' }
GrayForm >> copy: aRect [
 	"Return a new instance containing the portion of the receiver delineated by aRect."

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
	^ newForm
]

{ #category : 'pixel accessing' }
GrayForm >> grayAt: aPoint [
	"Return the color of the pixel at aPoint."

	^Color gray: (self pixelValueAt: aPoint) asFloat / 255.0
]

{ #category : 'pixel accessing' }
GrayForm >> grayAt: aPoint put: aGrayColor [
	"Store the given color into the pixel at aPoint.
	Convert to gray level."

	self pixelValueAt: aPoint put: (aGrayColor luminance * 255) rounded

]

{ #category : 'testing' }
GrayForm >> is: aSymbol [
	^ aSymbol == #GrayForm or: [ super is: aSymbol ]
]

{ #category : 'pixel accessing' }
GrayForm >> isTransparentAt: aPoint  [
	"Return true if the receiver is transparent at the given point."

	^ false
]

{ #category : 'color manipulation' }
GrayForm >> mapColor: oldColor to: newColor [
	"Replace all occurances of the given color with the given new color in my color map."

	^self error: 'Unsupported for GrayForms'
]

{ #category : 'displaying' }
GrayForm >> maskingMap [
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."

	^Bitmap new: (1 bitShift: depth) withAll: 16rFFFFFFFF
]

{ #category : 'testing' }
GrayForm >> mightBeTranslucent [
	"Answer whether this form may be translucent"
	^false
]

{ #category : 'analyzing' }
GrayForm >> pixelCompare: aRect with: otherForm at: otherLoc [
	"Use RGB comparison. Divide by 3 because we have one sample per pixel, not 3."
	^ ((self asFormOfDepth: 32) pixelCompare: aRect with: (otherForm asFormOfDepth: 32) at: otherLoc) / 3
]

{ #category : 'private' }
GrayForm >> setExtent: extent depth: bitsPerPixel [
	"Create a virtual bit map with the given extent and bitsPerPixel."

	bitsPerPixel abs = 8 ifFalse: [self error: 'GrayForms only support depths of +/-8 bits'].
	super setExtent: extent depth: bitsPerPixel
]
