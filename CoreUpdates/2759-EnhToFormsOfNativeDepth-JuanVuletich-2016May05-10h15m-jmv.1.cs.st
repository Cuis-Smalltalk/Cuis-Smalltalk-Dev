'From Cuis 4.2 of 25 July 2013 [latest update: #2758] on 5 May 2016 at 10:16:19.855158 am'!

!Bitmap methodsFor: 'accessing' stamp: 'jmv 5/4/2016 17:28'!
byteAt: byteAddress bigEndian: isBigEndian
	"Extract a byte from a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:.  See Form pixelAt:  7/1/96 tk"
	"Assumes big endian format. Not nice at all. Breaks for ColorForms
	created with #grayScaleExtent: on little endian machines..."
"cambiar senders. ojo que no podemos saber..."
	| lowBits shift |
	lowBits _ byteAddress - 1 bitAnd: 3.
	shift _ isBigEndian 
		ifTrue: [ (lowBits - 3) * 8 ]
		ifFalse: [ (0 - lowBits) * 8 ].
	^((self at: byteAddress - 1 - lowBits // 4 + 1)
		bitShift: shift * 8)
		bitAnd: 16rFF! !

!Bitmap methodsFor: 'accessing' stamp: 'jmv 5/4/2016 17:31'!
byteAt: byteAddress put: byte bigEndian: isBigEndian
	"Insert a byte into a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:put:.  See Form pixelAt:put:  7/1/96 tk"
	"Assumes big endian format. Not nice at all. Breaks for ColorForms
	created with #grayScaleExtent: on little endian machines..."
"cambiar senders. ojo que no podemos saber..."
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


!Form methodsFor: 'converting' stamp: 'jmv 5/4/2016 16:27'!
asGrayScaleAndTransparent
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)"

	"By default, take the green component"
	^self asGrayScaleAndTransparent: 2! !

!Form methodsFor: 'converting' stamp: 'jmv 5/4/2016 16:30'!
asGrayScaleAndTransparent: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)
	
	If component = 1, take the red component
	If component = 2, take the green component
	If component = 3, take the blue component
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


!Form class methodsFor: 'instance creation' stamp: 'jmv 5/4/2016 12:56'!
extent: extentPoint nativeDepthFrom: bitsPerPixel
	"Answer an instance of me with blank bitmap of the given dimensions and depth.
	Assume bitsPerPixel is positive. Use native endianness."

	^ self
		extent: extentPoint
		depth: (Smalltalk isLittleEndian ifTrue: [ bitsPerPixel negated ] ifFalse: [ bitsPerPixel ])! !

!Form class methodsFor: 'instance creation' stamp: 'jmv 5/4/2016 12:56'!
extent: extentPoint nativeDepthFrom: bitsPerPixel bits: aBitmap
	"Answer an instance of me with blank bitmap of the given dimensions and depth.
	Assume bitsPerPixel is positive. Use native endianness."

	^ self
		extent: extentPoint
		depth: (Smalltalk isLittleEndian ifTrue: [ bitsPerPixel negated ] ifFalse: [ bitsPerPixel ])
		bits: aBitmap! !


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


!Form methodsFor: 'converting' stamp: 'jmv 5/4/2016 13:07'!
asFormOfDepth: d
	| newForm source |
	d = self depth ifTrue: [ ^self ].
	source _ self depth = 32
		ifTrue: [ self copy convertAlphaToZeroValueTransparency ]
		ifFalse: [ self ].
	newForm _ self class extent: source extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (source colormapIfNeededFor: newForm);
		copy: (source boundingBox)
		from: 0@0 in: source
		fillColor: nil rule: Form over.
	"If we build a 32bpp from one of smaller depth,
	it will have zero in the alpha channel (until BitBlt is fixed!!)"
	d = 32 ifTrue: [
		newForm fixAlpha ].
	^newForm! !

!Form methodsFor: 'converting' stamp: 'jmv 5/4/2016 16:26'!
asGrayScale
"native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)"

	"By default, take the green component"
	^self asGrayScale: 2! !

!Form methodsFor: 'converting' stamp: 'jmv 5/4/2016 16:31'!
asGrayScale: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)
	
	If component = 1, take the red component
	If component = 2, take the green component
	If component = 3, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: width@height depth: 32.
		self displayOn: f32.
		^ f32 asGrayScale: componentIndex ].
	
	result _ ColorForm grayScaleExtent: width@height.
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


!ColorForm methodsFor: 'private' stamp: 'jmv 5/4/2016 12:49'!
setExtent: extent depth: bitsPerPixel
	"Create a virtual bit map with the given extent and bitsPerPixel."

	bitsPerPixel abs > 8 ifTrue: [self error: 'ColorForms only support depths up to 8 bits'].
	super setExtent: extent depth: bitsPerPixel.
! !


!ColorForm class methodsFor: 'instance creation' stamp: 'jmv 5/4/2016 17:03'!
grayScaleExtent: aPoint
"Native depth"
	"Answer a ColorForm for storing 8bpp gray scale. (do not include any slot for transparent)"

	| grays result |
	result _ self extent: aPoint depth: (Smalltalk isLittleEndian ifTrue: [ -8 ] ifFalse: [ 8 ]).
	grays _ (0 to: 255) collect: [ :brightness | Color gray: brightness asFloat / 255.0].
	result colors: grays.
	^result! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 5/4/2016 16:32'!
drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: (0@0 extent: extent) color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScaleAndTransparent ].
			aCanvas image: iconForm at: leftEdge+1 @ (extent y - iconForm height // 2).
			leftEdge _ leftEdge + iconForm width + self iconSeparation].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - 8 @ (extent y - self class subMenuMarker height // 2) ]! !

!methodRemoval: ColorForm #depth:!
ColorForm removeSelector: #depth:!
!methodRemoval: Form #depth:!
Form removeSelector: #depth:!
!methodRemoval: Bitmap #byteAt:!
Bitmap removeSelector: #byteAt:!
!methodRemoval: Bitmap #byteAt:put:!
Bitmap removeSelector: #byteAt:put:!
