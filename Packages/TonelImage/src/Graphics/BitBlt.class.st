"
WARNING: BitBlt's shape cannot be modified since WarpBlt relies on the exact layout. Important primitives will break if you fail to heed this warning.

I represent a block transfer (BLT) of pixels from one Form ( the sourceForm) into a rectangle (destX, destY, width, height) of the destinationForm, as modified by a combination rule, a possible halftoneForm and a possible color map.

The source of pixels may be a similar rectangle (at sourceX, sourceY) in the sourceForm, or the halftoneForm, or both.  If both are specified, their pixel values are combined by a logical AND function prior to any further combination rule processing. The halftoneForm may be an actual Form or a simple WordArray of 32 bit values usually intended to represent Color values. In either case the 'top' of the form is effectively aligned with the top of the destinationForm and for each scanline the destination y modulo the size of the halftoneForm gives the index of the word to use. This makes it easy to make horizontal stripes, for example.

In any case, the pixels from the source (AND'd with the halftone, remember) are combined with those of the destination by as specified by the combinationRules below- 
	name	rule		result 

					0	always 0
	and				1	src AND dst
					2	src AND not(dst)
	over				3	src only
	erase				4	not(src) AND dst
					5 	dst only
	reverse				6	src XOR dst
	under				7	src OR dst
					8	not(src) AND not(dst)
					9	not(src) XOR dst
					10	not(dst)
					11	src OR not(dst)
					12	not(src)
					13	not(src) OR dst
					14	not(src) OR not(dst)
					15	always 1
(You can find an interesting explanation of how this comes to be in http://dev-docs.atariforge.org/files/BLiTTER_1-25-1990.pdf - which interestingly fails to mention any connection to Smalltalk and PARC.)

Forms may be of different depths, see the comment in class Form.

In addition to the original 16 combination rules invented for monochrome Forms, this BitBlt supports
					16	fails (to simulate paint bits)
					17	fails (to simulate erase bits)
					18	sourceWord + destinationWord
					19	sourceWord - destinationWord
					20	rgbAdd: sourceWord with: destinationWord.  Sum of color components
					21	rgbSub: sourceWord with: destinationWord.  Difference of color components
					22	OLDrgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
					23	OLDtallyIntoMap: destinationWord.  Tallies pixValues into a colorMap
							these old versions don't do bitwise dest clipping.  Use 32 and 33 now.
	blend				24	alphaBlend: sourceWord with: destinationWord.  32-bit source and dest only. Blend sourceWord 
							with destinationWord, assuming both are 32-bit pixels. The source is assumed to have 255*alpha
							in the high 8 bits of each pixel, while the high 8 bits of the destinationWord will be ignored.
							The blend produced is alpha*source + (1-alpha)*dest, with the computation being performed
							independently on each color component.  The high byte of the result will be 0.
	paint				25	pixPaint: sourceWord with: destinationWord.  Wherever the sourceForm is non-zero, it replaces
							the destination.  Can be used with a 1-bit source color mapped to (0, FFFFFFFF), and a fillColor 
							to fill the dest with that color wherever the source is 1.
	erase1BitShape				26	pixMask: sourceWord with: destinationWord.  Like pixPaint, but fills with 0.
					27	rgbMax: sourceWord with: destinationWord.  Max of each color component.
					28	rgbMin: sourceWord with: destinationWord.  Min of each color component.
					29	rgbMin: sourceWord bitInvert32 with: destinationWord.  Min with (max-source)
	blendAlpha				30	alphaBlendConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits. Blend
							sourceWord with destinationWord using a constant alpha. Alpha is encoded as 0 meaning
							0.0, and 255 meaning 1.0. The blend produced is alpha*source + (1.0-alpha)*dest, with
							the computation being performed independently on each color component. 
	paintAlpha				31	alphaPaintConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits.
					32	rgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
					33	tallyIntoMap: destinationWord.  Tallies pixValues into a colorMap - Those tallied are exactly 
							those in the destination rectangle.  Note that the source should be specified == destination,
							in order for the proper color map checks  be performed at setup.
	blendAlphaScaled				34	alphaBlendScaled: srcWord with: dstWord. Alpha blend of scaled srcWord and destWord. In contrast
							to alphaBlend:with: the color produced is srcColor + (1-srcAlpha) * dstColor
					35	& 36 not used
	rgbMul				37	rgbMul: srcWord with: dstWord. 
					38	pixSwap: srcWord with: dstWord.
					39	pixClear: srcWord with: dstWord. Clear all pixels in destinationWord for which the pixels of 
							sourceWord have the same values. Used to clear areas of some constant color to zero.
					40	fixAlpha: srcWord with: dstWord. For any non-zero pixel value in destinationWord with zero alpha 
							channel take the alpha from sourceWord and fill it in. Intended for fixing alpha channels left at 
							zero during 16->32 bpp conversions.
					41	rgbComponentAlpha: srcWord with: dstWord.

Any transfer specified is further clipped by the specified clipping rectangle (clipX, clipY, clipWidth, clipHeight), and also by the bounds of the source and destination forms.
	To make a small Form repeat and fill a big form, use an InfiniteForm as the source.

Pixels copied from a source to a destination whose pixels have a different depth are converted based on the optional colorMap.  If colorMap is nil, then conversion to more bits is done by filling the new high-order bits with zero, and conversion to fewer bits is done by truncating the lost high-order bits.  

The colorMap, if specified, must be a either word array (ie Bitmap) with 2^n elements, where n is the pixel depth of the source, or a fully specified ColorMap which may contain a lookup table (ie Bitmap) and/or four separate masks and shifts which are applied to the pixels. For every source pixel, BitBlt will first perform masking and shifting and then index the lookup table, and select the corresponding pixelValue and mask it to the destination pixel size before storing.
	When blitting from a 32 or 16 bit deep Form to one 8 bits or less, the default is truncation.  This will produce very strange colors, since truncation of the high bits does not produce the nearest encoded color.  Supply a 512 long colorMap, and red, green, and blue will be shifted down to 3 bits each, and mapped.  The message copybits...stdColors will use the best map to the standard colors for destinations of depths 8, 4, 2 and 1.  Two other sized of colorMaps are allowed, 4096 (4 bits per color) and 32786 (five bits per color).
	Normal blits between 16 and 32 bit forms truncates or pads the colors automatically to provide the best preservation of colors.
	Colors can be remapped at the same depth.  Sometimes a Form is in terms of colors that are not the standard colors for this depth, for example in a GIF file.  Convert the Form to a MaskedForm and send colorMap: the list of colors that the picture is in terms of. (Note also that a Form can be copied to itself, and transformed in the process, if a non-nil colorMap is supplied.)
"
Class {
	#name : 'BitBlt',
	#superclass : 'Object',
	#instVars : [
		'destForm',
		'sourceForm',
		'halftoneForm',
		'combinationRule',
		'destX',
		'destY',
		'width',
		'height',
		'sourceX',
		'sourceY',
		'clipX',
		'clipY',
		'clipWidth',
		'clipHeight',
		'colorMap'
	],
	#classVars : [
		'CachedFontColorMaps',
		'ColorConvertingMaps'
	],
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'examples' }
BitBlt class >> alphaBlendDemo [
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
		mapDto32 := Color cachedColormapFrom: Display depth to: 32.
		map32toD := Color cachedColormapFrom: 32 to: Display depth].

	"display 10 different alphas, across top of screen"
	buff := Form extent: 500@50 depth: 32.
	dispToBuff := BitBlt toForm: buff.
	dispToBuff colorMap: mapDto32.
	dispToBuff copyFrom: (50@10 extent: 500@50) in: Display to: 0@0.
	1 to: 10 do: [:i | dispToBuff fill: (50*(i-1)@0 extent: 50@50)
						fillColor: (Color red alpha: i/10)
						rule: Form blend].
	buffToDisplay := BitBlt toForm: Display.
	buffToDisplay colorMap: map32toD.
	buffToDisplay copyFrom: buff boundingBox in: buff to: 50@10.
	Display forceToScreen.

	"Create a brush with radially varying alpha"
	brush := Form extent: 30@30 depth: 32.
	1 to: 5 do: 
		[:i | brush fillShape: (Form dotOfSize: brush width*(6-i)//5)
				fillColor: (Color red alpha: 0.02 * i - 0.01)
				at: brush extent // 2].

	"Now paint with the brush using alpha blending."
	buffSize := 100.
	buff := Form extent: brush extent + buffSize depth: 32.  "Travelling 32-bit buffer"
	dispToBuff := BitBlt toForm: buff.  "This is from Display to buff"
	dispToBuff colorMap: mapDto32.
	brushToBuff := BitBlt toForm: buff.  "This is from brush to buff"
	brushToBuff sourceForm: brush; sourceOrigin: 0@0.
	brushToBuff combinationRule: Form blend.
	buffToBuff := BitBlt toForm: buff.  "This is for slewing the buffer"

	[Sensor isMouseButton2Pressed] whileFalse:
		[prevP := nil.
		buffRect := Sensor mousePoint - (buffSize // 2) extent: buff extent.
		dispToBuff copyFrom: buffRect in: Display to: 0@0.
		[Sensor isMouseButton1Pressed] whileTrue:
			["Here is the painting loop"
			p := Sensor mousePoint - (brush extent // 2).
			(prevP == nil or: [prevP ~= p]) ifTrue:
				[prevP == nil ifTrue: [prevP := p].
				(p dist: prevP) > buffSize ifTrue:
					["Stroke too long to fit in buffer -- clip to buffer,
						and next time through will do more of it"
					theta := (p-prevP) theta.
					p := ((theta cos@theta sin) * buffSize asFloat + prevP) truncated].
				brushRect := p extent: brush extent.
				(buffRect containsRect: brushRect) ifFalse:
					["Brush is out of buffer region.  Scroll the buffer,
						and fill vacated regions from the display"
					delta := brushRect amountToTranslateWithin: buffRect.
					buffToBuff copyFrom: buff boundingBox in: buff to: delta.
					newBuffRect := buffRect translatedBy: delta negated.
					newBuffRect
						areasOutside: buffRect
						do: [ :r | dispToBuff copyFrom: r in: Display to: r origin - newBuffRect origin ].
					buffRect := newBuffRect].

				"Interpolate from prevP to p..."
				brushToBuff drawFrom: prevP - buffRect origin
									to: p - buffRect origin
									withFirstPoint: false.

				"Update (only) the altered pixels of the destination"
				updateRect := (p min: prevP) corner: (p max: prevP) + brush extent.
				buffToDisplay copy: updateRect from: updateRect origin - buffRect origin in: buff.
				Display forceToScreen: updateRect.
				prevP := p]]]
]

{ #category : 'instance creation' }
BitBlt class >> bitPeekerFromForm: sourceForm [
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord answer |
	pixPerWord := sourceForm pixelsPerWord.
	answer := self destForm: (Form extent: pixPerWord@1 depth: sourceForm depth)
	 	sourceForm: nil 			"To ensure no colormap set"
		combinationRule: Form over
		destOrigin: (pixPerWord - 1)@0
		sourceOrigin: `0@0`
		extent: `1@1`
		clipRect: (`0@0` extent: pixPerWord@1).
	"To ensure no colormap set"
	answer sourceForm: sourceForm.
	^ answer
]

{ #category : 'instance creation' }
BitBlt class >> bitPokerToForm: destForm [
	"Answer an instance to be used for valueAt: aPoint put: pixValue.
	The source for a 1x1 copyBits will be the low order of (bits at: 1)"
	| pixPerWord answer |
	pixPerWord := 32//destForm depth.
	answer := self destForm: destForm
	 	sourceForm: nil 		"To ensure no colormap set"
		combinationRule: Form over
		destOrigin: `0@0`
		sourceOrigin: (pixPerWord-1)@0
		extent: `1@1`
		clipRect: (`0@0` extent: destForm extent).
	"To ensure no colormap set"
	answer sourceForm: (Form extent: pixPerWord@1 depth: destForm depth).
	^ answer
]

{ #category : 'instance creation' }
BitBlt class >> destForm: df sourceForm: sf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect  [
	"Answer an instance of me with values set according to the arguments."

	^ self new
		setDestForm: df
		sourceForm: sf
		fillColor: nil
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: sourceOrigin
		extent: extent
		clipRect: clipRect
]

{ #category : 'instance creation' }
BitBlt class >> destForm: df sourceForm: sf fillColor: fc combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect  [
	"Answer an instance of me with values set according to the arguments."

	^ self new
		setDestForm: df
		sourceForm: sf
		fillColor: fc
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: sourceOrigin
		extent: extent
		clipRect: clipRect
]

{ #category : 'examples' }
BitBlt class >> exampleColorMap	"BitBlt exampleColorMap" [
	"This example shows what one can do with the fixed part of a color map. The color map, as setup below, rotates the bits of a pixel all the way around. Thus you'll get a (sometime strange looking ;-) animation of colors which will end up exactly the way it looked at the beginning. The example is given to make you understand that the masks and shifts can be used for a lot more than simply color converting pixels. In this example, for instance, we use only two of the four independent shifters."
	| cc bb |
	cc := ColorMap masks: {
		1 << (Display depth-1). "mask out high bit of color component"
		1 << (Display depth-1) - 1. "mask all other bits"
		0.
		0}
		shifts: {
			1 - Display depth. "shift right to bottom most position"
			1. "shift all other pixels one bit left"
			0.
			0}.
	bb := BitBlt toForm: Display.
	bb 
		sourceForm: Display;
		combinationRule: 3;
		colorMap: cc.
	1 to: Display depth do: [ :i |
		bb copyBits.
		Display forceToScreen.
	]
]

{ #category : 'cached state access' }
BitBlt class >> releaseClassState [
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."

	CachedFontColorMaps := ColorConvertingMaps := nil
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapBytesIn16BitHalfWords: aNonPointerThing [
	"Perform a bigEndian/littleEndian byte reversal of my 16 bit short words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.
	
	| ba |
	ba := #[1 2 3 4 5 6 7 8] copy.
	BitBlt swapBytesIn16BitHalfWords: ba.
	ba
	"

	self swapBytesIn16BitHalfWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapBytesIn16BitHalfWords: aNonPointerThing from: start to: stop [
	"Perform a bigEndian/littleEndian byte reversal of my 16 bit short words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack := Form new hackBits: aNonPointerThing.
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 1.

	"Exchange bytes 0 and 1"
	blt sourceX: 0; destX: 1; copyBits.
	blt sourceX: 1; destX: 0; copyBits.
	blt sourceX: 0; destX: 1; copyBits.

	"Exchange bytes 2 and 3"
	blt sourceX: 2; destX: 3; copyBits.
	blt sourceX: 3; destX: 2; copyBits.
	blt sourceX: 2; destX: 3; copyBits.
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapBytesIn32BitWords: aNonPointerThing [
	"Perform a bigEndian/littleEndian byte reversal of my 32 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8] copy.
	BitBlt swapBytesIn32BitWords: ba.
	ba
	"

	self swapBytesIn32BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapBytesIn32BitWords: aNonPointerThing from: start to: stop [
	"Perform a bigEndian/littleEndian byte reversal of my 32 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack := Form new hackBits: aNonPointerThing.
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 1.

	"Exchange bytes 0 and 3"
	blt sourceX: 0; destX: 3; copyBits.
	blt sourceX: 3; destX: 0; copyBits.
	blt sourceX: 0; destX: 3; copyBits.

	"Exchange bytes 1 and 2"
	blt sourceX: 1; destX: 2; copyBits.
	blt sourceX: 2; destX: 1; copyBits.
	blt sourceX: 1; destX: 2; copyBits.
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapBytesIn64BitWords: aNonPointerThing [
	"Perform a bigEndian/littleEndian byte reversal of my 64 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] copy.
	BitBlt swapBytesIn64BitWords: ba.
	ba
	"

	self swapBytesIn64BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapBytesIn64BitWords: aNonPointerThing from: start to: stop [
	"Perform a bigEndian/littleEndian byte reversal of my 64 bit words.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack := Form new hackBits64: aNonPointerThing.
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 1.

	"Exchange bytes 0 and 7"
	blt sourceX: 0; destX: 7; copyBits.
	blt sourceX: 7; destX: 0; copyBits.
	blt sourceX: 0; destX: 7; copyBits.

	"Exchange bytes 1 and 6"
	blt sourceX: 1; destX: 6; copyBits.
	blt sourceX: 6; destX: 1; copyBits.
	blt sourceX: 1; destX: 6; copyBits.

	"Exchange bytes 2 and 5"
	blt sourceX: 2; destX: 5; copyBits.
	blt sourceX: 5; destX: 2; copyBits.
	blt sourceX: 2; destX: 5; copyBits.

	"Exchange bytes 3 and 4"
	blt sourceX: 3; destX: 4; copyBits.
	blt sourceX: 4; destX: 3; copyBits.
	blt sourceX: 3; destX: 4; copyBits.
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapHalvesIn32BitWords: aNonPointerThing [
	"Swap 16 bit halves in each 32 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8] copy.
	BitBlt swapHalvesIn32BitWords: ba.
	ba
	
	| sb |
	sb := SoundBuffer new: 12.
	1 to: sb size do: [ :i | sb at: i put: i ].
	BitBlt swapHalvesIn32BitWords: sb.
	sb
	"

	self swapHalvesIn32BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapHalvesIn32BitWords: aNonPointerThing from: start to: stop [
	"Swap 16 bit halves in each 32 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack := Form new hackBits: aNonPointerThing.
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 2.

	"Exchange bytes 0&1 with 2&3"
	blt sourceX: 0; destX: 2; copyBits.
	blt sourceX: 2; destX: 0; copyBits.
	blt sourceX: 0; destX: 2; copyBits
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapHalvesIn64BitWords: aNonPointerThing [
	"Swap 32 bit halves in each 64 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers.

	| ba |
	ba := #[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] copy.
	BitBlt swapHalvesIn64BitWords: ba.
	ba
	"

	self swapHalvesIn64BitWords: aNonPointerThing from: 1 to: aNonPointerThing basicSize
]

{ #category : 'byte and word utilities' }
BitBlt class >> swapHalvesIn64BitWords: aNonPointerThing from: start to: stop [
	"Swap 32 bit halves in each 64 bit word.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack := Form new hackBits64: aNonPointerThing.
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 8.

	"Exchange bytes 0123 with 4567"
	blt sourceX: 0; destX: 4; copyBits.
	blt sourceX: 4; destX: 0; copyBits.
	blt sourceX: 0; destX: 4; copyBits
]

{ #category : 'instance creation' }
BitBlt class >> toForm: aForm [
	^ self new setDestForm: aForm
]

{ #category : 'accessing' }
BitBlt >> clipBy: aRectangle [
	| aPoint right bottom |
	right := clipX + clipWidth.
	bottom := clipY + clipHeight.
	aPoint := aRectangle origin.
	aPoint x > clipX ifTrue:[clipX := aPoint x].
	aPoint y > clipY ifTrue:[clipY := aPoint y].
	aPoint := aRectangle corner.
	aPoint x < right ifTrue:[right := aPoint x].
	aPoint y < bottom ifTrue:[bottom := aPoint y].
	clipWidth := right - clipX.
	clipHeight := bottom - clipY.
	clipWidth < 0 ifTrue:[clipWidth := 0].
	clipHeight < 0 ifTrue:[clipHeight := 0].
]

{ #category : 'accessing' }
BitBlt >> clipByX1: x1 y1: y1 x2: x2 y2: y2 [
	| right bottom |
	right := clipX + clipWidth.
	bottom := clipY + clipHeight.
	x1 > clipX ifTrue:[clipX := x1].
	y1 > clipY ifTrue:[clipY := y1].
	x2 < right ifTrue:[right := x2].
	y2 < bottom ifTrue:[bottom := y2].
	clipWidth := right - clipX.
	clipHeight := bottom - clipY.
	clipWidth < 0 ifTrue:[clipWidth := 0].
	clipHeight < 0 ifTrue:[clipHeight := 0].
]

{ #category : 'private' }
BitBlt >> clipRange [
	"clip and adjust source origin and extent appropriately"
	"first in x"
	| sx sy dx dy bbW bbH |
	"fill in the lazy state if needed"
	destX ifNil:[destX := 0].
	destY ifNil:[destY := 0].
	width ifNil:[width := destForm width].
	height ifNil:[height := destForm height].
	sourceX ifNil:[sourceX := 0].
	sourceY ifNil:[sourceY := 0].
	clipX ifNil:[clipX := 0].
	clipY ifNil:[clipY := 0].
	clipWidth ifNil:[clipWidth := destForm width].
	clipHeight ifNil:[clipHeight := destForm height].

	destX >= clipX
		ifTrue: [sx := sourceX.
				dx := destX.
				bbW := width]
		ifFalse: [sx := sourceX + (clipX - destX).
				bbW := width - (clipX - destX).
				dx := clipX].
	(dx + bbW) > (clipX + clipWidth)
		ifTrue: [bbW := bbW - ((dx + bbW) - (clipX + clipWidth))].
	"then in y"
	destY >= clipY
		ifTrue: [sy := sourceY.
				dy := destY.
				bbH := height]
		ifFalse: [sy := sourceY + clipY - destY.
				bbH := height - (clipY - destY).
				dy := clipY].
	(dy + bbH) > (clipY + clipHeight)
		ifTrue: [bbH := bbH - ((dy + bbH) - (clipY + clipHeight))].
	sourceForm ifNotNil:[
		sx < 0
			ifTrue: [dx := dx - sx.
					bbW := bbW + sx.
					sx := 0].
		sx + bbW > sourceForm width
			ifTrue: [bbW := bbW - (sx + bbW - sourceForm width)].
		sy < 0
			ifTrue: [dy := dy - sy.
					bbH := bbH + sy.
					sy := 0].
		sy + bbH > sourceForm height
			ifTrue: [bbH := bbH - (sy + bbH - sourceForm height)].
	].
	(bbW <= 0 or:[bbH <= 0]) ifTrue:[
		sourceX := sourceY := destX := destY := clipX := clipY := width := height := 0.
		^true].
	(sx = sourceX 
		and:[sy = sourceY 
		and:[dx = destX 
		and:[dy = destY 
		and:[bbW = width 
		and:[bbH = height]]]]]) ifTrue:[^false].
	sourceX := sx.
	sourceY := sy.
	destX := dx.
	destY := dy.
	width := bbW.
	height := bbH.
	^true
]

{ #category : 'accessing' }
BitBlt >> clipRect [
	"Answer the receiver's clipping area rectangle."

	^clipX @ clipY extent: clipWidth @ clipHeight
]

{ #category : 'accessing' }
BitBlt >> clipRect: aRectangle  [
	"Set the receiver's clipping area rectangle to be the argument, aRectangle."

	clipX := aRectangle left truncated.
	clipY := aRectangle top truncated.
	clipWidth := aRectangle right truncated - clipX.
	clipHeight := aRectangle bottom truncated - clipY.
]

{ #category : 'accessing' }
BitBlt >> colorMap [
	^ colorMap
]

{ #category : 'accessing' }
BitBlt >> colorMap: map [
	"See last part of BitBlt comment. 6/18/96 tk"
	colorMap := map.
]

{ #category : 'accessing' }
BitBlt >> combinationRule: anInteger  [
	"Set the receiver's combination rule to be the argument, anInteger, a 
	number in the range 0-15."

	combinationRule := anInteger
]

{ #category : 'copying' }
BitBlt >> copy: destRectangle from: sourcePt in: srcForm [
	| destOrigin |
	sourceForm := srcForm.
	halftoneForm := nil.		"No fillColor. Just srcForm"
	combinationRule := 3.  "store"
	destOrigin := destRectangle origin.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourcePt x.
	sourceY := sourcePt y.
	width := destRectangle width.
	height := destRectangle height.
	self copyBits
]

{ #category : 'copying' }
BitBlt >> copy: destRectangle from: sourcePt in: srcForm fillColor: hf rule: rule [
	"Specify a Color to fill, not a Form. 6/18/96 tk"  
	| destOrigin |
	sourceForm := srcForm.
	self fillColor: hf.	"sets halftoneForm"
	combinationRule := rule.
	destOrigin := destRectangle origin.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourcePt x.
	sourceY := sourcePt y.
	width := destRectangle width.
	height := destRectangle height.
	srcForm
		ifNotNil: [colorMap := srcForm colormapIfNeededFor: destForm].
	^ self copyBits
]

{ #category : 'copying' }
BitBlt >> copyBits [
	"Primitive. Perform the movement of bits from the source form to the 
	destination form. Fail if any variables are not of the right type (Integer, 
	Float, or Form) or if the combination rule is not implemented. 
	In addition to the original 16 combination rules, this BitBlt supports
	16	fail (to simulate paint)
	17	fail (to simulate mask)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord
	21	rgbSub: sourceWord with: destinationWord
	22	rgbDiff: sourceWord with: destinationWord
	23	tallyIntoMap: destinationWord
	24	alphaBlend: sourceWord with: destinationWord
	25	pixPaint: sourceWord with: destinationWord
	26	pixMask: sourceWord with: destinationWord
	27	rgbMax: sourceWord with: destinationWord
	28	rgbMin: sourceWord with: destinationWord
	29	rgbMin: sourceWord bitInvert32 with: destinationWord
"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	"No alpha specified -- re-run with alpha = 1.0"
	(combinationRule >= 30 and: [combinationRule <= 31]) ifTrue: [
		^ self copyBitsTranslucent: 255].

	"Check for unimplmented rules"
	combinationRule = Form oldPaint ifTrue: [^ self oldPaintBits].
	combinationRule = Form oldErase1bitShape ifTrue: [^ self oldErase1bitShapeBits].

	"Check if BitBlt doesn't support full color maps"
	(colorMap notNil and:[colorMap isColormap]) ifTrue:[
		colorMap := colorMap colors.
		^self copyBits].
	"Check if clipping gots us way out of range"
	self clipRange ifTrue:[self roundVariables. ^self copyBitsAgain].

	'Bad BitBlt argument (Maybe a Float or Fraction?); will retry rounding.' print.
	"Convert all numeric parameters to integers and try again."
	self roundVariables.
	^ self copyBitsAgain
]

{ #category : 'private' }
BitBlt >> copyBitsAgain [
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	'#copyBitsAgain failed. Requested BitBlt operation not performed. Call stack follows:' print.
	thisContext printStack: 15.
]

{ #category : 'copying' }
BitBlt >> copyBitsTranslucent: factor [
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"
]

{ #category : 'copying' }
BitBlt >> copyForm: srcForm to: destPt rule: rule [
	^ self copyForm: srcForm to: destPt rule: rule
		colorMap: (srcForm colormapIfNeededFor: destForm)
]

{ #category : 'copying' }
BitBlt >> copyForm: srcForm to: destPt rule: rule colorMap: map [
	sourceForm := srcForm.
	halftoneForm := nil.
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	colorMap := map.
	self copyBits
]

{ #category : 'copying' }
BitBlt >> copyForm: srcForm to: destPt rule: rule fillColor: color [
	sourceForm := srcForm.
	self fillColor: color.	"sets halftoneForm"
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	self copyBits
]

{ #category : 'copying' }
BitBlt >> copyFrom: sourceRectangle in: srcForm to: destPt [
	| sourceOrigin |
	sourceForm := srcForm.
	halftoneForm := nil.		"No fillColor. Just srcForm"
	combinationRule := 3.  "store"
	destX := destPt x.
	destY := destPt y.
	sourceOrigin := sourceRectangle origin.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := sourceRectangle width.
	height := sourceRectangle height.
	colorMap := srcForm colormapIfNeededFor: destForm.
	self copyBits
]

{ #category : 'accessing' }
BitBlt >> destForm [
	^ destForm
]

{ #category : 'accessing' }
BitBlt >> destOrigin: aPoint  [
	"Set the destination coordinates to be those of aPoint."
	destX := aPoint x.
	destY := aPoint y
]

{ #category : 'accessing' }
BitBlt >> destRect: aRectangle  [
	"Set the destination coordinates to be those of aRectangle top left and
	the width and height of the receiver to be the width and height of aRectangle."
	destX := aRectangle left.
	destY := aRectangle top.
	width := aRectangle width.
	height := aRectangle height
]

{ #category : 'accessing' }
BitBlt >> destX: anInteger  [
	"Set the top left x coordinate of the receiver's destination form to be the 
	argument, anInteger."

	destX := anInteger
]

{ #category : 'accessing' }
BitBlt >> destX: x destY: y width: w height: h [
	destX := x.
	destY := y.
	width := w.
	height := h.
]

{ #category : 'accessing' }
BitBlt >> destY: anInteger  [
	"Set the top left y coordinate of the receiver's destination form to be the 
	argument, anInteger."

	destY := anInteger
]

{ #category : 'line drawing' }
BitBlt >> drawFrom: startPoint to: stopPoint  [
	
	 ^ self drawFrom: startPoint to: stopPoint withFirstPoint: true
]

{ #category : 'line drawing' }
BitBlt >> drawFrom: startPoint to: stopPoint withFirstPoint: drawFirstPoint [
	"Draw a line whose end points are startPoint and stopPoint.
	The line is formed by repeatedly calling copyBits at every
	point along the line.  If drawFirstPoint is false, then omit
	the first point so as not to overstrike at line junctions."
	| offset point1 point2 forwards |
	"Always draw down, or at least left-to-right"
	forwards := (startPoint y = stopPoint y and: [startPoint x < stopPoint x])
				or: [startPoint y < stopPoint y].
	forwards
		ifTrue: [point1 := startPoint. point2 := stopPoint]
		ifFalse: [point1 := stopPoint. point2 := startPoint].
	sourceForm
		ifNil: [
			destX := point1 x.
			destY := point1 y]
		ifNotNil: [
			width := sourceForm width.
			height := sourceForm height.
			offset := sourceForm offset.
			destX := (point1 x + offset x) roundedHAFZ.
			destY := (point1 y + offset y) roundedHAFZ].

	"Note that if not forwards, then the first point is the last and vice versa.
	We agree to always paint stopPoint, and to optionally paint startPoint."
	(drawFirstPoint or: [forwards == false  "ie this is stopPoint"])
		ifTrue: [self copyBits].
	self drawLoopX: (point2 x - point1 x) roundedHAFZ 
				  Y: (point2 y - point1 y) roundedHAFZ.
	(drawFirstPoint or: [forwards  "ie this is stopPoint"])
		ifTrue: [self copyBits].

]

{ #category : 'line drawing' }
BitBlt >> drawLoopX: xDelta Y: yDelta  [
	"Primitive. Implements the Bresenham plotting algorithm (IBM Systems
	Journal, Vol. 4 No. 1, 1965). It chooses a principal direction, and
	maintains a potential, P. When P's sign changes, it is time to move in
	the minor direction as well. This particular version does not write the
	first and last points, so that these can be called for as needed in client code.
	Optional. See Object documentation whatIsAPrimitive."
	| dx dy px py P |
	<primitive: 'primitiveDrawLoop' module: 'BitBltPlugin'>
	dx := xDelta sign.
	dy := yDelta sign.
	px := yDelta abs.
	py := xDelta abs.
	"self copyBits."
	py > px
		ifTrue: 
			["more horizontal"
			P := py // 2.
			1 to: py do: 
				[:i |
				destX := destX + dx.
				(P := P - px) < 0 ifTrue: 
						[destY := destY + dy.
						P := P + py].
				i < py ifTrue: [self copyBits]]]
		ifFalse: 
			["more vertical"
			P := px // 2.
			1 to: px do:
				[:i |
				destY := destY + dy.
				(P := P - py) < 0 ifTrue: 
						[destX := destX + dx.
						P := P + px].
				i < px ifTrue: [self copyBits]]]
]

{ #category : 'copying' }
BitBlt >> fill: destRect fillColor: grayForm rule: rule [
	"Fill with a Color, not a Form. 6/18/96 tk"
	sourceForm := nil.
	self fillColor: grayForm.		"sets halftoneForm"
	combinationRule := rule.
	destX := destRect left.
	destY := destRect top.
	sourceX := 0.
	sourceY := 0.
	width := destRect width.
	height := destRect height.
	self copyBits
]

{ #category : 'accessing' }
BitBlt >> fillBitmap: aBitmap [
	"Sets the receivers half tone form. See class commment."
	
	halftoneForm := aBitmap
]

{ #category : 'accessing' }
BitBlt >> fillColor [
	"Return the current fill color as a Color.  
	 Gives the wrong answer if the halftoneForm is a complex pattern of more than one word."

	halftoneForm ifNil: [^ `Color black` ].
	^ Color colorFromPixelValue: halftoneForm first depth: destForm depth
]

{ #category : 'accessing' }
BitBlt >> fillColor: aColor  [
	"The destForm will be filled with this color or pattern of colors.  May be an old Color, a new type Color, a Bitmap (see BitBlt comment), a Pattern, or a Form.  6/18/96 tk"

	aColor ifNil: [halftoneForm := nil. ^ self].
	destForm ifNil: [self error: 'Must set destForm first'].
	halftoneForm := destForm bitPatternFor: aColor 
]

{ #category : 'private' }
BitBlt >> getPluginName [
	"Private. Return the name of the plugin representing BitBlt.
	Used for dynamically switching between different BB representations only."
	^'BitBltPlugin'
]

{ #category : 'accessing' }
BitBlt >> height: anInteger  [
	"Set the receiver's destination form height to be the argument, anInteger."

	height := anInteger
]

{ #category : 'private' }
BitBlt >> oldErase1bitShapeBits [
	"Perform the erase operation, which puts 0's in the destination
	wherever the source (which is assumed to be just 1 bit deep)
	has a 1.  This requires the colorMap to be set in order to AND
	all 1's into the destFrom pixels regardless of their size."
	| oldFillBitmap oldMap |
	oldFillBitmap := halftoneForm.
	halftoneForm := nil.
	oldMap := colorMap.
	colorMap := Bitmap with: 0 with: 16rFFFFFFFF.
	combinationRule := Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"
	halftoneForm := oldFillBitmap.
	colorMap := oldMap
]

{ #category : 'private' }
BitBlt >> oldPaintBits [
	"Perform the old paint operation, which requires two calls to BitBlt."
	| requestedFill oldMap requestedRule |
	sourceForm depth = 1 ifFalse: [
		^ self error: 'paint operation is only defined for 1-bit deep sourceForms'].

	requestedRule := combinationRule.
	requestedFill := halftoneForm.
	oldMap := colorMap.

	halftoneForm := nil.
	colorMap := Bitmap with: 0 with: 16rFFFFFFFF.	"Map 1's to ALL ones, not just one"
	combinationRule := Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"

	halftoneForm := requestedFill.
	combinationRule := Form under.
	self copyBits.	"then OR, with whatever color, into the hole"
	colorMap := oldMap.
	combinationRule := requestedRule

" | dot |
dot := Form dotOfSize: 32.
((BitBlt destForm: Display
		sourceForm: dot
		fillColor: Color lightGray
		combinationRule: Form paint
		destOrigin: Sensor mousePoint
		sourceOrigin: 0@0
		extent: dot extent
		clipRect: Display boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF)) copyBits"
]

{ #category : 'copying' }
BitBlt >> pixelAt: aPoint [
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPeekerFromForm:.  Returns the pixel at aPoint."
	sourceX := aPoint x.
	sourceY := aPoint y.
	destForm bits at: 1 put: 0.  "Just to be sure"
	self copyBits.
	^ destForm bits at: 1
]

{ #category : 'copying' }
BitBlt >> pixelAt: aPoint put: pixelValue [
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPokerToForm:.  Overwrites the pixel at aPoint."
	destX := aPoint x.
	destY := aPoint y.
	sourceForm bits at: 1 put: pixelValue.
	self copyBits
"
| bb |
bb := (BitBlt bitPokerToForm: Display).
[Sensor isAnyButtonPressed] whileFalse:
	[bb pixelAt: Sensor mousePoint put: 55. Display forceToScreen]
"
]

{ #category : 'private' }
BitBlt >> roundVariables [

	| maxVal minVal |
	'-----------' print.
	'BitBlt >> copyBits failed. Will retry with parameters rounded. Requested parameters are:' print.
	{'dest, source, halftone, rule:' . destForm . sourceForm . halftoneForm . combinationRule } print.
	{'dest, extent, source, clipOrigin, clipExtent'. destX@destY. width@height. sourceX@sourceY. clipX@clipY. clipWidth@clipHeight } print.
	{'colorMap'. colorMap } print.
	
	maxVal := SmallInteger maxVal.
	minVal := SmallInteger minVal.
	"The BitBltPlugin was written assuming 31 bit SmallInteger. It protects itself against non-SmallInteger
	arguments, but not against too large SmallIntegers for its int32 arguments.
	These two lines are not a real protection against that."
	minVal := ((1 bitShift: 29) negated bitShift: 1).
	maxVal :=  -1 - minVal.
	
	destX := destX asInteger min: maxVal max: minVal.
	destY := destY asInteger min: maxVal max: minVal.
	width := width asInteger min: maxVal max: minVal.
	height := height asInteger min: maxVal max: minVal.
	sourceX := sourceX asInteger min: maxVal max: minVal.
	sourceY := sourceY asInteger min: maxVal max: minVal.
	clipX := clipX asInteger min: maxVal max: minVal.
	clipY := clipY asInteger min: maxVal max: minVal.
	clipWidth := clipWidth asInteger min: maxVal max: minVal.
	clipHeight := clipHeight asInteger min: maxVal max: minVal.

]

{ #category : 'private' }
BitBlt >> setDestForm: df [

	destForm := df.
	clipX := 0.
	clipY := 0.
	clipWidth := df width.
	clipHeight := df height
]

{ #category : 'private' }
BitBlt >> setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin [
	"Set up a BitBlt for copying a Rectangle as large as possible, with sourceOrigin and destOrigin. Bound by sourceForm and destForm extents. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf := FloatImage lena asForm.
	sourceOrigin := 30@30.
	df := Form extent: 156@156 depth: 32.
	df fillColor: Color red.
	destOrigin := 10@20.
	blitter := BitBlt new setSourceForm: sf sourceOrigin: sourceOrigin destForm: df destOrigin: destOrigin.
	blitter copyBits.
	df display
	"
	self
		setDestForm: df sourceForm: sf
		destOrigin: destOrigin sourceOrigin: sourceOrigin
		extent: (sf width - sourceOrigin x) @ (sf height - sourceOrigin y)
]

{ #category : 'private' }
BitBlt >> setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent [
	"Set up a BitBlt for copying a Rectangle of extent w@h , with sourceOrigin and destOrigin. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf := FloatImage lena asForm.
	sourceOrigin := 30@30.
	sourceOrigin := 0@0.
	df := Form extent: 256@256 depth: 32.
	df fillColor: Color red.
	destOrigin := 0@0.
	blitter := BitBlt new
		setSourceForm: sf sourceOrigin: sourceOrigin
		destForm: df destOrigin: destOrigin
		width: 256 height: 256.
	blitter copyBits.
	df display
	"
	self setDestForm: df.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceForm := sf.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := extent x.
	height := extent y.
	halftoneForm := nil.
	combinationRule := 3. 	"Form over"
	colorMap := nil
]

{ #category : 'private' }
BitBlt >> setDestForm: df sourceForm: sf fillColor: fc combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect [

	| aPoint |
	destForm := df.
	sourceForm := sf.
	self fillColor: fc.
	combinationRule := cr.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := extent x.
	height := extent y.
	aPoint := clipRect origin.
	clipX := aPoint x.
	clipY := aPoint y.
	aPoint := clipRect corner.
	clipWidth := aPoint x - clipX.
	clipHeight := aPoint y - clipY.
	sourceForm ifNotNil: [
		colorMap := sourceForm colormapIfNeededFor: destForm]
]

{ #category : 'accessing' }
BitBlt >> sourceForm [

	^ sourceForm
]

{ #category : 'accessing' }
BitBlt >> sourceForm: aForm  [
	"Set the receiver's source form to be the argument, aForm."

	sourceForm := aForm
]

{ #category : 'accessing' }
BitBlt >> sourceOrigin: aPoint  [
	"Set the receiver's source form coordinates to be those of the argument, 
	aPoint."

	sourceX := aPoint x.
	sourceY := aPoint y
]

{ #category : 'accessing' }
BitBlt >> sourceRect: aRectangle  [
	"Set the receiver's source form top left x and y, width and height to be 
	the top left coordinate and extent of the argument, aRectangle."

	sourceX := aRectangle left.
	sourceY := aRectangle top.
	width := aRectangle width.
	height := aRectangle height
]

{ #category : 'accessing' }
BitBlt >> sourceX: anInteger  [
	"Set the receiver's source form top left x to be the argument, anInteger."

	sourceX := anInteger
]

{ #category : 'accessing' }
BitBlt >> sourceY: anInteger  [
	"Set the receiver's source form top left y to be the argument, anInteger."

	sourceY := anInteger
]

{ #category : 'accessing' }
BitBlt >> width: anInteger  [
	"Set the receiver's destination form width to be the argument, anInteger."

	width := anInteger
]
