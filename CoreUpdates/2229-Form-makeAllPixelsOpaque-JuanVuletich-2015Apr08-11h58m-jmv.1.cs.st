'From Cuis 4.2 of 25 July 2013 [latest update: #2229] on 8 April 2015 at 11:58:37.512991 am'!

!Form methodsFor: 'other' stamp: 'jmv 4/8/2015 11:39'!
makeAllPixelsOpaque
	"Fix the alpha channel if the receiver is 32bit
	Set alpha values to 255 for all pixels.
	Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16).
	See #fixAlpha"
	| bitBlt |
	self depth = 32 ifFalse: [ ^self ].
	bitBlt := BitBlt toForm: self.
	bitBlt combinationRule: 7. "bitOr:with:"
	bitBlt fillBitmap: (Bitmap with: 16rFF000000).
	bitBlt copyBits! !


!Form methodsFor: 'other' stamp: 'jmv 4/8/2015 11:39'!
fixAlpha
	"Fix the alpha channel if the receiver is 32bit
	For any pixel with zero alpha value, and not zero rgb, set alpha to 255.
	Note: assumes that a pixel with 0 is meant to be transparent, and won't make it opaque black.
	See #makeAllPixelsOpaque"
	| bb |
	self depth = 32 ifFalse: [ ^self ].
	bb _ BitBlt toForm: self.
	bb combinationRule: 40. "fixAlpha:with:"
	bb copyBits! !


!BMPReadWriter methodsFor: 'reading' stamp: 'jmv 4/8/2015 11:45'!
read24BmpFile
	"Read 24-bit pixel data from the given a BMP stream."
	| form formBits pixelLine bitsIndex |
	form := Form extent: biWidth@biHeight depth: 32.
	pixelLine := ByteArray new: (((24 * biWidth) + 31) // 32) * 4.
	bitsIndex := form height - 1 * biWidth + 1.
	formBits := form bits.
	1 to: biHeight do: [:i |
		pixelLine := stream nextInto: pixelLine.
		self read24BmpLine: pixelLine into: formBits startingAt: bitsIndex width: biWidth.
		bitsIndex := bitsIndex - biWidth.
	].
	form makeAllPixelsOpaque.
	^ form
! !

