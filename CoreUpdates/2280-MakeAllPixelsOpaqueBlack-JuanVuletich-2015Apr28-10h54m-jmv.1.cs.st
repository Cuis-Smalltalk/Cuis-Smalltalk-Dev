'From Cuis 4.2 of 25 July 2013 [latest update: #2278] on 28 April 2015 at 10:56:24.261009 am'!

!Form methodsFor: 'other' stamp: 'jmv 4/28/2015 10:56'!
makeAllPixelsOpaqueBlack
	"Make all pixels opaque black.
	Useful for preparing 32bpp forms for later stuff.
	Set alpha values to 255 for all pixels.
	Set r, g, b values to zero for all pixels."
	| bitBlt |
	bitBlt _ BitBlt toForm: self.
	bitBlt combinationRule: 3.
	bitBlt fillBitmap: (Bitmap with: 16rFF000000).
	bitBlt copyBits! !

