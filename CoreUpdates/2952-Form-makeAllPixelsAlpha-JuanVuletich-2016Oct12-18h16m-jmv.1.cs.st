'From Cuis 4.2 of 25 July 2013 [latest update: #2950] on 13 October 2016 at 10:06:13 am'!

!Form methodsFor: 'other' stamp: 'jmv 10/12/2016 19:03:35'!
makeAllPixelsAlpha: a
	"Fix the alpha channel if the receiver is 32bit
	Set alpha values to 255 for all pixels.
	Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16).
	See #fixAlpha"
	| bitBlt |
	self depth = 32 ifFalse: [ ^self ].
	bitBlt := BitBlt toForm: self.
	bitBlt combinationRule: 1.
	bitBlt fillBitmap: (Bitmap with: (16r00FFFFFF)).
	bitBlt copyBits.
	bitBlt combinationRule: 7.
	bitBlt fillBitmap: (Bitmap with: (a bitShift: 24)).
	bitBlt copyBits! !

