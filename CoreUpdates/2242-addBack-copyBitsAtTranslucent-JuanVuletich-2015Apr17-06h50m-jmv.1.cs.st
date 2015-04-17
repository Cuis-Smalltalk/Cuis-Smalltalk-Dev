'From Cuis 4.2 of 25 July 2013 [latest update: #2241] on 17 April 2015 at 6:51:14.919511 am'!

!Form methodsFor: 'copying' stamp: 'jmv 4/17/2015 06:50'!
copyBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: 30
		destOrigin: destOrigin
		sourceOrigin: 0@0
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f _ Form fromUser. f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 copyBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"! !

