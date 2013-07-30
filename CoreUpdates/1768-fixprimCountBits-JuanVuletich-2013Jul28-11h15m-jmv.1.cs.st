'From Cuis 4.2 of 25 July 2013 [latest update: #1766] on 28 July 2013 at 11:25:55.504 am'!

!Bitmap methodsFor: 'accessing'!
bitPatternForDepth: depth
	"The raw call on BitBlt needs a Bitmap to represent this color.  I already am Bitmap like.  I am already adjusted for a specific depth.  Interpret me as an array of (32/depth) Color pixelValues.  BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary. 6/18/96 tk"

	^ self! !
