'From Cuis 5.0 of 7 November 2016 [latest update: #3331] on 23 May 2018 at 11:33:10 am'!

!Form methodsFor: 'analyzing' stamp: 'jmv 5/23/2018 11:28:54'!
isAnyPixel: pv
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv.
	Based on #yTallyPixelValue:orNot:
	Form lena isAnyPixel: 0
	Form lena isAnyPixel: 100
	"
	| cm slice copyBlt countBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: width@1.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: slice width @ 1
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	(0 to: height-1) do: [ :y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits > 0 ifTrue: [ ^ true ]].
	^ false! !

