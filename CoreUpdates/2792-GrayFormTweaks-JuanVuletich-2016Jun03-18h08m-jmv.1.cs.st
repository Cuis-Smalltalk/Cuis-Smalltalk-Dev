'From Cuis 4.2 of 25 July 2013 [latest update: #2791] on 3 June 2016 at 6:14:27.23576 pm'!

!Color methodsFor: 'conversions' stamp: 'jmv 6/3/2016 17:28'!
bitPatternForGrayForm
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"
	"Details: The pattern for the most recently requested depth is cached."

	^Bitmap with: (self pixelWordFor: 8 filledWith: (self luminance * 255) rounded)! !


!GrayForm methodsFor: 'color mapping' stamp: 'jmv 6/3/2016 17:28'!
bitPatternFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	^aColor bitPatternForGrayForm! !

!GrayForm methodsFor: 'analyzing' stamp: 'jmv 6/3/2016 17:33'!
pixelCompare: aRect with: otherForm at: otherLoc
	"Use RGB comparison. Divide by 3 because we have one sample per pixel, not 3."
	^ ((self asFormOfDepth: 32) pixelCompare: aRect with: (otherForm asFormOfDepth: 32) at: otherLoc) / 3! !


!Form methodsFor: 'analyzing' stamp: 'jmv 6/3/2016 18:13'!
pixelCompare: aRect with: otherForm at: otherLoc
	"Compare the selected bits of this form (those within aRect) against
	those in a similar rectangle of otherFrom.  Return the sum of the
	absolute value of the differences of the color values of every pixel.
	Obviously, this is most useful for rgb (16- or 32-bit) pixels but,
	in the case of 8-bits or less, this will return the sum of the differing
	bits of the corresponding pixel values (somewhat less useful)"
	"Just use 32 bits..."
	| pixPerWord temp |
	otherForm nativeDepth = 32 ifFalse: [
		^ self pixelCompare: aRect with: (otherForm asFormOfDepth: 32) at: otherLoc ].
	self nativeDepth = 32 ifFalse: [
		^ (self asFormOfDepth: 32) pixelCompare: aRect with: otherForm at: otherLoc ].
	
	pixPerWord _ 32//self depth.
	(aRect left\\pixPerWord = 0 and: [aRect right\\pixPerWord = 0]) ifTrue: [
		"If word-aligned, use on-the-fly difference"
		^ (BitBlt toForm: self) copy: aRect from: otherLoc in: otherForm
				fillColor: nil rule: 32].
	"Otherwise, combine in a word-sized form and then compute difference"
	temp _ self copy: aRect.
	temp copy: aRect from: otherLoc in: otherForm rule: 21.
	^ (BitBlt toForm: temp) copy: aRect from: otherLoc in: nil
				fillColor: (Bitmap with: 0) rule: 32
"  Dumb example prints zero only when you move over the original rectangle...
 | f diff |
f _ Form fromUser.
[ Sensor isAnyButtonPressed ] whileFalse: [
	diff _ f
		pixelCompare: f boundingBox
		with: Display
		at: Sensor mousePoint.
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	diff printString , '        ' displayAt: 0@0 ]
"! !

