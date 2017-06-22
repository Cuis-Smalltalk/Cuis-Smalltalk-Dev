'From Cuis 5.0 of 7 November 2016 [latest update: #3115] on 20 June 2017 at 5:51:43 pm'!

!Color methodsFor: 'conversions' stamp: 'jmv 6/20/2017 17:46:14'!
pixelValueForDepth: d
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8 and less than 32 (no Indexed colors, no real alpha), black maps to the darkest possible blue.
	Note that 
		Color transparent class = TranslucentColor
	this special case is handled in TranslucentColor >> #pixelValueForDepth:
	"

	| bitBltFakeBlack val |
	"Most common case"
	"eight bits per component; top 8 bits set to all ones (opaque alpha)"
	d = 32 ifTrue: [
		^ 16rFF000000 bitOr:
			((((self at: 1) * 255.999) truncated bitShift: 16) bitOr:
			((((self at: 2) * 255.999) truncated bitShift: 8) bitOr: 
			(((self at: 3) * 255.999) truncated))) ].

"Faster in 32 bit systems, but slower in 64 bits"
"	d = 32 ifTrue: [
		val _ LargePositiveInteger new: 4.
		val at: 3 put: ((self at: 1) * 255.999) truncated.
		val at: 2 put: ((self at: 2)  * 255.999) truncated.
		val at: 1 put: ((self at: 3)  * 255.999) truncated.
		val at: 4 put: 16rFF.
		^ val normalize]."

	d = 8 ifTrue: [^ self closestPixelValue8].  "common case"
	d < 8 ifTrue: [
		d = 4 ifTrue: [^ self closestPixelValue4].
		d = 2 ifTrue: [^ self closestPixelValue2].
		d = 1 ifTrue: [^ self closestPixelValue1]].

	"For the depth 16, pixelValue = 0 means transparent, black is represented as 16r8000 (rgb=0, pixelvalue !!= 0)."
	(d = 16) | (d = 15) ifTrue: [
		"five bits per component; top bits ignored"
		val _ ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0
			ifTrue: [d = 16 ifTrue: [16r8000] ifFalse: [1]]
			ifFalse: [val]].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 12 ifTrue: [  "for indexing a color map with 4 bits per color component"
		val _ ((self red * 15) rounded bitShift: 8) bitOr:
			(((self green * 15) rounded bitShift: 4) bitOr: 
			((self blue * 15) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	d = 9 ifTrue: [  "for indexing a color map with 3 bits per color component"
		val _ ((self red * 7) rounded bitShift: 6) bitOr:
			(((self green * 7) rounded bitShift: 3) bitOr: 
			((self blue * 7) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	self error: 'unknown pixel depth: ', d printString! !

