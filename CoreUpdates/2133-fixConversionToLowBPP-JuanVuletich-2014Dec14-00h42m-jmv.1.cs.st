'From Cuis 4.2 of 25 July 2013 [latest update: #2132] on 14 December 2014 at 12:43:35.047595 am'!

!Color methodsFor: 'conversions' stamp: 'jmv 12/13/2014 16:18'!
closestPixelValue2
	"Return the nearest approximation to this color for a 2-bit deep Form."

	| lum |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 2].  "opaque white"

	lum _ self luminance.
	lum < 0.2 ifTrue: [^ 1].  "black"
	lum > 0.6 ifTrue: [^ 2].  "opaque white"
	^ 3  "50% gray"
! !

!Color methodsFor: 'conversions' stamp: 'jmv 12/13/2014 16:17'!
closestPixelValue4
	"Return the nearest approximation to this color for a 4-bit deep Form."

	| bIndex |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 2].  "opaque white"

	self isRed ifTrue: [^ 4].
	self isGreen ifTrue: [^ 5].
	self isBlue ifTrue: [^ 6].
	self isCyan ifTrue: [^ 7].
	self isYellow ifTrue: [^ 8].
	self isMagenta ifTrue: [^ 9].

	bIndex _ (self luminance * 8.0) rounded.  "bIndex in [0..8]"
	^ #(
		1	"black"
		10	"1/8 gray"
		11	"2/8 gray"
		12	"3/8 gray"
		3	"4/8 gray"
		13	"5/8 gray"
		14	"6/8 gray"
		15	"7/8 gray"
		2	"opaque white"
	) at: bIndex + 1.
! !

!Color methodsFor: 'conversions' stamp: 'jmv 12/13/2014 16:16'!
closestPixelValue8
	"Return the nearest approximation to this color for an 8-bit deep Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 255].  "white"

	^self saturation < 0.2
		ifTrue: [
			^ GrayToIndexMap at:(self green * 255) rounded + 1.  "nearest gray"
			]
		ifFalse: [
			"compute nearest entry in the color cube"
			40 + 
				((self red * 5) rounded * 36) +
				((self blue * 5) rounded * 6) +
				(self green * 5) rounded ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Color shutDown!

