'From Cuis 5.0 of 7 November 2016 [latest update: #3106] on 19 June 2017 at 11:52:30 am'!
!Colour methodsFor: 'transformations' stamp: 'jmv 6/19/2017 12:10:13'!
* aNumber
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Colour brown *2) display
	"
	^ (Color new
		setRed: (self red * aNumber min: 1.0 max: 0.0)
		green: (self green * aNumber min: 1.0 max: 0.0)
		blue: (self blue * aNumber min: 1.0 max: 0.0))
			alpha: self alpha! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

| all |
all := Colour allInstances.
all elementsForwardIdentityTo: (all collect: [ :c | c * 1 ]).
all := TranslucentColour allInstances.
all elementsForwardIdentityTo: (all collect: [ :c | c * 1 ]).!

