'From Cuis 5.0 of 7 November 2016 [latest update: #3106] on 19 June 2017 at 12:18:58 pm'!

!Colour methodsFor: 'transformations' stamp: 'jmv 6/19/2017 12:18:51'!
* aNumber
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Colour brown *2) display
	"
	^ (Colour new
		setRed: (self red * aNumber min: 1.0 max: 0.0)
		green: (self green * aNumber min: 1.0 max: 0.0)
		blue: (self blue * aNumber min: 1.0 max: 0.0))
			alpha: self alpha! !


!Color methodsFor: 'transformations' stamp: 'jmv 6/19/2017 12:18:50'!
* aNumber
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Color brown *2) display
	"
	^ (Colour new
		setRed: (self red * aNumber min: 1.0 max: 0.0)
		green: (self green * aNumber min: 1.0 max: 0.0)
		blue: (self blue * aNumber min: 1.0 max: 0.0)
		colorSpace: colorSpace)
			alpha: self alpha! !

