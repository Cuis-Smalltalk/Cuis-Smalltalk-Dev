'From Cuis 4.2 of 25 July 2013 [latest update: #1940] on 26 January 2014 at 8:45:45.33714 pm'!

!Color methodsFor: 'transformations' stamp: 'jmv 1/21/2014 12:08'!
+ aColor
	"Answer this color mixed with the given color in an additive color space.  "
	"
	(Color blue + Color green) display
	"
	^ Color basicNew
		setRed: (self red + aColor red min: 1.0 max: 0.0)
		green: (self green + aColor green min: 1.0 max: 0.0)
		blue: (self blue + aColor blue min: 1.0 max: 0.0)
		colorSpace: colorSpace! !

