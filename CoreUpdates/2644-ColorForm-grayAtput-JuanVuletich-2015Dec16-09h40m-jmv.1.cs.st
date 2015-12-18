'From Cuis 4.2 of 25 July 2013 [latest update: #2632] on 16 December 2015 at 9:44:05.411127 am'!

!ColorForm methodsFor: 'pixel accessing' stamp: 'jmv 12/10/2015 13:35'!
grayAt: aPoint put: aGrayColor
	"Store the given color into the pixel at aPoint.
	Assume we were built with #grayScaleExtent:, and aGrayColor is gray."

	self pixelValueAt: aPoint put: (aGrayColor green * 255) rounded
! !

