'From Cuis 4.1 of 12 December 2012 [latest update: #1655] on 1 April 2013 at 7:00:42 pm'!

!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 4/1/2013 16:30'!
drawOn: aCanvas 
	| baseColor r |
	baseColor _ owner color.
	r _ 0@0 extent: self morphExtent.
	aCanvas
		fillRectangle: (r topLeft corner: r rightCenter)
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (r leftCenter corner: r bottomRight)
		color: baseColor twiceLighter! !

