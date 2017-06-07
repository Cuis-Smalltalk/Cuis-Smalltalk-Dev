'From Cuis 5.0 of 7 November 2016 [latest update: #3094] on 3 June 2017 at 8:55:22 pm'!

!StringMorph methodsFor: 'drawing' stamp: 'HAW 6/3/2017 20:55:08'!
drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: 0@0
		font: self fontToUse
		color: color
		! !

