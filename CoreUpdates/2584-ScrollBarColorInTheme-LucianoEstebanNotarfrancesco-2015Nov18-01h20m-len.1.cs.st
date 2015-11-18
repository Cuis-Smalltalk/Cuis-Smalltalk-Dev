'From Cuis 4.2 of 25 July 2013 [latest update: #2583] on 18 November 2015 at 1:23:43.93366 am'!

!Theme methodsFor: 'colors' stamp: 'len 11/17/2015 23:55'!
scrollbarColor
	^Color white! !


!ScrollBar methodsFor: 'drawing' stamp: 'len 11/17/2015 23:55'!
drawOn: aCanvas

	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: (color alphaMixed: 0.3 with: Theme current scrollbarColor)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !

