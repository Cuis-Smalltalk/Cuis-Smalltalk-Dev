'From Cuis 5.0 [latest update: #4442] on 9 November 2020 at 12:12:47 pm'!

!ImageMorph methodsFor: 'drawing' stamp: 'KenD 10/29/2020 13:33:06'!
drawOn: aCanvas

	aCanvas image: image at: self morphTopLeft + self borderWidth.
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle:  (0@0 extent: self morphExtent)
			color:  borderColor
			borderWidth:  borderWidth 
			borderStyleSymbol:  #simple ]! !

!ImageMorph methodsFor: 'drawing' stamp: 'KenD 10/29/2020 13:21:26'!
minimumExtent
	^image extent + self extentBorder ! !

