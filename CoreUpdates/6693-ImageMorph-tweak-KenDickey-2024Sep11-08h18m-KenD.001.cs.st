'From Cuis7.1 [latest update: #6692] on 11 September 2024 at 8:18:41 am'!

!ImageMorph methodsFor: 'drawing' stamp: 'KenD 9/10/2024 13:41:43'!
drawOn: aCanvas

	((padding > `0@0`) or: [self morphExtent > self minimumExtent]) 
		ifTrue: [ 	| innerOrigin |
			"Make use of my color in area between border and image"
			innerOrigin := borderWidth @ borderWidth.
			aCanvas
				fillRectangle:  (innerOrigin extent: extent - (2 * innerOrigin ))
				color:  color].
			
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle:  (`0@0` extent: extent)
			color:  borderColor
			borderWidth:  borderWidth 
			borderStyleSymbol:  #simple ].
		
	aCanvas image: image at: self interiorOrigin.
! !

