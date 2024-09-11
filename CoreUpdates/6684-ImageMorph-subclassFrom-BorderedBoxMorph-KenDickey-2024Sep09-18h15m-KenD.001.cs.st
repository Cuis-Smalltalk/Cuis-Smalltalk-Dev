'From Cuis7.1 [latest update: #6683] on 9 September 2024 at 6:16:28 pm'!
!classDefinition: #ImageMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #ImageMorph
	instanceVariableNames: 'image '
	classVariableNames: 'DefaultForm '
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!ImageMorph methodsFor: 'initialization' stamp: 'KenD 9/9/2024 17:15:03'!
defaultPadding
	"answer the default inner padding for the receiver"
	^ `0@0`! !


!ImageMorph methodsFor: 'drawing' stamp: 'KenD 9/8/2024 12:28:10'!
drawOn: aCanvas

	(padding > `0@0`) ifTrue: [ 	| innerOrigin |
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

!ImageMorph methodsFor: 'drawing' stamp: 'KenD 9/9/2024 17:24:29'!
minimumExtent

	^ image extent + (2 * self interiorOrigin)! !


!ImageMorph class methodsFor: 'instance creation' stamp: 'KenD 9/9/2024 17:18:32'!
new
	^super new
		image: self defaultForm;
		padding: `6@4`.! !

!classDefinition: #ImageMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #ImageMorph
	instanceVariableNames: 'image'
	classVariableNames: 'DefaultForm'
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
