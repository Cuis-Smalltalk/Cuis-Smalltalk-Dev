'From Cuis7.1 [latest update: #6684] on 9 September 2024 at 6:25:19 pm'!
!classDefinition: #LabelMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #LabelMorph
	instanceVariableNames: 'font emphasis contents '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!LabelMorph methodsFor: 'initialization' stamp: 'KenD 9/9/2024 17:22:24'!
defaultPadding
	"answer the default inner padding for the receiver"
	^ `0@0`! !


!LabelMorph methodsFor: 'drawing' stamp: 'KenD 9/8/2024 13:06:07'!
drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: self interiorOrigin 
		font: self fontToUse
		color: color.
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle: (`0@0` extent: extent)
			color: borderColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple ]! !

!LabelMorph methodsFor: 'geometry' stamp: 'KenD 9/8/2024 13:05:40'!
minimumExtent

	^ self measureContents + (2 * self interiorOrigin) ! !

!classDefinition: #LabelMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #LabelMorph
	instanceVariableNames: 'font emphasis contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LabelMorph allSubInstances do: [ :m | m instVarNamed: 'padding' put: 0@0].!

