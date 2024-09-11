'From Cuis7.1 [latest update: #6682] on 9 September 2024 at 3:01:32 pm'!
!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 14:40:23'!
defaultPadding
	"answer the default inner padding for the receiver"
	^ 0@0! !

!LayoutMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2024 14:51:18'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation axisEdgeWeight doAdoptWidgetsColor useEdgeSpace'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LayoutMorph allSubInstances do: [ :m | m instVarNamed: 'padding' put: m defaultPadding ].
!

