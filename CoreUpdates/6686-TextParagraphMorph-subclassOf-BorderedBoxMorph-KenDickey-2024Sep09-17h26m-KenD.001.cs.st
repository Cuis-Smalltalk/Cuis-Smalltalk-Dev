'From Cuis7.1 [latest update: #6682] on 9 September 2024 at 5:39:24 pm'!
!classDefinition: #TextParagraphMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #TextParagraphMorph
	instanceVariableNames: 'textComposition setsHeightOnContent shrinkFromExtent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!TextParagraphMorph methodsFor: 'accessing' stamp: 'KenD 9/9/2024 17:35:05'!
extentForComposing

	shrinkFromExtent ifNotNil: [
		^shrinkFromExtent ].

	^ self interiorExtent x @ 9999999! !

!TextParagraphMorph methodsFor: 'drawing' stamp: 'KenD 9/8/2024 12:35:17'!
drawOn: aCanvas

	aCanvas
		fillRectangle: self localBounds color: color.
	aCanvas
		drawTextComposition: textComposition
		at: self interiorOrigin
		extent: self interiorExtent
		color: Theme current text
		selectionColor: `Color red`
		avoidOverhang: true.
	aCanvas
		frameRectangle: self localBounds
		color: borderColor
		borderWidth: borderWidth
		borderStyleSymbol: nil! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'KenD 9/9/2024 17:31:25'!
minimumExtent

	^3@1 * FontFamily defaultLineSpacing ceiling + (2 *  self interiorOrigin) ! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'KenD 9/9/2024 17:38:03'!
reflowAndMaybeResize
	"Reflow text. If appropriate, adjust height or extent."

	| newHeight |
	textComposition
		extentForComposing: self extentForComposing;
		composeAll.
	setsHeightOnContent ifTrue: [
		newHeight := textComposition usedHeight + (2 * self interiorOrigin)
				max: self minimumExtent y.
		extent y = newHeight ifFalse: [
			extent := extent x @ newHeight.
			self redrawNeeded.
			owner ifNotNil: [
				owner someSubmorphPositionOrExtentChanged ]].
		(owner is: #ScrollPane) ifTrue: [
			owner
				updateScrollBarsBounds;
				setScrollDeltas ].
		^self ].
	shrinkFromExtent ifNotNil: [
		extent := textComposition usedExtent + (2 * self interiorOrigin) ].
	self redrawNeeded.! !

!classDefinition: #TextParagraphMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #TextParagraphMorph
	instanceVariableNames: 'textComposition setsHeightOnContent shrinkFromExtent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
