'From Cuis 6.0 [latest update: #5954] on 27 July 2023 at 2:09:38 pm'!
!classDefinition: #TextParagraphMorph category: 'Morphic-Widgets'!
BoxedMorph subclass: #TextParagraphMorph
	instanceVariableNames: 'textComposition setsHeightOnContent shrinkFromExtent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!TextParagraphMorph commentStamp: 'jmv 7/27/2023 14:01:58' prior: 0!
TextParagraphMorph support display of text with emphasis. It can be used on its own, no need to include them in some kind of TextModelMorph / ScrollPane.

Support all features of Text, including fonts, sizes, emphasis and embedded morphs.

Several examples of usage:


"Allows resize by halo. Owner controls extent."
(TextParagraphMorph contents: 'Hello
World!!') openInWorld.

(TextParagraphMorph contents: Utilities defaultTextEditorContents) openInWorld.


"Resize by halo only sets width. Owner controls width. Height set automatically. Full contents always visible."
(TextParagraphMorph contents: 'Hello
World!!') setHeightOnContent; openInWorld.

(TextParagraphMorph contents: Utilities defaultTextEditorContents) setHeightOnContent; openInWorld.


"Auto shrink contents. Resize disabled. Owner controls neither width nor height."
(TextParagraphMorph contents: 'Hello
World!!') shrinkFromExtent: 1000@1000; openInWorld.

(TextParagraphMorph contents: Utilities defaultTextEditorContents) shrinkFromExtent: 1000@1000; openInWorld.


"Scrollable contents. Usual behavior. Owner of the ScrollPane control visible extent. Same with halo."
| t s |
t := TextParagraphMorph contents: Utilities defaultTextEditorContents.
s := PluggableScrollPane new.
s scroller: t setHeightOnContent.
s openInWorld.!


!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 7/27/2023 12:55:36'!
shrinkFromExtent: aPoint
	"Automatically shrink extent to whatever the text needs,
	but starting from argument.
	Extent will never exceed argument."

	shrinkFromExtent := aPoint.
	setsHeightOnContent := false.
	self reflowAndMaybeResize.! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 13:10:10'!
borderWidth: aNumber
	super borderWidth: aNumber.
	self reflowAndMaybeResize.! !


!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 7/27/2023 12:33:31'!
contents: aStringOrText

	textComposition textComposed ~= aStringOrText ifTrue: [
		textComposition	 	setModel: (TextModel withText: aStringOrText).
		self reflowAndMaybeResize ]! !

!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 7/27/2023 12:27:37'!
extentForComposing

	shrinkFromExtent ifNotNil: [
		^shrinkFromExtent ].

	^ extent x - (4 * borderWidth) @ 9999999! !

!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 7/27/2023 14:03:01'!
setHeightOnContent
	"So we always adjust our own height to match contents.
	Disables auto-shrink to contents, meaning height not only shrinks but expands, and width is not modified.
	Usually when used in a PluggableScrollPane.
	Also useful for elements in a list."

	setsHeightOnContent := true.
	shrinkFromExtent := nil.
	self reflowAndMaybeResize.! !

!TextParagraphMorph methodsFor: 'initialization' stamp: 'jmv 7/27/2023 12:53:08'!
initialize
	super initialize.
	extent := self minimumExtent * 5.
	shrinkFromExtent := nil.
	setsHeightOnContent := false.
	textComposition := TextComposition new.
	textComposition
		setModel: (TextModel withText: '');
		extentForComposing: self extentForComposing;
		composeAll.! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 12:44:22'!
adjustExtent
	"Sent by owner if kind of PluggableScrollPane.
	This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!!"

	self morphExtent: owner viewableExtent x @ extent y! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 12:59:59'!
privateExtent: aPoint
	| newExtent |

	newExtent := aPoint.
	(setsHeightOnContent or: [shrinkFromExtent notNil]) ifTrue: [
		"We decide our own height"
		newExtent := aPoint x truncated @ extent y ].
	^ super privateExtent: newExtent! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 14:07:58'!
reflowAndMaybeResize
	"Reflow text. If appropriate, adjust height or extent."

	| newHeight |
	textComposition
		extentForComposing: self extentForComposing;
		composeAll.
	setsHeightOnContent ifTrue: [
		newHeight := textComposition usedHeight + (4 * borderWidth)
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
		extent := textComposition usedExtent + (4 * borderWidth) ].
	self redrawNeeded.! !

!classDefinition: #TextParagraphMorph category: 'Morphic-Widgets'!
BoxedMorph subclass: #TextParagraphMorph
	instanceVariableNames: 'textComposition setsHeightOnContent shrinkFromExtent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
