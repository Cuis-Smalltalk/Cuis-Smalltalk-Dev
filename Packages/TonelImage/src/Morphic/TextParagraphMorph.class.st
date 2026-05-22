"
TextParagraphMorph support display of text with emphasis. It can be used on its own, no need to include them in some kind of TextModelMorph / ScrollPane.

Support all features of Text, including fonts, sizes, emphasis and embedded morphs.

Several examples of usage:


"Allows resize by halo. Owner controls extent."
(TextParagraphMorph contents: 'Hello
World!') openInWorld.

(TextParagraphMorph contents: Utilities defaultTextEditorContents) openInWorld.


"Resize by halo only sets width. Owner controls width. Height set automatically. Full contents always visible."
(TextParagraphMorph contents: 'Hello
World!') setHeightOnContent; openInWorld.

(TextParagraphMorph contents: Utilities defaultTextEditorContents) setHeightOnContent; openInWorld.


"Auto shrink contents. Resize disabled. Owner controls neither width nor height."
(TextParagraphMorph contents: 'Hello
World!') shrinkFromExtent: 1000@1000; openInWorld.

(TextParagraphMorph contents: Utilities defaultTextEditorContents) shrinkFromExtent: 1000@1000; openInWorld.


"Scrollable contents. Usual behavior. Owner of the ScrollPane control visible extent. Same with halo."
| t s |
t := TextParagraphMorph contents: Utilities defaultTextEditorContents.
s := PluggableScrollPane new.
s scroller: t setHeightOnContent.
s openInWorld.
"
Class {
	#name : 'TextParagraphMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'textComposition',
		'setsHeightOnContent',
		'shrinkFromExtent'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
TextParagraphMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'instance creation' }
TextParagraphMorph class >> contents: aStringOrText [
	^ self new 
		contents: aStringOrText ;
		yourself 
]

{ #category : 'instance creation' }
TextParagraphMorph class >> initializedInstance [

	^self contents:
'I am a read-only text paragraph.
I can hold several lines of text.
Even with ', ' text ' bold, 'styles.' italic, '
Try resizing me with the Halo.'
]

{ #category : 'geometry' }
TextParagraphMorph >> adjustExtent [
	"Sent by owner if kind of PluggableScrollPane.
	This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!"

	self morphExtent: owner viewableExtent x @ extent y
]

{ #category : 'geometry' }
TextParagraphMorph >> borderWidth: aNumber [

	super borderWidth: aNumber.
	self reflowAndMaybeResize.
]

{ #category : 'accessing' }
TextParagraphMorph >> contents: aStringOrText [

	| current |
	current := textComposition textComposed.
	current = aStringOrText ifTrue: [
		((current is: #Text) and: [aStringOrText is: #Text])
		ifTrue: [
			current runs = aStringOrText runs ifTrue: [
				^self ]]
		ifFalse: [
			^self ]].
	textComposition setModel: (TextModel withText: aStringOrText).
	self reflowAndMaybeResize.
]

{ #category : 'drawing' }
TextParagraphMorph >> drawOn: aCanvas [

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
		borderStyleSymbol: nil
]

{ #category : 'geometry' }
TextParagraphMorph >> extentChanged: oldExtent [
	"Our extent changed. Reflow text."

	super extentChanged: oldExtent.
	extent x = oldExtent x ifFalse: [
		self reflowAndMaybeResize ].
]

{ #category : 'accessing' }
TextParagraphMorph >> extentForComposing [

	shrinkFromExtent ifNotNil: [
		^shrinkFromExtent ].

	^ self interiorExtent x @ 9999999
]

{ #category : 'initialization' }
TextParagraphMorph >> initialize [
	super initialize.
	extent := self minimumExtent * 5.
	shrinkFromExtent := nil.
	setsHeightOnContent := false.
	textComposition := TextComposition new.
	textComposition
		setModel: (TextModel withText: '');
		extentForComposing: self extentForComposing;
		composeAll.
]

{ #category : 'geometry' }
TextParagraphMorph >> minimumExtent [

	^3@1 * FontFamily defaultLineSpacing ceiling + (2 *  self interiorOrigin) 
]

{ #category : 'geometry' }
TextParagraphMorph >> privateExtent: aPoint [
	| newExtent |

	newExtent := aPoint.
	(setsHeightOnContent or: [shrinkFromExtent notNil]) ifTrue: [
		"We decide our own height"
		newExtent := aPoint x truncated @ extent y ].
	^ super privateExtent: newExtent
]

{ #category : 'geometry' }
TextParagraphMorph >> reflowAndMaybeResize [
	"Reflow text. If appropriate, adjust height or extent."

	| extentForComposing usedExtent newHeight newExtent |
	extentForComposing := self extentForComposing.
	"If we must shrink, but have centered or right aligned text"
	(shrinkFromExtent notNil and: [textComposition textComposed hasAnyCenteredOrRightFlush]) ifTrue: [
		| leftAlignedText auxTC |
		leftAlignedText := textComposition textComposed copy removeAttributesThat: [ :attribute | attribute isCenteredOrRightFlush ].
		auxTC := TextComposition new.
		auxTC setModel: (TextModel withText: leftAlignedText).
		auxTC
			extentForComposing: extentForComposing;
			composeAll.
		extentForComposing := auxTC usedExtent ].

	textComposition
		extentForComposing: extentForComposing;
		composeAll.
	usedExtent := textComposition usedExtent.

	newExtent := extent.
	setsHeightOnContent ifTrue: [
		newHeight := usedExtent y + (2 * self interiorOrigin y)
				max: self minimumExtent y.
		newExtent := extent x @ newHeight ].
	shrinkFromExtent ifNotNil: [
		newExtent := usedExtent + (2 * self interiorOrigin) ].

	newExtent = extent ifFalse: [
		extent := newExtent.
		owner ifNotNil: [
			owner someSubmorphPositionOrExtentChanged ].
		(owner is: #ScrollPane) ifTrue: [
			owner
				updateScrollBarsBounds;
				setScrollDeltas ]].

	self redrawNeeded.
]

{ #category : 'accessing' }
TextParagraphMorph >> setHeightOnContent [
	"So we always adjust our own height to match contents.
	Disables auto-shrink to contents, meaning height not only shrinks but expands, and width is not modified.
	Usually when used in a PluggableScrollPane.
	Also useful for elements in a list."

	setsHeightOnContent := true.
	shrinkFromExtent := nil.
	self reflowAndMaybeResize.
]

{ #category : 'accessing' }
TextParagraphMorph >> shrink [
	"Automatically shrink extent to whatever the text needs"
	self shrinkFromExtent: 9999999@9999999

]

{ #category : 'accessing' }
TextParagraphMorph >> shrinkFromExtent: aPoint [
	"Automatically shrink extent to whatever the text needs,
	but starting from argument.
	Extent will never exceed argument."

	shrinkFromExtent := aPoint.
	setsHeightOnContent := false.
	self reflowAndMaybeResize.
]
