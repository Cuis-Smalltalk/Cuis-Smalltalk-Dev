"
A balloon with text used for the display of explanatory information.
"
Class {
	#name : 'HoverHelpMorph',
	#superclass : 'ColoredBoxMorph',
	#instVars : [
		'target',
		'getter',
		'contents',
		'textComposition'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'new-morph participation' }
HoverHelpMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'instance creation' }
HoverHelpMorph class >> contents: aStringOrText [
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.') openInHand
	"

	^ self contents: aStringOrText doWordWrap: true
]

{ #category : 'instance creation' }
HoverHelpMorph class >> contents: aStringOrText doWordWrap: aBoolean [
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: true) openInHand
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' blue doWordWrap: false) openInHand
	"

	| widthOrNil |
	widthOrNil := aBoolean ifTrue: [FontFamily defaultLineSpacing * (aStringOrText size sqrt * 2.0 max: 13)].
	^self contents: aStringOrText wrappedTo: widthOrNil.
]

{ #category : 'instance creation' }
HoverHelpMorph class >> contents: aStringOrText wrappedTo: wordWrapWidthOrNil [
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather long contents to see how it gets wrapped. Is this long enough? Maybe a few more words are in order.'
		wrappedTo: 150) openInHand
	"

	^self new contents: aStringOrText wrappedTo: wordWrapWidthOrNil
]

{ #category : 'instance creation' }
HoverHelpMorph class >> initializedInstance [
	"Answer an instance of the receiver which in some sense is initialized.  In the case of Morphs, this will yield an instance that can be attached to the Hand after having received the same kind of basic initialization that would be obtained from an instance chosen from the 'new morph' menu.
	Return nil if the receiver is reluctant for some reason to return such a thing"

	^ self contents: 'A Hover Help'
]

{ #category : 'instance creation' }
HoverHelpMorph class >> target: aTargetMorph getter: aSelector [
	"Make up and return a HoverHelp for morph.
	"

	^self new target: aTargetMorph getter: aSelector
]

{ #category : 'accessing' }
HoverHelpMorph >> contents: aStringOrText wrappedTo: wordWrapWidthOrNil [
	"If wordWrapWidthOrNil is nil, don't do wordwrap, and make the morph as wide as needed"

	| width |
	contents := aStringOrText.
	width := wordWrapWidthOrNil ifNil: [9999999].
	textComposition := TextComposition new.
	textComposition
		setModel: (TextModel withText: contents asText);
		extentForComposing: 	width@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8.
]

{ #category : 'initialization' }
HoverHelpMorph >> defaultColor [

	^ Theme current hoverHelp
]

{ #category : 'accessing' }
HoverHelpMorph >> defaultLayerNumber [
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be on top."

	^10
]

{ #category : 'drawing' }
HoverHelpMorph >> drawOn: aCanvas [

	| r |
	r := self localBounds.
	aCanvas roundRect: r color: color radius: 4.
	aCanvas
		drawTextComposition: textComposition
		at: `4@4`
		extent: extent - 8
		color: `Color black`
		selectionColor: (Theme current textHighlightFocused: false).
]

{ #category : 'testing' }
HoverHelpMorph >> isIncludedInTaskbar [
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false
]

{ #category : 'initialization' }
HoverHelpMorph >> popUpForHand: aHand [
	"Pop up the receiver as balloon help for the given hand"

	| newPos x y |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [
		self
			contents: (target ifNotNil: [getter ifNotNil: [target perform: getter] ifNil: [target balloonText]])
			wrappedTo: nil ].
	x := aHand morphPosition x - 20.
	y := aHand morphPosition y + 20.
	x + extent x > aHand world morphWidth ifTrue: [
		x := aHand world morphWidth - extent x ].
	y + extent y > aHand world morphHeight ifTrue: [
		y := aHand morphPosition y - extent y - 12 ].
	newPos := x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self
]

{ #category : 'accessing' }
HoverHelpMorph >> target [
	^target
]

{ #category : 'accessing' }
HoverHelpMorph >> target: aTargetMorph getter: aSelector [
	target := aTargetMorph.
	getter := aSelector.
]
