'From Cuis7.7 [latest update: #7794] on 12 January 2026 at 3:38:08 pm'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'target getter contents textComposition doAutoWordWrap '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 12/Jan/2026 12:20:24'!
doAutoWordWrap: aBooleanOrNil
	"true -> do it
	false -> don't do it
	nil -> Let contents decide.
		Do it unless contents last is newline character.
		Add a bogus newline at the end if your contents
		is a single line and you don't want wordwrap."

	doAutoWordWrap := aBooleanOrNil.! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 12/Jan/2026 15:37:35'!
showBalloon: msgStringOrText hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	self
		showBalloon: msgStringOrText
		hand: aHand
		doWordWrap: nil.! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 12/Jan/2026 15:34:58'!
contents: aStringOrText wrappedTo: wordWrapWidthOrNil
	"Do wordwrap as set up.
	If requested, honor wordWrapWidthOrNil or find a good default if nil.
	If not requested, make the morph as wide as needed"

	| contentsToCompose lastIsLineSeparator width |
	contents := aStringOrText.
	contentsToCompose := aStringOrText.
	lastIsLineSeparator := aStringOrText last isLineSeparator.
	lastIsLineSeparator ifTrue: [
		contentsToCompose := aStringOrText allButLast ].
	width := (doAutoWordWrap ifNil: [lastIsLineSeparator not])
		ifTrue: [
			wordWrapWidthOrNil ifNil: [
				FontFamily defaultLineSpacing * (aStringOrText size sqrt * 2.0 max: 13)]]
		ifFalse: [9999999].
	textComposition := TextComposition new.
	textComposition
		setModel: (TextModel withText: contentsToCompose asText);
		extentForComposing: width@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8.! !

!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 12/Jan/2026 15:35:18'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| dynamicContents newPos x y |
	dynamicContents := target
		ifNil: [ contents ]
		ifNotNil: [
			getter
				ifNotNil: [target perform: getter]
				ifNil: [target balloonText]].
	self contents: dynamicContents wrappedTo: nil.
	x := aHand morphPosition x - 20.
	y := aHand morphPosition y + 20.
	x + extent x > aHand world morphWidth ifTrue: [
		x := aHand world morphWidth - extent x ].
	y + extent y > aHand world morphHeight ifTrue: [
		y := aHand morphPosition y - extent y - 12 ].
	newPos := x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self.! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 12/Jan/2026 15:35:23'!
contents: aStringOrText doWordWrap: aBoolean
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: true) openInHand
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' blue doWordWrap: false) openInHand
	Contenido fijo. decision de wrappear en creacion. openInWorld a cargo del usuario.
	Este parece ser el unico lugr donde se determina un ancho razonable al cual wrappear.
	Solo hacerlo si hay que autowrappear, pero en todos lados!!
	"

	| answer |
	answer := self new.
	answer doAutoWordWrap: aBoolean.
	answer contents: aStringOrText wrappedTo: nil.
	^answer! !

!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 12/Jan/2026 15:35:27'!
contents: aStringOrText wrappedTo: wordWrapWidthOrNil
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather long contents to see how it gets wrapped. Is this long enough? Maybe a few more words are in order.'
		wrappedTo: 150) openInHand
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather long contents to see how it gets wrapped. Is this long enough? Maybe a few more words are in order.'
		wrappedTo: nil) openInHand
	"
	| answer |
	answer := self new.
	answer doAutoWordWrap: wordWrapWidthOrNil notNil.
	answer contents: aStringOrText wrappedTo: wordWrapWidthOrNil.
	^answer! !


!SystemWindow methodsFor: 'label' stamp: 'jmv 12/Jan/2026 12:28:12'!
balloonText
	^labelString, String newLineString! !

!classDefinition: #HoverHelpMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'target getter doAutoWordWrap contents textComposition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
