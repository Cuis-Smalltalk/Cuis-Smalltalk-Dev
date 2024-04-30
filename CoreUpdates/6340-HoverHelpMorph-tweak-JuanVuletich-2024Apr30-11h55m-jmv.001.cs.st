'From Cuis6.3 [latest update: #6339] on 30 April 2024 at 11:56:16 am'!

!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 4/30/2024 11:55:59'!
contents: aStringOrText doWordWrap: aBoolean
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: true) openInHand
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' blue doWordWrap: false) openInHand
	"

	| widthOrNil |
	widthOrNil := aBoolean ifTrue: [FontFamily defaultLineSpacing * (aStringOrText size sqrt * 2.0 max: 13)].
	^self contents: aStringOrText wrappedTo: widthOrNil.! !

