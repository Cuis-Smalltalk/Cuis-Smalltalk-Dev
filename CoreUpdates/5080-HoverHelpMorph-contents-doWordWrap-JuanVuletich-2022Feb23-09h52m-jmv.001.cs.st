'From Cuis 6.0 [latest update: #5075] on 23 February 2022 at 9:55:28 am'!

!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 2/23/2022 09:54:20'!
contents: aString
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.') openInHand
	"

	^ self contents: aString doWordWrap: true! !

!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 2/23/2022 09:55:05'!
contents: aString doWordWrap: aBoolean
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: true) openInHand
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: false) openInHand
	"

	^self contents: aString wrappedTo: (aBoolean ifTrue: [FontFamily defaultLineSpacing * 13])! !

