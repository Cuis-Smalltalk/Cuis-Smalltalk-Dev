'From Cuis 6.0 [latest update: #5078] on 23 February 2022 at 10:52:53 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 2/23/2022 10:43:04'!
showBalloon: msgStringOrText hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| doWordWrap msg |
	doWordWrap _ true.
	msg _ msgStringOrText.
	msgStringOrText last isLineSeparator ifTrue: [
		doWordWrap _ false.
		msg _ msg allButLast ].
	self
		showBalloon: msg
		hand: aHand
		doWordWrap: doWordWrap.! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 2/23/2022 10:30:25'!
contents: aStringOrText doWordWrap: aBoolean
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: true) openInHand
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' blue doWordWrap: false) openInHand
	"

	| widthOrNil |
	widthOrNil _ aBoolean ifTrue: [FontFamily defaultLineSpacing * (aStringOrText size sqrt * 1.5 max: 13)].
	^self contents: aStringOrText wrappedTo: widthOrNil! !

