'From Cuis 6.0 [latest update: #5075] on 23 February 2022 at 9:52:32 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 2/23/2022 09:44:22'!
showBalloon: msgString hand: aHand doWordWrap: aBoolean
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| w balloon h |
	w _ self world ifNil: [^ self].
	h _ aHand ifNil: [ w activeHand ].
	balloon _ HoverHelpMorph contents: msgString doWordWrap: aBoolean.
	
	"Do it in a while. In some cases, processing the event that
	might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [
		balloon popUpForHand: h ].! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 2/23/2022 09:48:39'!
contents: aString doWordWrap: aBoolean
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: true) openInHand
	(HoverHelpMorph contents: 'This is a HoverHelpMorph with a rather largish contents, to show the effect of word wrap.' doWordWrap: false) openInHand
	"

	^self new contents: aString wrappedTo: (aBoolean ifTrue: [FontFamily defaultLineSpacing * 13])! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 2/23/2022 09:52:20'!
showBalloon: msgString hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	self showBalloon: msgString hand: aHand doWordWrap: false.! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 2/23/2022 09:47:55'!
contents: aString
	"Make up and return a HoverHelp for morph.
	(HoverHelpMorph contents: 'This is a HoverHelpMorph') openInHand
	"

	^ self new contents: aString wrappedTo: nil! !

!methodRemoval: HoverHelpMorph class #contentsWrapped: stamp: 'jmv 2/23/2022 09:49:52'!
HoverHelpMorph class removeSelector: #contentsWrapped:!
!methodRemoval: HoverHelpMorph #contents: stamp: 'jmv 2/23/2022 09:48:06'!
HoverHelpMorph removeSelector: #contents:!
!methodRemoval: HoverHelpMorph #contents:doWordWrap: stamp: 'jmv 2/23/2022 09:48:53'!
HoverHelpMorph removeSelector: #contents:doWordWrap:!
!methodRemoval: HoverHelpMorph #contentsWrapped: stamp: 'jmv 2/23/2022 09:49:55'!
HoverHelpMorph removeSelector: #contentsWrapped:!
